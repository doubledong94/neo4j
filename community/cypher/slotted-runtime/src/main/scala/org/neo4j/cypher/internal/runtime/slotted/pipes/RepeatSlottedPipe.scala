/*
 * Copyright (c) "Neo4j"
 * Neo4j Sweden AB [https://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
package org.neo4j.cypher.internal.runtime.slotted.pipes

import org.neo4j.collection.trackable.HeapTrackingArrayList
import org.neo4j.collection.trackable.HeapTrackingCollections
import org.neo4j.collection.trackable.HeapTrackingCollections.newArrayDeque
import org.neo4j.collection.trackable.HeapTrackingLongHashSet
import org.neo4j.cypher.internal.physicalplanning.Slot
import org.neo4j.cypher.internal.physicalplanning.SlotConfiguration
import org.neo4j.cypher.internal.physicalplanning.SlotConfigurationUtils.makeGetPrimitiveNodeFromSlotFunctionFor
import org.neo4j.cypher.internal.runtime.CastSupport.castOrFail
import org.neo4j.cypher.internal.runtime.ClosingIterator
import org.neo4j.cypher.internal.runtime.CypherRow
import org.neo4j.cypher.internal.runtime.NestedGroupValues
import org.neo4j.cypher.internal.runtime.PrefetchingIterator
import org.neo4j.cypher.internal.runtime.ReadableRow
import org.neo4j.cypher.internal.runtime.RuntimeMetadataValue
import org.neo4j.cypher.internal.runtime.WritableRow
import org.neo4j.cypher.internal.runtime.interpreted.commands.expressions.Expression
import org.neo4j.cypher.internal.runtime.interpreted.pipes.Pipe
import org.neo4j.cypher.internal.runtime.interpreted.pipes.PipeWithSource
import org.neo4j.cypher.internal.runtime.interpreted.pipes.QueryState
import org.neo4j.cypher.internal.runtime.interpreted.pipes.RepeatPipe.emptyLists
import org.neo4j.cypher.internal.runtime.interpreted.pipes.StackActionRuntime
import org.neo4j.cypher.internal.runtime.slotted.SlottedRow
import org.neo4j.cypher.internal.runtime.slotted.expressions.AcyclicState
import org.neo4j.cypher.internal.runtime.slotted.expressions.TrailState
import org.neo4j.cypher.internal.runtime.slotted.helpers.NullChecker
import org.neo4j.cypher.internal.runtime.slotted.pipes.RepeatSlottedPipe.AcyclicModeConstraint
import org.neo4j.cypher.internal.runtime.slotted.pipes.RepeatSlottedPipe.SlottedAllReduceAcc
import org.neo4j.cypher.internal.runtime.slotted.pipes.RepeatSlottedPipe.TrailModeConstraint
import org.neo4j.cypher.internal.runtime.slotted.pipes.RepeatSlottedPipe.TraversalModeConstraint
import org.neo4j.cypher.internal.runtime.slotted.pipes.RepeatSlottedPipe.WalkModeConstraint
import org.neo4j.cypher.internal.util.Repetition
import org.neo4j.cypher.internal.util.attribution.Id
import org.neo4j.memory.HeapEstimator
import org.neo4j.memory.Measurable
import org.neo4j.memory.MemoryTracker
import org.neo4j.values.AnyValue
import org.neo4j.values.virtual.ListValue
import org.neo4j.values.virtual.VirtualNodeValue
import org.neo4j.values.virtual.VirtualValues

import scala.annotation.tailrec

sealed trait SlottedRepeatState {
  val node: Long
  val groupNodes: HeapTrackingArrayList[ListValue]
  val groupRelationships: HeapTrackingArrayList[ListValue]
  val iterations: Int
  val accumulators: Array[AnyValue]
  val stacks: Map[String, StackActionRuntime.StackValue]

  /** Heap allocated for [[stacks]] when this state was created, released again on close. */
  val stacksHeapUsage: Long

  protected def releaseStacks(tracker: MemoryTracker): Unit =
    if (stacksHeapUsage != 0L) tracker.releaseHeap(stacksHeapUsage)

  def close(tracker: MemoryTracker): Unit
}

case class SlottedAcyclicState(
  constraint: AcyclicModeConstraint,
  node: Long,
  groupNodes: HeapTrackingArrayList[ListValue],
  nodesSeen: HeapTrackingLongHashSet,
  groupRelationships: HeapTrackingArrayList[ListValue],
  relationshipsSeen: HeapTrackingLongHashSet,
  iterations: Int,
  closeGroupsOnClose: Boolean,
  accumulators: Array[AnyValue],
  stacks: Map[String, StackActionRuntime.StackValue],
  stacksHeapUsage: Long = 0L
) extends AcyclicState with SlottedRepeatState with Measurable {

  override def estimatedHeapUsage(): Long =
    SlottedAcyclicState.SHALLOW_SIZE + StackActionRuntime.estimatedHeapUsage(stacks)

  def close(tracker: MemoryTracker): Unit = {
    if (closeGroupsOnClose) {
      groupNodes.close()
      groupRelationships.close()
    }
    nodesSeen.close()
    relationshipsSeen.close()
    releaseStacks(tracker)
  }
}

object SlottedAcyclicState {
  final val SHALLOW_SIZE: Long = HeapEstimator.shallowSizeOfInstance(classOf[SlottedAcyclicState])
}

case class SlottedTrailState(
  constraint: TrailModeConstraint,
  node: Long,
  groupNodes: HeapTrackingArrayList[ListValue],
  groupRelationships: HeapTrackingArrayList[ListValue],
  relationshipsSeen: HeapTrackingLongHashSet,
  iterations: Int,
  closeGroupsOnClose: Boolean,
  accumulators: Array[AnyValue],
  stacks: Map[String, StackActionRuntime.StackValue],
  stacksHeapUsage: Long = 0L
) extends TrailState with SlottedRepeatState with Measurable {

  override def estimatedHeapUsage(): Long =
    SlottedTrailState.SHALLOW_SIZE + StackActionRuntime.estimatedHeapUsage(stacks)

  def close(tracker: MemoryTracker): Unit = {
    if (closeGroupsOnClose) {
      groupNodes.close()
      groupRelationships.close()
    }
    relationshipsSeen.close()
    releaseStacks(tracker)
  }
}

object SlottedTrailState {
  final val SHALLOW_SIZE: Long = HeapEstimator.shallowSizeOfInstance(classOf[SlottedTrailState])
}

case class SlottedWalkState(
  node: Long,
  groupNodes: HeapTrackingArrayList[ListValue],
  groupRelationships: HeapTrackingArrayList[ListValue],
  iterations: Int,
  closeGroupsOnClose: Boolean,
  accumulators: Array[AnyValue],
  stacks: Map[String, StackActionRuntime.StackValue],
  stacksHeapUsage: Long = 0L
) extends SlottedRepeatState {

  def close(tracker: MemoryTracker): Unit = {
    if (closeGroupsOnClose) {
      groupNodes.close()
      groupRelationships.close()
    }
    releaseStacks(tracker)
  }
}

case class RepeatSlottedPipe(
  source: Pipe,
  inner: Pipe,
  repetition: Repetition,
  startSlot: Slot,
  endSlot: Slot,
  innerStartOffset: Int,
  innerEndSlot: Slot,
  groupNodes: Array[GroupSlot],
  groupRelationships: Array[GroupSlot],
  uniquenessConstraint: TraversalModeConstraint,
  slots: SlotConfiguration,
  rhsSlots: SlotConfiguration,
  argumentSize: SlotConfiguration.Size,
  reverseGroupVariableProjections: Boolean,
  nodeInScope: Boolean,
  accumulatorMappings: Array[SlottedAllReduceAcc],
  stackActions: Array[StackActionRuntime] = Array.empty
)(val id: Id = Id.INVALID_ID) extends PipeWithSource(source) {

  private[this] val emptyGroupNodes = emptyLists(groupNodes.length)
  private[this] val emptyGroupRelationships = emptyLists(groupRelationships.length)
  private[this] val getStartNodeFunction = makeGetPrimitiveNodeFromSlotFunctionFor(startSlot)
  private[this] val getEndNodeFunction = makeGetPrimitiveNodeFromSlotFunctionFor(endSlot)
  private[this] val previousAccumulatorSlotOffsets = accumulatorMappings.map(_.previous.offset)
  private[this] val nextAccumulatorSlotOffsets = accumulatorMappings.map(_.next.offset)

  private def createNewState(
    outerRowForReading: ReadableRow,
    startNode: Long,
    tracker: MemoryTracker,
    state: QueryState
  ): SlottedRepeatState = {
    val initialAccumulatorValues = accumulatorMappings.map(_.initial(outerRowForReading, state))
    uniquenessConstraint match {
      case constraint @ RepeatSlottedPipe.AcyclicModeConstraint(
          _,
          _,
          previouslyBoundRelationships,
          previouslyBoundRelationshipGroups,
          _,
          previouslyBoundNodes,
          previouslyBoundNodeGroups
        ) =>
        val relationshipsSeen = HeapTrackingCollections.newLongSet(tracker)
        val ir = previouslyBoundRelationships.iterator
        while (ir.hasNext) {
          relationshipsSeen.add(outerRowForReading.getLongAt(ir.next().offset))
        }

        val ig = previouslyBoundRelationshipGroups.iterator
        while (ig.hasNext) {
          NestedGroupValues.foreachRelationshipId(outerRowForReading.getRefAt(ig.next().offset))(relationshipsSeen.add)
        }

        val nodesSeen = HeapTrackingCollections.newLongSet(tracker)
        val nodeIterator = previouslyBoundNodes.iterator
        while (nodeIterator.hasNext) {
          nodesSeen.add(outerRowForReading.getLongAt(nodeIterator.next().offset))
        }

        val nodeGroupIterator = previouslyBoundNodeGroups.iterator
        while (nodeGroupIterator.hasNext) {
          val i = castOrFail[ListValue](outerRowForReading.getRefAt(nodeGroupIterator.next().offset)).iterator()
          while (i.hasNext) {
            nodesSeen.add(castOrFail[VirtualNodeValue](i.next()).id())
          }
        }

        SlottedAcyclicState(
          constraint,
          startNode,
          groupNodes = emptyGroupNodes,
          nodesSeen = nodesSeen,
          groupRelationships = emptyGroupRelationships,
          relationshipsSeen = relationshipsSeen,
          iterations = 1,
          // empty groups are reused for every argument, so can not be closed until the whole query finishes
          closeGroupsOnClose = false,
          accumulators = initialAccumulatorValues,
          stacks = Map.empty
        )
      case constraint @ RepeatSlottedPipe.TrailModeConstraint(
          _,
          _,
          previouslyBoundRelationships,
          previouslyBoundRelationshipGroups
        ) =>
        val relationshipsSeen = HeapTrackingCollections.newLongSet(tracker)
        val ir = previouslyBoundRelationships.iterator
        while (ir.hasNext) {
          relationshipsSeen.add(outerRowForReading.getLongAt(ir.next().offset))
        }

        val ig = previouslyBoundRelationshipGroups.iterator
        while (ig.hasNext) {
          NestedGroupValues.foreachRelationshipId(outerRowForReading.getRefAt(ig.next().offset))(relationshipsSeen.add)
        }

        SlottedTrailState(
          constraint,
          startNode,
          emptyGroupNodes,
          emptyGroupRelationships,
          relationshipsSeen,
          iterations = 1,
          // empty groups are reused for every argument, so can not be closed until the whole query finishes
          closeGroupsOnClose = false,
          accumulators = initialAccumulatorValues,
          stacks = Map.empty
        )

      case WalkModeConstraint =>
        SlottedWalkState(
          startNode,
          emptyGroupNodes,
          emptyGroupRelationships,
          iterations = 1,
          // empty groups are reused for every argument, so can not be closed until the whole query finishes
          closeGroupsOnClose = false,
          accumulators = initialAccumulatorValues,
          stacks = Map.empty
        )
    }
  }

  /**
   * A nested quantified path pattern exposes the relationships it matched through its group variable, which holds a
   * list of the values bound by one repetition of the inner pattern. Since quantified path patterns may be nested
   * arbitrarily deep, that value may in turn be a list of lists. Runs `f` for every relationship id, short-circuiting
   * as soon as `f` returns false.
   */
  private def forEachInnerRelationshipId(row: ReadableRow, slot: Slot)(f: Long => Boolean): Boolean =
    if (slot.isLongSlot) {
      f(row.getLongAt(slot.offset))
    } else {
      NestedGroupValues.foreachRelationshipId(row.getRefAt(slot.offset))(f)
    }

  private def createNextState(
    state: SlottedRepeatState,
    rowForReading: ReadableRow,
    innerEndNode: Long,
    tracker: MemoryTracker,
    newStacks: Map[String, StackActionRuntime.StackValue],
    newStacksHeapUsage: Long
  ): SlottedRepeatState = {
    val accumulatorValues = new Array[AnyValue](accumulatorMappings.length)
    var i = 0
    while (i < accumulatorMappings.length) {
      accumulatorValues(i) = rowForReading.getRefAt(nextAccumulatorSlotOffsets(i))
      i += 1
    }

    // Only account for the memory the new state adds on top of its parent. The immutable stack lists
    // share their tails, so a state must not be charged for the whole depth it references; the delta
    // is accumulated incrementally by the stack actions.
    tracker.allocateHeap(newStacksHeapUsage)

    state match {
      case acyclicState: SlottedAcyclicState =>
        val newRelationshipsSeen = HeapTrackingCollections.newLongSet(tracker, acyclicState.relationshipsSeen)
        val newNodesSeen = HeapTrackingCollections.newLongSet(tracker, acyclicState.nodesSeen)

        var i = 0
        while (i < acyclicState.constraint.innerRelationships.length) {
          val r = acyclicState.constraint.innerRelationships(i)
          if (!forEachInnerRelationshipId(rowForReading, r)(newRelationshipsSeen.add)) {
            throw new IllegalStateException(
              "Method should only be called when all relationships are known to be unique"
            )
          }
          i += 1
        }
        if (reverseGroupVariableProjections) {
          i = acyclicState.constraint.innerNodes.length - 1
          while (i > 0) {
            val n = acyclicState.constraint.innerNodes(i)
            if (
              !newNodesSeen.add(rowForReading.getLongAt(n.offset)) && i != acyclicState.constraint.innerNodes.length
            ) {
              throw new IllegalStateException(
                "Method should only be called when all nodes are known to be unique"
              )
            }
            i -= 1
          }
        } else {
          i = 0
          while (i < acyclicState.constraint.innerNodes.length) {
            val n = acyclicState.constraint.innerNodes(i)
            if (!newNodesSeen.add(rowForReading.getLongAt(n.offset)) && i != 0) {
              throw new IllegalStateException(
                "Method should only be called when all nodes are known to be unique"
              )
            }
            i += 1
          }
        }
        SlottedAcyclicState(
          acyclicState.constraint,
          innerEndNode,
          groupNodes =
            RepeatSlottedPipe.computeNodeGroupVariables(groupNodes, acyclicState.groupNodes, rowForReading, tracker),
          nodesSeen = newNodesSeen,
          groupRelationships = RepeatSlottedPipe.computeRelGroupVariables(
            groupRelationships,
            acyclicState.groupRelationships,
            rowForReading,
            tracker
          ),
          relationshipsSeen = newRelationshipsSeen,
          acyclicState.iterations + 1,
          closeGroupsOnClose = true,
          accumulatorValues,
          newStacks,
          newStacksHeapUsage
        )
      case trailState: SlottedTrailState =>
        val newSet = HeapTrackingCollections.newLongSet(tracker, trailState.relationshipsSeen)

        var i = 0
        while (i < trailState.constraint.innerRelationships.length) {
          val r = trailState.constraint.innerRelationships(i)
          if (!forEachInnerRelationshipId(rowForReading, r)(newSet.add)) {
            throw new IllegalStateException(
              "Method should only be called when all relationships are known to be unique"
            )
          }
          i += 1
        }

        SlottedTrailState(
          trailState.constraint,
          innerEndNode,
          RepeatSlottedPipe.computeNodeGroupVariables(groupNodes, trailState.groupNodes, rowForReading, tracker),
          RepeatSlottedPipe.computeRelGroupVariables(
            groupRelationships,
            trailState.groupRelationships,
            rowForReading,
            tracker
          ),
          newSet,
          trailState.iterations + 1,
          closeGroupsOnClose = true,
          accumulatorValues,
          newStacks,
          newStacksHeapUsage
        )

      case walkState: SlottedWalkState =>
        SlottedWalkState(
          innerEndNode,
          RepeatSlottedPipe.computeNodeGroupVariables(groupNodes, walkState.groupNodes, rowForReading, tracker),
          RepeatSlottedPipe.computeRelGroupVariables(
            groupRelationships,
            walkState.groupRelationships,
            rowForReading,
            tracker
          ),
          walkState.iterations + 1,
          closeGroupsOnClose = true,
          accumulatorValues,
          newStacks,
          newStacksHeapUsage
        )
    }
  }

  override protected def internalCreateResults(
    input: ClosingIterator[CypherRow],
    state: QueryState
  ): ClosingIterator[CypherRow] = {

    val tracker = state.memoryTrackerForOperatorProvider.memoryTrackerForOperator(id.x)
    input.flatMap { (outerRow: ReadableRow) =>
      def newResultRowWithEmptyGroups(innerEndNode: Long): SlottedRow = {
        val resultRow = SlottedRow(slots)
        resultRow.copyFrom(outerRow, argumentSize.nLongs, argumentSize.nReferences)
        RepeatSlottedPipe.writeResultColumnsWithProvidedGroups(
          emptyGroupNodes,
          emptyGroupRelationships,
          innerEndNode,
          resultRow,
          groupNodes,
          groupRelationships,
          endSlot.offset,
          nodeInScope
        )
        resultRow
      }

      def newResultRow(
        rhsInnerRow: CypherRow,
        prevRepetitionGroupNodes: HeapTrackingArrayList[ListValue],
        prevRepetitionGroupRelationships: HeapTrackingArrayList[ListValue],
        innerEndNode: Long,
        accumulatorValues: Array[AnyValue]
      ): Some[CypherRow] = {
        RepeatSlottedPipe.writeResultColumns(
          prevRepetitionGroupNodes,
          prevRepetitionGroupRelationships,
          innerEndNode,
          rhsInnerRow,
          groupNodes,
          groupRelationships,
          endSlot.offset,
          reverseGroupVariableProjections,
          nodeInScope
        )
        Some(rhsInnerRow)
      }

      val startNode = getStartNodeFunction.applyAsLong(outerRow)
      if (NullChecker.entityIsNull(startNode)) {
        ClosingIterator.empty
      } else {
        // Every invocation of RHS for this incoming LHS row can use the same argument row (avoids unnecessary row creation)
        val rhsInitialRow = SlottedRow(rhsSlots)
        rhsInitialRow.copyFrom(outerRow, argumentSize.nLongs, argumentSize.nReferences)

        val stack = newArrayDeque[SlottedRepeatState](tracker)
        if (repetition.max.isGreaterThan(0)) {
          stack.push(createNewState(outerRowForReading = outerRow, startNode, tracker, state))
        }
        new PrefetchingIterator[CypherRow] {
          private var innerResult: ClosingIterator[CypherRow] = ClosingIterator.empty
          private var stackHead: SlottedRepeatState = _
          private var emitFirst = repetition.min == 0

          override protected[this] def closeMore(): Unit = {
            if (stackHead != null) {
              stackHead.close(tracker)
            }
            // States still waiting on the stack were never popped, so close them here as well.
            while (!stack.isEmpty) {
              stack.pop().close(tracker)
            }
            innerResult.close()
            stack.close()
          }

          private def allocateZeroRepetitionResultRowOrNull(): CypherRow = {
            if (emitFirst) {
              emitFirst = false
              val resultRow = newResultRowWithEmptyGroups(startNode)
              if (testEndNode(resultRow, startNode)) {
                resultRow
              } else {
                null
              }
            } else {
              null
            }
          }

          @tailrec
          def produceNext(): Option[CypherRow] = {
            val firstRowOrNull = allocateZeroRepetitionResultRowOrNull()
            if (firstRowOrNull != null) {
              Some(firstRowOrNull)
            } else if (innerResult.hasNext) {
              val row = innerResult.next()
              val innerEndNode = row.getLongAt(innerEndSlot.offset)
              val nextStacksOpt: Option[StackActionRuntime.Applied] =
                if (stackActions.isEmpty) None else applyStackActions(stackHead.stacks, row, state)
              if (nextStacksOpt.isEmpty && stackActions.nonEmpty) {
                // A stack action pruned this repetition
                produceNext()
              } else {
                val (nextStacks, nextStacksHeapUsage) = nextStacksOpt match {
                  case Some(applied) => (applied.stacks, math.max(0L, applied.heapDelta))
                  case None          => (stackHead.stacks, 0L)
                }
                if (repetition.max.isGreaterThan(stackHead.iterations)) {
                  stack.push(createNextState(
                    stackHead,
                    rowForReading = row,
                    innerEndNode,
                    tracker,
                    nextStacks,
                    nextStacksHeapUsage
                  ))
                }
                val balanced = StackActionRuntime.areRequiredStacksBalanced(stackActions, nextStacks)
                // if iterated long enough emit, otherwise recurse
                if (stackHead.iterations >= repetition.min && balanced && testEndNode(row, innerEndNode)) {
                  newResultRow(
                    row,
                    stackHead.groupNodes,
                    stackHead.groupRelationships,
                    innerEndNode,
                    stackHead.accumulators
                  )
                } else {
                  produceNext()
                }
              }
            } else if (!stack.isEmpty) {
              // close previous state
              if (stackHead != null) {
                stackHead.close(tracker)
              }
              // Run RHS with previous end-node as new innerStartNode
              stackHead = stack.pop()
              rhsInitialRow.setLongAt(innerStartOffset, stackHead.node)

              stackHead match {
                case a: SlottedAcyclicState =>
                  rhsInitialRow.setRefAt(
                    a.constraint.acyclicStateMetadataSlot,
                    RuntimeMetadataValue(a)
                  )
                case t: SlottedTrailState =>
                  rhsInitialRow.setRefAt(t.constraint.trailStateMetadataSlot, RuntimeMetadataValue(t))
                case _: SlottedWalkState => ()
              }

              // Set initial accumulator values
              var i = 0
              while (i < previousAccumulatorSlotOffsets.length) {
                rhsInitialRow.setRefAt(previousAccumulatorSlotOffsets(i), stackHead.accumulators(i))
                i += 1
              }
              val innerState = state.withInitialContext(rhsInitialRow)
              innerResult = inner.createResults(innerState)
              produceNext()
            } else {
              if (stackHead != null) {
                stackHead.close(tracker)
                stackHead = null
              }
              None
            }
          }
        }
      }
    }
  }

  private def testEndNode(row: CypherRow, endNode: Long): Boolean = {
    !nodeInScope || getEndNodeFunction.applyAsLong(row) == endNode
  }

  private def applyStackActions(
    stacks: Map[String, StackActionRuntime.StackValue],
    row: ReadableRow,
    state: QueryState
  ): Option[StackActionRuntime.Applied] =
    StackActionRuntime.applyAll(stackActions, stacks, row, state)
}

object RepeatSlottedPipe {

  case class SlottedAllReduceAcc(initial: Expression, previous: Slot, next: Slot)

  sealed trait TraversalModeConstraint

  case class TrailModeConstraint(
    trailStateMetadataSlot: Int,
    innerRelationships: Array[Slot],
    previouslyBoundRelationships: Array[Slot],
    previouslyBoundRelationshipGroups: Array[Slot]
  ) extends TraversalModeConstraint

  case class AcyclicModeConstraint(
    acyclicStateMetadataSlot: Int,
    innerRelationships: Array[Slot],
    previouslyBoundRelationships: Array[Slot],
    previouslyBoundRelationshipGroups: Array[Slot],
    innerNodes: Array[Slot],
    previouslyBoundNodes: Array[Slot],
    previouslyBoundNodeGroups: Array[Slot]
  ) extends TraversalModeConstraint

  case object WalkModeConstraint extends TraversalModeConstraint

  /**
   * Read the value of a group slot in the inner row. For a plain entity this is the id in the long slot; for a
   * nested quantified path pattern the slot is a reference to the nested group variable, which already is a list
   * and is appended as-is, so that nested group variables become nested lists.
   */
  private def groupValue(row: ReadableRow, slot: Slot, isRelationship: Boolean): AnyValue =
    if (slot.isLongSlot) {
      val id = row.getLongAt(slot.offset)
      if (isRelationship) VirtualValues.relationship(id) else VirtualValues.node(id)
    } else {
      row.getRefAt(slot.offset)
    }

  def computeNodeGroupVariables(
    groupNodeSlots: Array[GroupSlot],
    groupVariables: HeapTrackingArrayList[ListValue],
    row: ReadableRow,
    tracker: MemoryTracker
  ): HeapTrackingArrayList[ListValue] = {
    val res = HeapTrackingCollections.newArrayList[ListValue](groupVariables.size(), tracker)
    var i = 0
    while (i < groupNodeSlots.length) {
      res.add(groupVariables.get(i).append(groupValue(row, groupNodeSlots(i).innerSlot, isRelationship = false)))
      i += 1
    }
    res
  }

  def computeRelGroupVariables(
    groupRelSlots: Array[GroupSlot],
    groupVariables: HeapTrackingArrayList[ListValue],
    row: ReadableRow,
    tracker: MemoryTracker
  ): HeapTrackingArrayList[ListValue] = {
    val res = HeapTrackingCollections.newArrayList[ListValue](groupVariables.size(), tracker)
    var i = 0
    while (i < groupRelSlots.length) {
      res.add(groupVariables.get(i).append(groupValue(row, groupRelSlots(i).innerSlot, isRelationship = true)))
      i += 1
    }
    res
  }

  def writeResultColumns(
    groupNodes: HeapTrackingArrayList[ListValue],
    groupRels: HeapTrackingArrayList[ListValue],
    innerEndNode: Long,
    row: CypherRow,
    groupNodeSlots: Array[GroupSlot],
    groupRelSlots: Array[GroupSlot],
    endOffset: Int,
    reverseGroupVariableProjections: Boolean,
    nodeInScope: Boolean
  ): Unit = {
    var i = 0
    while (i < groupNodeSlots.length) {
      val nodeGroup = groupNodes.get(i).append(groupValue(row, groupNodeSlots(i).innerSlot, isRelationship = false))
      val projectedNodeGroup = if (reverseGroupVariableProjections) nodeGroup.reverse() else nodeGroup
      row.setRefAt(groupNodeSlots(i).outerSlot.offset, projectedNodeGroup)
      i += 1
    }

    i = 0
    while (i < groupRelSlots.length) {
      val relGroup = groupRels.get(i).append(groupValue(row, groupRelSlots(i).innerSlot, isRelationship = true))
      val projectedRelGroup = if (reverseGroupVariableProjections) relGroup.reverse() else relGroup
      row.setRefAt(groupRelSlots(i).outerSlot.offset, projectedRelGroup)
      i += 1
    }

    if (!nodeInScope) {
      row.setLongAt(endOffset, innerEndNode)
    }
  }

  def writeResultColumnsWithProvidedGroups(
    groupNodes: HeapTrackingArrayList[ListValue],
    groupRels: HeapTrackingArrayList[ListValue],
    innerEndNode: Long,
    resultRow: WritableRow,
    groupNodeSlots: Array[GroupSlot],
    groupRelSlots: Array[GroupSlot],
    endOffset: Int,
    nodeInScope: Boolean
  ): Unit = {
    var i = 0
    while (i < groupNodeSlots.length) {
      resultRow.setRefAt(groupNodeSlots(i).outerSlot.offset, groupNodes.get(i))
      i += 1
    }
    i = 0
    while (i < groupRelSlots.length) {
      resultRow.setRefAt(groupRelSlots(i).outerSlot.offset, groupRels.get(i))
      i += 1
    }
    if (!nodeInScope) {
      resultRow.setLongAt(endOffset, innerEndNode)
    }
  }
}

case class GroupSlot(innerSlot: Slot, outerSlot: Slot)
