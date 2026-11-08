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
package org.neo4j.cypher.internal.runtime.interpreted.pipes

import org.neo4j.cypher.internal.expressions.PopFromStack
import org.neo4j.cypher.internal.expressions.PushOntoStack
import org.neo4j.cypher.internal.expressions.{Expression => AstExpression}
import org.neo4j.cypher.internal.logical.plans.Repeat
import org.neo4j.cypher.internal.runtime.ReadableRow
import org.neo4j.cypher.internal.runtime.interpreted.commands.expressions.{Expression => CompiledExpression}
import org.neo4j.memory.HeapEstimator
import org.neo4j.values.AnyValue
import org.neo4j.values.Equality
import org.neo4j.values.storable.Values

/**
 * A compiled stack action evaluated once per repetition of a quantified path pattern.
 *
 * A stack is either positive (keys that were pushed and not popped yet) or negative (pops that did
 * not find a key to pop). A stack is "signed" if it may become negative: a POP on an empty stack
 * then pushes its key onto the negative side instead of pruning the path, and a later PUSH pops the
 * negative side again as long as the keys match.
 *
 * `applyTo` returns the resulting stacks, or `None` if the action prunes the path.
 */
sealed trait StackActionRuntime {

  def stackName: String

  /**
   * Whether the stack this action is applied to must be balanced (depth zero) for the quantified
   * path pattern to emit a result.
   */
  def requiresBalance: Boolean

  /** Whether the stack this action is applied to may have a negative depth. */
  def signed: Boolean

  /** @return a copy of this action where the stack allows a negative depth. */
  def withSigned: StackActionRuntime

  /** @return the resulting stacks together with the heap this action added or freed, in O(1). */
  def applyToWithHeapDelta(
    stacks: Map[String, StackActionRuntime.StackValue],
    row: ReadableRow,
    state: QueryState
  ): Option[StackActionRuntime.Applied]

  /** @return the resulting stacks, or `None` if the action prunes the path. */
  final def applyTo(
    stacks: Map[String, StackActionRuntime.StackValue],
    row: ReadableRow,
    state: QueryState
  ): Option[Map[String, StackActionRuntime.StackValue]] =
    applyToWithHeapDelta(stacks, row, state).map(_.stacks)
}

object StackActionRuntime {

  /** Name used for an anonymous stack (no explicit `STACK ... <name>: ...` name). */
  val DefaultStackName: String = ""

  /**
   * The state of one stack. At most one of the two sides is non-empty: a PUSH cancels the negative
   * side first and a POP empties the positive side first.
   *
   * @param positive keys that were pushed and not popped yet, newest first. A keyless push is stored
   *                 as `NO_VALUE`, which no keyed pop can match.
   * @param negative entries of pops that did not find a key to pop, newest first. `None` represents
   *                 a keyless pop, i.e. "pop anything"; a keyed pop that evaluated to a null key is
   *                 stored as `Some(NO_VALUE)` and only matches a push with an equal key.
   */
  final case class StackValue(positive: List[AnyValue], negative: List[Option[AnyValue]]) {
    def depth: Int = positive.length - negative.length
    def isEmpty: Boolean = positive.isEmpty && negative.isEmpty
  }

  object StackValue {
    val Empty: StackValue = StackValue(Nil, Nil)

    final val SHALLOW_SIZE: Long = HeapEstimator.shallowSizeOfInstance(classOf[StackValue])

    /** Estimated shallow size of one `::` cell of the immutable lists held by a stack. */
    final val CONS_CELL_SIZE: Long = 24L
  }

  /** Estimated shallow size of the map holding the stacks, per stack. */
  final val STACK_MAP_ENTRY_SIZE: Long = 40L

  /** The result of applying one stack action: the new stacks and the net heap it added. */
  final case class Applied(stacks: Map[String, StackValue], heapDelta: Long)

  /**
   * Estimated retained heap of the given stacks. Because the lists are immutable and shared between
   * repeat states, this over-estimates whenever states share a tail. Callers should prefer the
   * incremental [[Applied.heapDelta]], which is O(1) per action.
   */
  def estimatedHeapUsage(stacks: Map[String, StackValue]): Long = {
    var total = 0L
    val it = stacks.iterator
    while (it.hasNext) {
      val (name, value) = it.next()
      total += stackOverhead(name)
      total += (value.positive.size + value.negative.size) * StackValue.CONS_CELL_SIZE
    }
    total
  }

  /** Heap accounted for a stack the first time it appears in the map. */
  private def stackOverhead(name: String): Long =
    StackValue.SHALLOW_SIZE + STACK_MAP_ENTRY_SIZE + 2L * name.length

  /**
   * @return the stack as it currently is, together with the overhead to charge if it is not in the map yet.
   */
  private def lookup(
    stacks: Map[String, StackValue],
    name: String
  ): (StackValue, Long) =
    stacks.get(name) match {
      case Some(stack) => (stack, 0L)
      case None        => (StackValue.Empty, stackOverhead(name))
    }

  /**
   * Compile the stack actions of the relationships directly contained in a quantified path pattern.
   * The actions are carried on the [[Repeat]] in the order they are written in the pattern, so they
   * can be applied in that order independent of the direction the inner plan was solved in.
   */
  def compile(
    repeat: Repeat,
    toCommandExpression: AstExpression => CompiledExpression
  ): Array[StackActionRuntime] = {
    val actions = repeat.stackActions.map { action =>
      val stackName = action.stack.map(_.name).getOrElse(DefaultStackName)
      action match {
        case _: PushOntoStack =>
          Push(stackName, action.key.map(toCommandExpression), action.requiresBalance, action.signed)
        case _: PopFromStack =>
          Pop(stackName, action.key.map(toCommandExpression), action.requiresBalance, action.signed)
      }
    }.toArray

    // `SIGNED` is a property of the stack, not of an individual action: if any action of a stack is
    // signed, the stack may become negative for all of its actions.
    val signedStacks = actions.collect { case action if action.signed => action.stackName }.toSet
    if (signedStacks.isEmpty) actions
    else actions.map(action => if (signedStacks.contains(action.stackName)) action.withSigned else action)
  }

  /**
   * @return whether all stacks that are annotated `BALANCED` are empty (depth zero).
   */
  def areRequiredStacksBalanced(
    actions: Array[StackActionRuntime],
    stacks: Map[String, StackValue]
  ): Boolean = {
    var i = 0
    while (i < actions.length) {
      val action = actions(i)
      if (action.requiresBalance && !stacks.getOrElse(action.stackName, StackValue.Empty).isEmpty)
        return false
      i += 1
    }
    true
  }

  /**
   * Apply `actions` in order to `stacks`. Returns `None` if any action prunes the path.
   * The heap delta is accumulated incrementally, so applying n actions is O(n).
   */
  def applyAll(
    actions: Array[StackActionRuntime],
    stacks: Map[String, StackValue],
    row: ReadableRow,
    state: QueryState
  ): Option[Applied] = {
    var current = stacks
    var heapDelta = 0L
    var i = 0
    while (i < actions.length) {
      actions(i).applyToWithHeapDelta(current, row, state) match {
        case Some(applied) =>
          current = applied.stacks
          heapDelta += applied.heapDelta
        case None => return None
      }
      i += 1
    }
    Some(Applied(current, heapDelta))
  }

  private def valueOf(key: Option[CompiledExpression], row: ReadableRow, state: QueryState): AnyValue =
    key.map(_(row, state)).getOrElse(Values.NO_VALUE)

  /**
   * A negative entry is created by a POP that did not find a key to pop. It matches a later PUSH if
   * either side has no key: a keyless POP means "pop anything", so any PUSH may cancel it, and a
   * keyless PUSH does not constrain the key. This is intentionally not symmetric to the positive
   * side, where a keyless PUSH pushes `NO_VALUE` and a keyed POP cannot match it.
   */
  private def negativeMatches(
    key: Option[CompiledExpression],
    negativeTop: Option[AnyValue],
    row: ReadableRow,
    state: QueryState
  ): Boolean =
    key match {
      case None => true
      // The recorded POP was keyless, so it pops anything.
      case Some(_) if negativeTop.isEmpty => true
      case Some(expr) =>
        negativeTop.get.ternaryEquals(expr(row, state)) == Equality.TRUE
    }

  final case class Push(
    stackName: String,
    key: Option[CompiledExpression],
    requiresBalance: Boolean = false,
    signed: Boolean = false
  ) extends StackActionRuntime {

    override def withSigned: StackActionRuntime = if (signed) this else copy(signed = true)

    override def applyToWithHeapDelta(
      stacks: Map[String, StackValue],
      row: ReadableRow,
      state: QueryState
    ): Option[Applied] = {
      val (stack, newStackOverhead) = lookup(stacks, stackName)
      if (stack.negative.nonEmpty) {
        if (negativeMatches(key, stack.negative.head, row, state))
          Some(Applied(
            stacks.updated(stackName, stack.copy(negative = stack.negative.tail)),
            newStackOverhead - StackValue.CONS_CELL_SIZE
          ))
        else None
      } else {
        Some(Applied(
          stacks.updated(stackName, stack.copy(positive = valueOf(key, row, state) :: stack.positive)),
          newStackOverhead + StackValue.CONS_CELL_SIZE
        ))
      }
    }
  }

  final case class Pop(
    stackName: String,
    key: Option[CompiledExpression],
    requiresBalance: Boolean = false,
    signed: Boolean = false
  ) extends StackActionRuntime {

    override def withSigned: StackActionRuntime = if (signed) this else copy(signed = true)

    override def applyToWithHeapDelta(
      stacks: Map[String, StackValue],
      row: ReadableRow,
      state: QueryState
    ): Option[Applied] = {
      val (stack, newStackOverhead) = lookup(stacks, stackName)
      if (stack.positive.nonEmpty) {
        (key, stack.positive) match {
          case (None, _ :: rest) =>
            Some(Applied(
              stacks.updated(stackName, stack.copy(positive = rest)),
              newStackOverhead - StackValue.CONS_CELL_SIZE
            ))
          case (Some(expr), top :: rest) if top.ternaryEquals(expr(row, state)) == Equality.TRUE =>
            Some(Applied(
              stacks.updated(stackName, stack.copy(positive = rest)),
              newStackOverhead - StackValue.CONS_CELL_SIZE
            ))
          case _ => None
        }
      } else if (signed) {
        // A pop on an empty stack pushes onto the negative side instead of pruning the path.
        Some(Applied(
          stacks.updated(
            stackName,
            stack.copy(negative = key.map(_(row, state)) :: stack.negative)
          ),
          newStackOverhead + StackValue.CONS_CELL_SIZE
        ))
      } else {
        None
      }
    }
  }
}
