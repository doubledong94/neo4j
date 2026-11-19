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
package org.neo4j.cypher.internal.runtime.interpreted.commands.expressions

import org.neo4j.cypher.internal.runtime.ReadableRow
import org.neo4j.cypher.internal.runtime.interpreted.commands.AstNode
import org.neo4j.cypher.internal.runtime.interpreted.commands.expressions.ProjectedPath.Projector
import org.neo4j.cypher.internal.runtime.interpreted.pipes.QueryState
import org.neo4j.cypher.operations.CypherTypeValueMapper
import org.neo4j.cypher.operations.PathValueBuilder
import org.neo4j.exceptions.CypherTypeException
import org.neo4j.values.AnyValue
import org.neo4j.values.storable.Value
import org.neo4j.values.storable.Values.NO_VALUE
import org.neo4j.values.virtual.ListValue
import org.neo4j.values.virtual.VirtualValues

object ProjectedPath {

  type Projector = (ReadableRow, PathValueBuilder) => PathValueBuilder

  object nilProjector extends Projector {
    def apply(ctx: ReadableRow, builder: PathValueBuilder): PathValueBuilder = builder
  }

  case class singleNodeProjector(node: String, tailProjector: Projector) extends Projector {

    def apply(ctx: ReadableRow, builder: PathValueBuilder): PathValueBuilder = {
      builder.addNode(ctx.getByName(node))
      tailProjector(ctx, builder)
    }
  }

  case class singleIncomingRelationshipProjector(rel: String, tailProjector: Projector) extends Projector {

    def apply(ctx: ReadableRow, builder: PathValueBuilder): PathValueBuilder = {
      builder.addIncoming(ctx.getByName(rel))
      tailProjector(ctx, builder)
    }
  }

  case class singleRelationshipWithKnownTargetProjector(rel: String, target: String, tailProjector: Projector)
      extends Projector {

    def apply(ctx: ReadableRow, builder: PathValueBuilder): PathValueBuilder = {
      builder.addRelationship(ctx.getByName(rel))
      builder.addNode(ctx.getByName(target))
      tailProjector(ctx, builder)
    }
  }

  case class singleOutgoingRelationshipProjector(rel: String, tailProjector: Projector) extends Projector {

    def apply(ctx: ReadableRow, builder: PathValueBuilder): PathValueBuilder = {
      builder.addOutgoing(ctx.getByName(rel))
      tailProjector(ctx, builder)
    }
  }

  case class singleUndirectedRelationshipProjector(rel: String, tailProjector: Projector) extends Projector {

    def apply(ctx: ReadableRow, builder: PathValueBuilder): PathValueBuilder = {
      builder.addUndirected(ctx.getByName(rel))
      tailProjector(ctx, builder)
    }
  }

  case class multiIncomingRelationshipProjector(rels: String, tailProjector: Projector) extends Projector {

    def apply(ctx: ReadableRow, builder: PathValueBuilder): PathValueBuilder = {
      builder.addMultipleIncoming(ctx.getByName(rels))
      tailProjector(ctx, builder)
    }
  }

  case class multiOutgoingRelationshipProjector(rels: String, tailProjector: Projector) extends Projector {

    def apply(ctx: ReadableRow, builder: PathValueBuilder): PathValueBuilder = {
      builder.addMultipleOutgoing(ctx.getByName(rels))
      tailProjector(ctx, builder)
    }
  }

  case class multiUndirectedRelationshipProjector(rels: String, tailProjector: Projector) extends Projector {

    def apply(ctx: ReadableRow, builder: PathValueBuilder): PathValueBuilder = {
      builder.addMultipleUndirected(ctx.getByName(rels))
      tailProjector(ctx, builder)
    }
  }

  case class multiIncomingRelationshipWithKnownTargetProjector(
    rels: String,
    node: String,
    tailProjector: Projector
  ) extends Projector {

    def apply(ctx: ReadableRow, builder: PathValueBuilder): PathValueBuilder = {
      ctx.getByName(rels) match {
        case list: ListValue if list.nonEmpty() =>
          val aggregated = addAllExceptLast(
            builder,
            list,
            (b, v) => {
              b.addIncoming(v)
              b
            }
          )
          aggregated.addRelationship(list.last())
          aggregated.addNode(ctx.getByName(node))
          tailProjector(ctx, aggregated)

        case _: ListValue => tailProjector(ctx, builder)
        case x if x eq NO_VALUE =>
          builder.addNoValue()
          builder
        case value: Value =>
          throw CypherTypeException.expectedListValue(
            String.valueOf(value),
            value.prettyPrint(),
            CypherTypeValueMapper.valueType(value)
          )
        case other => throw CypherTypeException.expectedListValue(
            String.valueOf(other),
            String.valueOf(other),
            CypherTypeValueMapper.valueType(other)
          )
      }
    }
  }

  case class multiOutgoingRelationshipWithKnownTargetProjector(
    rels: String,
    node: String,
    tailProjector: Projector
  ) extends Projector {

    def apply(ctx: ReadableRow, builder: PathValueBuilder): PathValueBuilder = {
      ctx.getByName(rels) match {
        case list: ListValue if list.nonEmpty() =>
          val aggregated = addAllExceptLast(
            builder,
            list,
            (b, v) => {
              b.addOutgoing(v)
              b
            }
          )
          aggregated.addRelationship(
            list.last()
          )
          aggregated.addNode(
            ctx.getByName(node)
          )
          tailProjector(ctx, aggregated)

        case _: ListValue => tailProjector(ctx, builder)
        case x if x eq NO_VALUE =>
          builder.addNoValue()
          builder
        case value: Value =>
          throw CypherTypeException.expectedListValue(
            String.valueOf(value),
            value.prettyPrint(),
            CypherTypeValueMapper.valueType(value)
          )
        case other =>
          throw CypherTypeException.expectedListValue(
            String.valueOf(other),
            String.valueOf(other),
            CypherTypeValueMapper.valueType(other)
          )

      }
    }
  }

  case class multiUndirectedRelationshipWithKnownTargetProjector(
    rels: String,
    node: String,
    tailProjector: Projector
  ) extends Projector {

    def apply(ctx: ReadableRow, builder: PathValueBuilder): PathValueBuilder = {
      ctx.getByName(rels) match {
        case list: ListValue if list.nonEmpty() =>
          val aggregated = addAllExceptLast(
            builder,
            list,
            (b, v) => {
              b.addUndirected(v)
              b
            }
          )
          aggregated.addRelationship(list.last())
          aggregated.addNode(ctx.getByName(node))
          tailProjector(ctx, aggregated)

        case _: ListValue => tailProjector(ctx, builder)
        case x if x eq NO_VALUE =>
          builder.addNoValue()
          builder
        case value: Value =>
          throw CypherTypeException.expectedListValue(
            String.valueOf(value),
            value.prettyPrint(),
            CypherTypeValueMapper.valueType(value)
          )
        case other =>
          throw CypherTypeException.expectedListValue(
            String.valueOf(other),
            String.valueOf(other),
            CypherTypeValueMapper.valueType(other)
          )
      }
    }
  }

  /**
   * One level of group variables of a quantified path pattern in a projected path. A nested quantified path
   * pattern is represented at the level below, which is why this is a tree: a path may mix relationships contained
   * directly in a quantified path pattern with nested ones, and the two have a different nesting depth.
   */
  sealed trait RepeatGroupProjection
  case class RepeatEntityProjection(node: String, relationship: String) extends RepeatGroupProjection
  case class NestedRepeatProjection(groups: Seq[RepeatGroupProjection]) extends RepeatGroupProjection

  case class quantifiedPathProjector(groups: Seq[RepeatGroupProjection], toNode: String, tailProjector: Projector)
      extends Projector {

    def apply(ctx: ReadableRow, builder: PathValueBuilder): PathValueBuilder = {
      def topLevelValue(name: String): ListValue =
        ctx.getByName(name) match {
          case value: ListValue   => value
          case x if x eq NO_VALUE => VirtualValues.EMPTY_LIST
          case value: Value =>
            throw CypherTypeException.expectedListValue(
              String.valueOf(value),
              value.prettyPrint(),
              CypherTypeValueMapper.valueType(value)
            )
          case other =>
            throw CypherTypeException.expectedListValue(
              String.valueOf(other),
              String.valueOf(other),
              CypherTypeValueMapper.valueType(other)
            )
        }

      val values = scala.collection.mutable.Map.empty[String, ListValue]
      def valueAt(name: String, path: Seq[Int]): AnyValue = {
        val root = values.getOrElseUpdate(name, topLevelValue(name))
        var value: AnyValue = root
        var level = 0
        while (level < path.length) {
          value = value match {
            case list: ListValue    => list.value(path(level))
            case x if x eq NO_VALUE => VirtualValues.EMPTY_LIST
            case other =>
              throw CypherTypeException.expectedListValue(
                String.valueOf(other),
                String.valueOf(other),
                CypherTypeValueMapper.valueType(other)
              )
          }
          level += 1
        }
        value
      }

      def sizeAt(name: String, path: Seq[Int]): Long =
        valueAt(name, path) match {
          case list: ListValue => list.actualSize()
          case _               => 0L
        }

      def repetitionCount(group: RepeatGroupProjection, path: Seq[Int]): Long =
        group match {
          case RepeatEntityProjection(node, _) => sizeAt(node, path)
          case NestedRepeatProjection(groups)  => groups.headOption.map(repetitionCount(_, path)).getOrElse(0L)
        }

      var skippedFirstNode = false
      var addedAnything = false

      /**
       * Emits the entities of one repetition level of a quantified path pattern. Every group variable of that
       * level holds one entry per repetition of that level, so all of them are indexed with the same offsets.
       * A nested quantified path pattern is expanded at the position where it occurs in the path.
       */
      def addRepetitions(groups: Seq[RepeatGroupProjection], path: Seq[Int]): Unit = {
        val repetitions = groups.headOption.map(repetitionCount(_, path)).getOrElse(0L)
        var repetition = 0L
        while (repetition < repetitions) {
          addGroups(groups, path :+ repetition.toInt)
          repetition += 1
        }
      }

      def addGroups(groups: Seq[RepeatGroupProjection], path: Seq[Int]): Unit = {
        var i = 0
        while (i < groups.size) {
          groups(i) match {
            case RepeatEntityProjection(node, relationship) =>
              // The very first node is equal to the node added by the previous projector.
              if (skippedFirstNode) {
                builder.addNode(valueAt(node, path))
                addedAnything = true
              } else {
                skippedFirstNode = true
              }
              builder.addRelationship(valueAt(relationship, path))
              addedAnything = true
            case nested: NestedRepeatProjection =>
              addRepetitions(nested.groups, path)
          }
          i += 1
        }
      }

      addRepetitions(groups, Seq.empty)
      if (addedAnything) {
        builder.addNode(ctx.getByName(toNode))
      }
      tailProjector(ctx, builder)
    }
  }

  private def addAllExceptLast(
    builder: PathValueBuilder,
    list: ListValue,
    f: (PathValueBuilder, AnyValue) => PathValueBuilder
  ): PathValueBuilder = {
    var aggregated = builder
    val size = list.actualSize()
    var i = 0
    while (i < size - 1) {
      // we know these relationships have already loaded start and end relationship
      // so we should not use CypherFunctions::[start,end]Node to look them up
      aggregated = f(aggregated, list.value(i))
      i += 1
    }
    aggregated
  }
}

/*
 Expressions for materializing new paths

 These expressions cannot be generated by the user directly
 */
case class ProjectedPath(projector: Projector) extends Expression {

  override def apply(row: ReadableRow, state: QueryState): AnyValue = {
    projector(row, new PathValueBuilder(state.query, state.cursors.relationshipScanCursor)).build()
  }

  override def arguments: Seq[Expression] = Seq.empty

  override def children: Seq[AstNode[_]] = Seq.empty

  override def rewrite(f: Expression => Expression): Expression = f(this)
}
