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
package org.neo4j.cypher.internal.ir.converters

import org.neo4j.cypher.internal.expressions.Expression
import org.neo4j.cypher.internal.expressions.FixedQuantifier
import org.neo4j.cypher.internal.expressions.GraphPatternQuantifier
import org.neo4j.cypher.internal.expressions.IntervalQuantifier
import org.neo4j.cypher.internal.expressions.LogicalVariable
import org.neo4j.cypher.internal.expressions.NamedPatternPart
import org.neo4j.cypher.internal.expressions.NodePattern
import org.neo4j.cypher.internal.expressions.ParenthesizedPath
import org.neo4j.cypher.internal.expressions.PathConcatenation
import org.neo4j.cypher.internal.expressions.PathPatternPart
import org.neo4j.cypher.internal.expressions.PatternElement
import org.neo4j.cypher.internal.expressions.PlusQuantifier
import org.neo4j.cypher.internal.expressions.QuantifiedPath
import org.neo4j.cypher.internal.expressions.RelationshipChain
import org.neo4j.cypher.internal.expressions.ShortestPathsPatternPart
import org.neo4j.cypher.internal.expressions.StarQuantifier
import org.neo4j.cypher.internal.expressions.VariableGrouping
import org.neo4j.cypher.internal.ir.ExhaustiveNodeConnection
import org.neo4j.cypher.internal.ir.ExhaustivePathPattern
import org.neo4j.cypher.internal.ir.ExhaustivePathPattern.NodeConnections
import org.neo4j.cypher.internal.ir.NodeBinding
import org.neo4j.cypher.internal.ir.PatternRelationship
import org.neo4j.cypher.internal.ir.QuantifiedPathPattern
import org.neo4j.cypher.internal.ir.Selections
import org.neo4j.cypher.internal.util.NonEmptyList
import org.neo4j.cypher.internal.util.Repetition
import org.neo4j.cypher.internal.util.UpperBound

object QuantifiedPathPatternConverters {

  def convertQuantifiedPath(
    outerLeft: LogicalVariable,
    quantifiedPath: QuantifiedPath,
    outerRight: LogicalVariable,
    convertElement: PatternElement => ExhaustivePathPattern[ExhaustiveNodeConnection]
  ): QuantifiedPathPattern = {
    val patternPart = getPathPattern(quantifiedPath)
    val patternElement = patternPart.element
    val (connections, parenthesizedPathPredicates) = getConnections(patternElement, convertElement)
    val groupings = VariableGroupings.build(quantifiedPath.variableGroupings)
    connections
      .map(connection => QuantifiedPathPatternBuilder.fromConnection(groupings, connection))
      .reduceLeft(_.append(_))
      .build(
        outerLeft = outerLeft,
        outerRight = outerRight,
        selections = Selections.from(
          quantifiedPath.optionalWhereExpression.toSeq ++ parenthesizedPathPredicates
        ),
        repetition = convertGraphPatternQuantifier(quantifiedPath.quantifier)
      )
  }

  def convertGraphPatternQuantifier(quantifier: GraphPatternQuantifier): Repetition =
    quantifier match {
      case PlusQuantifier()       => Repetition(min = 1, max = UpperBound.Unlimited)
      case StarQuantifier()       => Repetition(min = 0, max = UpperBound.Unlimited)
      case FixedQuantifier(value) => Repetition(min = value.value, max = UpperBound.Limited(value.value))
      case IntervalQuantifier(lower, upper) =>
        Repetition(
          min = lower.fold(0L)(_.value),
          max = upper.fold(UpperBound.unlimited)(x => UpperBound.Limited(x.value))
        )
    }

  private def getPathPattern(quantifiedPath: QuantifiedPath): PathPatternPart =
    quantifiedPath.part match {
      case part: PathPatternPart => part
      case shortest: ShortestPathsPatternPart =>
        throw new IllegalArgumentException(s"${shortest.name}() is not allowed inside of a quantified path pattern")
      case _: NamedPatternPart =>
        throw new IllegalArgumentException("sub-path assignment is not currently supported")
    }

  /**
   * Convert the body of a quantified path pattern into the connections it is made up of.
   *
   * A body is usually a plain relationship chain, but may also be a concatenation (or a nested quantified
   * path pattern, which is padded with filler nodes into a concatenation by an earlier rewriting step).
   * For those cases we delegate back to the regular [[ExhaustivePathPattern]] conversion, which already
   * knows how to fold quantified path patterns and simple patterns into a [[NodeConnections]] list.
   */
  private def getConnections(
    patternElement: PatternElement,
    convertElement: PatternElement => ExhaustivePathPattern[ExhaustiveNodeConnection]
  ): (NonEmptyList[ExhaustiveNodeConnection], Seq[Expression]) =
    patternElement match {
      case relationshipChain: RelationshipChain =>
        (SimplePatternConverters.convertRelationshipChain(relationshipChain), Seq.empty)
      case PathConcatenation(_) =>
        (convertToConnections(patternElement, convertElement), Seq.empty)
      case quantifiedPath: QuantifiedPath =>
        val (connections, innerPredicates) =
          getConnections(getPathPattern(quantifiedPath).element, convertElement)
        (connections, quantifiedPath.optionalWhereExpression.toSeq ++ innerPredicates)
      case ParenthesizedPath(part, optionalWhereClause) =>
        // UnwrapParenthesizedPath normally moves the predicate of a parenthesized path to the enclosing
        // quantified path pattern. It does not do that inside selective path patterns, so we have to keep
        // the predicate as a selection of this quantified path pattern, where it is evaluated per repetition.
        val (connections, innerPredicates) = getConnections(part.element, convertElement)
        (connections, optionalWhereClause.toSeq ++ innerPredicates)
      case _: NodePattern =>
        throw new IllegalArgumentException("quantified path patterns must contain at least one relationship")
    }

  private def convertToConnections(
    patternElement: PatternElement,
    convertElement: PatternElement => ExhaustivePathPattern[ExhaustiveNodeConnection]
  ): NonEmptyList[ExhaustiveNodeConnection] =
    convertElement(patternElement) match {
      case NodeConnections(connections) => connections
      case _: ExhaustivePathPattern.SingleNode[_] =>
        throw new IllegalArgumentException("quantified path patterns must contain at least one relationship")
    }

  final private case class QuantifiedPathPatternBuilder(
    leftMostNode: LogicalVariable,
    rightMostNode: LogicalVariable,
    patternRelationships: NonEmptyList[ExhaustiveNodeConnection],
    nodeVariableGroupings: Set[VariableGrouping],
    relationshipVariableGroupings: Set[VariableGrouping]
  ) {

    def append(right: QuantifiedPathPatternBuilder): QuantifiedPathPatternBuilder =
      QuantifiedPathPatternBuilder(
        leftMostNode = leftMostNode,
        rightMostNode = right.rightMostNode,
        patternRelationships = patternRelationships ++ right.patternRelationships,
        nodeVariableGroupings = nodeVariableGroupings.union(right.nodeVariableGroupings),
        relationshipVariableGroupings = relationshipVariableGroupings.union(right.relationshipVariableGroupings)
      )

    def build(
      outerLeft: LogicalVariable,
      outerRight: LogicalVariable,
      selections: Selections,
      repetition: Repetition
    ): QuantifiedPathPattern =
      QuantifiedPathPattern(
        leftBinding = NodeBinding(leftMostNode, outerLeft),
        rightBinding = NodeBinding(rightMostNode, outerRight),
        patternRelationships = patternRelationships,
        selections = selections,
        repetition = repetition,
        nodeVariableGroupings = nodeVariableGroupings,
        relationshipVariableGroupings = relationshipVariableGroupings
      )
  }

  private object QuantifiedPathPatternBuilder {

    def fromConnection(
      variableGroupings: VariableGroupings,
      connection: ExhaustiveNodeConnection
    ): QuantifiedPathPatternBuilder = connection match {
      case patternRelationship: PatternRelationship =>
        val patternNodes = Set(patternRelationship.left, patternRelationship.right)
        QuantifiedPathPatternBuilder(
          leftMostNode = patternRelationship.left,
          rightMostNode = patternRelationship.right,
          patternRelationships = NonEmptyList(patternRelationship),
          nodeVariableGroupings = variableGroupings.forSingletonNames(patternNodes),
          relationshipVariableGroupings = variableGroupings.forSingletonName(patternRelationship.variable).toSet
        )
      case nestedQpp: QuantifiedPathPattern =>
        // A nested quantified path pattern exposes its own group variables. The enclosing quantified path
        // pattern groups those once more, one level up: each group variable of the nested pattern becomes a
        // singleton of the enclosing pattern, mapped to a new group variable of the enclosing pattern.
        // This is exactly the grouping the AST already carries in `variableGroupings` (which is keyed by the
        // nested pattern's group variables and by the enclosing boundary node variables).
        def regroup(innerGroup: LogicalVariable): LogicalVariable =
          variableGroupings.forSingletonName(innerGroup).map(_.group).getOrElse(innerGroup)

        // At the enclosing level the nested pattern is represented by its *group* variables, so those become the
        // singletons of the enclosing pattern (each mapped to a fresh group variable one level up).
        def regroupGrouping(grouping: VariableGrouping): VariableGrouping =
          VariableGrouping(grouping.group, regroup(grouping.group))(grouping.position)

        // The boundary nodes of the nested pattern are plain nodes of the enclosing pattern and get their own
        // group variables directly from the enclosing pattern's groupings.
        val boundaryNodeGroupings =
          variableGroupings.forSingletonNames(Set(nestedQpp.left, nestedQpp.right))

        QuantifiedPathPatternBuilder(
          leftMostNode = nestedQpp.left,
          rightMostNode = nestedQpp.right,
          patternRelationships = NonEmptyList(nestedQpp),
          nodeVariableGroupings = nestedQpp.nodeVariableGroupings.map(regroupGrouping) ++ boundaryNodeGroupings,
          relationshipVariableGroupings = nestedQpp.relationshipVariableGroupings.map(regroupGrouping)
        )
    }
  }

  final private case class VariableGroupings(singletonNameToVariableGrouping: Map[LogicalVariable, VariableGrouping])
      extends AnyVal {

    def forSingletonNames(singletonNames: Set[LogicalVariable]): Set[VariableGrouping] =
      singletonNames.flatMap(forSingletonName)

    def forSingletonName(singletonName: LogicalVariable): Option[VariableGrouping] =
      singletonNameToVariableGrouping
        .get(singletonName)
  }

  private object VariableGroupings {

    def build(groupings: Set[VariableGrouping]): VariableGroupings =
      VariableGroupings(
        groupings
          .view
          .map(grouping => grouping.singleton -> grouping)
          .toMap
      )
  }
}
