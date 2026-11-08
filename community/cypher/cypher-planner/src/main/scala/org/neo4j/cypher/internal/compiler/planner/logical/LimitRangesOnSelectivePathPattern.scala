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
package org.neo4j.cypher.internal.compiler.planner.logical

import org.neo4j.cypher.internal.expressions.Expression
import org.neo4j.cypher.internal.expressions.FunctionInvocation
import org.neo4j.cypher.internal.expressions.GreaterThanOrEqual
import org.neo4j.cypher.internal.expressions.LessThanOrEqual
import org.neo4j.cypher.internal.expressions.LogicalVariable
import org.neo4j.cypher.internal.expressions.SignedDecimalIntegerLiteral
import org.neo4j.cypher.internal.ir.PatternRelationship
import org.neo4j.cypher.internal.ir.QuantifiedPathPattern
import org.neo4j.cypher.internal.ir.Selections
import org.neo4j.cypher.internal.ir.SelectivePathPattern
import org.neo4j.cypher.internal.ir.VarPatternLength
import org.neo4j.cypher.internal.ir.helpers.ExpressionConverters.PredicateConverter
import org.neo4j.cypher.internal.util.FunctionName
import org.neo4j.cypher.internal.util.InputPosition
import org.neo4j.cypher.internal.util.UpperBound

/**
 * Update large quantified ranges in Selective Path Patterns to instead be a size restricted pre-filter predicate to limit size of built NFA and add path length restriction.
 */
case class LimitRangesOnSelectivePathPattern(rewriteQuantifiersAbove: Int) {

  def apply(spp: SelectivePathPattern): SelectivePathPattern = {
    val (boundaryPredicates, newConnections) =
      spp.pathPattern.connections.foldMap(Set.empty[Expression]) {
        case (extractedPredicates, nodeConnection) =>
          nodeConnection match {
            case patternRelationship: PatternRelationship =>
              patternRelationship.length match {
                case VarPatternLength(min, max) =>
                  limitRangesOnPatternRelationship(extractedPredicates, patternRelationship, min, max)
                case _ => (extractedPredicates, patternRelationship)
              }
            case qpp: QuantifiedPathPattern =>
              limitRangesOnQPP(extractedPredicates, qpp)
          }
      }

    spp.copy(
      pathPattern = spp.pathPattern.copy(connections = newConnections),
      selections = spp.selections ++ Selections(boundaryPredicates.flatMap(_.asPredicates))
    )
  }

  private def limitRangesOnPatternRelationship(
    extractedPredicates: Set[Expression],
    patternRelationship: PatternRelationship,
    min: Int,
    max: Option[Int]
  ): (Set[Expression], PatternRelationship) = {
    val startRelationship = patternRelationship.relationships.headOption
    val (newMin, maybeMinExpression) =
      if (min < rewriteQuantifiersAbove)
        (min, None)
      else
        (1, startRelationship.map(minRestrictionPredicate(_, min.toString)))
    val (newMax, maybeMaxExpression) = max match {
      case None =>
        (None, None)
      case Some(limit) =>
        if (limit < rewriteQuantifiersAbove)
          (Some(limit), None)
        else
          (None, startRelationship.map(maxRestrictionPredicate(_, limit.toString)))
    }
    val prWithUpdatedRange = patternRelationship.copy(length = VarPatternLength(newMin, newMax))

    (extractedPredicates ++ maybeMaxExpression ++ maybeMinExpression, prWithUpdatedRange)
  }

  /**
   * Limits the range of the quantified path pattern itself. A boundary predicate can only refer to a group
   * variable that is bound by the selective path pattern, and such a variable holds one entry per repetition
   * of the enclosing quantified path pattern.
   *
   * Ranges are only rewritten for quantified path patterns that do not contain a nested quantified path
   * pattern. The nested pattern exposes its own group variables, which the enclosing group variable aggregates
   * into a list per repetition. The selective path runtime only materializes one flat list per group variable
   * and does not populate the enclosing group variable of a nested pattern, so a `size(...)` predicate on it
   * can neither count repetitions nor relationships. The ranges of nested quantified path patterns (and of
   * patterns containing them) are therefore deliberately left untouched.
   */
  private def limitRangesOnQPP(
    extractedPredicates: Set[Expression],
    qpp: QuantifiedPathPattern
  ): (Set[Expression], QuantifiedPathPattern) = {
    if (qpp.allQuantifiedPathPatterns.nonEmpty) {
      return (extractedPredicates, qpp)
    }
    // Each relationship of one repetition maps to its own group variable, so the number of group variables
    // is the number of relationships traversed per repetition.
    val amountOfRelationships = qpp.relationshipVariableGroupings.size.toLong
    val startRelationship = qpp.relationships.toSeq.sortBy(_.name).headOption
    val (newMin, maybeMinExpression) =
      if (
        startRelationship.isEmpty ||
        !producesAtLeast(rewriteQuantifiersAbove, qpp.repetition.min, amountOfRelationships)
      )
        (qpp.repetition.min.toInt, None)
      else
        (1, startRelationship.map(minRestrictionPredicate(_, qpp.repetition.min.toString)))
    val (newMax, maybeMaxExpression) = qpp.repetition.max match {
      case UpperBound.Unlimited =>
        (UpperBound.Unlimited, None)
      case UpperBound.Limited(limit) =>
        if (
          startRelationship.isEmpty ||
          !producesAtLeast(rewriteQuantifiersAbove, limit, amountOfRelationships)
        )
          (UpperBound.Limited(limit), None)
        else
          (UpperBound.Unlimited, startRelationship.map(maxRestrictionPredicate(_, limit.toString)))
    }

    val qppWithUpdatedRange = qpp.copy(repetition = qpp.repetition.copy(min = newMin, max = newMax))

    (extractedPredicates ++ maybeMaxExpression ++ maybeMinExpression, qppWithUpdatedRange)
  }

  /**
   * Whether `iterations` repetitions of a pattern that traverses `amountPerIteration` relationships each can
   * produce at least `threshold` relationships. Uses BigInt to be safe against overflows of large quantifiers.
   */
  private def producesAtLeast(threshold: Int, iterations: Long, amountPerIteration: Long): Boolean =
    BigInt(iterations) * BigInt(amountPerIteration) >= BigInt(threshold)

  private def minRestrictionPredicate(variable: LogicalVariable, min: String): GreaterThanOrEqual = {
    val pos = InputPosition.NONE
    GreaterThanOrEqual(
      FunctionInvocation(FunctionName("size")(pos), variable)(pos),
      SignedDecimalIntegerLiteral(min)(pos.zeroLength)
    )(pos)
  }

  private def maxRestrictionPredicate(variable: LogicalVariable, max: String): LessThanOrEqual = {
    val pos = InputPosition.NONE
    LessThanOrEqual(
      FunctionInvocation(FunctionName("size")(pos), variable)(pos),
      SignedDecimalIntegerLiteral(max)(pos.zeroLength)
    )(pos)
  }
}
