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

import org.neo4j.cypher.internal.ast.AstConstructionTestSupport
import org.neo4j.cypher.internal.ast.AstConstructionTestSupport.VariableStringInterpolator
import org.neo4j.cypher.internal.compiler.CypherPlannerTestSuite
import org.neo4j.cypher.internal.compiler.planner.LogicalPlanningTestSupport
import org.neo4j.cypher.internal.expressions.Disjoint
import org.neo4j.cypher.internal.expressions.Expression
import org.neo4j.cypher.internal.expressions.FunctionInvocation
import org.neo4j.cypher.internal.expressions.GreaterThanOrEqual
import org.neo4j.cypher.internal.expressions.LessThanOrEqual
import org.neo4j.cypher.internal.expressions.LogicalVariable
import org.neo4j.cypher.internal.expressions.SemanticDirection
import org.neo4j.cypher.internal.expressions.SignedDecimalIntegerLiteral
import org.neo4j.cypher.internal.expressions.Unique
import org.neo4j.cypher.internal.expressions.VarLengthLowerBound
import org.neo4j.cypher.internal.expressions.VarLengthUpperBound
import org.neo4j.cypher.internal.ir.ExhaustivePathPattern.NodeConnections
import org.neo4j.cypher.internal.ir.NodeBinding
import org.neo4j.cypher.internal.ir.PatternRelationship
import org.neo4j.cypher.internal.ir.QuantifiedPathPattern
import org.neo4j.cypher.internal.ir.Selections
import org.neo4j.cypher.internal.ir.SelectivePathPattern
import org.neo4j.cypher.internal.ir.SelectivePathPattern.CountInteger
import org.neo4j.cypher.internal.ir.SimplePatternLength
import org.neo4j.cypher.internal.ir.VarPatternLength
import org.neo4j.cypher.internal.util.FunctionName
import org.neo4j.cypher.internal.util.NonEmptyList
import org.neo4j.cypher.internal.util.Repetition
import org.neo4j.cypher.internal.util.UpperBound

class LimitRangesOnSelectivePathPatternTest extends CypherPlannerTestSuite with LogicalPlanningTestSupport
    with AstConstructionTestSupport {

  test("should move QPP max limit from QPP to SPP predicate when above 100") {
    val rewrittenSpp =
      buildSinglePlannerQueryAndRewriteSelectivePathPattern(
        "MATCH ANY SHORTEST ((start) ((a)-[r]->(b)){1,1000} (end)) RETURN 1 AS one"
      )
    val spp =
      getSPPWithLimits(Seq((1, UpperBound.unlimited)), Seq(maxRestrictionPredicate(v"r", "1000")))
    rewrittenSpp shouldEqual Set(spp)
  }

  test("should not move QPP max limit from QPP to SPP predicate when below 100") {
    val rewrittenSpp = buildSinglePlannerQueryAndRewriteSelectivePathPattern(
      "MATCH ANY SHORTEST ((start) ((a)-[r]->(b)){1,8} (end)) RETURN 1 AS one"
    )

    val spp = getSPPWithLimits(Seq((1, UpperBound.Limited(8))), Seq.empty)
    rewrittenSpp shouldEqual Set(spp)
  }

  test("should not move QPP max limit from QPP to SPP predicate with no upper limit") {
    val rewrittenSpp = buildSinglePlannerQueryAndRewriteSelectivePathPattern(
      "MATCH ANY SHORTEST ((start) ((a)-[r]->(b))+ (end)) RETURN 1 AS one"
    )
    val spp = getSPPWithLimits(Seq((1, UpperBound.unlimited)), Seq.empty)
    rewrittenSpp shouldEqual Set(spp)
  }

  test("should move QPP min limit from QPP to SPP predicate when above 100") {
    val rewrittenSpp = buildSinglePlannerQueryAndRewriteSelectivePathPattern(
      "MATCH ANY SHORTEST ((start) ((a)-[r]->(b)){101,} (end)) RETURN 1 AS one"
    )
    val spp = getSPPWithLimits(Seq((1, UpperBound.unlimited)), Seq(minRestrictionPredicate(v"r", "101")))
    rewrittenSpp shouldEqual Set(spp)
  }

  test("should not move QPP min limit from QPP to SPP predicate when below 100") {
    val rewrittenSpp = buildSinglePlannerQueryAndRewriteSelectivePathPattern(
      "MATCH ANY SHORTEST ((start) ((a)-[r]->(b)){8,} (end)) RETURN 1 AS one"
    )

    val spp = getSPPWithLimits(Seq((8, UpperBound.unlimited)), Seq.empty)
    rewrittenSpp shouldEqual Set(spp)
  }

  test("should not move QPP min limit from QPP to SPP predicate with 0 lower limit") {
    val rewrittenSpp = buildSinglePlannerQueryAndRewriteSelectivePathPattern(
      "MATCH ANY SHORTEST ((start) ((a)-[r]->(b))* (end)) RETURN 1 AS one"
    )
    val spp = getSPPWithLimits(Seq((0, UpperBound.unlimited)), Seq.empty)
    rewrittenSpp shouldEqual Set(spp)
  }

  test("should move QPP (2 rels) min limit from QPP to SPP predicate when above 50") {
    val rewrittenSpp = buildSinglePlannerQueryAndRewriteSelectivePathPattern(
      "MATCH ANY SHORTEST ((start) ((a)-[r]->(b)-[r2]->(c)){1,51} (end)) RETURN 1 AS one"
    )
    val spp = get2RelsSPPWithLimits(1, UpperBound.unlimited, Seq(maxRestrictionPredicate(v"r", "51")))
    rewrittenSpp shouldEqual Set(spp)
  }

  test("should not move QPP (2 rels) min limit from QPP to SPP predicate when below 50") {
    val rewrittenSpp = buildSinglePlannerQueryAndRewriteSelectivePathPattern(
      "MATCH ANY SHORTEST ((start) ((a)-[r]->(b)-[r2]->(c)){1,49} (end)) RETURN 1 AS one"
    )
    val spp = get2RelsSPPWithLimits(1, UpperBound.Limited(49), Seq.empty)
    rewrittenSpp shouldEqual Set(spp)
  }

  // Nested quantified path patterns that bind variables are not supported in selective path patterns
  // (they are rejected during semantic analysis), so the nested cases are tested on the IR directly.
  test("should leave the max limit of a QPP containing a nested QPP untouched") {
    // One repetition of the outer QPP traverses the nested QPP twice, i.e. 2 relationships, so 51 repetitions
    // already exceed the configured limit of 100.
    val outerQpp = getQppWithNestedLimits(1, UpperBound.Limited(51), 2, UpperBound.Limited(2))
    val rewrittenSpp = rewriteSelectivePathPattern(outerQpp)

    val rewrittenOuterQpp = rewrittenSpp.pathPattern.connections.head.asInstanceOf[QuantifiedPathPattern]
    // The selective path runtime does not populate the group variable of a QPP that contains a nested QPP,
    // so no size predicate can express the range.
    rewrittenOuterQpp.repetition should equal(Repetition(1, UpperBound.Limited(51)))
    rewrittenOuterQpp.patternRelationships.head.asInstanceOf[QuantifiedPathPattern].repetition should equal(
      Repetition(2, UpperBound.Limited(2))
    )
    rewrittenSpp.selections.flatPredicates.flatMap(sizeComparison) shouldBe empty
  }

  test("should leave the min limit of a QPP containing a nested QPP untouched") {
    val outerQpp = getQppWithNestedLimits(51, UpperBound.unlimited, 2, UpperBound.Limited(2))
    val rewrittenSpp = rewriteSelectivePathPattern(outerQpp)

    val rewrittenOuterQpp = rewrittenSpp.pathPattern.connections.head.asInstanceOf[QuantifiedPathPattern]
    rewrittenOuterQpp.repetition should equal(Repetition(51, UpperBound.unlimited))
    rewrittenOuterQpp.patternRelationships.head.asInstanceOf[QuantifiedPathPattern].repetition should equal(
      Repetition(2, UpperBound.Limited(2))
    )
    rewrittenSpp.selections.flatPredicates.flatMap(sizeComparison) shouldBe empty
  }

  test("should leave a nested QPP's own range untouched, since no predicate can express it") {
    val outerQpp = getQppWithNestedLimits(1, UpperBound.Limited(2), 101, UpperBound.Limited(101))
    val rewrittenSpp = rewriteSelectivePathPattern(outerQpp)

    val rewrittenOuterQpp = rewrittenSpp.pathPattern.connections.head.asInstanceOf[QuantifiedPathPattern]
    val rewrittenNestedQpp = rewrittenOuterQpp.patternRelationships.head.asInstanceOf[QuantifiedPathPattern]

    rewrittenOuterQpp.repetition should equal(Repetition(1, UpperBound.Limited(2)))
    rewrittenNestedQpp.repetition should equal(Repetition(101, UpperBound.Limited(101)))
    rewrittenSpp.selections.flatPredicates.flatMap(sizeComparison) shouldBe empty
  }

  test("should leave nested QPP ranges below the limit untouched") {
    val outerQpp = getQppWithNestedLimits(1, UpperBound.Limited(2), 2, UpperBound.Limited(2))
    val rewrittenSpp = rewriteSelectivePathPattern(outerQpp)

    val rewrittenOuterQpp = rewrittenSpp.pathPattern.connections.head.asInstanceOf[QuantifiedPathPattern]
    val rewrittenNestedQpp = rewrittenOuterQpp.patternRelationships.head.asInstanceOf[QuantifiedPathPattern]

    rewrittenOuterQpp.repetition should equal(Repetition(1, UpperBound.Limited(2)))
    rewrittenNestedQpp.repetition should equal(Repetition(2, UpperBound.Limited(2)))
    rewrittenSpp.selections.flatPredicates.flatMap(sizeComparison) shouldBe empty
  }

  private def sizeComparison(expression: Expression): Option[(String, LogicalVariable, String)] =
    expression match {
      case GreaterThanOrEqual(
          FunctionInvocation(_, _, Seq(variable: LogicalVariable), _, _, _, _),
          SignedDecimalIntegerLiteral(value)
        ) =>
        Some((">=", variable, value))
      case LessThanOrEqual(
          FunctionInvocation(_, _, Seq(variable: LogicalVariable), _, _, _, _),
          SignedDecimalIntegerLiteral(value)
        ) =>
        Some(("<=", variable, value))
      case _ => None
    }

  test("should move QPP limit from multiple QPPs to SPP predicate when above 100") {
    val rewrittenSpp = buildSinglePlannerQueryAndRewriteSelectivePathPattern(
      "MATCH ANY SHORTEST ((start) ((a)-[r]->(b)){1,100} (end) ((c)-[s]->(d)){1,100} (end2)) RETURN 1 AS one"
    )
    val spp = getSPPWithLimits(
      Seq((1, UpperBound.unlimited), (1, UpperBound.unlimited)),
      Seq(maxRestrictionPredicate(v"r", "100"), maxRestrictionPredicate(v"s", "100"))
    )
    rewrittenSpp shouldEqual Set(spp)
  }

  test(
    "should move QPP limit from multiple QPPs to SPP predicate but not set a path limit if query contains an unlimited UpperBound"
  ) {
    val rewrittenSpp = buildSinglePlannerQueryAndRewriteSelectivePathPattern(
      "MATCH ANY SHORTEST ((start) ((a)-[r]->(b))+ (end) ((c)-[s]->(d)){1,100} (end2)) RETURN 1 AS one"
    )

    val spp = getSPPWithLimits(
      Seq((1, UpperBound.unlimited), (1, UpperBound.unlimited)),
      Seq(maxRestrictionPredicate(v"s", "100"))
    )
    rewrittenSpp shouldEqual Set(spp)
  }

  test(
    "should move varLength with large lower and upper bound limit to SPP predicate"
  ) {
    val rewrittenSpp = buildSinglePlannerQueryAndRewriteSelectivePathPattern(
      "MATCH ANY SHORTEST (a)-[r*101..1001]->(b) RETURN 1 AS one"
    )
    val varLengthPattern =
      PatternRelationship(v"r", (v"a", v"b"), SemanticDirection.OUTGOING, Seq.empty, VarPatternLength(1, None))
    val spp = SelectivePathPattern(
      pathPattern = NodeConnections(NonEmptyList.from(Seq(varLengthPattern))),
      Selections.from(Seq(
        Unique(v"r")(pos),
        minRestrictionPredicate(v"r", "101"),
        maxRestrictionPredicate(v"r", "1001"),
        VarLengthLowerBound(v"r", 101L)(pos),
        VarLengthUpperBound(v"r", 1001L)(pos)
      )),
      SelectivePathPattern.Selector.Shortest(CountInteger(1))
    )
    rewrittenSpp shouldEqual Set(spp)
  }

  test(
    "should move varLength with large lower bound limit to SPP predicate"
  ) {
    val rewrittenSpp = buildSinglePlannerQueryAndRewriteSelectivePathPattern(
      "MATCH ANY SHORTEST (a)-[r*101..]->(b) RETURN 1 AS one"
    )
    val varLengthPattern =
      PatternRelationship(v"r", (v"a", v"b"), SemanticDirection.OUTGOING, Seq.empty, VarPatternLength(1, None))
    val spp = SelectivePathPattern(
      pathPattern = NodeConnections(NonEmptyList.from(Seq(varLengthPattern))),
      Selections.from(Seq(
        Unique(v"r")(pos),
        minRestrictionPredicate(v"r", "101"),
        VarLengthLowerBound(v"r", 101L)(pos)
      )),
      SelectivePathPattern.Selector.Shortest(CountInteger(1))
    )
    rewrittenSpp shouldEqual Set(spp)
  }

  test(
    "should move varLength with large upper bound limit to SPP predicate"
  ) {
    val rewrittenSpp = buildSinglePlannerQueryAndRewriteSelectivePathPattern(
      "MATCH ANY SHORTEST (a)-[r*..1001]->(b) RETURN 1 AS one"
    )
    val varLengthPattern =
      PatternRelationship(v"r", (v"a", v"b"), SemanticDirection.OUTGOING, Seq.empty, VarPatternLength(1, None))
    val spp = SelectivePathPattern(
      pathPattern = NodeConnections(NonEmptyList.from(Seq(varLengthPattern))),
      Selections.from(Seq(
        Unique(v"r")(pos),
        maxRestrictionPredicate(v"r", "1001"),
        VarLengthLowerBound(v"r", 1L)(pos),
        VarLengthUpperBound(v"r", 1001L)(pos)
      )),
      SelectivePathPattern.Selector.Shortest(CountInteger(1))
    )
    rewrittenSpp shouldEqual Set(spp)
  }

  test(
    "should not move varLength with small lower and upper bound limit to SPP predicate"
  ) {
    val rewrittenSpp = buildSinglePlannerQueryAndRewriteSelectivePathPattern(
      "MATCH ANY SHORTEST (a)-[r*1..8]->(b) RETURN 1 AS one"
    )
    val varLengthPattern =
      PatternRelationship(v"r", (v"a", v"b"), SemanticDirection.OUTGOING, Seq.empty, VarPatternLength(1, Some(8)))
    val spp = SelectivePathPattern(
      pathPattern = NodeConnections(NonEmptyList.from(Seq(varLengthPattern))),
      Selections.from(Seq(Unique(v"r")(pos), VarLengthLowerBound(v"r", 1L)(pos), VarLengthUpperBound(v"r", 8L)(pos))),
      SelectivePathPattern.Selector.Shortest(CountInteger(1))
    )
    rewrittenSpp shouldEqual Set(spp)
  }

  test("should move QPP max limit from QPP to SPP predicate when above configured limit") {
    val rewrittenSpp =
      buildSinglePlannerQueryAndRewriteSelectivePathPattern(
        "MATCH ANY SHORTEST ((start) ((a)-[r]->(b)){1,1000} (end)) RETURN 1 AS one",
        limit = 999
      )
    val spp =
      getSPPWithLimits(Seq((1, UpperBound.unlimited)), Seq(maxRestrictionPredicate(v"r", "1000")))
    rewrittenSpp shouldEqual Set(spp)
  }

  test("should not move QPP max limit from QPP to SPP predicate when below configured limit") {
    val rewrittenSpp = buildSinglePlannerQueryAndRewriteSelectivePathPattern(
      "MATCH ANY SHORTEST ((start) ((a)-[r]->(b)){1,1000} (end)) RETURN 1 AS one",
      limit = 1001
    )

    val spp = getSPPWithLimits(Seq((1, UpperBound.Limited(1000))), Seq.empty)
    rewrittenSpp shouldEqual Set(spp)
  }

  private def rewriteSelectivePathPattern(qpp: QuantifiedPathPattern): SelectivePathPattern = {
    val spp = SelectivePathPattern(
      pathPattern = NodeConnections(NonEmptyList(qpp)),
      Selections.from(Seq(Unique(v"r")(pos))),
      SelectivePathPattern.Selector.Shortest(CountInteger(1))
    )
    LimitRangesOnSelectivePathPattern(100).apply(spp)
  }

  /**
   * Builds the IR of `((start) (((a)-[r]->(b)){nestedMin,nestedMax}){outerMin,outerMax} (end))` directly, since
   * a nested quantified path pattern that binds variables is rejected in a selective path pattern.
   */
  private def getQppWithNestedLimits(
    outerMin: Int,
    outerMax: UpperBound,
    nestedMin: Int,
    nestedMax: UpperBound
  ): QuantifiedPathPattern = {
    val nested = QuantifiedPathPattern(
      leftBinding = NodeBinding(v"a", v"f1"),
      rightBinding = NodeBinding(v"b", v"f2"),
      patternRelationships = NonEmptyList(PatternRelationship(
        v"r",
        (v"a", v"b"),
        SemanticDirection.OUTGOING,
        Seq.empty,
        SimplePatternLength
      )),
      repetition = Repetition(nestedMin, nestedMax),
      nodeVariableGroupings = Set(variableGrouping(v"a", v"a"), variableGrouping(v"b", v"b")),
      relationshipVariableGroupings = Set(variableGrouping(v"r", v"r"))
    )
    QuantifiedPathPattern(
      leftBinding = NodeBinding(v"f1", v"start"),
      rightBinding = NodeBinding(v"f2", v"end"),
      patternRelationships = NonEmptyList(nested),
      repetition = Repetition(outerMin, outerMax),
      nodeVariableGroupings = Set("a", "b", "f1", "f2").map(name => variableGrouping(varFor(name), varFor(name))),
      relationshipVariableGroupings = Set(variableGrouping(v"r", v"r"))
    )
  }

  private def buildSinglePlannerQueryAndRewriteSelectivePathPattern(
    query: String,
    limit: Int = 100
  ): Set[SelectivePathPattern] = {
    val q = buildSinglePlannerQuery(query)
    q.queryGraph.selectivePathPatterns.map(LimitRangesOnSelectivePathPattern(limit).apply)
  }

  private def getQPP1WithLimits(qppMin: Int, qppMax: UpperBound): QuantifiedPathPattern =
    QuantifiedPathPattern(
      leftBinding = NodeBinding(v"a", v"start"),
      rightBinding = NodeBinding(v"b", v"end"),
      patternRelationships =
        NonEmptyList(PatternRelationship(
          v"r",
          (v"a", v"b"),
          SemanticDirection.OUTGOING,
          Seq.empty,
          SimplePatternLength
        )),
      repetition = Repetition(min = qppMin, max = qppMax),
      nodeVariableGroupings = Set(variableGrouping(v"a", v"a"), variableGrouping(v"b", v"b")),
      relationshipVariableGroupings = Set(variableGrouping(v"r", v"r"))
    )

  private def get2RelsQPP1WithLimits(qppMin: Int, qppMax: UpperBound): QuantifiedPathPattern =
    QuantifiedPathPattern(
      leftBinding = NodeBinding(v"a", v"start"),
      rightBinding = NodeBinding(v"c", v"end"),
      patternRelationships =
        NonEmptyList(
          PatternRelationship(
            v"r",
            (v"a", v"b"),
            SemanticDirection.OUTGOING,
            Seq.empty,
            SimplePatternLength
          ),
          PatternRelationship(
            v"r2",
            (v"b", v"c"),
            SemanticDirection.OUTGOING,
            Seq.empty,
            SimplePatternLength
          )
        ),
      repetition = Repetition(min = qppMin, max = qppMax),
      nodeVariableGroupings =
        Set(variableGrouping(v"a", v"a"), variableGrouping(v"b", v"b"), variableGrouping(v"c", v"c")),
      relationshipVariableGroupings = Set(variableGrouping(v"r", v"r"), variableGrouping(v"r2", v"r2")),
      selections = Selections.from(differentRelationships("r2", "r"))
    )

  private def getQPP2WithLimits(qppMin: Int, qppMax: UpperBound): QuantifiedPathPattern =
    QuantifiedPathPattern(
      leftBinding = NodeBinding(v"c", v"end"),
      rightBinding = NodeBinding(v"d", v"end2"),
      patternRelationships =
        NonEmptyList(PatternRelationship(
          v"s",
          (v"c", v"d"),
          SemanticDirection.OUTGOING,
          Seq.empty,
          SimplePatternLength
        )),
      repetition = Repetition(min = qppMin, max = qppMax),
      nodeVariableGroupings = Set(variableGrouping(v"c", v"c"), variableGrouping(v"d", v"d")),
      relationshipVariableGroupings = Set(variableGrouping(v"s", v"s"))
    )

  private def getSPPWithLimits(
    qppLimits: Seq[(Int, UpperBound)],
    predicates: Seq[Expression]
  ): SelectivePathPattern = {
    val qpps = qppLimits match {
      case Seq((min, max))                 => Seq(getQPP1WithLimits(min, max))
      case Seq((min1, max1), (min2, max2)) => Seq(getQPP1WithLimits(min1, max1), getQPP2WithLimits(min2, max2))
      case _                               => throw new IllegalStateException()
    }
    val relUniqPreds = if (qppLimits.size == 2) Seq(Disjoint(v"r", v"s")(pos), Unique(v"r")(pos), Unique(v"s")(pos))
    else Seq(Unique(v"r")(pos))
    SelectivePathPattern(
      pathPattern = NodeConnections(NonEmptyList.from(qpps)),
      Selections.from(relUniqPreds ++ predicates),
      SelectivePathPattern.Selector.Shortest(CountInteger(1))
    )
  }

  private def get2RelsSPPWithLimits(
    qppMin: Int,
    qppMax: UpperBound,
    predicates: Seq[Expression]
  ): SelectivePathPattern = {
    val qpp = get2RelsQPP1WithLimits(qppMin, qppMax)
    val relUniqPreds = Seq(Unique(add(v"r", v"r2"))(pos))
    SelectivePathPattern(
      pathPattern = NodeConnections(NonEmptyList(qpp)),
      Selections.from(relUniqPreds ++ predicates),
      SelectivePathPattern.Selector.Shortest(CountInteger(1))
    )
  }

  private def minRestrictionPredicate(variable: LogicalVariable, min: String): GreaterThanOrEqual =
    GreaterThanOrEqual(
      FunctionInvocation(FunctionName("size")(pos), variable)(pos),
      SignedDecimalIntegerLiteral(min)(pos)
    )(pos)

  private def maxRestrictionPredicate(variable: LogicalVariable, max: String): LessThanOrEqual =
    LessThanOrEqual(
      FunctionInvocation(FunctionName("size")(pos), variable)(pos),
      SignedDecimalIntegerLiteral(max)(pos)
    )(pos)
}
