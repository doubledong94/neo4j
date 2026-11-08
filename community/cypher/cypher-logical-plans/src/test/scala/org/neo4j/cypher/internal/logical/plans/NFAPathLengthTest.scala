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
package org.neo4j.cypher.internal.logical.plans

import org.neo4j.cypher.internal.ast.AstConstructionTestSupport
import org.neo4j.cypher.internal.ast.AstConstructionTestSupport.VariableStringInterpolator
import org.neo4j.cypher.internal.expressions.SemanticDirection
import org.neo4j.cypher.internal.ir.ExhaustiveNodeConnection
import org.neo4j.cypher.internal.ir.ExhaustivePathPattern.NodeConnections
import org.neo4j.cypher.internal.ir.NodeBinding
import org.neo4j.cypher.internal.ir.PatternRelationship
import org.neo4j.cypher.internal.ir.QuantifiedPathPattern
import org.neo4j.cypher.internal.ir.SimplePatternLength
import org.neo4j.cypher.internal.util.NonEmptyList
import org.neo4j.cypher.internal.util.Repetition
import org.neo4j.cypher.internal.util.UpperBound
import org.neo4j.cypher.internal.util.test_helpers.CypherFunSuite

class NFAPathLengthTest extends CypherFunSuite with AstConstructionTestSupport {

  private val relationship =
    PatternRelationship(v"r", (v"a", v"b"), SemanticDirection.OUTGOING, Seq.empty, SimplePatternLength)

  private def quantify(
    patternRelationships: NonEmptyList[ExhaustiveNodeConnection],
    repetition: Repetition
  ): QuantifiedPathPattern =
    QuantifiedPathPattern(
      leftBinding = NodeBinding(v"a", v"a"),
      rightBinding = NodeBinding(v"b", v"b"),
      patternRelationships = patternRelationships,
      repetition = repetition,
      nodeVariableGroupings = Set.empty,
      relationshipVariableGroupings = Set.empty
    )

  test("nested quantified path pattern bounds do not overflow an Int") {
    val nested = quantify(NonEmptyList(relationship), Repetition(50000L, UpperBound.Limited(50000L)))
    val outer = quantify(NonEmptyList(nested), Repetition(50000L, UpperBound.Limited(50000L)))

    val length = NFA.PathLength.from(NodeConnections(NonEmptyList(outer)))

    length.min shouldEqual 2_500_000_000L
    length.maybeMax shouldEqual Some(2_500_000_000L)
  }

  test("nested quantified path pattern bounds saturate at Long.MaxValue") {
    val nested = quantify(NonEmptyList(relationship), Repetition(Long.MaxValue, UpperBound.Limited(Long.MaxValue)))
    val outer = quantify(NonEmptyList(nested), Repetition(Long.MaxValue, UpperBound.Limited(Long.MaxValue)))

    val length = NFA.PathLength.from(NodeConnections(NonEmptyList(outer)))

    length.min shouldEqual Long.MaxValue
    length.maybeMax shouldEqual Some(Long.MaxValue)
  }
}
