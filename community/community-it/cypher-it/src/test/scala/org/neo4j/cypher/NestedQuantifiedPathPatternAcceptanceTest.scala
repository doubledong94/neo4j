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
package org.neo4j.cypher

import org.neo4j.graphdb.Path

/**
 * End-to-end tests for nested quantified path patterns.
 *
 * The nested group variables are exposed one level up, which means that a nested quantified path pattern binds
 * lists of lists (of lists, ...) of nodes and relationships.
 */
class NestedQuantifiedPathPatternAcceptanceTest extends ExecutionEngineFunSuite {

  /**
   * Creates a directed cycle of `size` nodes and relationships, where relationship `i` points from node `i` to
   * node `i + 1` (modulo `size`). Every node and relationship also has the property `i`.
   */
  private def createCycle(size: Int): Unit = {
    val nodes = (1 to size).map(i => createNode(Map("name" -> s"n$i", "i" -> i)))
    nodes.zipWithIndex.foreach {
      case (node, i) =>
        val next = nodes((i + 1) % size)
        relate(node, next, "R", Map("i" -> (i + 1)))
    }
  }

  test("a nested quantified path pattern exposes nested group variables") {
    createCycle(4)

    val result = execute(
      """CYPHER 25 MATCH (start {name: 'n1'}) (((x)-[r:R]->(y)){2}){2} (end)
        |RETURN size(r) AS outerRelationships, size(r[0]) AS innerRelationships,
        |       size(x) AS outerNodes, size(x[0]) AS innerNodes""".stripMargin
    )

    result.toList should equal(List(Map(
      "outerRelationships" -> 2,
      "innerRelationships" -> 2,
      "outerNodes" -> 2,
      "innerNodes" -> 2
    )))
  }

  test("relationship uniqueness is enforced across nested repetitions") {
    // Four relationships are required to match the pattern, so the only trail starts and ends in n1.
    // Without trail uniqueness across the outer repetitions, the pattern could revisit relationships.
    createCycle(4)

    val result = execute(
      """CYPHER 25 MATCH (start {name: 'n1'}) (((x)-[r:R]->(y)){2}){2} (end)
        |RETURN count(*) AS count""".stripMargin
    )

    result.toList should equal(List(Map("count" -> 1)))
  }

  test("quantified path patterns can be nested more than once") {
    // Eight relationships are required to match the pattern, so only the full cycle is a valid trail.
    createCycle(8)

    val sizes = execute(
      """CYPHER 25 MATCH (start {name: 'n1'}) ((((x)-[r:R]->(y)){2}){2}){2} (end)
        |RETURN size(r) AS first, size(r[0]) AS second, size(r[0][0]) AS third""".stripMargin
    )

    sizes.toList should equal(List(Map("first" -> 2, "second" -> 2, "third" -> 2)))

    val count = execute(
      """CYPHER 25 MATCH (start {name: 'n1'}) ((((x)-[r:R]->(y)){2}){2}){2} (end)
        |RETURN count(*) AS count""".stripMargin
    )

    count.toList should equal(List(Map("count" -> 1)))
  }

  test("named paths over nested quantified path patterns are projected correctly") {
    createCycle(4)

    val result = execute(
      """CYPHER 25 MATCH p = (start {name: 'n1'}) (((x)-[r:R]->(y)){2}){2} (end)
        |RETURN p AS p, length(p) AS length""".stripMargin
    ).single

    result("length") should equal(4)
    result("p").asInstanceOf[Path].length() should equal(4)
  }

  test("named paths over a mix of direct and nested quantified path patterns") {
    createCycle(4)

    // One relationship of the enclosing pattern (n1 -> n2) followed by two repetitions of the nested one
    // (n2 -> n3 -> n4). The group variables of the two levels have a different nesting depth.
    val result = execute(
      """CYPHER 25 MATCH p = (start {name: 'n1'}) ((x)-[r:R]->(y) ((u)-[v:R]->(w)){2}){1} (end)
        |RETURN p AS p, length(p) AS length, [n IN nodes(p) | n.name] AS nodes""".stripMargin
    ).single

    result("length") should equal(3)
    result("p").asInstanceOf[Path].length() should equal(3)
    result("nodes") should equal(List("n1", "n2", "n3", "n4"))
  }

  test("named paths over a mix of direct and nested quantified path patterns in the interpreted runtime") {
    createCycle(4)

    val result = execute(
      """CYPHER 25 runtime=legacy MATCH p = (start {name: 'n1'}) ((x)-[r:R]->(y) ((u)-[v:R]->(w)){2}){1} (end)
        |RETURN p AS p, length(p) AS length, [n IN nodes(p) | n.name] AS nodes""".stripMargin
    ).single

    result("length") should equal(3)
    result("p").asInstanceOf[Path].length() should equal(3)
    result("nodes") should equal(List("n1", "n2", "n3", "n4"))
  }

  test("predicates of nested quantified path patterns can reference outer variables") {
    createCycle(4)

    // Excluding relationship 1 breaks the only four-relationship trail around the cycle.
    val result = execute(
      """CYPHER 25 MATCH (start {name: 'n1'}) (((x)-[r:R]->(y) WHERE r.i <> start.i){2}){2} (end)
        |RETURN count(*) AS count""".stripMargin
    )

    result.toList should equal(List(Map("count" -> 0)))
  }

  test("ANY SHORTEST does not reduce the range of a pattern containing a nested quantified path pattern") {
    createCycle(100)

    // The outer quantifier is at the configured rewriting threshold. Moving it into a size predicate on the
    // outer group variable would filter out the path, because the selective path runtime does not populate the
    // group variable of a pattern that contains a nested quantified path pattern.
    val result = execute(
      """CYPHER 25 MATCH ANY SHORTEST (start {name: 'n1'}) ((()-->()){1}){100} (end)
        |RETURN end.name AS name""".stripMargin
    )

    result.toList should equal(List(Map("name" -> "n1")))
  }

  test("a parenthesized path with a predicate may precede a nested quantified path pattern") {
    createCycle(4)

    // The predicate of the parenthesized path is hoisted to the enclosing quantified path pattern, which is
    // juxtaposed with a nested (anonymous) quantified path pattern.
    val result = execute(
      """CYPHER 25 MATCH ANY SHORTEST (start {name: 'n1'})
        |      ( ((x)-[r:R]->(y) WHERE r.i > 0) (()-->()){2} ){1} (end)
        |RETURN end.name AS name""".stripMargin
    )

    result.toList should equal(List(Map("name" -> "n4")))
  }
}
