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

/**
 * End-to-end tests for stack-constrained quantified path patterns.
 *
 * Graph:
 *   (A) -[:CALL {site:'A'}]-> (F) -[:RETURN {site:'A'}]-> (A)
 *   (B) -[:CALL {site:'B'}]-> (F) -[:RETURN {site:'B'}]-> (B)
 *
 * Traversing from A with a matching call/return stack can only return to A.
 */
class StackConstraintAcceptanceTest extends ExecutionEngineFunSuite {

  private def createCallGraph(): Unit = {
    val a = createNode(Map("name" -> "A"))
    val b = createNode(Map("name" -> "B"))
    val f = createNode(Map("name" -> "F"))
    relate(a, f, "CALL", Map("site" -> "A"))
    relate(b, f, "CALL", Map("site" -> "B"))
    relate(f, a, "RETURN", Map("site" -> "A"))
    relate(f, b, "RETURN", Map("site" -> "B"))
  }

  test("a stack constraint only returns to the caller that entered the function") {
    createCallGraph()

    val result = execute(
      """CYPHER 25 MATCH (start {name: 'A'})
        |      ((x)-[c:CALL STACK PUSH UNBALANCED call: c.site]->(y)
        |        -[r:RETURN STACK POP UNBALANCED call: r.site]->(z))+
        |      (end)
        |RETURN DISTINCT end.name AS name""".stripMargin
    )

    result.toList should equal(List(Map("name" -> "A")))
  }

  test("the stack constraint is enforced when the quantified path pattern is solved from the right") {
    createCallGraph()

    // The end of the quantified path pattern is bound before the start, so the planner may solve the
    // pattern from the right. The PUSH/POP actions are order sensitive and must still be applied in the
    // order they are written in the pattern.
    val result = execute(
      """CYPHER 25 MATCH (caller {name: 'A'}),
        |      (start) ((x)-[c:CALL STACK PUSH UNBALANCED call: c.site]->(y)
        |              -[r:RETURN STACK POP UNBALANCED call: r.site]->(z))+ (caller)
        |RETURN DISTINCT start.name AS name""".stripMargin
    )

    result.toList should equal(List(Map("name" -> "A")))
  }

  test("a stack key may reference a variable bound before the quantified path pattern when solving from the right") {
    createCallGraph()

    // The end of the quantified path pattern is bound before its start, so the planner naturally starts from
    // the right. The stack key references `start`, which is only bound when the pattern is solved from the
    // left. The inner plan of the Repeat cannot import a variable that is not yet bound, so this expansion
    // direction must not be used; solving `(start)` first and expanding from the left must still be possible.
    val result = execute(
      """CYPHER 25 MATCH (end {name: 'A'}), (start)
        |      ((x)-[c:CALL STACK PUSH UNBALANCED call: start.name]->(y)
        |        -[r:RETURN STACK POP UNBALANCED call: r.site]->(z))+ (end)
        |RETURN DISTINCT start.name AS name""".stripMargin
    )

    result.toList should equal(List(Map("name" -> "A")))
  }

  test("entry and exit nodes may differ (param node vs return node)") {
    // Function a has a distinct param node (entry) and return node (exit).
    // Two callers call a; the return edge from a.return must go back to the matching caller.
    val c1 = createNode(Map("name" -> "caller1"))
    val c2 = createNode(Map("name" -> "caller2"))
    val param = createNode(Map("name" -> "a.param"))
    val ret = createNode(Map("name" -> "a.return"))
    relate(c1, param, "CALL", Map("site" -> 1))
    relate(c2, param, "CALL", Map("site" -> 2))
    relate(param, ret, "BODY", Map[String, Any]())
    relate(ret, c1, "RETURN", Map("site" -> 1))
    relate(ret, c2, "RETURN", Map("site" -> 2))

    val result = execute(
      """CYPHER 25 MATCH (start {name: 'caller1'})
        |      ((x)-[c:CALL STACK PUSH UNBALANCED call: c.site]->(p)
        |        -[b:BODY]->(q)
        |        -[r:RETURN STACK POP UNBALANCED call: r.site]->(z))+
        |      (end)
        |RETURN DISTINCT end.name AS name""".stripMargin
    )

    result.toList should equal(List(Map("name" -> "caller1")))
  }

  test("without the stack constraint, the differing exit node returns to both callers") {
    val c1 = createNode(Map("name" -> "caller1"))
    val c2 = createNode(Map("name" -> "caller2"))
    val param = createNode(Map("name" -> "a.param"))
    val ret = createNode(Map("name" -> "a.return"))
    relate(c1, param, "CALL", Map("site" -> 1))
    relate(c2, param, "CALL", Map("site" -> 2))
    relate(param, ret, "BODY", Map[String, Any]())
    relate(ret, c1, "RETURN", Map("site" -> 1))
    relate(ret, c2, "RETURN", Map("site" -> 2))

    val result = execute(
      """CYPHER 25 MATCH (start {name: 'caller1'})
        |      ((x)-[c:CALL]->(p)-[b:BODY]->(q)-[r:RETURN]->(z))+
        |      (end)
        |RETURN DISTINCT end.name AS name ORDER BY name""".stripMargin
    )

    result.toList should equal(List(Map("name" -> "caller1"), Map("name" -> "caller2")))
  }

  test("the interpreted runtime also enforces the stack constraint") {
    createCallGraph()

    val result = execute(
      """CYPHER 25 runtime=legacy MATCH (start {name: 'A'})
        |      ((x)-[c:CALL STACK PUSH UNBALANCED call: c.site]->(y)
        |        -[r:RETURN STACK POP UNBALANCED call: r.site]->(z))+
        |      (end)
        |RETURN DISTINCT end.name AS name""".stripMargin
    )

    result.toList should equal(List(Map("name" -> "A")))
  }

  test("by default a stack may stay non-empty when emitting results") {
    createCallGraph()

    val result = execute(
      """CYPHER 25 MATCH (start {name: 'A'})
        |      ((x)-[c:CALL STACK PUSH UNBALANCED call: c.site]->(y))+
        |      (end)
        |RETURN DISTINCT end.name AS name""".stripMargin
    )

    result.toList should equal(List(Map("name" -> "F")))
  }

  test("a stack marked BALANCED must be empty when emitting results") {
    createCallGraph()

    val result = execute(
      """CYPHER 25 MATCH (start {name: 'A'})
        |      ((x)-[c:CALL STACK PUSH BALANCED call: c.site]->(y))+
        |      (end)
        |RETURN DISTINCT end.name AS name""".stripMargin
    )

    result.toList should equal(List.empty)
  }

  test("an unsigned pop on an empty stack prunes the path") {
    createCallGraph()

    val result = execute(
      """CYPHER 25 MATCH (start {name: 'F'})
        |      ((x)-[r:RETURN STACK POP BALANCED call: r.site]->(y)
        |        -[c:CALL STACK PUSH BALANCED call: c.site]->(z))+
        |      (end)
        |RETURN DISTINCT end.name AS name""".stripMargin
    )

    result.toList should equal(List.empty)
  }

  test("a signed pop on an empty stack is cancelled by a matching push") {
    createCallGraph()

    val result = execute(
      """CYPHER 25 MATCH (start {name: 'F'})
        |      ((x)-[r:RETURN STACK SIGNED POP BALANCED call: r.site]->(y)
        |        -[c:CALL STACK PUSH BALANCED call: c.site]->(z))+
        |      (end)
        |RETURN DISTINCT end.name AS name""".stripMargin
    )

    result.toList should equal(List(Map("name" -> "F")))
  }

  test("a signed pop on an empty stack is cancelled by a matching push in the interpreted runtime") {
    createCallGraph()

    val result = execute(
      """CYPHER 25 runtime=legacy MATCH (start {name: 'F'})
        |      ((x)-[r:RETURN STACK SIGNED POP BALANCED call: r.site]->(y)
        |        -[c:CALL STACK PUSH BALANCED call: c.site]->(z))+
        |      (end)
        |RETURN DISTINCT end.name AS name""".stripMargin
    )

    result.toList should equal(List(Map("name" -> "F")))
  }

  test("without the stack constraint the return edge of the other caller is reachable") {
    createCallGraph()

    val result = execute(
      """CYPHER 25 MATCH (start {name: 'A'})
        |      ((x)-[c:CALL]->(y)-[r:RETURN]->(z))+
        |      (end)
        |RETURN DISTINCT end.name AS name ORDER BY name""".stripMargin
    )

    result.toList should equal(List(Map("name" -> "A"), Map("name" -> "B")))
  }

  test("a stack constraint is applied when a relationship predicate is solved in the inner plan") {
    val a = createNode(Map("name" -> "A"))
    val b = createNode(Map("name" -> "B"))
    relate(a, b, "R", Map("p" -> 1))
    relate(a, b, "S")

    // The relationship predicate may be solved with an Apply/SemiApply around the expand, which must
    // not prevent the stack action from being applied.
    val result = execute(
      """CYPHER 25 MATCH (start {name: 'A'})
        |      ((x)-[r:R STACK PUSH UNBALANCED s: r.p WHERE (x)-[:S]->(y)]->(y))+
        |      (end)
        |RETURN DISTINCT end.name AS name""".stripMargin
    )

    result.toList should equal(List(Map("name" -> "B")))
  }
}
