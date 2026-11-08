/*
 * Copyright (c) "Neo4j"
 * Neo4j Sweden AB [https://neo4j.com]
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.neo4j.cypher.internal.frontend

import org.neo4j.cypher.internal.CypherVersion

class StackConstraintSemanticAnalysisTest extends NameBasedSemanticAnalysisTestSuite {

  test(
    "MATCH (start) ((x)-[c:CALL STACK PUSH UNBALANCED call: c.site]->(y)-[r:RETURN STACK POP UNBALANCED call: r.site]->(z))+ (end) RETURN end"
  ) {
    run(Set(CypherVersion.Cypher5)).hasNoErrors
  }

  test("MATCH (((x)-[r:CALL STACK PUSH UNBALANCED s: x.p]->(y)){2}){2} (b) RETURN b") {
    run(Set(CypherVersion.Cypher5)).hasErrorMessages(
      "Stack constraints inside nested quantified path patterns are not supported."
    )
  }

  test("MATCH ANY SHORTEST (x)-[r:CALL STACK PUSH UNBALANCED s: x.p]->+(y) RETURN y") {
    run(Set(CypherVersion.Cypher5)).hasErrorMessages(
      "Stack constraints are not supported together with shortest/any path selection."
    )
  }

  test("MATCH p = shortestPath((a)-[r:CALL STACK PUSH UNBALANCED s: a.p]->(b)) RETURN p") {
    run(Set(CypherVersion.Cypher5)).hasErrorMessages(
      "Stack constraints are not supported together with shortest/any path selection."
    )
  }

  test("MATCH p = allShortestPaths((a)-[r:CALL STACK PUSH UNBALANCED s: a.p]->(b)) RETURN p") {
    run(Set(CypherVersion.Cypher5)).hasErrorMessages(
      "Stack constraints are not supported together with shortest/any path selection."
    )
  }

  test("MATCH (a), (b) RETURN shortestPath((a)-[:CALL STACK PUSH UNBALANCED s: 1]->(b))") {
    run(Set(CypherVersion.Cypher5)).hasErrorMessages(
      "Stack constraints are not supported together with shortest/any path selection."
    )
  }

  test("MATCH ()-[r:CALL STACK PUSH UNBALANCED call: r.site]->() RETURN r") {
    run(Set(CypherVersion.Cypher5)).hasErrorMessages(
      "Stack constraints are only supported inside quantified path patterns."
    )
  }

  test(
    "MATCH (start) ((x)-[c:CALL STACK PUSH BALANCED call: c.site]->(y)-[r:RETURN STACK POP UNBALANCED call: r.site]->(z))+ (end) RETURN end"
  ) {
    run(Set(CypherVersion.Cypher5)).hasNoErrors
  }

  test("MATCH (start) ((x)-[c:CALL STACK POP BALANCED call: c.site]->(y))+ (end) RETURN end") {
    run(Set(CypherVersion.Cypher5)).hasErrorMessages(
      "Stack `call` is marked BALANCED but is never pushed in this quantified path pattern."
    )
  }

  test("MATCH (start) ((x)-[c:CALL STACK POP BALANCED c.site]->(y))+ (end) RETURN end") {
    run(Set(CypherVersion.Cypher5)).hasErrorMessages(
      "The anonymous stack is marked BALANCED but is never pushed in this quantified path pattern."
    )
  }

  test("MATCH (start) ((x)-[c:CALL STACK POP BALANCED call: c.site]->(y))* (end) RETURN end") {
    // A quantified path pattern that can be repeated zero times may emit its zero-repetition match
    // with an empty stack, so a missing push is not an error here.
    run(Set(CypherVersion.Cypher5)).hasNoErrors
  }

  test(
    "MATCH (start) ((x)-[c:CALL STACK SIGNED POP BALANCED call: c.site]->(y)-[r:RETURN STACK PUSH BALANCED call: r.site]->(z))+ (end) RETURN end"
  ) {
    run(Set(CypherVersion.Cypher5)).hasNoErrors
  }

  test(
    "MATCH (start) ((x)-[r:R STACK PUSH UNBALANCED s: size([(x)-->() | 1])]->(y))+ (end) RETURN end"
  ) {
    // The variables of a quantified path pattern are renamed per iteration, and a sub-query in a
    // stack key would reference them through its own plan, which cannot be rebound by the repetition.
    run(Set(CypherVersion.Cypher5)).hasErrorMessages(
      "Pattern and sub-query expressions are not supported in the key of a stack constraint."
    )
  }

  test("MATCH (start) ((x)-[r:R STACK PUSH UNBALANCED s: (x)-->()]->(y))+ (end) RETURN end") {
    // A bare pattern expression is also rejected by the general pattern expression rules.
    run(Set(CypherVersion.Cypher5)).hasAtLeastOneErrorMessageIn(
      "Pattern and sub-query expressions are not supported in the key of a stack constraint."
    )
  }

  test("MATCH (n) RETURN [(n)-[r:R STACK PUSH UNBALANCED s: 1]->(m) | m] AS l") {
    run(Set(CypherVersion.Cypher5)).hasErrorMessages(
      "Stack constraints are only supported inside quantified path patterns."
    )
  }

  test(
    "MATCH (a) ((x)-[r:R WHERE EXISTS { MATCH (p) ((u)-[w:W STACK PUSH UNBALANCED s: w.p]->(v))+ (q) }]->(y))+ (b) RETURN b"
  ) {
    // A quantified path pattern inside a subquery is independent of the enclosing quantified path
    // pattern, so its stack constraints are not nested stack constraints.
    run(Set(CypherVersion.Cypher5)).hasNoErrors
  }
}
