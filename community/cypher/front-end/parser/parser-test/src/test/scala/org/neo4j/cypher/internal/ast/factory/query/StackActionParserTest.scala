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
package org.neo4j.cypher.internal.ast.factory.query

import org.neo4j.cypher.internal.ast.Statements
import org.neo4j.cypher.internal.ast.test.util.AstParsing.Cypher5
import org.neo4j.cypher.internal.ast.test.util.AstParsingTestBase
import org.neo4j.cypher.internal.expressions.PopFromStack
import org.neo4j.cypher.internal.expressions.PushOntoStack
import org.neo4j.cypher.internal.expressions.RelationshipPattern
import org.neo4j.cypher.internal.expressions.StackVariable
import org.neo4j.exceptions.SyntaxException

class StackActionParserTest extends AstParsingTestBase {

  test("MATCH (a)-[r:CALL STACK PUSH UNBALANCED call: a.site]->(f) RETURN f") {
    parsesIn[Statements] {
      case Cypher5 => _.withError(t => t shouldBe a[SyntaxException])
      case _ => _.containing[RelationshipPattern] {
          relPat(
            name = Some("r"),
            labelExpression = Some(labelRelTypeLeaf("CALL")),
            stackAction = Some(PushOntoStack(Some(StackVariable("call")(pos)), Some(prop("a", "site")))(pos))
          )
        }
    }
  }

  test("MATCH (a)-[r:CALL STACK PUSH call: a.site]->(f) RETURN f") {
    // The BALANCED/UNBALANCED qualifier is mandatory.
    parsesIn[Statements] {
      case Cypher5 => _.withError(t => t shouldBe a[SyntaxException])
      case _       => _.withError(t => t shouldBe a[SyntaxException])
    }
  }

  test("MATCH (a)-[r:CALL STACK PUSH BALANCED call: a.site]->(f) RETURN f") {
    parsesIn[Statements] {
      case Cypher5 => _.withError(t => t shouldBe a[SyntaxException])
      case _ => _.containing[RelationshipPattern] {
          relPat(
            name = Some("r"),
            labelExpression = Some(labelRelTypeLeaf("CALL")),
            stackAction =
              Some(PushOntoStack(Some(StackVariable("call")(pos)), Some(prop("a", "site")), true)(pos))
          )
        }
    }
  }

  test("MATCH (a)-[r:CALL STACK SIGNED PUSH UNBALANCED call: a.site]->(f) RETURN f") {
    parsesIn[Statements] {
      case Cypher5 => _.withError(t => t shouldBe a[SyntaxException])
      case _ => _.containing[RelationshipPattern] {
          relPat(
            name = Some("r"),
            labelExpression = Some(labelRelTypeLeaf("CALL")),
            stackAction = Some(
              PushOntoStack(Some(StackVariable("call")(pos)), Some(prop("a", "site")), false, true)(pos)
            )
          )
        }
    }
  }

  test("MATCH (a)-[r:CALL STACK PUSH BALANCED a.site]->(f) RETURN f") {
    parsesIn[Statements] {
      case Cypher5 => _.withError(t => t shouldBe a[SyntaxException])
      case _ => _.containing[RelationshipPattern] {
          relPat(
            name = Some("r"),
            labelExpression = Some(labelRelTypeLeaf("CALL")),
            stackAction = Some(PushOntoStack(None, Some(prop("a", "site")), true)(pos))
          )
        }
    }
  }

  test("MATCH (a)-[r:RETURN STACK POP UNBALANCED call: a.site]->(f) RETURN f") {
    parsesIn[Statements] {
      case Cypher5 => _.withError(t => t shouldBe a[SyntaxException])
      case _ => _.containing[RelationshipPattern] {
          relPat(
            name = Some("r"),
            labelExpression = Some(labelRelTypeLeaf("RETURN")),
            stackAction = Some(PopFromStack(Some(StackVariable("call")(pos)), Some(prop("a", "site")))(pos))
          )
        }
    }
  }

  test("MATCH (a)-[r:CALL STACK PUSH UNBALANCED a.site]->(f) RETURN f") {
    parsesIn[Statements] {
      case Cypher5 => _.withError(t => t shouldBe a[SyntaxException])
      case _ => _.containing[RelationshipPattern] {
          relPat(
            name = Some("r"),
            labelExpression = Some(labelRelTypeLeaf("CALL")),
            stackAction = Some(PushOntoStack(None, Some(prop("a", "site")))(pos))
          )
        }
    }
  }

  test("MATCH (a)-[r:CALL*1..3 STACK PUSH UNBALANCED call: a.site]->(f) RETURN f") {
    parsesIn[Statements] {
      case Cypher5 => _.withError(t => t shouldBe a[SyntaxException])
      case _ => _.containing[RelationshipPattern] {
          relPat(
            name = Some("r"),
            labelExpression = Some(labelRelTypeLeaf("CALL")),
            length = Some(Some(range(Some(1L), Some(3L)))),
            stackAction = Some(PushOntoStack(Some(StackVariable("call")(pos)), Some(prop("a", "site")))(pos))
          )
        }
    }
  }

  test("MATCH (a)-[r:CALL STACK PUSH UNBALANCED call: a.site WHERE a.site IS NOT NULL]->(f) RETURN f") {
    parsesIn[Statements] {
      case Cypher5 => _.withError(t => t shouldBe a[SyntaxException])
      case _ => _.containing[RelationshipPattern] {
          relPat(
            name = Some("r"),
            labelExpression = Some(labelRelTypeLeaf("CALL")),
            predicates = Some(isNotNull(prop("a", "site"))),
            stackAction = Some(PushOntoStack(Some(StackVariable("call")(pos)), Some(prop("a", "site")))(pos))
          )
        }
    }
  }

  test("MATCH (((x)-[r:CALL STACK PUSH UNBALANCED s: x.p]->(y)){2}){2} (b) RETURN b") {
    parsesIn[Statements] {
      case Cypher5 => _.withError(t => t shouldBe a[SyntaxException])
      case _       => _.withoutErrors
    }
  }

  test("MATCH (a)-[r:CALL STACK]->(f) RETURN f") {
    parsesIn[Statements] {
      case Cypher5 => _.withError(t => t shouldBe a[SyntaxException])
      case _       => _.withError(t => t shouldBe a[SyntaxException])
    }
  }
}
