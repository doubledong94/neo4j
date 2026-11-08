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
package org.neo4j.cypher.internal.expressions

import org.neo4j.cypher.internal.util.ASTNode
import org.neo4j.cypher.internal.util.InputPosition

/**
 * A stack action attached to a relationship pattern.
 *
 * Traversing the relationship either pushes a key onto a named stack ([[PushOntoStack]])
 * or requires that the key matches the current top of the stack before popping it ([[PopFromStack]]).
 * A stack that is not named explicitly uses the anonymous/default stack.
 */
sealed trait StackAction extends ASTNode {

  def stack: Option[StackVariable]
  def key: Option[Expression]

  /**
   * Whether the stack this action is applied to must be empty for the quantified path pattern to
   * emit a result. This is what the mandatory `BALANCED` / `UNBALANCED` qualifier of a stack action
   * expresses; stacks that are only annotated `UNBALANCED` may remain non-empty.
   */
  def requiresBalance: Boolean

  /**
   * Whether the stack this action is applied to may have a negative depth, i.e. a POP on an empty
   * stack reverses into a push onto the negative side of the stack instead of pruning the path.
   * This is what the `SIGNED` qualifier of a stack action expresses.
   */
  def signed: Boolean

  /** The action keyword, `PUSH` or `POP`. */
  def keyword: String = this match {
    case _: PushOntoStack => "PUSH"
    case _: PopFromStack  => "POP"
  }

  /**
   * The `STACK` clause without the optional stack name and the key, in Cypher syntax order:
   * `SIGNED` comes before the action keyword and `BALANCED`/`UNBALANCED` after it,
   * e.g. `STACK SIGNED POP BALANCED`.
   */
  def stackClause: String =
    s"STACK ${if (signed) "SIGNED " else ""}$keyword ${if (requiresBalance) "BALANCED" else "UNBALANCED"}"

  def mapExpressions(f: Expression => Expression): StackAction
}

final case class StackVariable(name: String)(val position: InputPosition) extends ASTNode

final case class PushOntoStack(
  stack: Option[StackVariable],
  key: Option[Expression],
  requiresBalance: Boolean = false,
  signed: Boolean = false
)(val position: InputPosition) extends StackAction {

  override def mapExpressions(f: Expression => Expression): StackAction =
    copy(key = key.map(f))(position)
}

final case class PopFromStack(
  stack: Option[StackVariable],
  key: Option[Expression],
  requiresBalance: Boolean = false,
  signed: Boolean = false
)(val position: InputPosition) extends StackAction {

  override def mapExpressions(f: Expression => Expression): StackAction =
    copy(key = key.map(f))(position)
}
