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
package org.neo4j.cypher.internal.runtime.interpreted.pipes

import org.neo4j.cypher.internal.runtime.interpreted.commands.expressions.Literal
import org.neo4j.cypher.internal.util.test_helpers.CypherFunSuite
import org.neo4j.values.storable.Values

class StackActionRuntimeTest extends CypherFunSuite {

  private val emptyStacks = Map.empty[String, StackActionRuntime.StackValue]
  private val stackName = StackActionRuntime.DefaultStackName

  test("push then pop without keys is last-in-first-out") {
    val push = StackActionRuntime.Push(stackName, None)
    val pop = StackActionRuntime.Pop(stackName, None)

    val one = push.applyTo(emptyStacks, null, null).get
    one(stackName).depth should equal(1)

    val two = push.applyTo(one, null, null).get
    two(stackName).depth should equal(2)

    val backToOne = pop.applyTo(two, null, null).get
    backToOne should equal(one)

    pop.applyTo(backToOne, null, null).get(stackName) should equal(StackActionRuntime.StackValue.Empty)
  }

  test("pop without key on an empty stack prunes when the stack is not signed") {
    StackActionRuntime.Pop(stackName, None).applyTo(emptyStacks, null, null) should equal(None)
  }

  test("pop without key on an empty stack pushes onto the negative side when signed") {
    val pop = StackActionRuntime.Pop(stackName, None, signed = true)

    val stacks = pop.applyTo(emptyStacks, null, null).get
    stacks(stackName).depth should equal(-1)
    stacks(stackName).negative should have size 1

    val moreNegative = pop.applyTo(stacks, null, null).get
    moreNegative(stackName).depth should equal(-2)
  }

  test("push cancels the negative side") {
    val pop = StackActionRuntime.Pop(stackName, None, signed = true)
    val push = StackActionRuntime.Push(stackName, None)

    val negative = pop.applyTo(emptyStacks, null, null).get
    val balanced = push.applyTo(negative, null, null).get
    balanced(stackName) should equal(StackActionRuntime.StackValue.Empty)
  }

  test("push with a differing key does not cancel the negative side") {
    val pop = StackActionRuntime.Pop(stackName, Some(Literal(Values.intValue(1))), signed = true)
    val push = StackActionRuntime.Push(stackName, Some(Literal(Values.intValue(2))))

    val negative = pop.applyTo(emptyStacks, null, null).get
    push.applyTo(negative, null, null) should equal(None)
  }

  test("push with the matching key cancels the negative side") {
    val pop = StackActionRuntime.Pop(stackName, Some(Literal(Values.intValue(1))), signed = true)
    val push = StackActionRuntime.Push(stackName, Some(Literal(Values.intValue(1))))

    val negative = pop.applyTo(emptyStacks, null, null).get
    push.applyTo(negative, null, null).get(stackName) should equal(StackActionRuntime.StackValue.Empty)
  }

  test("a keyed pop with a null key does not behave like a keyless pop") {
    val pop = StackActionRuntime.Pop(stackName, Some(Literal(Values.NO_VALUE)), signed = true)
    val push = StackActionRuntime.Push(stackName, Some(Literal(Values.intValue(1))))

    val negative = pop.applyTo(emptyStacks, null, null).get
    negative(stackName).negative should have size 1
    // NO_VALUE never compares equal to anything, so the push must not cancel the negative entry.
    push.applyTo(negative, null, null) should equal(None)
  }

  test("a keyed pop on a non-empty stack still prunes on a key mismatch, even when signed") {
    val push = StackActionRuntime.Push(stackName, Some(Literal(Values.intValue(1))))
    val pop = StackActionRuntime.Pop(stackName, Some(Literal(Values.intValue(2))), signed = true)

    val stacks = push.applyTo(emptyStacks, null, null).get
    pop.applyTo(stacks, null, null) should equal(None)
  }

  test("named stacks are independent") {
    val pushA = StackActionRuntime.Push("a", None)
    val pushB = StackActionRuntime.Push("b", None)

    val stacks = pushB.applyTo(pushA.applyTo(emptyStacks, null, null).get, null, null).get
    stacks("a").depth should equal(1)
    stacks("b").depth should equal(1)
  }

  test("only stacks marked BALANCED are required to be empty") {
    val pushRequired = StackActionRuntime.Push("required", None, requiresBalance = true)
    val pushOptional = StackActionRuntime.Push("optional", None, requiresBalance = false)

    val stacks = pushOptional
      .applyTo(pushRequired.applyTo(emptyStacks, null, null).get, null, null)
      .get

    StackActionRuntime.areRequiredStacksBalanced(Array(pushRequired, pushOptional), emptyStacks) shouldBe true
    StackActionRuntime.areRequiredStacksBalanced(Array(pushRequired, pushOptional), stacks) shouldBe false
    StackActionRuntime.areRequiredStacksBalanced(Array(pushOptional), stacks) shouldBe true
  }

  test("a required stack is balanced again after a matching pop") {
    val push = StackActionRuntime.Push("s", None, requiresBalance = true)
    val pop = StackActionRuntime.Pop("s", None)

    val stacks = pop.applyTo(push.applyTo(emptyStacks, null, null).get, null, null).get
    StackActionRuntime.areRequiredStacksBalanced(Array(push, pop), stacks) shouldBe true
  }

  test("a required stack with a negative depth is not balanced") {
    val pop = StackActionRuntime.Pop("s", None, requiresBalance = true, signed = true)
    val stacks = pop.applyTo(emptyStacks, null, null).get
    StackActionRuntime.areRequiredStacksBalanced(Array(pop), stacks) shouldBe false
  }
}
