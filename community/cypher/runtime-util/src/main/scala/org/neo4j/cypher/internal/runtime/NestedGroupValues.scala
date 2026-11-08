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
package org.neo4j.cypher.internal.runtime

import org.neo4j.cypher.internal.runtime.CastSupport.castOrFail
import org.neo4j.values.AnyValue
import org.neo4j.values.virtual.ListValue
import org.neo4j.values.virtual.VirtualRelationshipValue

/**
 * Access to the values bound by a group variable of a quantified path pattern.
 *
 * A nested quantified path pattern exposes the values it matched through its group variable, one nesting level per
 * enclosing quantified path pattern. A value is therefore either a single entity or a (possibly deeply nested)
 * list of entities. This is the single place where that convention is unwrapped, so the interpreted and the
 * slotted runtime agree on it.
 */
object NestedGroupValues {

  /**
   * Runs `f` for every value in `value`, descending into nested lists. Short-circuits as soon as `f` returns `false`.
   */
  def foreachValue(value: AnyValue)(f: AnyValue => Boolean): Boolean =
    value match {
      case list: ListValue =>
        val iterator = list.iterator()
        var result = true
        while (result && iterator.hasNext) {
          result = foreachValue(iterator.next())(f)
        }
        result
      case other =>
        f(other)
    }

  /**
   * Runs `f` for the id of every relationship in `value`, descending into nested lists. Short-circuits as soon as
   * `f` returns `false`.
   */
  def foreachRelationshipId(value: AnyValue)(f: Long => Boolean): Boolean =
    foreachValue(value)(relationship => f(castOrFail[VirtualRelationshipValue](relationship).id()))
}