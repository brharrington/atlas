/*
 * Copyright 2014-2017 Netflix, Inc.
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
package com.netflix.atlas.core.index

import java.math.BigInteger
import java.util
import java.util.Comparator

import com.netflix.atlas.core.model.Query
import com.netflix.atlas.core.model.Tag
import com.netflix.atlas.core.model.TagKey
import com.netflix.atlas.core.model.TaggedItem
import com.netflix.atlas.core.util.Interner
import com.netflix.atlas.core.util.NoopInterner
import com.netflix.atlas.core.util.RefIntHashMap
import org.roaringbitmap.RoaringBitmap
import org.slf4j.LoggerFactory


object RoaringTagIndex {
  private val logger = LoggerFactory.getLogger(getClass)

  private val Items = 0
  private val Keys = 1
  private val Values = 2
  private val Tags = 3

  def empty[T <: TaggedItem: Manifest]: RoaringTagIndex[T] = {
    new RoaringTagIndex(new Array[T](0))
  }

  private case class Positions(key: Int, value: Int, tag: Int)
}

/**
  * Create a new index based on roaring bitmaps.
  *
  * https://github.com/RoaringBitmap/RoaringBitmap
  *
  * @param items
  *     Items to include in the index.
  * @param internWhileBuilding
  *     Should strings get interned while building the index? This should be true unless
  *     it is known that strings have been interned before being added to the index.
  * @param interner
  *     Interner used to ensure we do not have duplicate string data. Internally there
  *     are usages of `java.util.IdentityHashMap` so we must have a unique copy of each
  *     string.
  */
class RoaringTagIndex[T <: TaggedItem](
  items: Array[T],
  internWhileBuilding: Boolean = true,
  interner: Interner[String] = Interner.forStrings)
  extends TagIndex[T] {

  import com.netflix.atlas.core.index.RoaringTagIndex._

  type RoaringValueMap = util.IdentityHashMap[String, Array[RoaringBitmap]]
  type RoaringKeyMap = util.IdentityHashMap[String, RoaringValueMap]

  // Interner to use for building the index
  private val buildInterner = if (internWhileBuilding) interner else new NoopInterner[String]

  // Comparator for ordering tagged items using the id
  private val idComparator = new Comparator[T] {
    def compare(t1: T, t2: T): Int = t1.id compareTo t2.id
  }

  private val (keys, values, tags) = {
    val keySet = new util.TreeSet[String]()
    val valueSet = new util.TreeSet[String]()
    val tagSet = new util.TreeSet[Tag]()
    var pos = 0
    while (pos < items.length) {
      items(pos).foreach { (k, v) =>
        keySet.add(k.intern())
        valueSet.add(v.intern())
        tagSet.add(Tag(k.intern(), v.intern()))
      }
      pos += 1
    }
    val ks = keySet.toArray(new Array[String](keySet.size()))
    val vs = valueSet.toArray(new Array[String](valueSet.size()))
    val ts = tagSet.toArray(new Array[Tag](tagSet.size()))
    (ks, vs, ts)
  }

  // Precomputed set of all items
  private val all = {
    val itemSet = new RoaringBitmap()
    itemSet.add(0L, items.length)
    val keySet = new RoaringBitmap()
    keySet.add(0L, keys.length)
    val valueSet = new RoaringBitmap()
    valueSet.add(0L, values.length)
    val tagSet = new RoaringBitmap()
    tagSet.add(0L, tags.length)
    Array(itemSet, keySet, valueSet, tagSet)
  }

  // Primary indexes to search for a tagged item:
  // * itemIds: sorted array of item ids
  // * itemIndex: key -> value -> set, the set contains indexes to the items array
  // * keyIndex: key -> set, precomputed union of all sets for a given key
  private val (itemIds, itemIndex, keyIndex) = buildItemIndex()

  private def newBitmaps(): Array[RoaringBitmap] = {
    val bitmaps = new Array[RoaringBitmap](4)
    bitmaps.indices.foreach { i => bitmaps(i) = new RoaringBitmap() }
    bitmaps
  }

  private def createPositionMap[V <: AnyRef](data: Array[V]): RefIntHashMap[V] = {
    val map = new RefIntHashMap[V](2 * keys.length)
    var i = 0
    while (i < data.length) {
      map.put(data(i), i)
      i += 1
    }
    map
  }

  private def buildItemIndex(): (Array[BigInteger], RoaringKeyMap, RoaringValueMap) = {
    // Sort items array based on the id, allows for efficient paging of requests using the id
    // as the offset
    logger.debug(s"building index with ${items.length} items, starting sort")
    util.Arrays.sort(items, idComparator)
    val itemIds = new Array[BigInteger](items.length)

    val keyPositions = createPositionMap(keys)
    val valuePositions = createPositionMap(values)
    val tagPositions = createPositionMap(tags)

    // Build the main index
    logger.debug(s"building index with ${items.length} items, create main key map")
    val kidx = new RoaringValueMap
    val idx = new RoaringKeyMap
    var pos = 0
    while (pos < items.length) {
      itemIds(pos) = items(pos).id
      val itemTags = new Array[Positions](items(pos).tags.size)
      var i = 0
      items(pos).foreach { (k, v) =>
        itemTags(i) = Positions(
          key = keyPositions.get(k, -1),
          value = valuePositions.get(v, -1),
          tag = tagPositions.get(Tag(k, v), -1))
        i += 1
      }
      items(pos).foreach { (k, v) =>
        val internedK = buildInterner.intern(k)
        var vidx = idx.get(internedK)
        if (vidx == null) {
          vidx = new RoaringValueMap
          idx.put(internedK, vidx)
        }

        // Add to value index
        val internedV = buildInterner.intern(v)
        var matchSet = vidx.get(internedV)
        if (matchSet == null) {
          matchSet = newBitmaps()
          vidx.put(internedV, matchSet)
        }
        matchSet(Items).add(pos)
        itemTags.foreach { t =>
          matchSet(Keys).add(t.key)
          matchSet(Values).add(t.value)
          matchSet(Tags).add(t.tag)
        }

        // Add to key index
        matchSet = kidx.get(internedK)
        if (matchSet == null) {
          matchSet = newBitmaps()
          kidx.put(internedK, matchSet)
        }
        matchSet(Items).add(pos)
        matchSet(Keys).add(keyPositions.get(internedK, -1))
        matchSet(Values).add(valuePositions.get(internedV, -1))
        itemTags.foreach { t =>
          matchSet(Tags).add(t.tag)
        }
      }
      pos += 1
    }

    (itemIds, idx, kidx)
  }

  private[index] def findImpl(idx: Int, query: Query, offset: Int): RoaringBitmap = {
    import com.netflix.atlas.core.model.Query._
    query match {
      case And(q1, q2)            => and(idx, q1, q2, offset)
      case Or(q1, q2)             => or(idx, q1, q2, offset)
      case Not(q)                 => diff(idx, all(idx), findImpl(idx, q, offset))
      case Equal(k, v)            => equal(idx, k, v, offset)
      case GreaterThan(k, v)      => greaterThan(idx, k, v, false)
      case GreaterThanEqual(k, v) => greaterThan(idx, k, v, true)
      case LessThan(k, v)         => lessThan(idx, k, v, false)
      case LessThanEqual(k, v)    => lessThan(idx, k, v, true)
      case q: In                  => findImpl(idx, q.toOrQuery, offset)
      case q: PatternQuery        => strPattern(idx, q, offset)
      case HasKey(k)              => hasKey(idx, k, offset)
      case True                   => all(idx).clone()
      case False                  => new RoaringBitmap()
    }
  }

  private def diff(idx: Int, s1: RoaringBitmap, s2: RoaringBitmap): RoaringBitmap = {
    val s = s1.clone()
    s.andNot(s2)
    s
  }

  private def withOffset(set: RoaringBitmap, offset: Int): RoaringBitmap = {
    val s = set.clone()
    if (offset > 0) s.remove(0L, offset + 1L)
    s
  }

  private def and(idx: Int, q1: Query, q2: Query, offset: Int): RoaringBitmap = {
    val s1 = findImpl(idx, q1, offset)
    if (s1.isEmpty) s1 else {
      // Short circuit, only perform second query if s1 is not empty
      val s2 = findImpl(idx, q2, offset)
      s1.and(s2)
      s1
    }
  }

  private def or(idx: Int, q1: Query, q2: Query, offset: Int): RoaringBitmap = {
    val s1 = findImpl(idx, q1, offset)
    val s2 = findImpl(idx, q2, offset)
    s1.or(s2)
    s1
  }

  private def equal(idx: Int, k: String, v: String, offset: Int): RoaringBitmap = {
    val internedK = interner.intern(k)
    val vidx = itemIndex.get(internedK)
    if (vidx == null) new RoaringBitmap() else {
      val internedV = interner.intern(v)
      val matchSet = vidx.get(internedV)
      if (matchSet == null) new RoaringBitmap() else withOffset(matchSet(idx), offset)
    }
  }

  private def greaterThan(idx: Int, k: String, v: String, orEqual: Boolean): RoaringBitmap = {
    val internedK = interner.intern(k)
    val vidx = itemIndex.get(internedK)
    if (vidx == null) new RoaringBitmap() else {
      val set = new RoaringBitmap()
      val tag = Tag(internedK, v, -1)
      var i = tagOffset(tag)
      // Skip if equal
      if (!orEqual && i < tags.length && tags(i).key == internedK && tags(i).value == v) {
        i += 1
      }
      // Data is sorted, no need to perform a check for each entry if key matches
      while (i < tags.length && tags(i).key == internedK) {
        set.or(vidx.get(tags(i).value)(idx))
        i += 1
      }
      set
    }
  }

  private def lessThan(idx: Int, k: String, v: String, orEqual: Boolean): RoaringBitmap = {
    val internedK = interner.intern(k)
    val vidx = itemIndex.get(internedK)
    if (vidx == null) new RoaringBitmap() else {
      val set = new RoaringBitmap()
      val tag = Tag(internedK, v, -1)
      var i = tagOffset(tag)
      // Skip if equal
      if (!orEqual && i >= 0 && tags(i).key == internedK && tags(i).value == v) {
        i -= 1
      }
      // Data is sorted, no need to perform a check for each entry if key matches
      while (i >= 0 && tags(i).key == internedK) {
        set.or(vidx.get(tags(i).value)(idx))
        i -= 1
      }
      set
    }
  }

  private def strPattern(idx: Int, q: Query.PatternQuery, offset: Int): RoaringBitmap = {
    val internedK = interner.intern(q.k)
    val vidx = itemIndex.get(internedK)
    if (vidx == null) new RoaringBitmap() else {
      val set = new RoaringBitmap()
      if (q.pattern.prefix.isDefined) {
        val prefix = q.pattern.prefix.get
        val tag = Tag(internedK, prefix, -1)
        var i = tagOffset(tag)
        while (i < tags.length &&
          tags(i).key == internedK &&
          tags(i).value.startsWith(prefix)) {
          if (q.check(tags(i).value)) {
            set.or(vidx.get(tags(i).value)(idx))
          }
          i += 1
        }
      } else {
        val entries = vidx.entrySet.iterator
        while (entries.hasNext) {
          val entry = entries.next()
          if (q.check(entry.getKey))
            set.or(withOffset(entry.getValue()(idx), offset))
        }
      }
      set
    }
  }

  private def hasKey(idx: Int, k: String, offset: Int): RoaringBitmap = {
    val internedK = interner.intern(k)
    val matchSet = keyIndex.get(internedK)
    if (matchSet == null) new RoaringBitmap() else withOffset(matchSet(idx), offset)
  }

  private def itemOffset(v: String): Int = {
    if (v == null || v == "") 0 else {
      val offsetV = new BigInteger(v, 16)
      val pos = util.Arrays.binarySearch(itemIds.asInstanceOf[Array[AnyRef]], offsetV)
      if (pos < 0) -pos - 1 else pos
    }
  }

  private def tagOffset(v: Tag): Int = {
    if (v == null || v.key == "") 0 else {
      val pos = util.Arrays.binarySearch(tags.asInstanceOf[Array[AnyRef]], v)
      if (pos == -1) 0 else if (pos < -1) -pos - 1 else pos
    }
  }

  private def findOffset[V <: AnyRef](vs: Array[V], v: V): Int = {
    if (v == null) 0 else {
      val pos = util.Arrays.binarySearch(vs.asInstanceOf[Array[AnyRef]], v)
      if (pos == -1) 0 else if (pos < -1) -pos - 2 else pos
    }
  }

  def findTags(query: TagQuery): List[Tag] = {
    import com.netflix.atlas.core.model.Query._
    val q = query.query.getOrElse(Query.True)
    val k = query.key
    if (k.isDefined) {
      val tq = query.copy(offset = query.offsetTag.value)
      findValues(tq).map(v => Tag(k.get, v))
    } else {
      val offset = tagOffset(query.offsetTag)
      // If key is restricted add a has query to search
      val finalQ = if (k.isEmpty) q else And(HasKey(k.get), q)

      val matchSet = withOffset(findImpl(Tags, finalQ, 0), offset)
      val result = List.newBuilder[Tag]
      val iter = matchSet.getIntIterator
      while (iter.hasNext) {
        result += this.tags(iter.next())
      }

      val ts = result.result()
      ts
    }
  }

  def findKeys(query: TagQuery): List[TagKey] = {
    val q = query.query.getOrElse(Query.True)
    val offset = findOffset(keys, query.offset)
    val matchSet = withOffset(findImpl(Keys, q, 0), offset)
    createResultList(keys, matchSet, query.limit).map(v => TagKey(v, -1))
  }

  def findValues(query: TagQuery): List[String] = {
    require(query.key.isDefined, "key must be defined for findValues query")
    import com.netflix.atlas.core.model.Query._
    val q = query.query
    val k = query.key.get
    val offset = findOffset(values, query.offset)

    // If key is restricted add a has query to search
    val finalQ = if (q.isDefined) And(HasKey(k), q.get) else HasKey(k)

    val matchSet = withOffset(findImpl(Values, finalQ, 0), offset)
    createResultList(values, matchSet, query.limit)
  }

  def findItems(query: TagQuery): List[T] = {
    val offset = itemOffset(query.offset)
    val limit = query.limit
    val intSet = query.query.fold(withOffset(all(Items), offset))(q => findImpl(Items, q, offset))
    createResultList(items, intSet, limit)
  }

  private def createResultList[V <: AnyRef](vs: Array[V], matchSet: RoaringBitmap, limit: Int): List[V] = {
    val list = List.newBuilder[V]
    val iter = matchSet.getIntIterator
    var count = 0
    while (iter.hasNext && count < limit) {
      list += vs(iter.next())
      count += 1
    }
    list.result
  }

  val size: Int = items.length
}
