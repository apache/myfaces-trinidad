/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidadinternal.util;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;

import java.lang.reflect.Array;

import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;

import java.util.Set;
import java.io.Serializable;

import java.util.AbstractCollection;
import java.util.AbstractSet;
import java.util.Arrays;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;
import java.util.concurrent.locks.ReentrantLock;

import org.apache.myfaces.trinidad.util.Args;


/**
 * CopyOnWrite-implementation of ArrayMap
 * Access is O(logN), using a binary search of the array of entries, sorted by key hash code.
 * Null keys are not supported.
 * Calling clone returns a new CopyOnWriteArrayMap with its own lock and its own set of
 * Map.Entry objects.  The keys and values themselves are not cloned.
 *
 * The iterators returned run off a snapshot of the array of entry objects until remove() is called
 * on the Iterator, If the set of entry objects has been modified since the Iterator was created,
 * a ConcurrentModificationException will be thrown.  Otherwise, the remove() operation succeeds
 * and the entry is removed from the CopyOnWriteArrayMap.
 * @param <K>
 * @param <V>
 */
public final class CopyOnWriteArrayMap<K,V> implements ConcurrentMap<K,V>, Cloneable, Serializable
{
  public static <K,V> CopyOnWriteArrayMap<K,V> newConcurrentMap()
  {
    ConcurrentEntryFactory<K,V> factory = ConcurrentEntryFactory.getInstance();

    return new CopyOnWriteArrayMap<K,V>(factory);
  }

  /**
   * Returns a CopyOnWriteArrayMap implementing a least-recently-used strategy to determine
   * which entries to prune when the maxSize is exceeded.
   *
   * An entry is considered accessed if the entry's value is retreived or modified.
   *
   * The precision with which the least recently used entry is determined is dependent on the
   * resolution of the platform's System.nanoTime() implementation.  In addition, no effort is
   * made to handle entries that are updated while the CopyOnWriteArrayMap is determining
   * which entry to purge.
   *
   * @param <K> Type of the keys in the CopyOnWriteArrayMap
   * @param <V> Type of the mapped values
   * @param maxSize The maximum number of entries allowed in the CopyOnWriteArrayMap
   * @return
   * @see java.lang.System#nanoTime
   */
  public static <K,V> CopyOnWriteArrayMap<K,V> newLRUConcurrentMap(int maxSize)
  {
    if (maxSize < 0)
      maxSize = 0;

    LRUEntryFactory<K,V> entryFactory = new LRUEntryFactory<K,V>(maxSize, System.nanoTime());

    return new CopyOnWriteArrayMap<K,V>(entryFactory);
  }

  private CopyOnWriteArrayMap(EntryFactory<?,K,V> entryFactory)
  {
    this(entryFactory, new ReentrantLock(), entryFactory.getEmptyEntries());
  }

  private CopyOnWriteArrayMap(
      EntryFactory<?,K,V> entryFactory, ReentrantLock writeLock, ConcurrentEntry<K,V>[] entries)
  {
    _entryFactory = entryFactory;
    _writeLock = writeLock;
    _entries   = entries;
  }

  @Override
  public V putIfAbsent(K key, V value)
  {
    V oldValue;

    final ReentrantLock writeLock = this._writeLock;

    writeLock.lock();

    try
    {
      ConcurrentEntry<K,V>[] entries = _entries;

      int entryIndex = _getEntryIndex(entries, key);

      if (entryIndex >= 0)
      {
        ConcurrentEntry<K,V> entry = entries[entryIndex];

        oldValue = entry.getValue();
      }
      else
      {
        // the insert locations are returned as two's complement
        int insertIndex = -(entryIndex + 1);

        _entries = _insertEntryAt(entries, key, value, insertIndex, _entryFactory);

        oldValue = null;
      }
    }
    finally
    {
      writeLock.unlock();
    }

    return oldValue;
  }

  @Override
  public boolean remove(Object key, Object value)
  {
    boolean removed = false;

    final ReentrantLock writeLock = this._writeLock;

    writeLock.lock();

    try
    {
      ConcurrentEntry<K,V>[] entries = _entries;

      int removeIndex = _getEntryIndex(entries, key);

      if (removeIndex >= 0)
      {
        ConcurrentEntry<K,V> entry = entries[removeIndex];

        V entryValue = entry.getValue();

        boolean valuesEqual = (entryValue != null) ? entryValue.equals(value) : (value == null);

        if (valuesEqual)
        {
          _entries = _removeEntryByIndex(entries, removeIndex);
          removed = true;
        }
      }
    }
    finally
    {
      writeLock.unlock();
    }

    return removed;
  }

  @Override
  public boolean replace(K key, V oldValue, V newValue)
  {
    ConcurrentEntry<K,V>[] entries = _entries;

    ConcurrentEntry<K,V> entry = _getEntry(entries, key);

    if (entry != null)
    {
      return entry.compareAndSetValue(oldValue, newValue);
    }
    else
    {
      return false;
    }
  }

  @Override
  public V replace(K key, V value)
  {
    ConcurrentEntry<K,V>[] entries = _entries;

    ConcurrentEntry<K,V> entry = _getEntry(entries, key);

    if (entry != null)
    {
      return entry.setValue(value);
    }
    else
    {
      return null;
    }
  }

  @Override
  public int size()
  {
    return _entries.length;
  }

  @Override
  public boolean isEmpty()
  {
    return _entries.length == 0;
  }

  @Override
  public boolean containsKey(Object key)
  {
    return _getEntry(_entries, key) != null;
  }

  private static <K,V> ConcurrentEntry<K,V> _getEntry(ConcurrentEntry<K,V>[] entries, Object key)
  {
    if (key == null)
      return null;

    int entryIndex = _getEntryIndex(entries, key);

    if (entryIndex >= 0)
    {
      return entries[entryIndex];
    }
    else
    {
      return null;
    }
  }

  /**
   * Handles the more complicated case where we matched the keyHashCode, but the key didn't match.
   * This implies we have a hash collision and need to search the adjacent entries for a match
   * @param entries
   * @param key
   * @param keyHashCode
   * @param startIndex
   * @return
   */
  private static <K,V> int _getHashCollsionMatchingEntryIndex(
      ConcurrentEntry<K,V>[] entries, Object key, int keyHashCode, int startIndex)
  {
    int beforeIndex = startIndex - 1;

    // search before the match
    while (beforeIndex >= 0)
    {
      ConcurrentEntry<K,V> entry = entries[beforeIndex];

      if (keyHashCode != entry.keyHashCode)
        break;

      if (key.equals(entry.getKey()))
        return beforeIndex;

      beforeIndex--;
    }

    // search after the match
    int entryCount = entries.length;
    int afterIndex = startIndex + 1;

    while (afterIndex < entryCount)
    {
      ConcurrentEntry<K,V> entry = entries[afterIndex];

      if (keyHashCode != entry.keyHashCode)
        break;

      if (key.equals(entry.getKey()))
        return afterIndex;

      afterIndex++;
    }

    // no match, but try ot optimize the index to be at the beginning or end of the array
    int insertIndex;

    if (beforeIndex == -1)
      insertIndex = 0;
    else
      insertIndex = afterIndex;

    // convert to two's complement
    return -(insertIndex + 1);
  }

  /**
   * Returns the positive index of the the entry, if the array contains an entry with the desired
   * key.  If no entry is found, the desired insertion location is returned as the two's
   * complement of the desired location.  For example, inserting before location 1, will be returned
   * as -2.
   * @param <K>
   * @param <V>
   * @param entries
   * @param key non-null key to search for
   * @return
   */
  private static <K,V> int _getEntryIndex(ConcurrentEntry<K,V>[] entries, Object key)
  {
    int keyHashCode = key.hashCode();

    // find key using a binary search of the key hash codes
    int lowIndex = 0;
    int highIndex = entries.length - 1;

    while (lowIndex <= highIndex)
    {
      int midIndex = (lowIndex + highIndex) >>> 1;

      ConcurrentEntry<K,V> entry = entries[midIndex];

      int midVal = entry.keyHashCode;

      if (midVal < keyHashCode)
      {
        lowIndex = midIndex + 1;
      }
      else if (midVal > keyHashCode)
      {
        highIndex = midIndex - 1;
      }
      else
      {
        if (key.equals(entry.getKey()))
        {
          // found it
          return midIndex;
        }
        else
        {
          // handle matching with hash collisions
          return _getHashCollsionMatchingEntryIndex(entries, key, keyHashCode, midIndex);
        }
      }
    }

    // key not found, so returns two's complement of the index where we would insert
    return -(lowIndex + 1);
  }

  private static <K,V> ConcurrentEntry<K,V>[] _removeEntryByIndex(
      ConcurrentEntry<K,V>[] entries, int removeIndex)
  {
    int originalSize = entries.length;
    int newSize = originalSize - 1;

    @SuppressWarnings("unchecked")
    ConcurrentEntry<K,V>[] newEntries = (ConcurrentEntry<K,V>[])
        Array.newInstance(entries.getClass().getComponentType(), newSize);

    if ((removeIndex == 0) || (removeIndex == newSize))
    {
      int srcStart = (removeIndex == 0) ? 1 : 0;

      System.arraycopy(entries, srcStart, newEntries, 0, newSize);
    }
    else
    {
      // copy everything before the removeIndex
      System.arraycopy(entries, 0, newEntries, 0, removeIndex);

      // copy everything after the removeIndex, shifting down 1
      System.arraycopy(entries, removeIndex + 1, newEntries, removeIndex, newSize - removeIndex);
    }

    return newEntries;
  }

  private static <K,V> ConcurrentEntry<K,V>[] _addEntryAtIndex(
      ConcurrentEntry<K,V>[] entries, ConcurrentEntry<K,V> entry, int insertIndex, int removeIndex)
  {
    int originalSize = entries.length;
    int newSize = originalSize;
    
    // if we haven't hit the LRU limit, increment the size
    if (removeIndex < 0)
      newSize++;

    @SuppressWarnings("unchecked")
    ConcurrentEntry<K,V>[] newEntries = (ConcurrentEntry<K,V>[])
        Array.newInstance(entry.getClass(), newSize);

    if (removeIndex >= 0)
    {
      if (removeIndex == insertIndex)
      {
        // inserting into same spot we removed, so just copy array
        System.arraycopy(entries, 0, newEntries, 0, originalSize);
      }
      else
      {
        if (removeIndex < insertIndex)
        {
          // copy everything before the removeIndex
          System.arraycopy(entries, 0, newEntries, 0, removeIndex);
          
          // copy everything between the removeIndex and the insertIndex, shifting things down
          System.arraycopy(entries, removeIndex + 1, newEntries, removeIndex, insertIndex - removeIndex - 1);
          
          // copy everything from the entry index to the end
          if (insertIndex < originalSize)
          {
            System.arraycopy(entries, insertIndex, newEntries, insertIndex, originalSize - insertIndex);
          }

          // we removed the entry before the insertion location, so decrement to account for this
          insertIndex--;
        }
        else
        {
          // copy everything before the insertIndex
          System.arraycopy(entries, 0, newEntries, 0, insertIndex);

          // copy everything between the insertIndex and the removeIndex, shifting things up
          System.arraycopy(entries, insertIndex, newEntries, insertIndex + 1, removeIndex - insertIndex);
          
          int afterRemoveIndex = removeIndex + 1;
          
          // copy everthing after the removeIndex
          if (afterRemoveIndex < originalSize)
          {
            System.arraycopy(entries, afterRemoveIndex, newEntries, afterRemoveIndex, originalSize - afterRemoveIndex);
          }
        }
      }
    }
    else
    {
      if ((insertIndex == 0) || (insertIndex == originalSize))
      {
        int destStart = (insertIndex == 0) ? 1 : 0;
          
        System.arraycopy(entries, 0, newEntries, destStart, originalSize);
      }
      else
      {
        // copy everything before the insertIndex
        System.arraycopy(entries, 0, newEntries, 0, insertIndex);
        
        // copy everything after the insertIndex
        System.arraycopy(entries, insertIndex, newEntries, insertIndex + 1, originalSize - insertIndex);
      }
    }

    newEntries[insertIndex] = entry;
    
    return newEntries;
  }

  @Override
  public boolean containsValue(Object value)
  {
    return _containsValue(_entries, value);
  }

  private static boolean _containsValue(ConcurrentEntry[] entries, Object value)
  {
    int entryCount = entries.length;

    if (value == null)
    {
      for (int i = 0; i < entryCount; i++)
      {
        // don't touch the values to avoid messing up the LRU
        if ( entries[i].getValueWithoutTouching() == null)
        {
          return true;
        }
      }
    }
    else
    {
      for (int i = 0; i < entryCount; i++)
      {
        // don't touch the values to avoid messing up the LRU
        if (value.equals(entries[i].getValueWithoutTouching()))
        {
          return true;
        }
      }
    }

    return false;
  }

  @Override
  public V get(Object key)
  {
    ConcurrentEntry<K,V> entry = _getEntry(_entries, key);

    if (entry != null)
    {
      return entry.getValue();
    }
    else
    {
      return null;
    }
  }

  private static <K,V> ConcurrentEntry<K,V>[] _insertEntryAt(
      ConcurrentEntry<K,V>[] entries, K key, V value, int insertIndex, EntryFactory<?,K,V> entryFactory)
  {
    ConcurrentEntry<K,V> entry = entryFactory.newEntry(key, value);

    int removeIndex = entryFactory.getIndexOfEntryToPurge(entries);

    return _addEntryAtIndex(entries, entry, insertIndex, removeIndex);
  }

  @Override
  public V put(K key, V value)
  {
    V oldValue;

    final ReentrantLock writeLock = this._writeLock;

    writeLock.lock();

    try
    {
      ConcurrentEntry<K,V>[] entries = _entries;

      int entryIndex = _getEntryIndex(entries, key);

      if (entryIndex >= 0)
      {
        ConcurrentEntry<K,V> entry = entries[entryIndex];

        oldValue = entry.setValue(value);
      }
      else
      {
        // the insert locations are returned as two's complement
        int insertIndex = -(entryIndex + 1);

        _entries = _insertEntryAt(entries, key, value, insertIndex, _entryFactory);

        oldValue = null;
      }
    }
    finally
    {
      writeLock.unlock();
    }

    return oldValue;
  }

  @Override
  public V remove(Object key)
  {
    V oldValue;

    final ReentrantLock writeLock = this._writeLock;

    writeLock.lock();

    try
    {
      ConcurrentEntry<K,V>[] entries = _entries;

      int removeIndex = _getEntryIndex(entries, key);

      if (removeIndex >= 0)
      {
        ConcurrentEntry<K,V> entry = entries[removeIndex];

        oldValue = entry.getValue();

        _entries = _removeEntryByIndex(entries, removeIndex);
      }
      else
      {
        oldValue = null;
      }
    }
    finally
    {
      writeLock.unlock();
    }

    return oldValue;
  }

  @Override
  public void putAll(Map<? extends K, ? extends V> m)
  {
    final ReentrantLock writeLock = this._writeLock;

    writeLock.lock();

    try
    {
      for (Map.Entry<? extends K, ? extends V> e : m.entrySet())
      {
        put(e.getKey(), e.getValue());
      }
    }
    finally
    {
      writeLock.unlock();
    }
  }

  @Override
  @SuppressWarnings("unchecked")
  public void clear()
  {
    final ReentrantLock writeLock = this._writeLock;

    writeLock.lock();

    try
    {
      _entries = _entryFactory.getEmptyEntries();
    }
    finally
    {
      writeLock.unlock();
    }
  }

  private final class KeySet extends AbstractSet<K>
  {
    @Override
    public Iterator<K> iterator()
    {
      return new Iterator<K>()
      {
        private final Iterator<Entry<K,V>> _i = entrySet().iterator();

        @Override
        public boolean hasNext()
        {
          return _i.hasNext();
        }

        @Override
        public K next()
        {
          return _i.next().getKey();
        }

        @Override
        public void remove()
        {
          _i.remove();
        }
      };
    }

    @Override
    public int size()
    {
      return CopyOnWriteArrayMap.this.size();
    }

    @Override
    public boolean isEmpty()
    {
      return CopyOnWriteArrayMap.this.isEmpty();
    }

    @Override
    public void clear()
    {
      CopyOnWriteArrayMap.this.clear();
    }

    @Override
    public boolean contains(Object k)
    {
      return CopyOnWriteArrayMap.this.containsKey(k);
    }

    @Override
    public boolean removeAll(Collection<?> c)
    {
      return CopyOnWriteArrayMap.this.__removeOrRetainAll(this, c, false);
    }

    @Override
    public boolean retainAll(Collection<?> c)
    {
      return CopyOnWriteArrayMap.this.__removeOrRetainAll(this, c, true);
    }
  }

  @Override
  public Set<K> keySet()
  {
    return new KeySet();
  }

  private final class ValueCollection extends AbstractCollection<V>
  {
    public Iterator<V> iterator()
    {
      return new Iterator<V>()
      {
        private final Iterator<Entry<K,V>> _i = entrySet().iterator();

        @Override
        public boolean hasNext()
        {
          return _i.hasNext();
        }

        @Override
        public V next()
        {
          ConcurrentEntry<K,V> entry = (ConcurrentEntry<K,V>)_i.next();

          // don't touch the values when iterating
          return entry.getValueWithoutTouching();
        }

        @Override
        public void remove()
        {
          _i.remove();
        }
      };
    }

    @Override
    public int size()
    {
      return CopyOnWriteArrayMap.this.size();
    }

    @Override
    public boolean isEmpty()
    {
      return CopyOnWriteArrayMap.this.isEmpty();
    }

    @Override
    public void clear()
    {
      CopyOnWriteArrayMap.this.clear();
    }

    @Override
    public boolean contains(Object v)
    {
      return CopyOnWriteArrayMap.this.containsValue(v);
    }

    @Override
    public boolean removeAll(Collection<?> c)
    {
      return CopyOnWriteArrayMap.this.__removeOrRetainAll(this, c, false);
    }

    @Override
    public boolean retainAll(Collection<?> c)
    {
      return CopyOnWriteArrayMap.this.__removeOrRetainAll(this, c, true);
    }
  }

  @Override
  public Collection<V> values()
  {
    return new ValueCollection();
  }

  @Override
  public Set<Entry<K, V>> entrySet()
  {
    return new EntrySet();
  }

  /**
   * Either remove or retain all of the entries in the Collection c that are in ourIterable,
   * grabbing a writeLock on the CopyOnWriteArrayMap before performing the operation
   * @param <E>
   * @param ourIterable
   * @param c
   * @param retain
   * @return
   */
  boolean __removeOrRetainAll(Iterable<?> ourIterable, Collection<?> c, boolean retain)
  {
    boolean modified = false;

    _writeLock.lock();

    try
    {
      Iterator<?> it = ourIterable.iterator();

      while (it.hasNext())
      {
        Object entry = it.next();

        if (retain ^ c.contains(entry))
        {
          it.remove();
          modified = true;
        }
      }
    }
    finally
    {
      _writeLock.unlock();
    }

    return modified;
  }

  private final class EntryIterator implements Iterator<Entry<K,V>>
  {
    EntryIterator()
    {
      _entries = CopyOnWriteArrayMap.this._entries;
      _cursorIndex = 0;
      _lastReturnedIndex = -1;
    }

    @Override
    public boolean hasNext()
    {
      return _cursorIndex < _entries.length;
    }

    @Override
    public Entry<K, V> next()
    {
      try
      {
        Entry<K,V> entry = _entries[_cursorIndex];
        _lastReturnedIndex = _cursorIndex;
        _cursorIndex++;

        return entry;
      }
      catch (IndexOutOfBoundsException ioobe)
      {
        throw new NoSuchElementException();
      }
    }

    @Override
    public void remove()
    {
      // can't remove an entry we haven't visited or one that we already removed
      if (_lastReturnedIndex < 0)
        throw new IllegalStateException();

      final ReentrantLock writeLock = CopyOnWriteArrayMap.this._writeLock;

      writeLock.lock();

      try
      {
        ConcurrentEntry<K,V>[] ourEntries = _entries;
        ConcurrentEntry<K,V>[] baseEntries = CopyOnWriteArrayMap.this._entries;

        // somebody has alread messed with the map since we were created
        if (ourEntries != baseEntries)
          throw new ConcurrentModificationException();

        _entries = _removeEntryByIndex(ourEntries, _lastReturnedIndex);
        CopyOnWriteArrayMap.this._entries = _entries;
      }
      finally
      {
        writeLock.unlock();
      }

      _cursorIndex--;

      // don't allow double removals
      _lastReturnedIndex = -1;
    }

    private ConcurrentEntry<K,V>[] _entries;
    private int _cursorIndex;
    private int _lastReturnedIndex;
  }


  private final class EntrySet extends AbstractSet<Entry<K,V>>
  {
    @Override
    public Iterator<Entry<K,V>> iterator()
    {
      return new EntryIterator();
    }

    @Override
    public boolean contains(Object o)
    {
      if (!(o instanceof Entry))
        return false;

      @SuppressWarnings("unchecked")
      Entry<K,V> theirEntry = (Entry<K,V>)o;

      ConcurrentEntry<K, V> ourEntry = _getEntry(_entries, theirEntry.getKey());

      if (ourEntry != null)
      {
        // entries are equal if their keys and values are equal
        Object ourValue = ourEntry.getValue();
        Object theirValue = theirEntry.getValue();

        return (ourValue != null) ? ourValue.equals(theirValue) : (theirValue == null);
      }

      return false;
    }

    @Override
    public boolean remove(Object o)
    {
      if (!(o instanceof Entry))
        return false;

      @SuppressWarnings("unchecked")
      Entry<K,V> removeEntry = (Entry<K,V>)o;

      return CopyOnWriteArrayMap.this.remove(removeEntry.getKey(), removeEntry.getValue());
    }

    @Override
    public int size()
    {
      return CopyOnWriteArrayMap.this.size();
    }

    @Override
    public void clear()
    {
      CopyOnWriteArrayMap.this.clear();
    }

    @Override
    public Object[] toArray()
    {
      // override for efficiency
      ConcurrentEntry<K,V>[] entries = CopyOnWriteArrayMap.this._entries;
      return Arrays.copyOf(entries, entries.length);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T[] toArray(T[] a)
    {
      // override for efficiency
      ConcurrentEntry<K,V>[] entries = CopyOnWriteArrayMap.this._entries;
      int entryCount = entries.length;

      if (a.length < entryCount)
      {
        // destination array is too small, so return a new array of the correct type
        return Arrays.copyOf(entries, entryCount, (Class<? extends T[]>) a.getClass());
      }
      else
      {
        System.arraycopy(entries, 0, a, 0, entryCount);

        // add end marker, if the destination array is too big
        if (a.length > entryCount)
          a[entryCount] = null;

        return a;
      }
    }

    @Override
    public boolean removeAll(Collection<?> c)
    {
      return CopyOnWriteArrayMap.this.__removeOrRetainAll(this, c, false);
    }

    @Override
    public boolean retainAll(Collection<?> c)
    {
      return CopyOnWriteArrayMap.this.__removeOrRetainAll(this, c, true);
    }
  }

  @Override
  public boolean equals(Object o)
  {
    if (o == this)
      return true;

    if (!(o instanceof Map))
      return false;

    Map<?,?> otherMap = (Map<?,?>)o;

    ConcurrentEntry<K,V>[] entries = _entries;
    int entryCount = entries.length;

    if (entryCount != otherMap.size())
      return false;

    for (int i = 0; i < entryCount; i++)
    {
      ConcurrentEntry<K,V> entry = entries[i];

      K entryKey = entry.getKey();
      V entryValue = entry.getValue();

      if (entryValue != null)
      {
        Object otherValue = otherMap.get(entryKey);

        if (!entryValue.equals(otherValue))
        {
          return false;
        }
      }
      else
      {
        Object otherValue = otherMap.get(entryKey);

        // use containsKey to handle case where the otherValue was null because the
        // otherMap didn't contain an entry for this key
        if ((otherValue != null) || !otherMap.containsKey(entryKey))
        {
          return false;
        }
      }
    }

    return true;
  }

  @Override
  public int hashCode()
  {
    int hash = 0;

    ConcurrentEntry<K,V>[] entries = _entries;

    for (Entry<K,V> entry : entries)
    {
      hash += entry.hashCode();
    }

    return hash;
  }

  @Override
  public String toString()
  {
    ConcurrentEntry<K,V>[] entries = _entries;

    // optimize the empty case
    if (entries.length == 0)
      return "{}";

    StringBuilder sb = new StringBuilder();

    sb.append('{');

    boolean isFirstEntry = true;

    for (Entry<K,V> entry : entries)
    {
      if (isFirstEntry)
      {
        isFirstEntry = false;
      }
      else
      {
        sb.append(", ");
      }

      K key   = entry.getKey();
      V value = entry.getValue();

      sb.append(key   == this ? "(this Map)" : key);
      sb.append('=');
      sb.append(value == this ? "(this Map)" : value);
    }

    sb.append('}');

    return sb.toString();
  }

  /**
   * Returns a shallow copy of this <tt>CopyOnWriteArrayMap</tt> instance with its own lock
   * and entry objects: the keys and values themselves are not cloned.
   *
   * @return a shallow copy of this map
   */
  @Override
  public CopyOnWriteArrayMap<K,V> clone()
  {
    ConcurrentEntry<K,V>[] entries = _entryFactory.cloneEntries(_entries);

    // use a copy constructor since the writeLock is final, but the clone needs a new one
    return new CopyOnWriteArrayMap<K,V>(_entryFactory, new ReentrantLock(), entries);
  }

  private void readObject(@SuppressWarnings("unused") ObjectInputStream inStream) throws InvalidObjectException
  {
    throw new InvalidObjectException("Proxy Required");
  }

  private Object writeReplace()
  {
    return _entryFactory.newSerializationProxy(_entries);
  }

  protected abstract static class SerializationProxy<E extends ConcurrentEntry<K,V>,K,V> implements Serializable
  {
    @SuppressWarnings("compatibility:-8476139976280416592")
    private static final long serialVersionUID = 1L;

    protected SerializationProxy(ConcurrentEntry<K,V>[] entries)
    {
      _keyValues = _createKeyValues(entries);
    }

    private Object readResolve()
    {
      ReentrantLock writeLock = new ReentrantLock();

      EntryFactory<E,K,V> entryFactory = instantiateEntryFactory();
      ConcurrentEntry<K,V>[] entries = instantiateEntries(_keyValues, entryFactory);

      return new CopyOnWriteArrayMap<K,V>(entryFactory, writeLock, entries);
    }

    protected abstract E[] instantiateEntries(Object[] keyValues, EntryFactory<E,K,V> entryFactory);

    protected abstract EntryFactory<E,K,V> instantiateEntryFactory();

    private static <K,V> Serializable[] _createKeyValues(ConcurrentEntry<K,V>[] entries)
    {
      int entryCount = entries.length;

      Serializable[] keyValues = new Serializable[entryCount *2];

      for (int entryIndex = 0, keyValueIndex = 0; entryIndex < entryCount; entryIndex++)
      {
        ConcurrentEntry<K,V> entry = entries[entryIndex];
        keyValues[keyValueIndex++] = (Serializable)entry.getKey();
        keyValues[keyValueIndex++] = (Serializable)entry.getValue();
      }

      return keyValues;
    }

    private final Serializable[] _keyValues;
  }

  /**
   * Manages the Entry-specific behavior between the ConcurrentEntries and the LRUEntries
   */
  private abstract static class EntryFactory<E extends ConcurrentEntry<K,V>, K, V>
  {
    /** Creates a new ConcurrentEntry with the specified key and value */
    public abstract E newEntry(K key, V value);

    /** Returns the index of an entry to remove as a result of adding another entry.  If
     * a value < 0 is returned, no entry will be removed
     * */
    public abstract int getIndexOfEntryToPurge(E[] entries);

    /** Returns a empty array of the correct type for this EntryFactory */
    public abstract E[] getEmptyEntries();

    /** Returns the Serialization proxy to use to serialize the CopyOnWriteArrayMap */
    public abstract SerializationProxy<E,K,V> newSerializationProxy(E[] entries);

    /**
     * @param entries
     * @return a deep copy of the ConcurrentEntry[].  The keys and values themselves are not cloned
     */
    public final E[] cloneEntries(E[] entries)
    {
      E[] clonedEntries = entries.clone();

      int entryCount = entries.length;

      for (int i = 0; i < entryCount; i++)
      {
        E originalEntry = clonedEntries[i];
        E clonedEntry = newEntry(originalEntry.getKey(), originalEntry.getValue());

        clonedEntries[i] = clonedEntry;
      }

      return clonedEntries;
    }
  }

  private final static class ConcurrentEntryFactory<K,V> extends EntryFactory<ConcurrentEntry<K,V>,K,V>
  {
    @SuppressWarnings("unchecked")
    public static <K,V> ConcurrentEntryFactory<K,V> getInstance()
    {
      return (ConcurrentEntryFactory<K,V>)_INSTANCE;
    }

    @Override
    public ConcurrentEntry<K,V> newEntry(K key, V value)
    {
      return new ConcurrentEntry<K,V>(key, value);
    }

    @Override
    public int getIndexOfEntryToPurge(ConcurrentEntry<K,V>[] entries)
    {
      // we never purge entries
      return -1;
    }

    @Override
    @SuppressWarnings("unchecked")
    public ConcurrentEntry<K,V>[] getEmptyEntries()
    {
      return (ConcurrentEntry<K,V>[])_EMPTY_ENTRIES;
    }

    @Override
    public SerializationProxy<ConcurrentEntry<K,V>,K,V> newSerializationProxy(ConcurrentEntry<K,V>[] entries)
    {
      return new ConcurrentSerializationProxy<K,V>(entries);
    }

    protected static final class ConcurrentSerializationProxy<K,V> extends SerializationProxy<ConcurrentEntry<K,V>, K,V>
    {
      @SuppressWarnings("compatibility:2346067066761682441")
      private static final long serialVersionUID = 1L;

      protected ConcurrentSerializationProxy(ConcurrentEntry<K,V>[] entries)
      {
        super(entries);
      }

      @Override
      public EntryFactory<ConcurrentEntry<K,V>,K,V> instantiateEntryFactory()
      {
        return getInstance();
      }

      @Override
      @SuppressWarnings({"cast", "unchecked"})
      protected ConcurrentEntry<K,V>[] instantiateEntries(
          Object[] keyValues, EntryFactory<ConcurrentEntry<K,V>,K,V> entryFactory)
      {
        int entryCount = keyValues.length / 2;

        ConcurrentEntry<K,V>[] entries = (ConcurrentEntry<K,V>[])new ConcurrentEntry[entryCount];

        for (int entryIndex = 0, keyValueIndex = 0; entryIndex < entryCount; entryIndex++)
        {
          K key   = (K)keyValues[keyValueIndex++];
          V value = (V)keyValues[keyValueIndex++];

          ConcurrentEntry<K,V> entry = new ConcurrentEntry<K,V>(key, value);
          entries[entryIndex] = entry;
        }

        return entries;
      }
    }

    private static final EntryFactory<?,?,?> _INSTANCE = new ConcurrentEntryFactory();
    private static final ConcurrentEntry[] _EMPTY_ENTRIES = new ConcurrentEntry[0];
  }

  private final static class LRUEntryFactory<K,V> extends EntryFactory<LRUEntry<K,V>, K, V>
  {
    public LRUEntryFactory(int maxEntries, long baseNanos)
    {
      _maxEntries = maxEntries;
      _baseNanos  = baseNanos;
    }

    @Override
    @SuppressWarnings("unchecked")
    public LRUEntry<K,V> newEntry(K key, V value)
    {
      return new LRUEntry<K,V>(key, value, this);
    }

    @Override
    public int getIndexOfEntryToPurge(LRUEntry<K,V>[] entries)
    {
      if (_maxEntries <= entries.length)
      {
        // purge the oldest entry to make room for the new one
        return _getOldestAccessesedEntryIndex(entries);
      }
      else
      {
        return -1;
      }
    }

    /**
     * @param entries
     * @return The index of the least recently accessed entry
     */
    private static <K,V> int _getOldestAccessesedEntryIndex(LRUEntry<K,V>[] entries)
    {
      int entryCount = entries.length;
      int oldestIndex = -1;
      long oldestAccessedNanos = Long.MAX_VALUE;

      for (int i = 0; i < entryCount; i++)
      {
        LRUEntry<K,V> entry = entries[i];
        long currAccessedNanos = entry.getLastAccessed();

        if (currAccessedNanos <= oldestAccessedNanos)
        {
          oldestIndex = i;
          oldestAccessedNanos = currAccessedNanos;
        }
      }

      return oldestIndex;
    }

    @Override
    @SuppressWarnings("unchecked")
    public LRUEntry<K,V>[] getEmptyEntries()
    {
      return (LRUEntry<K,V>[])_EMPTY_LRU_ENTRIES;
    }

    @Override
    public SerializationProxy<LRUEntry<K,V>,K,V> newSerializationProxy(LRUEntry<K,V>[] entries)
    {
      return new LRUSerializationProxy<K,V>(_maxEntries, _baseNanos, entries);
    }

    /**
     * @return delta in nano seconds from when the LRUConcurrentArrayMap was created.  If we exceed
     * the magnitude of a long, return Long.MAX_VALUE.  In practice, this isn't a problem as
     * it means that more than 292 years have elapsed.
     */
    public long nanosSinceCreated()
    {
      long nanos = System.nanoTime();
      long delta = nanos - _baseNanos;

      if (delta >= 0)
      {
        return delta;
      }
      else
      {
        // we have exceeded the magnitude of a long because the nano value wrapped around
        return Long.MAX_VALUE;
      }
    }

    protected static final class LRUSerializationProxy<K,V> extends SerializationProxy<LRUEntry<K,V>,K,V>
    {
      @SuppressWarnings("compatibility:-4809944737577473688")
      private static final long serialVersionUID = 1L;

      protected LRUSerializationProxy(int maxEntries, long baseNanos, ConcurrentEntry<K,V>[] entries)
      {
        super(entries);

        _maxEntries = maxEntries;
        _baseNanos  = baseNanos;
      }

      @Override
      public EntryFactory<LRUEntry<K,V>, K, V> instantiateEntryFactory()
      {
        return new LRUEntryFactory<K,V>(_maxEntries, _baseNanos);
      }

      @Override
      @SuppressWarnings({"cast", "unchecked"})
      protected LRUEntry<K,V>[] instantiateEntries(
          Object[] keyValues, EntryFactory<LRUEntry<K,V>,K,V> entryFactory)

      {
        int entryCount = keyValues.length / 2;

        LRUEntry<K,V>[] entries = (LRUEntry<K,V>[])new LRUEntry[entryCount];

        for (int entryIndex = 0, keyValueIndex = 0; entryIndex < entryCount; entryIndex++)
        {
          K key   = (K)keyValues[keyValueIndex++];
          V value = (V)keyValues[keyValueIndex++];

          LRUEntry<K,V> entry = new LRUEntry<K,V>(key, value, (LRUEntryFactory<K,V>)entryFactory);
          entries[entryIndex] = entry;
        }

        return entries;
      }

      private final int _maxEntries;
      private final long _baseNanos;
    }

    private static final ConcurrentEntry[] _EMPTY_LRU_ENTRIES = new LRUEntry[0];

    private final int _maxEntries;
    private final long _baseNanos;
  }

  protected static class ConcurrentEntry<K,V> implements Entry<K,V>
  {
    protected ConcurrentEntry(K key, V value)
    {
      Args.notNull(key, "key");

      _key = key;
      _value = value;
      keyHashCode = key.hashCode();
    }

    @Override
    public final K getKey()
    {
      return _key;
    }

    /** Returns the value without counting as accessing the entry */
    public V getValueWithoutTouching()
    {
      return _value;
    }

    @Override
    public V getValue()
    {
      return _value;
    }

    /**
     * Version of setValue with compareAndSet semantics
     * @param expected
     * @param newValue
     * @return
     */
    public boolean compareAndSetValue(V expected, V newValue)
    {
      return _VALUE_UPDATER.compareAndSet(this, expected, newValue);
    }

    @Override
    @SuppressWarnings({"cast", "unchecked"})
    public V setValue(V newValue)
    {
      return (V)_VALUE_UPDATER.getAndSet(this, newValue);
    }

    @Override
    public boolean equals(Object o)
    {
      if (o == this)
        return true;

      if (o instanceof Entry)
      {
        Entry otherEntry = (Entry)o;

        if (getKey().equals(otherEntry.getKey()))
        {
          Object otherValue = otherEntry.getValue();
          V value = _value;

          return (value != null) ? value.equals(otherValue) : otherValue == null;
        }
      }

      return false;
    }

    @Override
    public int hashCode()
    {
      V value = _value;
      int valueHashCode = (value != null) ? value.hashCode() : 0;

      return keyHashCode ^ valueHashCode;
    }

    @Override
    public String toString()
    {
      return getKey() + "=" + _value;
    }

    private volatile V _value;
    private final K _key;
    public  final int keyHashCode;

    // Apply AtomicReference love to the _value field
    private static final AtomicReferenceFieldUpdater<ConcurrentEntry, Object> _VALUE_UPDATER =
        AtomicReferenceFieldUpdater.newUpdater(ConcurrentEntry.class, Object.class, "_value");
  }

  private static final class LRUEntry<K,V> extends ConcurrentEntry<K,V>
  {
    public LRUEntry(K key, V value, LRUEntryFactory<K,V> nanoCalculator)
    {
      super(key, value);

      _nanoCalculator = nanoCalculator;
      _lastAccessed = _nanoCalculator.nanosSinceCreated();
    }

    public long getLastAccessed()
    {
      return _lastAccessed;
    }

    @Override
    public V getValue()
    {
      // we don't especially care that we don't update the last accessed time atomically with
      // updating the value
      _lastAccessed = _nanoCalculator.nanosSinceCreated();

      return super.getValue();
    }

    @Override
    public V setValue(V newValue)
    {
      // we don't especially care that we don't update the last accessed time atomically with
      // updating the value
      _lastAccessed = _nanoCalculator.nanosSinceCreated();

      return super.setValue(newValue);
    }

    private final LRUEntryFactory<K,V> _nanoCalculator;
    private volatile long _lastAccessed;
  }

  @SuppressWarnings("compatibility:4274080938865508278")
  private static final long serialVersionUID = 1;

  private transient final EntryFactory<?,K,V> _entryFactory;

  // lock protecting mutators
  private transient final ReentrantLock _writeLock;

  private volatile transient ConcurrentEntry<K,V>[] _entries;
}