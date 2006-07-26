/*
 * Copyright  2001-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.trinidadinternal.util;

import java.util.AbstractMap;
import java.util.Enumeration;
import java.util.NoSuchElementException;
import java.util.Set;




/**
 * A Map implementation that behaves like a Hashtable
 * with optimistic synchronization of gets.
 * This results in the following behavior:
 * <ol>
     <li>Hits on gets are unsynchronized</li>
     <li>Misses on gets are retried with synchronization on, resulting
         in slower behavior than a fully synchronized Hashtable</li>
     <li>Rehashing is slower and may happen more often<li>
     <li>Removed entries aren't removed until the OptimisticHashMap
         is rehashed or cloned, but the reference to the value
         <strong>is</strong> removed immediately.
 * </ol>
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/util/OptimisticHashMap.java#0 $) $Date: 10-nov-2005.19:56:01 $
 * @author The Oracle ADF Faces Team
 */
public class OptimisticHashMap extends AbstractMap implements Cloneable
{
  /**
   * Constructs a new, empty OptimisticHashMap with the specified initial
   * capacity and the specified load factor.
   *
   * @param      initialCapacity   the initial capacity of the hashtable.
   * @param      loadFactor        the load factor of the hashtable.
   * @exception  IllegalArgumentException  if the initial capacity is less
   *             than zero, or if the load factor is nonpositive.
   */
  public OptimisticHashMap(
    int   initialCapacity,
    float loadFactor
    )
  {
    if (initialCapacity < 0)
      throw new IllegalArgumentException("Illegal Capacity: "+
                                         initialCapacity);
    if (loadFactor <= 0)
      throw new IllegalArgumentException("Illegal Load: "+loadFactor);

    if (initialCapacity == 0)
      initialCapacity = 1;

    _loadFactor = loadFactor;
    _bins = new Entry[initialCapacity];
    _threshold = (int)(initialCapacity * loadFactor);
  }


  /**
   * Constructs a new, empty OptimisticHashMap with the specified initial
   * capacity and default load factor, which is <tt>0.75</tt>.
   *
   * @param     initialCapacity   the initial capacity of the hashtable.
   * @exception IllegalArgumentException if the initial capacity is less
   *              than zero.
   */
  public OptimisticHashMap(
    int initialCapacity
    )
  {
    this(initialCapacity, 0.75f);
  }


  /**
   * Constructs a new, empty OptimisticHashMap with a default capacity and load
   * factor, which is <tt>0.75</tt>.
   */
  public OptimisticHashMap()
  {
    this(101, 0.75f);
  }

  /**
   *
   */
  public Set entrySet()
  {
    throw new UnsupportedOperationException("entrySet not supported for OptimisticHashMap");
  }

  /**
   * Returns the number of keys in this hashtable.
   *
   * @return  the number of keys in this hashtable.
   */
  public int size()
  {
    return _entryCount;
  }

  /**
   * Tests if this hashtable maps no keys to values.
   *
   * @return  <code>true</code> if this hashtable maps no keys to values;
   *          <code>false</code> otherwise.
   */
  public boolean isEmpty()
  {
    return (_entryCount == 0);
  }

  /**
   * Returns an enumeration of the keys in this hashtable.
   *
   * @return  an enumeration of the keys in this hashtable.
   * @see     Enumeration
   */
  public synchronized Enumeration keys()
  {
    return new Enumerator(true);
  }

  /**
   * Returns an enumeration of the values in this hashtable.
   * Use the Enumeration methods on the returned object to fetch the elements
   * sequentially.
   *
   * @return  an enumeration of the values in this hashtable.
   * @see     java.util.Enumeration
   * @see     #keys()
   */
  public synchronized Enumeration elements()
  {
    return new Enumerator(false);
  }


  /**
   * Tests if some key maps into the specified value in this hashtable.
   * This operation is more expensive than the <code>containsKey</code>
   * method.<p>
   *
   * @param      value   a value to search for.
   * @return     <code>true</code> if and only if some key maps to the
   *             <code>value</code> argument in this hashtable as
   *             determined by the <tt>equals</tt> method;
   *             <code>false</code> otherwise.
   * @exception  NullPointerException  if the value is <code>null</code>.
   * @see        #containsKey(Object)
   */
  public boolean contains(
    Object value
    )
  {
    if (value == null)
    {
      throw new NullPointerException();
    }

    Entry[] bins = _bins;

    for (int i = bins.length ; i-- > 0 ;)
    {
      for (Entry entry = bins[i] ; entry != null ; entry = entry.next)
      {
        if (value.equals(entry.value))
        {
          return true;
        }
      }
    }

    //
    // try again with synchronization in case of memory model false negatives
    //
    synchronized (this)
    {
      bins = _bins;

      for (int i = bins.length ; i-- > 0 ;)
      {
        for (Entry entry = bins[i] ; entry != null ; entry = entry.next)
        {
          if (value.equals(entry.value))
          {
            return true;
          }
        }
      }
    }

    return false;
  }


  /**
   * Tests if the specified object is a key in this hashtable.
   *
   * @param   key   possible key.
   * @return  <code>true</code> if and only if the specified object
   *          is a key in this hashtable, as determined by the
   *          <tt>equals</tt> method; <code>false</code> otherwise.
   * @see     #contains(Object)
   */
  public boolean containsKey(
    Object key
    )
  {
    return (get(key) != null);
  }

  /**
   * Returns the value to which the specified key is mapped in this hashtable.
   *
   * @param   key   a key in the hashtable.
   * @return  the value to which the key is mapped in this hashtable;
   *          <code>null</code> if the key is not mapped to any value in
   *          this hashtable.
   * @see     #put(Object, Object)
   */
  public Object get(
    Object key
    )
  {
    if (key == null)
      throw new IllegalArgumentException();

    int     hash  = key.hashCode();
    Entry[] bins  = _bins;
    int     index = (hash & 0x7FFFFFFF) % bins.length;

    Object result = null;

    for (Entry entry = bins[index]; entry != null ; entry = entry.next)
    {
      if ((entry.hash == hash) && key.equals(entry.key))
      {
        result = entry.value;
        break;
      }
    }

    if (result == null)
    {
      //
      // try again synchronized
      //
      synchronized (this)
      {
        bins  = _bins;
        index = (hash & 0x7FFFFFFF) % bins.length;

        for (Entry entry = bins[index]; entry != null ; entry = entry.next)
        {
          if ((hash == entry.hash) && key.equals(entry.key))
          {
            return entry.value;
          }
        }
      }
    }

    return result;
  }

  /**
   * Increases the capacity of and internally reorganizes this
   * hashtable, in order to accommodate and access its entries more
   * efficiently.  This method is called automatically when the
   * number of keys in the hashtable exceeds this hashtable's capacity
   * and load factor.
   */
  protected void rehash()
  {
    Entry[] oldBins = _bins;
    int oldCapacity = oldBins.length;

    // detemine the percentage of of the load that was real entries
    double realFactor = ((double)_realCount) / ((double)_entryCount);

    int newCapacity;

    if (realFactor > 0.8)
    {
      // we most likely needed to bump up the capacity by a lot anyway
      newCapacity = oldCapacity * 2 + 1;
    }
    else
    {
      //
      // we have a lot of empty entries, so increase the capacity by
      // the empty factor
      newCapacity = (int)(((double)oldCapacity) * (1.0 / realFactor) + 1.0);

      // make sure that the new bin size is at least odd
      if ((newCapacity & 1) == 1)
      {
        newCapacity++;
      }
    }

    // allocate the new bins
    Entry[] newBins = new Entry[newCapacity];

    // calculate the new load factor
    _threshold = (int)(newCapacity * _loadFactor);

    for (int i = oldCapacity ; i-- > 0 ;)
    {
      for (Entry oldEntry = oldBins[i];
          oldEntry != null;
          oldEntry = oldEntry.next)
      {
        // only copy non-empty entries
        if (oldEntry.value != null)
        {
          // calculate the new bin index
          int newBinIndex = (oldEntry.hash & 0x7FFFFFFF) % newCapacity;

          // create a new entry instance of the new entry to avoid hosing
          // unsynchronized readers
          Entry newEntry = new Entry(oldEntry.hash,
                                     oldEntry.key,
                                     oldEntry.value,
                                     newBins[newBinIndex]);


          // add the entry to the front of the correct bin
          newBins[newBinIndex] = newEntry;
        }
      }
    }

    // we've purged the empty entries, the the real count is the
    // same as the entryCount
    _realCount = _entryCount;

    // atomically replace the bins
    _bins = newBins;
  }

  /**
   * Maps the specified <code>key</code> to the specified
   * <code>value</code> in this hashtable. Neither the key nor the
   * value can be <code>null</code>.
   * <p>
   * The value can be retrieved by calling the <code>get</code> method
   * with a key that is equal to the original key.
   * <p>
   * @param      key     the hashtable key.
   * @param      value   the value.
   * @return     The old value if replacing
   * @exception  NullPointerException  if the key or value is
   *               <code>null</code>.
   * @see     Object#equals(Object)
   * @see     #get(Object)
   */
  public synchronized Object put(
    Object key,
    Object value
    )
  {
    // make sure that the key isn't null
    if (key == null)
      throw new IllegalArgumentException();

    // Make sure the value is not null
    if (value == null)
    {
      throw new NullPointerException();
    }

    //
    // Check if the key is already in the Hashtable
    //
    Entry[] bins  = _bins;
    int     hash  = key.hashCode();
    int     index = (hash & 0x7FFFFFFF) % bins.length;

    for (Entry entry = bins[index] ; entry != null; entry = entry.next)
    {
      if ((entry.hash == hash) && key.equals(entry.key))
      {
        Object returnValue = entry.value;

        // assign the new value
        entry.value = value;

        // return the old value
        return returnValue;
      }
    }

    if (_entryCount >= _threshold)
    {
      // Rehash the table if the threshold is exceeded
      rehash();

      //
      // get the bins to add to and the bin index after we rehash
      //
      bins  = _bins;
      index = (hash & 0x7FFFFFFF) % bins.length;
    }

    // Creates the new entry
    bins[index] = new Entry(hash, key, value, bins[index]);
    _entryCount++;
    _realCount++;

    return null;
  }

  /**
   * Removes the key (and its corresponding value) from this
   * hashtable.
   *
   * @param   key   the key that needs to be removed.
   * @return  the value to which the key had been mapped in this hashtable,
   *          or <code>null</code> if the key did not have a mapping.
   */
  public synchronized Object remove(
    Object key
    )
  {
    // make sure that the key isn't null
    if (key == null)
      throw new IllegalArgumentException();

    //
    // Check if the key is already in the Hashtable
    //
    Entry[] bins  = _bins;
    int     hash  = key.hashCode();
    int     index = (hash & 0x7FFFFFFF) % bins.length;

    for (Entry entry = bins[index] ; entry != null; entry = entry.next)
    {
      if ((entry.hash == hash) && key.equals(entry.key))
      {
        Object returnValue = entry.value;

        // mark the entry as removed
        entry.value = null;

        // one less item
        _entryCount--;

        // return the old value
        return returnValue;
      }
    }

    return null;
  }


  /**
   * Clears this hashtable so that it contains no keys.
   */
  public synchronized void clear()
  {
    // reset the count of the number of entries
    _entryCount = 0;

    // reset the actual number of entries
    _realCount = 0;

    // clear out all of the bins
    _bins = new Entry[_bins.length];
  }


  /**
   * Creates a shallow copy of this OptimisticHashMap. All the structure of the
   * hashtable itself is copied, but the keys and values are not cloned.
   * This is a relatively expensive operation.
   *
   * @return  a clone of the hashtable.
   */
  public synchronized Object clone()
  {
    try
    {
      OptimisticHashMap newHashtable = (OptimisticHashMap)super.clone();

      Entry[] oldBins = newHashtable._bins;
      Entry[] newBins = new Entry[oldBins.length];

      for (int i = newBins.length ; i-- > 0 ;)
      {
        Entry currBin = oldBins[i];

        // clone the list of bins (Entry does this for us)
        newBins[i] = ((currBin != null) && (currBin.value != null))
                       ? (Entry)currBin.clone()
                       : null;
      }

      // empty entries aren't cloned so the realCount and the
      // entryCount should now be the same
      newHashtable._realCount = newHashtable._entryCount;

      newHashtable._bins = newBins;

      return newHashtable;
    }
    catch (Exception e)
    {
      // this shouldn't happen, since we are Cloneable
      throw new InternalError();
    }
  }


  /**
   * Returns a string representation of this <tt>OptimisticHashMap</tt> object
   * in the form of a set of entries, enclosed in braces and separated
   * by the ASCII characters "<tt>,&nbsp;</tt>" (comma and space). Each
   * entry is rendered as the key, an equals sign <tt>=</tt>, and the
   * associated element, where the <tt>toString</tt> method is used to
   * convert the key and element to strings. <p>Overrides to
   * <tt>toString</tt> method of <tt>Object</tt>.
   *
   * @return  a string representation of this hashtable.
   */
  public String toString()
  {
    //
    // this method doesn't need to be syncrhonized, since in the worst
    // case we will just miss some new entries, since entries can't be removed
    // or replaced
    //
    StringBuffer buff = new StringBuffer();

    Enumeration keys = keys();

    buff.append('{');

    boolean isFirst = true;

    while (keys.hasMoreElements())
    {
      if (!isFirst)
        buff.append(", ");
      else
        isFirst = false;

      Object currKey = keys.nextElement();

      buff.append(currKey);
      buff.append('=');
      buff.append(get(currKey));
    }


    buff.append('}');

    return buff.toString();
  }


  // Comparison and hashing

  /**
   * Compares the specified Object with this OptimisticHashMap for equality,
   * as per the definition in the Map interface.
   *
   * @return true if the specified Object is equal to this OptimisticHashMap.
  public synchronized boolean equals(
    Object o
    )
  {
    if (o == this)
      return true;

    if (o == null)
      return false;

    if (!(o instanceof Dictionary))
        return false;

    Dictionary target = (Dictionary)o;

    synchronized(target)
    {
      int targetSize = target.size();

      if (targetSize != size())
        return false;

      Enumeration keys = target.keys();

      while(targetSize > 0)
      {
        Object currKey = (keys.nextElement();

        if (!target.get(currKey).equals(get(currKey)))
          return false;

        targetSize--
      }
    }

    return true;
  }
  */

  /**
   * Returns the hash code value for this Map as per the definition in the
   * Map interface.
   *
   * @see Map#hashCode()
   * @since JDK1.2
  public synchronized int hashCode() {
int h = 0;
Iterator i = entrySet().iterator();
while (i.hasNext())
    h += i.next().hashCode();
return h;
  }
   */

  /**
   * Hashtable collision list.
   *
   */
  private static class Entry implements Cloneable
  {
    int    hash;
    Object key;
    Object value;
    Entry  next;

    protected Entry(
      int    hash,
      Object key,
      Object value,
      Entry  next
      )
    {
      this.hash  = hash;
      this.key   = key;
      this.value = value;
      this.next  = next;
    }

    // clone the entire entry chain
    protected Object clone()
    {
      //
      // Don't clone any chains to empty entrys
      //
      Entry nextEntry = next;

      while ((nextEntry != null) && (nextEntry.value == null))
      {
        nextEntry = nextEntry.next;
      }

      return new Entry(hash, key, value, nextEntry);
    }
  }

  /**
   * A hashtable enumerator class.  This implementation tries to return almost
   * all of the request keys or values, but may not
   */
  private class Enumerator implements Enumeration
  {
    public Enumerator(
      boolean returnKeys
      )
    {
      _returnKeys = returnKeys;

      // get a reference to the current bins to avoid
      _bins = OptimisticHashMap.this._bins;
      _binIndex = _bins.length;
    }

    public boolean hasMoreElements()
    {
      if (_currEntry != null)
        return true;
      else
      {
        while (_binIndex-- > 0)
        {
          _currEntry = _bins[_binIndex];

          // skip the empty entries that haven't been removed yet
          while ((_currEntry != null) && (_currEntry.value == null))
          {
            _currEntry = _currEntry.next;
          }

          if (_currEntry != null)
          {
            return true;
          }
        }
      }

      return false;
    }

    public Object nextElement()
    {
      if (_currEntry == null)
      {
        if (!hasMoreElements())
        {
          throw new NoSuchElementException(this.toString());
        }
      }

      Entry result = _currEntry;

      _currEntry = _currEntry.next;

      // skip the empty entries that haven't been removed yet
      while ((_currEntry != null) && (_currEntry.value == null))
      {
        _currEntry = _currEntry.next;
      }

      if (_returnKeys)
      {
        return result.key;
      }
      else
      {
        return result.value;
      }
    }

    private Entry[] _bins;
    private Entry _currEntry;
    private int _binIndex;

    private boolean _returnKeys;
  }

  // The hash table data.
  private transient Entry _bins[];

  // The total number of entries in the hash table.
  private transient int _entryCount;

  // the actual number of Entry Objects in the hash table
  private transient int _realCount;

  // The table is rehashed when its size exceeds this threshold.  (The
  // value of this field is (int)(capacity * loadFactor).)
  private int _threshold;

  // The load factor for the hashtable.
  private float _loadFactor;
}
