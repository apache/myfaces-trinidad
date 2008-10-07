/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidad.util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamException;
import java.io.Serializable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.RandomAccess;
import java.util.Set;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * This class contains Collection utilities.
 */
public final class CollectionUtils
{
  private CollectionUtils()
  {
    // no-op
  }

  /**
   * Returns a Collection based on the passed in Collection <code>c</code>,
   * guaranteed to be Serializable. If <code>c</code> is Serializable,
   * <code>c</code> will be returned, otherwise, <code>c</code> will be
   * wrapped in a Collection that implements Serializable and upon
   * Serialization the contents of <code>c</code> will be copied into
   * the result.
   * <p>
   * The results is very similar to creating a new ArrayList with the
   * contents of <code>c</code>, but no wrapper is created unless necessary
   * and the actual creation of the Serializable copy is deferred until
   * Serialization occurs.
   * @param c The Collection to get a Serializable version of
   * @return A Serializable version of Collection <code>c</code>
   * @see #getSerializableList
   */
  public static <T> Collection<T> getSerializableCollection(Collection<T> c)
  {
    if (c instanceof Serializable)
      return c;
    else
      return new SerializableCollection<T>(c);
  }

  /**
   * Returns a List based on the passed in List <code>l</code>,
   * guaranteed to be Serializable. List <code>l</code> will be
   * wrapped in a List that implements Serializable and upon
   * Serialization the contents of <code>l</code> will be copied into
   * the result.
   * <p>
   * If <code>l</code> implements RandomAccess, any returned List will also
   * implement RandomAccess.
   * <p>
   * The results is very similar to creating a new ArrayList with the
   * contents of <code>l</code>, but no wrapper is created unless necessary
   * and the actual creation of the Serializable copy is deferred until
   * Serialization occurs.
   * <p>
   * Code that calls List.subList() and needs the result to be Serializable should always
   * use <code>newSerializableList</code> rather than <code>getSerializableList</code> because
   * the <code>java.util.Collections</code> implementations of <code>checkedList</code>,
   * <code>unmodifiableList</code>, and <code>synchronizedList</code> all lie and always implement
   * Serializable, regardless of the serializability of their backing List.
   * @param l The List to get a Serializable version of
   * @return A Serializable version of List <code>l</code>
   * @see #getSerializableList
   * @see #getSerializableCollection
   */
  public static <T> List<T> newSerializableList(List<T> l)
  {
    if (l instanceof RandomAccess)
    {
      return new SerializableRandomAccessList<T>(l);
    }
    else
    {
      return new SerializableList<T>(l);
    }
  }

  /**
   * Returns a List based on the passed in List <code>l</code>,
   * guaranteed to be Serializable. If <code>l</code> is Serializable,
   * <code>l</code> will be returned, otherwise, <code>l</code> will be
   * wrapped in a List that implements Serializable and upon
   * Serialization the contents of <code>l</code> will be copied into
   * the result.
   * <p>
   * If <code>l</code> implements RandomAccess, any returned List will also
   * implement RandomAccess.
   * <p>
   * The results is very similar to creating a new ArrayList with the
   * contents of <code>l</code>, but no wrapper is created unless necessary
   * and the actual creation of the Serializable copy is deferred until
   * Serialization occurs.
   * <p>
   * Code that calls List.subList() and needs the result to be Serializable should always
   * use <code>newSerializableList</code> rather than <code>getSerializableList</code> because
   * the <code>java.util.Collections</code> implementations of <code>checkedList</code>,
   * <code>unmodifiableList</code>, and <code>synchronizedList</code> all lie and always implement
   * Serializable, regardless of the serializability of their backing List.
   * @param l The List to get a Serializable version of
   * @return A Serializable version of List <code>l</code>
   * @see #newSerializableList
   * @see #getSerializableCollection
   */
  public static <T> List<T> getSerializableList(List<T> l)
  {
    // because we can't trust the implementations of the checked, unmodifiable, and synchronized
    // versions, always create a Serializable wrapper if we see one of these classes
    if ((l instanceof Serializable) &&
         !_CHECKED_LIST.isInstance(l) &&
         !_UNMODIFIABLE_LIST.isInstance(l) &&
         !_SYNCHRONIZED_LIST.isInstance(l))
      return l;
    else
    {
      return newSerializableList(l);
    }
  }
  
  /**
   * Creates a Map that dynamically verifies that all keys and values added to it will
   * succeed Serialization.  The validations checks not only that the keys and values added
   * to the Map implement Serializable, but that these instances will actually succeed
   * Serialization.
   * <p>
   * This checking can be defeated by either modifying the backing map directly or by modifying
   * an object added to the checked Map after adding it.
   * </p>
   * @param map Map to wrap for Serialization validation
   * @return Map where all modifications are checked to ensure that they will succeeed if
   * Serialized
   */
  public static <K,V> Map<K, V> getCheckedSerializationMap(Map<K, V> map)
  {
    if (map instanceof CheckedSerializationMap)
      return map;
    else
      return new CheckedSerializationMap<K,V>(map);
  }

  protected abstract static class DelegatingCollection<E> implements Collection<E>
  {
    protected abstract Collection<E> getDelegate();

    public int size()
    {
      return getDelegate().size();
    }

    public boolean isEmpty()
    {
      return getDelegate().isEmpty();
    }

    public boolean contains(Object o)
    {
      return getDelegate().contains(o);
    }

    public Iterator<E> iterator()
    {
      return getDelegate().iterator();
    }

    public Object[] toArray()
    {
      return getDelegate().toArray();
    }

    public <T> T[] toArray(T[] a)
    {
      return getDelegate().toArray(a);
    }

    public boolean add(E e)
    {
      return getDelegate().add(e);
    }

    public boolean remove(Object o)
    {
      return getDelegate().remove(0);
    }

    public boolean containsAll(Collection<?> c)
    {
      return getDelegate().containsAll(c);
    }

    public boolean addAll(Collection<? extends E> c)
    {
      return getDelegate().addAll(c);
    }

    public boolean removeAll(Collection<?> c)
    {
      return getDelegate().removeAll(c);
    }

    public boolean retainAll(Collection<?> c)
    {
      return getDelegate().retainAll(c);
    }

    public void clear()
    {
      getDelegate().clear();
    }
    
    /**
     * All Collections
     * @param o
     * @return
     */
    public boolean equals(Object o)
    {
      return (o == this) || getDelegate().equals(o);
    }

    public int hashCode()
    {
      return getDelegate().hashCode();
    }

    public String toString()
    {
      return getDelegate().toString();
    }
  }
    
  private static class SerializableCollection<E> extends DelegatingCollection<E>
                                                 implements Serializable
  {
    SerializableCollection(Collection<E> delegate)
    {
      // we don't check that the delegate is Serializable because of the Collections
      // classes that lie about Serializability
      if (delegate == null)
        throw new NullPointerException();
           
      _delegate = delegate;
    }

    protected Collection<E> getDelegate()
    {
      return _delegate;
    }
    
    protected Object writeReplace() throws ObjectStreamException
    {
      // copy delegate into a Serializable ArrayList on Serialization
      return new ArrayList(_delegate);
    }

    private static final long serialVersionUID = 0L;

    private final transient Collection<E> _delegate;
  }


  private static class SerializableList<E> extends SerializableCollection<E> implements List<E>
  {
    SerializableList(List<E> delegate)
    {
      super(delegate);
      _delegate = delegate;
    }
    
    public void add(int index, E element)
    {
      _delegate.add(index, element);
    }

    public E remove(int index)
    {
      return _delegate.remove(index);
    }

    public boolean addAll(int index, Collection<? extends E> c)
    {
      return _delegate.addAll(index, c);
    }

    public E get(int index)
    {
      return _delegate.get(index);
    }

    public E set(int index, E element)
    {
      return _delegate.set(index, element);
    }

    public int indexOf(Object o)
    {
      return _delegate.indexOf(o);
    }

    public int lastIndexOf(Object o)
    {
      return _delegate.lastIndexOf(o);
    }

    public ListIterator<E> listIterator()
    {
      return _delegate.listIterator();
    }

    public ListIterator<E> listIterator(int index)
    {
      return _delegate.listIterator(index);
    }

    public List<E> subList(int fromIndex, int toIndex)
    {
      return CollectionUtils.getSerializableList(_delegate.subList(fromIndex, toIndex));
    }
 
    private static final long serialVersionUID = 0L;
    
    private final transient List<E> _delegate;
  }

  private static class SerializableRandomAccessList<E> extends SerializableList<E> implements RandomAccess
  {
    SerializableRandomAccessList(List<E> delegate)
    {
      super(delegate);
    }

    private static final long serialVersionUID = 0L;
  }

  protected static abstract class DelegatingMap<K,V> implements Map<K,V>
  {
    protected abstract Map<K,V> getDelegate();

    public int size()
    {
      return getDelegate().size();
    }

    public boolean isEmpty()
    {
      return getDelegate().isEmpty();
    }

    public boolean containsKey(Object key)
    {
      return getDelegate().containsKey(key);
    }

    public boolean containsValue(Object value)
    {
      return getDelegate().containsValue(value);
    }

    public V get(Object key)
    {
      return getDelegate().get(key);
    }

    // Modification Operations

    public V put(K key, V value)
    {
      return getDelegate().put(key, value);
    }

    public V remove(Object key)
    {
      return getDelegate().remove(key);
    }

    // Bulk Operations

    public void putAll(Map<? extends K, ? extends V> m)
    {
      getDelegate().putAll(m);
    }

    public void clear()
    {
      getDelegate().clear();
    }

    // Views
    
    public Set<K> keySet()
    {
      return getDelegate().keySet();
    }

    public Collection<V> values()
    {
      return getDelegate().values();
    }

    public Set<Map.Entry<K, V>> entrySet()
    {
      return getDelegate().entrySet();      
    }

    // Comparison and hashing

    public boolean equals(Object o)
    {
      return getDelegate().equals(o);
    }

    public int hashCode()
    {
      return getDelegate().hashCode();
    }
  }
  
  protected static abstract class DelegatingEntry<K,V> implements Map.Entry<K,V>
  {
    protected abstract Map.Entry<K,V> getDelegate();
                                
    public K getKey()
    {
      return getDelegate().getKey();
    }

    public V getValue()
    {
      return getDelegate().getValue();
    }

    public V setValue(V value)
    {
      return getDelegate().setValue(value);
    }

    public boolean equals(Object o)
    {
      return getDelegate().equals(o);
    }

    public int hashCode()
    {
      return getDelegate().hashCode();
    }
  }

  // Map that validates that the keys and values added to the map are Serializable
  private final static class CheckedSerializationMap<K, V> extends DelegatingMap<K,V>
                                                           implements Serializable
  {
    public CheckedSerializationMap(Map<K, V> delegate)
    {
      if (delegate == null)
        throw new NullPointerException();
      
      if (delegate instanceof Serializable)
        throw new IllegalArgumentException("Unserializable delegate");
      
      _delegate = delegate;
    }

    protected Map<K, V> getDelegate()
    {
      return _delegate;
    }

    public V put(K key, V value)
    {
      _checkSerialization(key, value);
      
      return super.put(key, value);
    }

    public void putAll(Map<? extends K, ? extends V> m)
    {
      
      Object[] keys = m.keySet().toArray();
      Object[] values = m.values().toArray();
      
      int keyCount = keys.length;
      
      // in case an entry was added or removed between to tow toArray calls above
      if (keyCount != values.length)
        throw new ConcurrentModificationException();
      
      // atomically check for serializability before adding
      for (int k = 0; k < keyCount; k++)
      {
        _checkSerialization(keys[k], values[k]);        
      }

      // add the contents we checked rather that calling super.putAll(m), in case
      // the map changed after we checked its contents above
      Map<K, V> delegate = getDelegate();
      
      for (int k = 0; k < keyCount; k++)
      {
        delegate.put((K)keys[k], (V)values[k]);
      }
    }

    public Set<Map.Entry<K, V>> entrySet()
    {
      return new CheckedSerializationEntrySet(getDelegate().entrySet());      
    }
    
    private void _checkSerialization(Object key, Object value)
    {
      if (!(key instanceof Serializable))
        throw new ClassCastException(_LOG.getMessage("UNSERIALIZABLE_PROPERTY_KEY",
                                                     new Object[]{key, this}));

      if (!(value instanceof Serializable))
        throw new ClassCastException(_LOG.getMessage("UNSERIALIZABLE_PROPERTY_VALUE",
                                                     new Object[]{value, key, this}));

 
      // don't bother checking common case of String
      if (!(key instanceof String))
      {
        // verify that the contents of the key are in fact Serializable
        try
        {
          new ObjectOutputStream(new ByteArrayOutputStream()).writeObject(key);
        }
        catch (IOException e)
        {          
          throw new IllegalArgumentException(_LOG.getMessage("FAILED_SERIALIZATION_PROPERTY_KEY",
                                                     new Object[]{key, this}),
                                                     e);
        }
      }
      
      // verify that the contents of the value are in fact Serializable
      try
      {
        new ObjectOutputStream(new ByteArrayOutputStream()).writeObject(value);
      }
      catch (IOException e)
      {          
        throw new IllegalArgumentException(_LOG.getMessage("FAILED_SERIALIZATION_PROPERTY_VALUE",
                                                   new Object[]{value, key, this}),
                                                   e);
      }
    }

    // Entry Set returns CheckedSerializationEntry Objects
    private class CheckedSerializationEntrySet extends DelegatingCollection<Entry<K,V>>
                                                            implements Set<Entry<K, V>>
    {
      private CheckedSerializationEntrySet(Set<Entry<K, V>> delegate)
      {
        if (delegate == null)
          throw new NullPointerException();
        
        _delegate = delegate;
      }

      protected Set<Entry<K, V>> getDelegate()
      {
        return _delegate;
      }

      public Iterator<Entry<K,V>> iterator()
      {
        return new CheckedSerializationEntrySetIterator(super.iterator());
      }
      
      public Object[] toArray()
      {
        Object[] delegateEntries = super.toArray();

        int entryCount = delegateEntries.length;
        
        // make sure that the array allows generic Entry objects.  If so, use the source array
        // as the destination array, otherwise create a new destination array for our entries
        Object[] entries = 
                       (delegateEntries.getClass().getComponentType().isAssignableFrom(Entry.class))
                         ? delegateEntries
                         : new Entry[entryCount];
                        
        for (int i = 0; i < entryCount; i++)
          entries[i] = new CheckedSerializationEntry((Entry<K,V>)delegateEntries[i]);
        
        return entries;
      }

      public <T> T[] toArray(T[] a)
      {
        int inputSize = a.length;
        
        // compute the output type array so that the delegate creates an array of the correct
        // type.  We always truncate the input array's size to 0 so that the delegate doesn't
        // attempt to copy any of its contents into the output array
        T[] outTypeArray = (inputSize == 0)
                             ? a
                             : Arrays.copyOf(a, 0);
        
        Object[] delegateEntries = super.toArray(outTypeArray);
        
        // now proxy the contents
        int entryCount = delegateEntries.length;
        
        for (int i = 0; i < entryCount; i++)
          delegateEntries[i] = new CheckedSerializationEntry((Entry<K,V>)delegateEntries[i]);
        
        // now figure out whether we have to copy the entries into the passed in array or not
        if (entryCount > inputSize)
          return (T[])delegateEntries;
        
        // they fit so we need to copy the values into the input array
        System.arraycopy(delegateEntries, 0, a, 0, entryCount);
       
        // see if we have room for the wacky null terminator
        if (inputSize > entryCount)
          a[entryCount] = null;
        
        return a;
      }

      // Iterator for CheckedSerializationEntrySet that returns CheckedSerializationEntry
      private class CheckedSerializationEntrySetIterator implements Iterator<Entry<K,V>>
      {
        private CheckedSerializationEntrySetIterator(Iterator<Entry<K, V>> delegate)
        {
          _delegate = delegate;
        }

        public boolean hasNext()
        {
          return _delegate.hasNext();
        }

        public Map.Entry<K,V> next()
        {
          return new CheckedSerializationEntry<K,V>(_delegate.next());
        }

        public void remove()
        {
          _delegate.remove();
        }

        private final Iterator<Entry<K, V>> _delegate;
      }

      // Entry implementation that checks calls to setValue
      private class CheckedSerializationEntry<K, V> extends DelegatingEntry<K, V>
      {
        private CheckedSerializationEntry(Entry<K, V> delegate)
        {
          if (delegate == null)
            throw new NullPointerException();
          
          _delegate = delegate;
        }
        
        protected Entry<K, V> getDelegate()
        {
          return _delegate;
        }
        
        public V setValue(V value)
        {
          _checkSerialization(getKey(), value);
          return super.setValue(value);
        }
      
        private final Entry<K, V> _delegate;
      }

      private final Set<Entry<K, V>> _delegate;
    }

    private final Map<K, V> _delegate;
  }

  //
  // Build up references to implementation classes used by Collections to implement the following
  // features.  This way we can detect when these classes are used and work around problems.
  //
  private static final Class<? extends List> _CHECKED_LIST;
  private static final Class<? extends List> _UNMODIFIABLE_LIST;
  private static final Class<? extends List> _SYNCHRONIZED_LIST;
  
  static
  {
    // use a LinkedList as it doesn't implement RandomAccess, so that we don't accidentally get
    // the RandomAccess subclasses
    LinkedList<Object> dummyList = new LinkedList<Object>();
    
    _CHECKED_LIST      = Collections.checkedList(dummyList, Object.class).getClass();
    _UNMODIFIABLE_LIST = Collections.unmodifiableList(dummyList).getClass();
    _SYNCHRONIZED_LIST = Collections.synchronizedList(dummyList).getClass();
  }
  
  private static final TrinidadLogger _LOG = 
                                        TrinidadLogger.createTrinidadLogger(CollectionUtils.class);

}
