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

import java.io.ObjectStreamException;
import java.io.Serializable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.RandomAccess;

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
   * @param l The List to get a Serializable version of
   * @return A Serializable version of List <code>l</code>
   * @see #getSerializableCollection
   */
  public static <T> List<T> getSerializableList(List<T> l)
  {
    if (l instanceof Serializable)
      return l;
    else
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
  }
  
  
  private static class SerializableCollection<E> implements Collection<E>, Serializable
  {
    SerializableCollection(Collection<E> delegate)
    {
      if (delegate == null)
        throw new NullPointerException();
      
      if (delegate instanceof Serializable)
        throw new IllegalStateException();
      
      _delegate = delegate;
    }
        
    public int size()
    {
      return _delegate.size();
    }

    public boolean isEmpty()
    {
      return _delegate.isEmpty();
    }

    public boolean contains(Object o)
    {
      return _delegate.contains(o);
    }

    public Iterator<E> iterator()
    {
      return _delegate.iterator();
    }

    public Object[] toArray()
    {
      return _delegate.toArray();
    }

    public <T> T[] toArray(T[] a)
    {
      return _delegate.toArray(a);
    }

    public boolean add(E e)
    {
      return _delegate.add(e);
    }

    public boolean remove(Object o)
    {
      return _delegate.remove(0);
    }

    public boolean containsAll(Collection<?> c)
    {
      return _delegate.containsAll(c);
    }

    public boolean addAll(Collection<? extends E> c)
    {
      return _delegate.addAll(c);
    }

    public boolean removeAll(Collection<?> c)
    {
      return _delegate.removeAll(c);
    }

    public boolean retainAll(Collection<?> c)
    {
      return _delegate.retainAll(c);
    }

    public void clear()
    {
      _delegate.clear();
    }
    
    protected Object writeReplace() throws ObjectStreamException
    {
      // copy delegate into a Serializable ArrayList on Serialization
      return new ArrayList(_delegate);
    }

    /**
     * All Collections
     * @param o
     * @return
     */
    public boolean equals(Object o)
    {
      return (o == this) || _delegate.equals(o);
    }

    public int hashCode()
    {
      return _delegate.hashCode();
    }

    public String toString()
    {
      return _delegate.toString();
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
   
}
