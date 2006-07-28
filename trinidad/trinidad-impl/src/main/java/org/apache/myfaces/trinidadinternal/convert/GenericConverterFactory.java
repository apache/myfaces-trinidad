/*
* Copyright 2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.convert;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * A factory of GenericConverters.
 * GenericConverters may be registered with this factory.
 * The factory supports converting between the types supported
 * by each individual converter. The factory also supports converting
 * between types supported by combining individual converters.
 * 
 * @author The Oracle ADF Faces Team
 */
public class GenericConverterFactory
{
  private GenericConverterFactory()
  {
    _converters = new ArrayList(3);
    registerConverter(new SqlConverter());
    registerConverter(new BaseConverter());
  }
  
  /**
   * Gets a converter that is capable of converting from the given
   * sourceType to the given targetType.
   * This method first searches to see if any of the registered converters
   * are capable of making this conversion alone. If one is found, it is
   * returned. Otherwise, this method will search to see if some combination
   * of registered converters can be used to perform this conversion.
   * If so, a composite converter will be returned.
   * <P>
   * The returned converter (or lack thereof) is cached so that subsequent
   * requests for the same source and target types will be fast.
   * @return null if there is no such converter.
   */
  public GenericConverter getConverter(Class sourceType, Class targetType)
  {
    Key key = new Key(sourceType, targetType);
    // check the cache first:
    Object cached = _cache.get(key);
    if (cached != null)
    {
      return (cached == _NULL) ? null : (GenericConverter) cached;
    }

    // we are going to start searching to see if some chain of converters
    // can be used to perform this conversion.
    // initial node in chain:
    Node start = new Node(null, null, sourceType);
    LinkedList sourcesToBeSearched = new LinkedList();
    sourcesToBeSearched.add(start);
    // cache to store all the classes we've tested already. This is to
    // avoid our chains from looping indefinitely:
    Set cache  = new HashSet(16);
    GenericConverter converter = _findConverter(sourcesToBeSearched, targetType, cache);
    if (converter == null)
    {
      // cache the fact that no such converter exists:
      _cache.put(key, _NULL);
    }
    else
    {
      _cache.put(key, converter);
    }

    return converter;
  }
  
  /**
   * tries to find a converter, or create a chain of converters that can
   * convert from one of the given sourceTypes to the specified targetType.
   * @param sourcesToBeSearched each element is a Node. Each Node is a pairing of
   * sourceType and the chain of converters needed to produce this sourceType.
   * @param targetType the type that is needed
   * @param cache used to record which classes we've searched already.
   * @return null if no converter was found.
   */
  private GenericConverter _findConverter(
    LinkedList sourcesToBeSearched,
    Class targetType, 
    Set cache)
  {
    while(!sourcesToBeSearched.isEmpty())
    {
      Node source = (Node) sourcesToBeSearched.removeFirst();
      // loop through all the converters and see what types they can turn 
      // the current sourceType into 
      // (the current sourceType is source.targetType):
      for(int j=0,jsz=_converters.size(); j<jsz; j++)
      {
        GenericConverter conv = (GenericConverter) _converters.get(j);
        // loop though all the targetTypes on this converter to see
        // if we can find the one we're looking for:
        if (_searchTargetTypes(sourcesToBeSearched, source, conv, targetType,
                               cache))
        {
          // see if there is no chain:
          if (source.previous == null)
            return conv;
            
          // there is a chain:
          return new CompositeConverter(source, conv, targetType);
        }
      }
    }
    return null;
  }
  
  /**
   * Searches the targetTypes of the given converter to see if we
   * can find the type we are searching for.
   * @param sourcesToBeSearched each element is a Node. Each Node is a pairing of
   * sourceType and the chain of converters needed to produce this sourceType.
   * @param currentSource a chain of converters has been used to produce the
   * type identified by this Node. The targetType of this Node will be used
   * to search the currentConverter.
   * @param searchType the targetType we are searching for.
   * @param cache used to record which classes we've searched already.
   * @return true if the currentConverter can convert from 
   * currentSource.targetType into searchType.
   */
  private boolean _searchTargetTypes(
    List sourcesToBeSearched,
    Node currentSource,
    GenericConverter currentConverter,
    Class searchType,
    Set cache)
  {
    Class sourceType = currentSource.targetType;
    List targetTypes = currentConverter.getTargetTypes(sourceType);
    for(int i=0,sz=targetTypes.size(); i<sz; i++)
    {
      Class targetType = (Class) targetTypes.get(i);
      // check to see if we've seen this targetType before:
      if (cache.add(targetType))
      {
        // check to see if the targetType is a subclass of the searchType:
        if (searchType.isAssignableFrom(targetType))
          return true;
          
        // create a new node in the chain by adding this targetType and converter
        Node newSource = new Node(currentSource, currentConverter, targetType);
        
        // add the new node so that we can continue searching by seeing if
        // we can convert the targetType into the searchType using some other
        // converter:
        sourcesToBeSearched.add(newSource);
      }
    }
    return false;
  }
  
  /**
   * Registers a converter. Registering a new converter causes the internal
   * cache to be cleared.
   */
  public void registerConverter(GenericConverter converter)
  {
    _converters.add(converter);
    _cache.clear();
  }

  /**
   * converts the given source instance into an object of the targetType.
   * @param source the object to convert
   * @param targetType the required type.
   * @return null, if the source is null.
   */
  public Object convert(Object source, Class targetType)
  {
    if (source == null)
      return null;
      
    if (targetType.isAssignableFrom(source.getClass()))
      return source;
  
    GenericConverter converter = getConverter(source.getClass(), targetType);
    if (converter != null)
    {
      return converter.convert(source, targetType);
    }
    throw new ConvertException(source, targetType);
  }
  
  /**
   * Checks to see if it is possible to convert the given instance 
   * into the specified targetType
   * @return true if conversion is possible.
   */
  public boolean isConvertible(Object source, Class targetType)
  {
    if (source == null)
      return false; // bug 4589048
  
    if (targetType.isAssignableFrom(source.getClass()))
      return true;
    
    GenericConverter converter = getConverter(source.getClass(), targetType);
    return converter != null;
  }
  
  private final Map _cache = new HashMap(16);
  private final List _converters;
  private static final Object _NULL = Node.class; // reuse an object

  private static final class Node
  {
    public Node(Node previous, GenericConverter converter, Class targetType)
    {
      this.previous = previous;
      this.converter = converter;
      this.targetType = targetType;
    }
    
    public Object convert(Object source)
    {
      if (previous != null)
      {
        source = previous.convert(source);
        source = converter.convert(source, targetType);
      }
      return source;
    }
    
    public Class getSourceType()
    {
      if (previous == null)
        return targetType;
      return previous.getSourceType();
    }
    
    public final Node previous;
    public final GenericConverter converter;
    public final Class targetType;
  }
  
  private static final class Key
  {
    public Key(Class source, Class target)
    {
      assert !source.equals(target);

      _source = source;
      _target = target;
      
      _hc = source.hashCode() + target.hashCode();
    }
  
    public int hashCode()
    {
      return _hc;
    }
    
    public boolean equals(Object other)
    {
      if (this == other)
        return true;
      if (other instanceof Key)
      {
        Key that = (Key) other;
        return (_source.equals(that._source) && _target.equals(that._target));
      }
      return false;
    }
    
    private final int _hc;  
    private final Class _source, _target;
  }
  
  private static final class CompositeConverter extends GenericConverter
  {
    public CompositeConverter(Node source, GenericConverter conv, Class targetType)
    {
      assert source != null;
      _chain = new Node(source, conv, targetType) ;
    }

    public Object convert(Object source, Class targetType)
    {
      if (targetType.isAssignableFrom(_chain.targetType))
      {
        return _chain.convert(source);
      }
      else
        throw new IllegalArgumentException("Cannot convert to:"+targetType);
    }

    public List getTargetTypes(Class sourceType)
    {
      if (_chain.getSourceType().isAssignableFrom(sourceType))
      {
        return Collections.singletonList(_chain.targetType);
      }
      return Collections.EMPTY_LIST;
    }
    
    private final Node _chain;
  }
  
  public static GenericConverterFactory getCurrentInstance()
  {
    return _INSTANCE;
  }
  
  private static final GenericConverterFactory _INSTANCE = 
    new GenericConverterFactory();
}
