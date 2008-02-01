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
package org.apache.myfaces.trinidad.bean.util;

import java.io.Serializable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.util.concurrent.ConcurrentMap;

import javax.faces.component.StateHolder;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.bean.PropertyMap;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Utilities for handling state persistance.
 */
public class StateUtils
{
  /**
   * Persists a property key.
   */
  static public Object saveKey(PropertyKey key)
  {
    int index = key.getIndex();
    if (index < 0)
      return key.getName();

    if (index < 128)
      return Byte.valueOf((byte) index);

    return Integer.valueOf(index);
  }


  /**
   * Restores a persisted PropertyKey.
   */
  static public PropertyKey restoreKey(FacesBean.Type type, Object value)
  {
    PropertyKey key;
    if (value instanceof Number)
    {
      key = type.findKey(((Number) value).intValue());
      if (key == null)
        throw new IllegalStateException(_LOG.getMessage(
          "INVALID_INDEX"));
    }
    else
    {
      key = type.findKey((String) value);
      if (key == null)
        key = PropertyKey.createPropertyKey((String) value);
    }

    return key;
  }

  /**
   * Generic (unoptimized) version of PropertyMap state saving.
   */
  static public Object saveState(
    PropertyMap    map,
    FacesContext   context,
    boolean        useStateHolder)
  {
    int size = map.size();
    if (size == 0)
      return null;

    Object[] values = new Object[2 * size];
    int i = 0;
    for(Map.Entry<PropertyKey, Object> entry : map.entrySet())
    {
      PropertyKey key = entry.getKey();
      if (key.isTransient())
        continue;

      Object value = entry.getValue();

      values[i] = saveKey(key);
      if (_LOG.isFinest())
      {
        _LOG.finest("SAVE {" + key + "=" + value + "}");
      }

      if (useStateHolder)
        values[i + 1] = saveStateHolder(context, value);
      else
        values[i + 1] = key.saveValue(context, value);
      i+=2;
    }

    return values;
  }


  /**
   * Generic (unoptimized) version of PropertyMap state restoring.
   */
  static public void restoreState(
    PropertyMap    map,
    FacesContext   context,
    FacesBean.Type type,
    Object         state,
    boolean        useStateHolder)
  {
    if (state == null)
      return;

    Object[] values = (Object[]) state;
    int size = values.length / 2;
    for (int i = 0; i < size; i++)
    {
      Object savedKey = values[i * 2];
      if (savedKey == null)
        continue;

      Object savedValue = values[i * 2 + 1];
      PropertyKey key = restoreKey(type, savedKey);
      Object value;

      if (useStateHolder)
        value = restoreStateHolder(context, savedValue);
      else
        value = key.restoreValue(context, savedValue);

      if (_LOG.isFinest())
      {
        _LOG.finest("RESTORE {" + key + "=" + value + "}");
      }

      map.put(key, value);
    }
  }

  /**
   * Saves an object that may implement StateHolder.
   */
  static public Object saveStateHolder(
    FacesContext context,
    Object       value)
  {
    if (value == null)
      return null;

    Saver saver = null;
    if (value instanceof StateHolder)
    {
      if (((StateHolder) value).isTransient())
        return null;

      saver = new SHSaver();
    }
    else if (value instanceof Serializable)
    {
      return value;
    }
    else
    {
      saver = new Saver();
    }

    if (saver != null)
      saver.saveState(context, value);

    return saver;
  }

  /**
   * Restores an object that was saved using saveStateHolder()
   */
  static public Object restoreStateHolder(
    FacesContext context,
    Object       savedValue)
  {
    if (!(savedValue instanceof Saver))
      return savedValue;

    return ((Saver) savedValue).restoreState(context);
  }



  /**
   * Saves a List whose elements may implement StateHolder.
   */
  @SuppressWarnings("unchecked")
  static public Object saveList(
    FacesContext context,
    Object       value)
  {
    if (value == null)
      return null;

    List<Object> list = (List<Object>) value;
    int size = list.size();
    if (size == 0)
      return null;

    Object[] array = new Object[size];
    // 2006-08-01: -= Simon Lessard =-
    //             Inefficient loop if the list implementation
    //             ever change to a linked data structure. Use
    //             iterators instead
    //for (int i = 0; i < size; i++)
    //  array[i] = saveStateHolder(context, list.get(i));
    int index = 0;
    for(Object object : list)
    {
      array[index++] = saveStateHolder(context, object);
    }

    return array;
  }

  /**
   * Restores a List whose elements may implement StateHolder.
   */
  static public Object restoreList(
    FacesContext context,
    Object       savedValue)
  {
    if (savedValue == null)
      return null;

    Object[] array = (Object[]) savedValue;
    int length = array.length;
    if (length == 0)
      return null;

    List<Object> list = new ArrayList<Object>(length);
    for(Object state : array)
    {
      Object restored = restoreStateHolder(context, state);
      if (restored != null)
      {
        list.add(restored);
      }
    }

    return list;
  }



  /**
   * Instance used to save generic instances;  simply saves
   * the class name.
   */
  static private class Saver implements Serializable
  {
    public void saveState(FacesContext context, Object saved)
    {
      _name = saved.getClass().getName();
    }

    public Object restoreState(FacesContext context)
    {
      // we don't need to use concurrent map methods like putIfAbsent. If someone happens to
      // add a name/value pair again it's fine because as the doc for put in HashMap says
      // "If the map previously contained a mapping for this key, the old value is replaced."
      ConcurrentMap<String, Object> appMap = 
                           RequestContext.getCurrentInstance().getApplicationScopedConcurrentMap();
      

      Map<String, Class> classMap = (Map<String, Class>) appMap.get(_CLASS_MAP_KEY);
      
      if (classMap == null)
      {    
        // the classMap doesn't need to worry about synchronization, 
        // if the Class is loaded twice that's fine. 
        Map<String, Class> newClassMap = new HashMap<String, Class>();
        Map<String, Class> oldClassMap = 
                              (Map<String, Class>) appMap.putIfAbsent(_CLASS_MAP_KEY, newClassMap);
        
        if (oldClassMap != null)
          classMap = oldClassMap;
        else
          classMap = newClassMap;
      }
            
      Class clazz = classMap.get(_name);

      if (clazz == null)
      {
        try
        {
          ClassLoader cl = _getClassLoader();
          clazz = cl.loadClass(_name);
          classMap.put(_name, clazz);
        }
        catch (Throwable t)
        {
          _LOG.severe(t);
          return null;
        }
      }

      try
      {
        return clazz.newInstance();
      }
      catch (Throwable t)
      {
        _LOG.severe(t);
        return null;
      }
    }

    private String _name;
    private static final long serialVersionUID = 1L;
  }


  /**
   * Instance used to save StateHolder objects.
   */
  static private class SHSaver extends Saver
  {
    @Override
    public void saveState(FacesContext context, Object value)
    {
      super.saveState(context, value);
      _save = ((StateHolder) value).saveState(context);
    }

    @Override
    public Object restoreState(FacesContext context)
    {
      Object o = super.restoreState(context);
      if (o != null)
        ((StateHolder) o).restoreState(context, _save);

      return o;
    }

    private Object _save;
    private static final long serialVersionUID = 1L;
  }


  //
  // Pick a ClassLoader
  //
  static private ClassLoader _getClassLoader()
  {
    ClassLoader cl = Thread.currentThread().getContextClassLoader();
    if (cl == null)
      cl = StateUtils.class.getClassLoader();

    return cl;
  }

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(StateUtils.class);

  private static final String _CLASS_MAP_KEY = 
                                           "org.apache.myfaces.trinidad.bean.util.CLASS_MAP_KEY";


}

