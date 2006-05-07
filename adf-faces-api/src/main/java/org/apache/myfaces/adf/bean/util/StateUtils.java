/*
 * Copyright  2004-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adf.bean.util;

import java.io.Serializable;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.faces.component.StateHolder;
import javax.faces.context.FacesContext;

import org.apache.myfaces.adf.logging.ADFLogger;

import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.bean.PropertyKey;
import org.apache.myfaces.adf.bean.PropertyMap;


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
      return new Byte((byte) index);

    return new Integer(index);
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
        throw new IllegalStateException("Invalid index");
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
    Iterator entries = map.entrySet().iterator();
    int i = 0;
    while (entries.hasNext())
    {
      Map.Entry entry = (Map.Entry) entries.next();
      PropertyKey key = (PropertyKey) entry.getKey();
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
  static public Object saveList(
    FacesContext context,
    Object       value)
  {
    if (value == null)
      return null;

    List list = (List) value;
    int size = list.size();
    if (size == 0)
      return null;

    Object[] array = new Object[size];
    for (int i = 0; i < size; i++)
      array[i] = saveStateHolder(context, list.get(i));
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

    List list = new ArrayList(length);
    for (int i = 0; i < length; i++)
    {
      Object restored = restoreStateHolder(context, array[i]);
      if (restored != null)
        list.add(restored);
    }

    return list;
  }



  /**
   * Instance used to save generic instances;  simply saves
   * the class name.
   */
  static private class Saver implements java.io.Serializable
  {
    public void saveState(FacesContext context, Object saved)
    {
      _name = saved.getClass().getName();
    }

    public Object restoreState(FacesContext context)
    {
      ClassLoader cl = _getClassLoader();
      try
      {
        Class clazz = cl.loadClass(_name);
        return clazz.newInstance();
      }
      catch (Throwable t)
      {
        _LOG.severe(t);
        return null;
      }
    }

    private String _name;
  }


  /**
   * Instance used to save StateHolder objects.
   */
  static private class SHSaver extends Saver
  {
    public void saveState(FacesContext context, Object value)
    {
      super.saveState(context, value);
      _save = ((StateHolder) value).saveState(context);
    }

    public Object restoreState(FacesContext context)
    {
      Object o = super.restoreState(context);
      if (o != null)
        ((StateHolder) o).restoreState(context, _save);

      return o;
    }

    private Object _save;
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

  static private final ADFLogger _LOG = ADFLogger.createADFLogger(StateUtils.class);
}

