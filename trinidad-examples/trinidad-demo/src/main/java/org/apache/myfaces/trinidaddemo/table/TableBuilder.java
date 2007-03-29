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
package org.apache.myfaces.trinidaddemo.table;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

public class TableBuilder implements java.io.Serializable
{
  @SuppressWarnings("unchecked")
  public TableBuilder()
  {
    // no arg contructor needed for usage as managed-bean
    _beanProps = Collections.EMPTY_LIST;
    _data = Collections.EMPTY_LIST;
    _result = null;
  }
  
  public void setBeanClass(String klass)
  {
    _beanClass = klass;
  }
  
  public void setBeanProperties(List<Object> props)
  {
    if (props == null)
      throw new NullPointerException("beanProperties");
    _beanProps = props;
  }
  
  public void setBeanData(List<?> data)
  {
    if (data == null)
      throw new NullPointerException("beanData");
    _data = data;
  }
  
  public List<?> getTableData()
    throws ClassNotFoundException, IntrospectionException, InstantiationException,
      IllegalAccessException, InvocationTargetException
  {
    if (_result == null)
    {
      _result = _getAsList();
    }
    return _result;
  }
  
  private List<Object> _getAsList() 
    throws ClassNotFoundException, IntrospectionException, InstantiationException,
      IllegalAccessException, InvocationTargetException
  {
    if (_beanClass == null)
      throw new NullPointerException("beanClass");
    
    Class<?> beanClass = Class.forName(_beanClass);
    _setPropertySetters(beanClass, _beanProps);
    int sz = _beanProps.size();
    List<Object> result = new ArrayList<Object>(sz);

    Iterator<?> cells = _data.iterator();
    while(cells.hasNext())
    {
      Object beanInstance = beanClass.newInstance();
      for(int i=0; i<sz; i++)
      {
        Object value = cells.next();
        Method setter = (Method) _beanProps.get(i);
        Class<?> expectedType = setter.getParameterTypes()[0];
        if (!expectedType.isAssignableFrom(value.getClass()))
        {
          value = _convert(value, expectedType);
        }
        
        setter.invoke(beanInstance, new Object[] {value});
      }
      result.add(beanInstance);
    }
    return result;
  }
  
  private Object _convert(Object instance, Class<?> expectedType)
  {
    if (Integer.TYPE == expectedType)
    {
      return new Integer(instance.toString());
    }
    throw new IllegalArgumentException("Could not convert instance:"+instance+
      " of class:"+instance.getClass()+" into "+expectedType);
  }
  
  private void _setPropertySetters(Class<?> klass, List<Object> props)
    throws IntrospectionException
  {
    BeanInfo beanInfo = Introspector.getBeanInfo(klass);
    PropertyDescriptor[] descs = beanInfo.getPropertyDescriptors();
    for(int i=0, sz=props.size(); i<sz; i++)
    {
      String name = (String)props.get(i);
      PropertyDescriptor desc = _getDescriptor(descs, name);
      if (desc == null)
      {
        throw new IllegalArgumentException("property:"+name+" not found on:"
          +klass);
      }
      Method setter = desc.getWriteMethod();
      if (setter == null)
      {
        throw new IllegalArgumentException("No way to set property:"+name+" on:"
          +klass);
      }
      props.set(i, setter);
    }
  }
  
  private PropertyDescriptor _getDescriptor(
    PropertyDescriptor[] descs,
    String name)
  {
    for(int i=0; i<descs.length; i++)
    {
      PropertyDescriptor desc = descs[i];
      if (name.equals(desc.getName()))
        return desc;
    }
    return null;      
  }
  
  private String _beanClass = null;
  // Transient as it contains java.lang.reflect.Method objects
  private transient List<Object> _beanProps;
  private List<?> _data;
  private List<?> _result;
}
