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
package org.apache.myfaces.trinidaddemo;

import java.beans.IntrospectionException;
import java.util.List;
import org.apache.myfaces.trinidad.model.ChildPropertyTreeModel;
import org.apache.myfaces.trinidad.model.TreeModel;

/**
 * This class facilitates the construction of a ChildPropertyTreeModel instance
 * via managed-beans. ChildPropertyTreeModel does not have a no-arg constructor.
 * This class does, and so can be instantiated as a managed-bean.
 * Two properties need to be set: "childProperty" and "instance"
 */
public class TreeModelAdapter implements java.io.Serializable
{
  public TreeModelAdapter()
  {
  }

  private String _propertyName = null;
  private Object _instance = null;
  private transient TreeModel _model = null;

  public TreeModel getModel() throws IntrospectionException
  {
    if (_model == null)
    {
      _model = new ChildPropertyTreeModel(getInstance(), getChildProperty());
    }
    return _model;
  }

  public String getChildProperty()
  {
    return _propertyName;
  }

  /**
   * Sets the property to use to get at child lists
   * @param propertyName
   */
  public void setChildProperty(String propertyName)
  {
    _propertyName = propertyName;
    _model = null;
  }

  public Object getInstance()
  {
    return _instance;
  }

  /**
   * Sets the root list for this tree.
   * @param instance must be something that can be converted into a List
   */
  public void setInstance(Object instance)
  {
    _instance = instance;
    _model = null;
  }
  
  /**
   * Sets the root list for this tree.
   * This is needed for passing a List when using the managed bean list  
   * creation facility, which requires the parameter type is List.
   * @param instance the list of root nodes
   */
  public void setListInstance(List<Object> instance)
  {
    setInstance(instance);
  }  
  
  /**
   * This should only be called if setListInstance was called.
   * 
   * This method shouldn't be needed according to 
   * faces spec 1.1 rev 1, see 5.3.1.3
   * However without this we get the following error in websphere:
   *                java.beans.IntrospectionException: No method 
   *                            "getListInstance" with 0 arg(s) of 
   *                            matching types in websphere
   */
  @SuppressWarnings("unchecked")
  public List<Object> getListInstance()
  {
    return (List<Object>)getInstance();
  }
  
}
