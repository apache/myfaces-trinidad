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
package org.apache.myfaces.trinidad.component;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.faces.component.UIComponent;

import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.event.FacesListener;
import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.event.AttributeChangeListener;

/**
 * Pure abstract base class for all UIX components.
 */
abstract public class UIXComponent extends UIComponent
{
  /**
   * Returns the FacesBean used for storing the component's state.
   */
  abstract public FacesBean getFacesBean();

  /**
   * Adds an AttributeChangeListener.  Attribute change events are not
   * delivered for any programmatic change to a property.  They are only
   * delivered when a renderer changes a property without the application's
   * specific request.  An example of an attribute change events might
   * include the width of a column that supported client-side resizing.
   */
  abstract public void addAttributeChangeListener(AttributeChangeListener acl);

  /**
   * Removes an AttributeChangeListener.  Attribute change events are not
   * delivered for any programmatic change to a property.  They are only
   * delivered when a renderer changes a property without the application's
   * specific request.  An example of an attribute change events might
   * include the width of a column that supported client-side resizing.
   */
  abstract public void removeAttributeChangeListener(AttributeChangeListener acl);

  /**
   * Gets the registered AttributeChangeListeners.
   */ 
  abstract public AttributeChangeListener[] getAttributeChangeListeners();

  /**
   * Sets a method binding to an AttributeChangeListener.  Attribute
   * change events are not
   * delivered for any programmatic change to a property.  They are only
   * delivered when a renderer changes a property without the application's
   * specific request.  An example of an attribute change events might
   * include the width of a column that supported client-side resizing.
   */
  abstract public void setAttributeChangeListener(MethodBinding mb);

  /**
   * Gets the method binding to an AttributeChangeListener.  Attribute
   * change events are not
   * delivered for any programmatic change to a property.  They are only
   * delivered when a renderer changes a property without the application's
   * specific request.  An example of an attribute change events might
   * include the width of a column that supported client-side resizing.
   */
  abstract public MethodBinding getAttributeChangeListener();

  abstract public void markInitialState();

  // JSF 1.2 methods that we're adding up front
  abstract public int getFacetCount();
  abstract public void encodeAll(FacesContext context) throws IOException;

  
  // Everything below here is a UIComponent method

  @SuppressWarnings("unchecked")
  @Override
  public abstract Map getAttributes();

  @SuppressWarnings("unchecked")
  @Override
  public abstract List getChildren();

  @SuppressWarnings("unchecked")
  @Override
  public abstract Map getFacets();

  @SuppressWarnings("unchecked")
  @Override
  public abstract Iterator getFacetsAndChildren();

  @SuppressWarnings("unchecked")
  @Override
  protected abstract FacesListener[] getFacesListeners(Class clazz);

  public abstract Object saveState(FacesContext context);
  public abstract void restoreState(FacesContext context, Object state);
  public abstract boolean isTransient();
  public abstract void setTransient(boolean trans);
}
