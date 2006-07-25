/*
 * Copyright  2005,2006 The Apache Software Foundation.
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

package org.apache.myfaces.adf.change;

import java.io.Serializable;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import org.apache.myfaces.adf.logging.ADFLogger;

/**
 * Proxy class representing the state of the UIComponent.
 * 'state' here means the state as served by saveState() method in interface
 * javax.faces.component.StateHolder.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/change/ChangeComponentProxy.java#0 $) $Date: 10-nov-2005.19:09:57 $
 * @author The Oracle ADF Faces Team
 * @todo =-=pu: saveState() just saves the state of this component, what if we
 *    were to represent a component subtree in ChangeComponentProxy. We would
 *    need to do things similar to UIComponent.processSaveState() does with the
 *    exception that we disregard the 'transient' attribute in the algorithm.
 */
class ChangeComponentProxy implements Serializable
{
  /**
   * Constructs an ChangeComponentProxy with the specified UIComponent instance.
   * @throws IllegalArgumentException if specified uiComponent were to be null.
   */
  public ChangeComponentProxy(
    FacesContext facesContext,
    UIComponent uiComponent)
  {
    if (uiComponent == null)
      throw new IllegalArgumentException(
        "Cannot construct an ChangeComponentProxy with null uiComponent.");
    _class = uiComponent.getClass();
    _className = _class.getName();
    _state = uiComponent.saveState(facesContext);
  }

  /**
   * Creates a new UIComponent, corresponding to this proxy, restores its state
   *  and returns the same. Returns 'null' if this process fails for any reason.
   */
  public UIComponent createComponent()
  {
    UIComponent uic = null;
    Class clazz = _getComponentClass();
    if (clazz == null)
    {
      // An error must have already been logged in _getComponentClass();
      return null;
    }

    try
    {
      uic = (UIComponent) clazz.newInstance();
      uic.restoreState(FacesContext.getCurrentInstance(), _state);
    }
    catch (InstantiationException ie)
    {
      _LOG.warning(
        "Error on trying to create new component instance for " +
          clazz.getName(),
        ie);
    }
    catch (IllegalAccessException iae)
    {
      _LOG.warning(
        "Error on trying to create new component instance for " +
          clazz.getName(),
        iae);
    }
    return uic;
  }

  private Class _getComponentClass()
  {
    Class clazz = _class;
    if (clazz == null)
    {
      try
      {
        ClassLoader cl = Thread.currentThread().getContextClassLoader();
        clazz = cl.loadClass(_className);
        _class = clazz;
      }
      catch (ClassNotFoundException e)
      {
        _LOG.severe(e);
      }
    }

    return clazz;
  }

  private transient Class _class;
  private String _className;
  private Object _state;
  static private final ADFLogger _LOG =
    ADFLogger.createADFLogger(org.apache.myfaces.adf.change.ChangeComponentProxy.class);
}
