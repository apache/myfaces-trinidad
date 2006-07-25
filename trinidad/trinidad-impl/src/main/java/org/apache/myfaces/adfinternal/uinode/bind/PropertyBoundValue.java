/*
 * Copyright  2003-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.uinode.bind;

import org.apache.myfaces.adfinternal.ui.data.BoundValue;
import org.apache.myfaces.adfinternal.ui.RenderingContext;

import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.bean.PropertyKey;

/**
 * BoundValue that will retrieve a specific property from
 * a bean.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/uinode/bind/PropertyBoundValue.java#0 $) $Date: 10-nov-2005.18:50:07 $
 * @author The Oracle ADF Faces Team
 */
public class PropertyBoundValue implements BoundValue
{
  /**
   * @param bean the FacesBean
   * @param key  the property name
   */
  public PropertyBoundValue(
    FacesBean   bean,
    String      name)
  {
    if ((bean == null) || (name == null))
      throw new NullPointerException();
    
    _bean = bean;
    _key = bean.getType().findKey(name);
  }

  /**
   * @param bean the FacesBean
   * @param key  the PropertyKey
   */
  public PropertyBoundValue(
    FacesBean   bean,
    PropertyKey key)
  {
    if ((bean == null) || (key == null))
      throw new NullPointerException();

    _bean = bean;
    // Re-get the key to make sure it's the proper type
    _key = bean.getType().findKey(key.getName());
  }

  public Object getValue(RenderingContext context)
  {
    return (_key != null) ? _bean.getProperty(_key) : null;
  }

  private final FacesBean   _bean;
  private final PropertyKey _key;
}

