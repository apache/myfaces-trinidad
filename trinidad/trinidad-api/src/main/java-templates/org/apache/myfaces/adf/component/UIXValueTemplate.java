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
package org.apache.myfaces.adf.component;

import javax.faces.component.ValueHolder;

import org.apache.myfaces.adf.bean.PropertyKey;

/**
 * Base class for components that have a value.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java-templates/oracle/adf/view/faces/component/UIXValueTemplate.java#0 $) $Date: 10-nov-2005.19:07:53 $
 * @author The Oracle ADF Faces Team
 */
abstract public class UIXValueTemplate extends UIXComponentBase
  implements ValueHolder
{
  /**
   * Return the locally stored value, ignoring any ValueBinding
   * set for "value".
   */
  public Object getLocalValue()
  {
    PropertyKey key = UIXValue.VALUE_KEY;
    return getFacesBean().getLocalProperty(key);
  }
}

