/*
 * Copyright  2000-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.ui.data;

import org.apache.myfaces.adfinternal.ui.RenderingContext;

/**
 * The BoundValue interface is used to dynamically compute
 * a value based on the current rendering context.  A common
 * implementation of this interface is to retrieve a DataObject
 * from the RenderingContext and get the data from that object -
 * <code>BoundDataValue</code> implements this.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/data/BoundValue.java#0 $) $Date: 10-nov-2005.18:56:29 $
 * @author The Oracle ADF Faces Team
 */
public interface BoundValue
{
  /**
   * Called to retrieve a value based on the current rendering
   * context.
   * @param context the rendering context
   */
  public Object getValue(RenderingContext context);
}
