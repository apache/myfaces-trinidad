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
package org.apache.myfaces.trinidad.component;

import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.model.MenuModel;
import org.apache.myfaces.trinidad.model.ModelUtils;


/**
 * Base class for the Menu component.
 * <p>
 * Work on modeling menus continues and it is very possible that this class
 * will change in a future release.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/component/UIXMenuHierarchy.java#0 $) $Date: 10-nov-2005.19:09:52 $
 * @author The Oracle ADF Faces Team
 * @todo these base classes need to be completely refactored
 */
abstract public class UIXMenuHierarchy extends UIXHierarchy
{

  /**
   * Create a Page component with the given render-type
   */
  protected UIXMenuHierarchy(String rendererType)
  {
    super(rendererType);
  }

  protected UIXMenuHierarchy()
  {
    this(null);
  }
}
