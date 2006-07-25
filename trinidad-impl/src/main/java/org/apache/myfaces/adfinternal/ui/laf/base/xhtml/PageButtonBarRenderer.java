/*
 * Copyright  2001-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.ui.laf.base.xhtml;

import java.util.HashMap;
import java.util.Map;

import org.apache.myfaces.adf.component.UIXHierarchy;

import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/PageButtonBarRenderer.java#0 $) $Date: 10-nov-2005.18:54:06 $
 * @author The Oracle ADF Faces Team
 */
public class PageButtonBarRenderer extends GlobalButtonBarRenderer
{
  protected UIXHierarchy getHierarchyBase(
    RenderingContext context,
    UINode           node
  )
  {
    return null;
  }

  protected void renderDefaultCellAttributes(
    RenderingContext context,
    UINode           child
    )
  {
  }

  protected boolean renderCellElement(
    RenderingContext context,
    UINode           child
    )
  {
    return (super.renderCellElement(context, child) &&
            !isEqualMarlinName(child, NAVIGATION_BAR_NAME));
  }
  
  protected String mapKey(String key)
  {
    return (String)_RESOURCE_KEY_MAP.get(key);
  }
 
  private static final Map _RESOURCE_KEY_MAP  =  new HashMap();
  static  
  {

    _RESOURCE_KEY_MAP.put("af_menuButtons.BLOCK_TITLE", 
                          "af_panelButtonBar.BLOCK_TITLE");
  }
}
