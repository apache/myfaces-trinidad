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

package org.apache.myfaces.adfinternal.ui.laf.xml.parse;



import org.apache.myfaces.adfinternal.skin.icon.Icon;

/**
 * Object which represents a single <icon> element.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/IconNode.java#0 $) $Date: 10-nov-2005.18:50:36 $
 * @author The Oracle ADF Faces Team
 */
public class IconNode
{
  /**
   * Creates a IconNode
   * @param namespace The namespace of the icon
   * @param name The name of the icon
   * @param icon The Icon instance
   */
  public IconNode(
    String name,
    Icon   icon
    )
  {
    if (name == null)
    {
      throw new NullPointerException("Null name");
    }

    _name = name;
    _icon = icon;
  }


  /**
   * Returns the name of the icon that is defined
   * by this IconNode.
   */
  public String getIconName()
  {
    return _name;
  }

  /**
   * Returns the Icon instance for this IconNode.
   */
  public Icon getIcon()
  {
    return _icon;
  }

  private String      _name;
  private Icon        _icon;
}
