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

package org.apache.myfaces.adfinternal.skin;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/** Stores information about the .css skin file.
 * namespaceMap, a List of SkinSelectorPropertiesNodes, and direction.
 * @todo honor the namespaces that are set in the css file. For now, we ignore
 * them. We need to honor them for icons, properties, and styles at the same
 * time so they are consistent. By honoring, I mean if the namespace is
 * af http://uix.faces.abc and the style name is af|navigationPath, we store the 
 * icon or property as http://uix.faces.abc|navigationPath. We need to do something
 * similar with styles.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/SkinStyleSheetNode.java#0 $) $Date: 10-nov-2005.18:58:59 $

 * @author The Oracle ADF Faces Team
 */
class SkinStyleSheetNode
{
    
    SkinStyleSheetNode(
      List <SkinSelectorPropertiesNode> skinSelectorNodeList,
      Map  namespaceMap,
      int  direction
      )
    {
      _skinSelectorNodeList = skinSelectorNodeList;
      _namespaceMap     = namespaceMap;
      _direction        = direction;
    } 
    
  
  SkinStyleSheetNode(
    Map  namespaceMap,
    int  direction
    )
  {
    _namespaceMap     = namespaceMap;
    _direction        = direction;
  }     
    
  public void add(SkinSelectorPropertiesNode node)
  {
    if (_skinSelectorNodeList == null)
      _skinSelectorNodeList = new ArrayList();
    _skinSelectorNodeList.add(node);
  } 
  
    /**
   * 
   * @return Map containing Strings keys/values of namespace prefix and 
   * namespaces specified in the style sheet.
   */
    public Map getNamespaceMap()
    {
      return _namespaceMap;
    }
    
    /**
   * 
   * @return List containing SkinSelectorPropertiesNodes
   */    
    public List <SkinSelectorPropertiesNode> getSelectorNodeList()
    {
      return _skinSelectorNodeList;
    }
    
    /**
    *
    * @return int indicating the direction
    * LocaleUtils.DIRECTION_DEFAULT 
    * LocaleUtils.DIRECTION_LEFTTORIGHT 
    * LocaleUtils.DIRECTION_RIGHTTOLEFT
    */
    public int getDirection()
    {
      return _direction;
    }    

    private Map  _namespaceMap;
    private List <SkinSelectorPropertiesNode> _skinSelectorNodeList;
    private int _direction;  // reading direction

}
