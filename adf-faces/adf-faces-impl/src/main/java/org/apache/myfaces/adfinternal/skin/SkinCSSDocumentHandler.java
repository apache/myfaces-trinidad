/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.adfinternal.skin;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.myfaces.adfinternal.style.util.StyleUtils;
import org.apache.myfaces.adfinternal.style.xml.parse.PropertyNode;
import org.apache.myfaces.adfinternal.util.nls.LocaleUtils;

/** As the Skin css file is parsed, methods in this class are called to
 * build up a SkinStyleSheetNode.
 */
public class SkinCSSDocumentHandler
{
 
  /**
   * Return the List of SkinStyleSheetNodes that was created
   *  at the end of parsing the skin css file (endDocument).
   */
  public List <SkinStyleSheetNode> getSkinStyleSheetNodes()
  {
    return _skinStyleSheetNodes;  
  }
  
  /**
   * Call this at the start of parsing the skin css file.
   */
  public void startDocument()
  {
    _namespaceMap = new HashMap();
    _completeSelectorNodeList = new ArrayList();
  }
  
  /**
   * Call this at the end of parsing the skin css file.
   */
  public void endDocument()
  {
    // We now have a list of CompleteSelectorNodes.
    // We need to group this list into stylesheet nodes by matching 
    // the additional information, like direction.
    // Then we create a list of SkinStyleSheetNodes.
    _skinStyleSheetNodes = 
      _createSkinStyleSheetNodes(_completeSelectorNodeList, _namespaceMap);
  }

  public void comment(String text)
  {
    // ignore comments
  }
  
  /**
   * Call this at the beginning of parsing one set of selectors/properties.
   * e.g., .AFDefaultFont, af|navigationPath::font 
   * {font-family:Arial,Helvetica; font-size:small}
   */
  public void startSelector()
  {
    _inStyleRule = true;
    _propertyNodeList = new ArrayList();
  }
  
  /**
   * Call this at the end of parsing one set of selectors/properties.
   * @param selectors A List of Strings, each String is a selector.
   * e.g., given the selectors/properties:
   * .AFDefaultFont, af|navigationPath::font 
   * {font-family:Arial,Helvetica; font-size:small}
   * The selectors are ".AFDefaultFont" and "af|navigationPath::font"
   */
  public void endSelector(List selectors)
  {
    if (selectors == null)
      return;
    int selectorNum = selectors.size();
    for (int i = 0; i < selectorNum; i++)
    {
      String selector = (String)selectors.get(i);
      CompleteSelectorNode node =
        _createCompleteSelectorNode(selector, _propertyNodeList);
      _completeSelectorNodeList.add(node);
    }
    _inStyleRule = false;
    _propertyNodeList = null;
  }

  /**
   * Call this when a property name/value is found.
   * e.g., given the selectors/properties:
   * .AFDefaultFont, af|navigationPath::font 
   * {font-family:Arial,Helvetica; font-size:small}
   * One property name/value pair is "font-family"/"Arial,Helvetica"
   * @param name 
   * @param value 
   */
  public void property(String name, String value)
  {
    if (_inStyleRule && (_propertyNodeList != null))
    {
      _propertyNodeList.add(new PropertyNode(name, value));
    }
  }

  /**
   * Call when you have an @ rule. This adds the @ rule to the object's
   * namespaceMap.
   * @param name The @rule string, which contains the entire line with the
   * @ rule, which ends with a semicolon.
   * e.g., @namespace af url(http:\\www.xxx.com)
   */
  public void atRule(String name)
  {
    // parse the atRule further here.
    // name will be something like:
    // "namespace af url(asdfadfadf)"
    if (name != null)
    {
      if (name.startsWith("@namespace"))
      {
        // @todo deal with default namespaces that don't have prefixes??
        String[] namespaceParts = name.split("\\s+");
        if (namespaceParts.length > 2)
        {
          String url = namespaceParts[2];
          
          // first, strip off the url( and );
          if (url.startsWith("url("))
            url = url.substring(4);
          if (url.endsWith(");"))
            url = url.substring(0, url.length() - 2);
          else if (url.endsWith(";"))
            url = url.substring(0, url.length() - 1);
            
          // second, strip off the starting/ending quotes if there are any                    
          url = SkinStyleSheetParserUtils.trimQuotes(url);
          _namespaceMap.put(namespaceParts[1], url);
        }
      }
    }
  }

  // create a CompleteSelectorNode (this is the selector, properties, and
  // additional info, like 'rtl' direction
  private CompleteSelectorNode _createCompleteSelectorNode(
    String selector, 
    List  propertyNodeList)
  {
    // parse the selector to see if there is a :rtl or :ltr ending.
    // if so, then set the reading direction.
    int direction = LocaleUtils.DIRECTION_DEFAULT;
    if (selector.endsWith(StyleUtils.RTL_CSS_SUFFIX))
    {
      int length = StyleUtils.RTL_CSS_SUFFIX.length();
      // strip off the SUFFIX  
      selector = selector.substring(0, selector.length()-length);
      direction = LocaleUtils.DIRECTION_RIGHTTOLEFT;
    }
    else if (selector.endsWith(StyleUtils.LTR_CSS_SUFFIX))
    {
      int length = StyleUtils.LTR_CSS_SUFFIX.length();
      // strip off the SUFFIX  
      selector = selector.substring(0, selector.length()-length);
      direction = LocaleUtils.DIRECTION_LEFTTORIGHT;
    }

    return 
      new CompleteSelectorNode(selector, propertyNodeList, direction);
  }

  /**
   * Given a List of CompleteSelectorNodes, we create a List of 
   * SkinStyleSheetNodes. We do this by looping through each
   * CompleteSeletcorNode and finding the SkinStyleSheetNode with matching
   * direction attribute (someday we'll add locale, browser, etc), or 
   * creaing a new SkinStyleSheetNode if a matching one doesn't exist.
   * @param selectorList a list of CompleteSelectorNodes.
   * @param namespaceMap the namespace map
   * @return a List of SkinStyleSheetNodes
   */
  private List <SkinStyleSheetNode> _createSkinStyleSheetNodes(
    List <CompleteSelectorNode> selectorList, 
    Map namespaceMap)
  {  
    List <SkinStyleSheetNode> ssNodeList = new ArrayList();
    // to start with ssNodeList is empty
    // for each selector node, look to see if we can find a SkinStyleSheetNode
    // that it belongs to (by matching direction).
    // if not, create a new ssNode, and add it to the ssNodeList, and
    // add the selector node to the ssNode.
    
    for (CompleteSelectorNode completeSelectorNode : selectorList) 
    {
       // we add to the ssNodeList in this method.
        int direction = completeSelectorNode.getDirection();
        // loop through the skinStyleSheetNodeList to find a match
        boolean match = false;
        for (SkinStyleSheetNode ssNode : ssNodeList) 
        {
            int ssNodeDirection = ssNode.getDirection();
            if (ssNodeDirection == direction)
            {
              // got a match! use this one to add the selector node to.
              ssNode.add(completeSelectorNode.getSkinSelectorPropertiesNode());
              match = true;
              break;
            }        
        }
        if (!match)
        {
          // no matching stylesheet node found, so create a new one
           SkinStyleSheetNode ssNode = 
            new SkinStyleSheetNode(namespaceMap, direction);
           ssNode.add(completeSelectorNode.getSkinSelectorPropertiesNode());
           // add the ssNode to the ssNodeList
           ssNodeList.add(ssNode);
        }
           
    }
     
    return ssNodeList;
     
  }

  /**
   * This Class contains a SkinSelectorPropertiesNode and a rtl direction.
   * We will use this information when creating a SkinStyleSheetNode.
   */
  private static class CompleteSelectorNode
  {
    public CompleteSelectorNode(
      String selectorName,
      List   propertyNodes,
      int    direction
      )
    {
      _node = new SkinSelectorPropertiesNode(selectorName, propertyNodes);
      _direction = direction;
    }
    
    public SkinSelectorPropertiesNode getSkinSelectorPropertiesNode()
    {
      return _node;
    }
    
    public int getDirection()
    {
      return _direction;
    }
    
    private SkinSelectorPropertiesNode _node;
    private int _direction;  // the reading direction
  }
  
  private boolean _inStyleRule = false;
 
  private List <SkinStyleSheetNode> _skinStyleSheetNodes = null;

  private List _propertyNodeList = null;

  // we build this list in this document handler. We use this 
  // list to create a list of 
  // SkinStyleSheetNodes and SkinSelectorPropertiesNodes
  private List <CompleteSelectorNode> _completeSelectorNodeList;

  private Map _namespaceMap;

}  
  
  
