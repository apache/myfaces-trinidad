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
import org.apache.myfaces.adfinternal.style.xml.parse.PropertyNode;

/** As the Skin css file is parsed, methods in this class are called to
 * build up a SkinStyleSheetNode.
 */
public class SkinCSSDocumentHandler
{
  /**
   * Return the SkinStyleSheetNode that was created at the end of parsing
   * the skin css file.
   */
  public SkinStyleSheetNode getSkinStyleSheetNode()
  {
    return _styleSheet;
  }

  /**
   * Call this at the start of parsing the skin css file.
   */
  public void startDocument()
  {
    _namespaceMap = new HashMap();
    _selectorPropertiesList = new ArrayList();
  }
  
  /**
   * Call this at the end of parsing the skin css file.
   */
  public void endDocument()
  {
    _styleSheet = 
      new SkinStyleSheetNode(_selectorPropertiesList, _namespaceMap);
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
      _selectorPropertiesList.add(
        new SkinSelectorPropertiesNode(selector, _propertyNodeList));
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

  private boolean _inStyleRule = false;

  private SkinStyleSheetNode _styleSheet = null;

  private List _propertyNodeList = null;

  // a List of SkinSelectorPropertiesNode objects
  private List _selectorPropertiesList;

  private Map _namespaceMap;

}  
  
  
