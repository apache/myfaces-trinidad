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
package org.apache.myfaces.trinidadinternal.skin;

import java.io.StringReader;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.style.util.NameUtils;
import org.apache.myfaces.trinidadinternal.style.util.StyleUtils;
import org.apache.myfaces.trinidadinternal.style.xml.parse.PropertyNode;
import org.apache.myfaces.trinidadinternal.util.IntegerUtils;
import org.apache.myfaces.trinidadinternal.util.nls.LocaleUtils;

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
    // We now have a list of CompleteSelectorNodes.
    // We need to group this list into stylesheet nodes by matching 
    // the additional information, like direction.
    // Then we create a list of SkinStyleSheetNodes.
    return 
      _createSkinStyleSheetNodes(_completeSelectorNodeList, _namespaceMap);
  }
   
  /**
  * Call this at the start of parsing the skin css file.
  */
  public void startDocument()
  {
    // do nothing
  }
   
  /**
  * Call this at the end of parsing the skin css file.
  */
  public void endDocument()
  {
    // do nothing
  }

  public void comment(String text)
  {
     // ignore comments
  }
   
  /**
  * Call this at the beginning of parsing one set of selectors/properties.
  * e.g., .AFDefaultFont, af|breadCrumbs::font 
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
   * .AFDefaultFont, af|breadCrumbs::font 
   * {font-family:Arial,Helvetica; font-size:small}
   * The selectors in the List are 
   * ".AFDefaultFont" and "af|breadCrumbs::font"
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
         _createCompleteSelectorNode(selector, 
                                     _propertyNodeList,
                                     _selectorAgents,
                                     _selectorPlatforms);
       _completeSelectorNodeList.add(node);
    }
    // reset flags
    _inStyleRule = false;
    _propertyNodeList = null;
  }

   /**
    * Call this when a property name/value is found.
    * e.g., given the selectors/properties:
    * .AFDefaultFont, af|breadCrumbs::font 
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
  * Call when you have an atRule. This will do further processing.
  * @param atRule The @rule string
  * e.g., @namespace af url(http:\\www.xxx.com);
  * e.g., @agent gecko { .foo {color:red}}
  */
  public void atRule(String atRule)
  {
    // parse the atRule further here.
    if (atRule != null)
    {
      if (atRule.startsWith("@namespace"))
      {
        // TODO deal with default namespaces that don't have prefixes??
        String[] namespaceParts = atRule.split("\\s+");
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
      else if (atRule.startsWith(_AT_AGENT))
      {
        _parseCustomAtRule(_AT_AGENT, atRule);
      }
      else if (atRule.startsWith(_AT_PLATFORM))
      {
        _parseCustomAtRule(_AT_PLATFORM, atRule);        
      }
      // for now, ignore other atRules in a skinning css file
      
    }
  }

  /** Get the atRule, and send its contents through the SkinCSSParser
   * again, using the current DocumentHandler object. The start/end
   * callbacks will be called again, but in the context of the atRule.
   */
  private void _parseCustomAtRule(String type, String atRule)
  {
    // get the @agent agents, they are deliminated by commas
    // parse out the content
    // save the atRule type, so the document handler code can get to it.
    // run this through parser again
    String content = _getAtRuleContent(atRule);
    int[] targetTypes = _getAtRuleTargetTypes(type, atRule);
    _setAtRuleTargetTypes(type, targetTypes);
    
    // use this current DocumentHandler. This way we can add to the 
    // CompleteSelectorNode list with agent information.
    SkinCSSParser parser = new SkinCSSParser();
    parser.parseCSSDocument(new StringReader(content), this);
    
    // reset
    _resetAtRuleTargetTypes(type);

  }
  
  private void _setAtRuleTargetTypes(
    String type,
    int[]  targetTypes)
  {
    
    if (type == _AT_AGENT)
      _selectorAgents = targetTypes;
    else if (type == _AT_PLATFORM)
      _selectorPlatforms = targetTypes;
  }
  
  private void _resetAtRuleTargetTypes(
    String type)
  {
    if (type == _AT_AGENT)
      _selectorAgents = null;
    else if (type == _AT_PLATFORM)
      _selectorPlatforms = null;
  }
  
   // create a CompleteSelectorNode (this is the selector, properties, and
   // additional info, like 'rtl' direction
  private CompleteSelectorNode _createCompleteSelectorNode(
    String selector, 
    List  propertyNodeList,
    int[] selectorAgents,
    int[] selectorPlatforms)
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
      new CompleteSelectorNode(
        selector, 
        propertyNodeList, 
        direction, 
        selectorAgents, 
        selectorPlatforms);
  }

  /**
    * Given a List of CompleteSelectorNodes (selector nodes with
    * infor about selectors, properties, direction, agent), we create a List of 
    * SkinStyleSheetNodes.
    * @param selectorList a list of CompleteSelectorNodes.
    * @param namespaceMap the namespace map
    * @return a List of SkinStyleSheetNodes
    */
  private List <SkinStyleSheetNode> _createSkinStyleSheetNodes(
    List <CompleteSelectorNode> selectorList, 
    Map namespaceMap)
  {  
    List <SkinStyleSheetNode> skinStyleSheetNodes = new ArrayList();
     
    for (CompleteSelectorNode completeSelectorNode : selectorList) 
    {
      // we add to the ssNodeList in this method.
      int direction = completeSelectorNode.getDirection();
      int[] agents = completeSelectorNode.getAgents();
      int[] platforms = completeSelectorNode.getPlatforms();
         
      // loop through the skinStyleSheetNodeList to find a match
      // of direction, agents, platforms, etc.
      boolean match = false;
         
      // iterate backwards, because the last node is most likely the
      // matching stylesheetnode.
      for (int i=skinStyleSheetNodes.size()-1; i >=0 && !match; i--)
      {
        SkinStyleSheetNode ssNode = skinStyleSheetNodes.get(i);
        match = ssNode.matches(direction, agents, platforms);
        if (match)
          ssNode.add(completeSelectorNode.getSkinSelectorPropertiesNode());
      }

      if (!match)
      {
       // no matching stylesheet node found, so create a new one
        SkinStyleSheetNode ssNode = 
         new SkinStyleSheetNode(namespaceMap, direction, agents, platforms);
        ssNode.add(completeSelectorNode.getSkinSelectorPropertiesNode());
        skinStyleSheetNodes.add(ssNode);
      }       
    }
    return skinStyleSheetNodes;
  }

  /**
   * 
   * @param type type of the at rule. _AT_AGENT or _AT_PLATFORM
   * @param atRule - the atRule string
   * @return int[] the target types using AdfFacesAgent constants like
   * AdfFacesAgent.APPLICATION_IEXPLORER
   */
  private int[] _getAtRuleTargetTypes(
    String type, 
    String atRule)
  {
    // given the atRule string, get the target types --
    // @agent ie, gecko {...} => target types are the 
    // AdfFacesAgent constants for ie and gecko.
    int firstSpace = atRule.indexOf(' ');
    int openBrace = atRule.indexOf('{');
    if (firstSpace != -1 && openBrace != -1)
    {
      String types = atRule.substring(firstSpace, openBrace);
      String[] typeArray = types.split(",");
      List <Integer> list = new ArrayList();
      
      if (type == _AT_AGENT)
      {
        for (int i=0; i < typeArray.length; i++)
        {
          int agentInt = NameUtils.getBrowser(typeArray[i].trim());
          
          if (agentInt != TrinidadAgent.APPLICATION_UNKNOWN)
            list.add(IntegerUtils.getInteger(agentInt));
        }
      }
      else if (type == _AT_PLATFORM)
      {
        for (int i=0; i < typeArray.length; i++)
        {
          int platformInt = NameUtils.getPlatform(typeArray[i].trim());           

          if (platformInt != TrinidadAgent.OS_UNKNOWN)
            list.add(IntegerUtils.getInteger(platformInt));
        }          
      }
      return _getIntArray(list);
   }
   else return null;
  }

  // Copies Integers from a List of Integers into an int array
  private int[] _getIntArray(List <Integer> integerList)
  {
    int count = integerList.size();
  
    if (count == 0)
      return null;
  
    int[] array = new int[count];
  
    for (int i = 0; i < count; i++)
      array[i] = integerList.get(i).intValue();
  
    return array;
  }
   
   /**
    * 
    * @param atRule - the entire @rule's definition, including content.
    * e.g., @agent ie, gecko { af|inputText::content {color:red}}
    * @return the content as a String
    * e.g., "af|inputText::content {color:red}"
    */
  private String _getAtRuleContent(String atRule)
  {
    int openBraceIndex = atRule.indexOf('{');
    int endBraceIndex = atRule.lastIndexOf('}');
    if (endBraceIndex == -1) 
      endBraceIndex = atRule.length();
      
    if (openBraceIndex == -1)
      return null;
    else
     return atRule.substring(openBraceIndex+1, endBraceIndex);
   
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
      int    direction,
      int[]  agents,
      int[]  platforms
      )
    {
      _node = new SkinSelectorPropertiesNode(selectorName, propertyNodes);
      _direction = direction;
      // copy the agents and platforms because these get nulled out 
      // at the end of the @rule parsing.
      _agents = _copyIntArray(agents);
      _platforms = _copyIntArray(platforms);
    }
    
    public SkinSelectorPropertiesNode getSkinSelectorPropertiesNode()
    {
      return _node;
    }
    
    public int getDirection()
    {
      return _direction;
    }
    
    public int[] getAgents()
    {
      return _agents;
    }
    
    public int[] getPlatforms()
    {
      return _platforms;
    }
    
    // Returns a copy of the int array
    private static int[] _copyIntArray(int[] array)
    {
      if (array == null)
        return null;
    
      int[] copy = new int[array.length];
      System.arraycopy(array, 0, copy, 0, array.length);
    
      return copy;
    }
     
    private SkinSelectorPropertiesNode _node;
    private int _direction;  // the reading direction
    private int[] _agents;
    private int[] _platforms;

  }

  private static String _AT_AGENT = "@agent";
  private static String _AT_PLATFORM = "@platform";

  // below are properties that we set and reset 
  // as the methods of this documentHandler get called.
  private boolean _inStyleRule = false;
  private List _propertyNodeList = null;
  // we build this list as we parse the skinning css file. We use this 
  // list to create a list of SkinStyleSheetNodes
  private List <CompleteSelectorNode> _completeSelectorNodeList = 
    new ArrayList<CompleteSelectorNode>();
  // these are the selector platform and agents of the selectors we
  // are currently parsing in this document.
  private int[] _selectorPlatforms = null;
  private int[] _selectorAgents = null;
  private Map _namespaceMap = new HashMap();
  
}  
   
   
