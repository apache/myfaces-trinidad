/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidadinternal.style.util;

import java.beans.Beans;

import java.io.PrintWriter;
import java.io.StringWriter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.xml.parse.PropertyNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleNode;


/**
 * CSS-generation related utilities used when we write out our css-2 stylesheet
 * document based on the skin's css-3 stylesheet document.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/util/CSSUtils.java#0 $) $Date: 10-nov-2005.18:58:49 $
 */

public class CSSGenerationUtils
{
  /**
   * Returns a MatchingStyles object of normalized propertyString to the StyleNodes that have them as properties.
   * MatchingStyles segregates the clientRules from the normal selectors for ease of css rendering.
   * MatchingStyles object uses LinkedHashMap within, to indicate that relative ordering is preserved between the first
   * appearance of selectors with unshared properties.
   * However, a later style rule that shares its properties with a much earlier style rule will be defined with that
   * style rule, thus having its definition moved up in the file.  Therefore, customers should rely on
   * specificity rather than ordering to override styles.
   * defined with 
   * @param styleNodes
   * @return
   */
  private static MatchingStyles _buildMatchingStylesMap(List<StyleNode> styleNodes)
  {
    int styleCount = styleNodes.size();

    MatchingStyles matchingStyles = new MatchingStyles(styleCount);

    // at this point the styles List<StyleNode> can contain both Styles with
    // non-null selector or non-null name(aka alias). We only generate
    // the styles where getSelector is non-null.
    for (StyleNode styleNode : styleNodes)
    {
      if (styleNode.getSelector() != null)
      {
        // Get the property string (properties are sorted so that
        // the order doesn't affect whether styles match).
        String propertyString = _getSortedPropertyString(styleNode);
        if (!("".equals(propertyString)))
        {
          if (styleNode.hasClientRule())
          {
            matchingStyles.addStyle(styleNode.getClientRule(), propertyString, styleNode);
          }
          else
          {
            matchingStyles.addStyle(propertyString, styleNode);
          }
        }
      }
    }
    
    return matchingStyles;
  }

  /**
   * Writes the properties of a merged set of style rules
   * @param out
   * @param properties
   * @param styleSheetName
   * @param compressStyles
   * @param baseURI
   * @return
   */
  private static void _writeMergedProperties(
    PrintWriter out, Iterable<PropertyNode> properties, String styleSheetName, boolean compressStyles, String baseURI)
  {
    out.print(" {");

    boolean first = true;

    for (PropertyNode property : properties)
    {
      String propName  = property.getName();
      String propValue = property.getValue();

      if ((propName != null) &&
          (propValue != null) &&
          (propValue.length() > 0 ))
      {
        // insert separator before all except the first property
        if (first)
          first = false;
        else
          out.print(';');

        out.print(propName);
        out.print(':');
        String resolvedPropValue = CSSUtils.resolvePropertyValue(styleSheetName, baseURI, propName, propValue);
        out.print(resolvedPropValue);
      }
    }

    out.print('}'); 

    // take out the newlines for performance
    if (!compressStyles)
    {
      out.println();
    }
  }

  /**
   * Writes out a List of valid selectors
   * @param out
   * @param validSelectors
   */
  private static void _writeValidSelectors(PrintWriter out, List<String> validSelectors)
  {
    boolean first = true;

    // Write out all of the style selectors for this property string
    for (String validSelector : validSelectors)
    {
      // insert separator before all except the first property
      if (first)
        first = false;
      else
        out.print(',');

      out.print(validSelector);
    }
  }

  /**
   * Given a List of mappedSelectors, returns a List of valid selectors to write
   * @param compressStyles
   * @param shortStyleClassMap
   * @param namespacePrefixArray
   * @param mappedSelectors
   * @return
   */
  private static List<String>  _calculateValidSelectors(
    boolean compressStyles, Map<String, String> shortStyleClassMap,
    String[] namespacePrefixArray, List<String> mappedSelectors)
  {
     List<String> validSelectors = new ArrayList<String>(mappedSelectors.size() * 2);

    // TODO: figure out why we write both the uncompressed & compressed styles for styles
    // without a '|' character, shouldn't the uncompressed be enough on its own? This results
    // in some ugly code here.
      
    // Write out all of the style selectors for this property string
    for (String mappedSelector : mappedSelectors)
    { 
      String validFullNameSelector = null;

      // write out the full selector if we aren't compressing styles or
      // it doesn't have a '|' in the name which means it may be a user's public styleclass
      // and we don't want to compress those; we will also write out the compressed
      // version the public styleclasses in the next step.
      if (!compressStyles || (mappedSelector.indexOf('|') == -1))
      {
        validFullNameSelector = getValidFullNameSelector(mappedSelector, namespacePrefixArray);

        if (validFullNameSelector != null)
        {
          validSelectors.add(validFullNameSelector);
        }
      }

      if (compressStyles)
      {
        String shortSelector = getShortSelector(shortStyleClassMap, namespacePrefixArray, mappedSelector);

        // if the transformed full name is different than the shortSelector
        // then write out the shortSelector, too.
        if (shortSelector != null)
        {
          String validShortSelector = getValidFullNameSelector(shortSelector, namespacePrefixArray);

          // if we wrote out a full style, check to see if we need to write out the short, too.
          // if it is something different, write out the short, too.
          if (validFullNameSelector != null)
          {
            //Since validFullNameSelector is not null, we know we wrote out a full style
            // we write out a short style too in this case if it is different
            // example: .PublicStyleClass is written out fully even in compressed mode, but
            // it is different in compressed mode, so we write that out, too.
            if (!validFullNameSelector.equals(validShortSelector))
            {
              validSelectors.add(validShortSelector);
            }
          }
          else
          {
            validSelectors.add(validShortSelector);
          }
        }
      }
    }
    
    return validSelectors;
  }

  /**
   * Writes the merged selectors represented by a mappedSelectors, returning how many selectors were written. If the number
   * of selectors that would be written is greater to or equal than maxSelectors, nothing will be written and
   * 0 will be returned.  It is up to the caller to retry with this entry later.
   * @param out
   * @param compressStyles
   * @param shortStyleClassMap
   * @param namespacePrefixArray
   * @param maxSelectors
   * @param mappedSelectors
   * @return
   */
  private static int _writeMergedSelectors(
    PrintWriter out, boolean compressStyles, Map<String, String> shortStyleClassMap,
    String[] namespacePrefixArray, int maxSelectors, List<String> mappedSelectors)
  {
    // to make this atomic, we first calculate all of the selectors that we will write out.  If we have space, we will
    // then write the selectors out
    List<String> validSelectors = _calculateValidSelectors(compressStyles, shortStyleClassMap, namespacePrefixArray,
                                                           mappedSelectors);

    int selectorsToWrite = validSelectors.size();

    if (selectorsToWrite >= maxSelectors)
    {
      // not enough space, so abort
      return 0;
    }
    else
    {
      _writeValidSelectors(out, validSelectors);

      return selectorsToWrite;
    }
  }

  /**
   * For a List of matching StyleNode, return the mapped version of their Selectors
   */
  private static List<String> _calculateMappedSelectors(
    String[] namespacePrefixArray, Map<String, String> afSelectorMap, List<StyleNode> matchingStyleNodes)
  {
    List<String> mappedSelectors = new ArrayList<String>(matchingStyleNodes.size());

    for (StyleNode matchingNode : matchingStyleNodes)
    {
      String matchingSelector = matchingNode.getSelector();

      // We should always have a selector at this point
      assert (matchingSelector != null);
      
      String mappedSelector = getMappedSelector(afSelectorMap, namespacePrefixArray, matchingSelector);

      mappedSelectors.add(mappedSelector);
    }
    
    return mappedSelectors;
  }

  /**
   * Writes a merged style rule represented by a mergedEntry, returning how many selectors were written.  If the number
   * of selectors that would be written is greater to or equal than maxSelectors, nothing will be written and
   * 0 will be returned.  It is up to the caller to retry with this entry later.
   * @param styleSheetName
   * @param compressStyles
   * @param shortStyleClassMap
   * @param namespacePrefixArray
   * @param afSelectorMap
   * @param out
   * @param maxSelectors
   * @param mergedEntry
   * @return
   */
  private static int _writeMergedEntry(
    String                             styleSheetName,
    String                             baseURI,
    boolean                            compressStyles,
    Map<String, String>                shortStyleClassMap,
    String[]                           namespacePrefixArray,
    Map<String, String>                afSelectorMap,
    PrintWriter                        out,
    int                                maxSelectors,
    Map.Entry<String, List<StyleNode>> mergedEntry
    )
  {
    String propertyString = mergedEntry.getKey();

    // we shouldn't have any null or empty Strings
    assert (propertyString != null);
    assert (!"".equals(propertyString));

    // Get all of the styles which share this property string.
    List<StyleNode> matchingStyleNodes = mergedEntry.getValue();

    // Actually, we should always have at least one StyleNode here
    assert (!matchingStyleNodes.isEmpty());
    
    List<String> mappedSelectors = _calculateMappedSelectors(namespacePrefixArray, afSelectorMap, matchingStyleNodes);

 
    // Write out all of the style selectors for this property string.  If 0 is returned, we've aborted
    int numberSelectorsWritten = _writeMergedSelectors(out, compressStyles, shortStyleClassMap, namespacePrefixArray,
                                                       maxSelectors, mappedSelectors);

    // Now that we have written out the selectors, write out
    // the properties
    if (numberSelectorsWritten > 0)
    {
      // At this point, we could just write out the property string
      // that we already created, but this string contains the properties
      // in sorted order.  We prefer to attempt to preserve the order
      // of the properties as specified in the XSS document.  So,
      // we get the properties from the StyleNode object instead of
      // using the propertyString
      // because of this lameness, we use the order from the original StyleNode
      Iterable<PropertyNode> properties = matchingStyleNodes.get(0).getProperties();
  
      _writeMergedProperties(out, properties, styleSheetName, compressStyles, baseURI);
    }
    
    return numberSelectorsWritten;
  }
  
    
  /**
   * Returns the maximum number of selectors per file that this agent supports
   * @param context
   * @return
   */
  private static int _getMaxSelectorsPerFile(StyleContext context)
  {
    if (TrinidadAgent.Application.IEXPLORER == context.getAgent().getAgentApplication())
    {
      return _MSIE_SELECTOR_LIMIT;
    }
    else
    {
      return Integer.MAX_VALUE;
    }
  }
  
  /**
   * Converts the specified set of StyleNodes to CSS. We output either full styleclass names or
   * compressed styleclass names.
   *
   * @param context The current StyleContext
   * @param styleSheetName The stylesheet name that is registered with the skin.
   *     e.g. skins/purple/purpleSkin.css
   * @param styleNodes The style nodes to convert
   * @param writerFactory The factory to obtain {@link PrintWriter} instances
   * @param compressStyles This tells us whether or not we want to output the short names.
   * @param shortStyleClassMap A Map which maps style
   *   class names to short names.
   * @param namespacePrefixArray an array with the namespace prefixes of our
   *  special selectors. e.g., "af|" or "tr|" .
   * @param afSelectorMap A Map which maps the namespaced component selectors
   *   to their base names (e.g., 'af|menuPath::step' maps to 'af|menuPath A')
   */
  public static void writeCSS(
    StyleContext        context,
    String              styleSheetName,
    List<StyleNode>     styleNodes,
    StyleWriterFactory  writerFactory,
    boolean             compressStyles,
    Map<String, String> shortStyleClassMap,
    String[]            namespacePrefixArray,
    Map<String, String> afSelectorMap)
  {
    // writeCSS() attempts to produce a minimal set of style rules
    // by combining selectors with identical properties into a
    // single rule.  For example, we convert the following two
    // styles:
    //
    //   .OraBGAccentVeryDark,.x8 {background-color:#999966}
    //   .OraTable,.x1x {background-color:#999966}
    //
    // into a single style:
    //
    //   .OraBGAccentVeryDark,.x8,.OraTable,.x1x {background-color:#999966}
    //
    // The intention of this optimization is to improve performance by
    // reducing the overall size of the generated CSS file - and hopefully
    // speed up download time.
    //
    // To implement this optimization, we perform two passes over the
    // set of styles.  During the first pass, we build up a hash map
    // which maps from the property string to an array of StyleNodes
    // which share the properties defined by the property string.
    // During the second pass over the styles, we write out all matching
    // selectors for each style followed by the shared set of properties.

    // This is the first pass where MatchingStyles object is created from
    // the styleNodes. MatchingStyles object segregates the normal selectors
    // rendered as selector {prop: value} and clientRule selectors which are
    // rendered as clientRule { selector { prop: value }}
    // The segregation of clientRules allow us to segregate the rendering logic.
    MatchingStyles matchingStyles = _buildMatchingStylesMap(styleNodes);

    // This is the second pass in which we write out the style rules
    // Get the baseURI up front so we don't have to recalculate it every time we find
    // a property value that contains url() and need to resolve the uri.
    String baseURI = CSSUtils.getBaseSkinStyleSheetURI(styleSheetName);

    // Because of IE's selector limit, we need a slightly more complicated scheme
    // for writing out the styles, so that we can create new CSS files as necessary
    final int maxSelectorsPerFile = _getMaxSelectorsPerFile(context);
    int fileSelectorsWritten = 0;

    // read out the normal selectors, we render these first
    LinkedHashMap<String, List<StyleNode>> noClientRuleMap = matchingStyles.getNonClientRuleMap();
    Iterator<Map.Entry<String, List<StyleNode>>> noClientRuleEntries = noClientRuleMap.entrySet().iterator();

    PrintWriter out = writerFactory.createWriter();

    if (out == null)
    {
      return;
    }

    _beginCssFile(out, compressStyles);

    // loop over all of the entries
    while (noClientRuleEntries.hasNext())
    {

      Map.Entry<String, List<StyleNode>> abortedEntry = null;

      // loop over all of the entries we can fit in a CSS file
      while (abortedEntry == null && noClientRuleEntries.hasNext())
      {
        // get the entry to write out.  This handles retrying any aborted entry
        Map.Entry<String, List<StyleNode>> currEntry;

        // aborted entry is set when the max number of selectors have already
        // written out for the current css file. if aborted entry is set then
        // a new file needs to be created to write remaining selectors
        if (abortedEntry != null)
        {
          currEntry = abortedEntry;
          abortedEntry = null;
        }
        else
        {
          currEntry = noClientRuleEntries.next();
        }

        // write the entry
        int selectorsLeft = maxSelectorsPerFile - fileSelectorsWritten;
        int entrySelectorsWritten = _writeMergedEntry(styleSheetName, baseURI, compressStyles, shortStyleClassMap,
                                                      namespacePrefixArray, afSelectorMap, out, selectorsLeft,
                                                      currEntry);
        // detect abort
        if (entrySelectorsWritten == 0)
        {
          // no selectors written, so we have reached the threshold for this css file
          // set the aborted entry so that we will create a new css file
          abortedEntry = currEntry;
        }
        else
        {
          // add up the selectors written to track the threshold for the css file
          fileSelectorsWritten += entrySelectorsWritten;
        }
      }

      if (abortedEntry != null)
      {
        // abort detected, close the current out and create a new one
        _endCssFile(out, styleSheetName, compressStyles, fileSelectorsWritten);
        fileSelectorsWritten = 0;

        // create new out
        out = writerFactory.createWriter();

        if (out == null)
        {
          return;
        }

        // put in the headers
        _beginCssFile(out, compressStyles);
      }
    }

    Set<String> clientRules = matchingStyles.getClientRules();

    // while writing a client rule, all selectors within the clien rule has to fit into the current css file.
    // If that cannot happen, then we need to create a new css file and continue writing.
    for (String clientRule : clientRules)
    {
      // write out all client rule entries into a temp out, so that we will know if it can be fitted in
      // the current css file
      StringWriter tempStringWriter = new StringWriter();
      PrintWriter tempWriter = new PrintWriter(tempStringWriter);
      
      // we cannot count the selectors within the client rule to decide the number of selectors
      // there are cases where both compressed and non-compressed selectors get written
      // so we create a temp StringWriter and get the selectors and properties written into that
      // we check if all selectors enclosed in a particular client rule can be incorporated in the current out
      // otherwise we create a new out and write out the client rule followed by the selectors within
      int clientRuleSelectorsWritten = _writeClientRuleStyles(tempWriter, clientRule, matchingStyles, maxSelectorsPerFile,
                                                              fileSelectorsWritten, styleSheetName, baseURI, compressStyles,
                                                              shortStyleClassMap,namespacePrefixArray, afSelectorMap);

      // if no selectors were written, then we need a new file to contain the contents of the current clientRule
      if (clientRuleSelectorsWritten == 0)
      {
        // all selectors inside client rule cannot be accommodated in current file
        _endCssFile(out, styleSheetName, compressStyles, fileSelectorsWritten);
        fileSelectorsWritten = 0;

        // create new out
        out = writerFactory.createWriter();

        if (out == null)
        {
          return;
        }

        // write headers
        _beginCssFile(out, compressStyles);

        // close the tempWriter and create a new one
        // some selectors inside the client rule may have got written to tempWriter
        tempWriter.close();
        tempStringWriter = new StringWriter();
        tempWriter = new PrintWriter(tempStringWriter);

        // now that we recreated the writer, so it should have space to write all the selectors in the client rule
        clientRuleSelectorsWritten = _writeClientRuleStyles(tempWriter, clientRule, matchingStyles, 
                                                            maxSelectorsPerFile, fileSelectorsWritten, styleSheetName, 
                                                            baseURI, compressStyles, shortStyleClassMap,
                                                            namespacePrefixArray, afSelectorMap);
      }

      // all selectors inside the current client rule should now be written to tempWriter
      assert (clientRuleSelectorsWritten != 0);

      if (tempWriter.checkError())
      {
        // check for errors in tempWriter and log it and return
        _LOG.severe("Error writing stylesheet:" + styleSheetName);
        return;
      }

      // the client rule can now be written to the actual out
      out.print(clientRule);
      _writeString(out, " {", compressStyles);

      // write the selectors and properties to the actual out
      out.print(tempStringWriter.toString());

      // closing braces for client rule
      _writeString(out, "}", compressStyles);

      fileSelectorsWritten += clientRuleSelectorsWritten;

      tempWriter.close();
    }

    _endCssFile(out, styleSheetName, compressStyles, fileSelectorsWritten);
  }

  /**
   * tests if all the selectors under the client rule can fit into the current css file.
   * writes the selectors and properties into the PrintWriter passed.
   * 
   * @param out
   * @param clientRule
   * @param matchingStyles
   * @param maxSelectorsPerFile
   * @param fileSelectorsWritten
   * @param styleSheetName
   * @param baseURI
   * @param compressStyles
   * @param shortStyleClassMap
   * @param namespacePrefixArray
   * @param afSelectorMap
   * @return number of selectors written into the css file. Returns 0, if all selectors does not fit
   *
   */
  private static int _writeClientRuleStyles(PrintWriter out, 
                                            String clientRule, 
                                            MatchingStyles matchingStyles, 
                                            int maxSelectorsPerFile,
                                            int fileSelectorsWritten, 
                                            String styleSheetName, 
                                            String baseURI,
                                            boolean compressStyles, 
                                            Map<String, String> shortStyleClassMap,
                                            String[] namespacePrefixArray,
                                            Map<String, String> afSelectorMap)
  {
    int selectorsWritten = 0;
    int entrySelectorsWritten = 0;
    
    // we know this cannot be null because the caller is using the client rule keySet from matchingStyles
    LinkedHashMap<String, List<StyleNode>> clientRuleMap = matchingStyles.getClientRuleMap(clientRule);
    Iterator<Map.Entry<String, List<StyleNode>>> clientRuleEntries = clientRuleMap.entrySet().iterator();
    
    while (clientRuleEntries.hasNext())
    {
      Map.Entry<String, List<StyleNode>> currEntry = clientRuleEntries.next();
      int selectorsLeft = maxSelectorsPerFile - (fileSelectorsWritten + selectorsWritten);
      entrySelectorsWritten = _writeMergedEntry(styleSheetName, baseURI, compressStyles, shortStyleClassMap,
                                                namespacePrefixArray, afSelectorMap, out, selectorsLeft,
                                                currEntry);
      
      // we want to write all selectors in clientRuleEntries into the same css file
      // so, if we cannot write any one of these selector we need to abort
      if (entrySelectorsWritten == 0)
      {
        return 0;
      }
      else
      {
        selectorsWritten += entrySelectorsWritten;
      }
    }

    return selectorsWritten;
  }

  /**
   * outputs header for a css file
   * @param out
   * @param compressStyles
   */
  private static void _beginCssFile(PrintWriter out, boolean compressStyles)
  {
    if (!compressStyles)
    {
      Date date = new Date();

      // write out the header with a time stamp
      out.println("/* This CSS file generated on " + date + " */");
    }
  }

  /**
   * outputs the footer for a css file
   * @param out
   * @param styleSheetName
   * @param compressStyles
   * @param fileSelectorsWritten
   */
  private static void _endCssFile(PrintWriter out, String styleSheetName, boolean compressStyles, int fileSelectorsWritten)
  {
    if (!compressStyles)
    {
      out.print("/* The number of CSS selectors in this file is ");
      out.print(fileSelectorsWritten);
      out.println(" */");
    }

    if (out.checkError())
    {
      _LOG.severe("Error writing stylesheet:" + styleSheetName);
    }

    out.close();
  }

  /**
   * Util method to output a outStr based on compressStyles
   * @param out
   * @param outStr
   * @param compressStyles
   */
  private static void _writeString(PrintWriter out, String outStr, boolean compressStyles)
  {
    if (compressStyles)
      out.print(outStr); // take out the newlines for performance
    else
      out.println(outStr);
  }

  /**
   * Shorten (compress) the selector.
   * @param shortStyleClassMap
   * @param namespacePrefixArray
   * @param selector
   * @return the shortened selector, or selector if nothing could be shortened.
   */
  public static String getShortSelector(
    Map<String, String> shortStyleClassMap,
    String[]            namespacePrefixArray,
    String              selector)
  {
    // shorten all the css-2 style class selectors (those that start with
    // '.' and don't have a namespace prefix in it)
    // and return the shortened string.
    // e.g., selector of '.OraBulletedList A' is shortened to '.xj A'
    // e.g., selector of af|inputText::content is not shortened since
    // it has no css-2 style class selector piece that starts with '.'.
    // e.g., selector of af|foo.Bar will shorten the '.Bar' piece
    // af|foo.xz
    // e.g., .Foo:hover -> .x0:hover
    String shortSelector = _getShortNonNamespacedSelector(selector,
                                             shortStyleClassMap);

    if (shortSelector == null)
      shortSelector = selector;

    // run it through a shortener one more time to shorten any
    // of the af component selectors.
    // e.g., 'af|menuPath' is shortened to '.x11'

    if (_hasNamespacePrefix(shortSelector, namespacePrefixArray))
    {
      String[] shortSelectorArray  = StyleUtils.splitStringByWhitespace(shortSelector);

      shortSelector = _getMappedNSSelector(shortStyleClassMap,
                                           namespacePrefixArray,
                                           shortSelectorArray,
                                           true);
    }
    return shortSelector;
  }

  /**
   * Tests whether the specified selector is a single style class
   * selector. A single style class selector is something like
   * ".AFInstructionText". Examples that are not single style class
   * selectors are "af|inputText" or ".AFFoo .AFBar" or ".foo:hover"
   */
  public static boolean isSingleStyleClassSelector(String selector)
  {
    if ((selector == null)      ||
        (selector.length() < 2) ||
        (selector.charAt(0) != '.'))
    {
      return false;
    }

    for (int i = 1; i < selector.length(); i++)
    {
      if (_isStyleClassTerminator(selector.charAt(i)))
        return false;
    }

    return true;
  }

  /**
   * Returns an
   * Iterator of all of the style class selectors included in the
   * specified selector.  For example, ".OraNav1Enabled" returns
   * a single element Iterator with the string "OraNav1Enabled".
   * "P.OraNav1Enabled SPAN.text" returns a two element Iterator
   * with "OraNav1Enabled" and "text".
   * .OraLink:visited returns "OraLink"
   * .star.moon returns "star" and "moon"
   */
  public static Iterator<String> getStyleClasses(String selector)
  {
    ArrayList<String> styleClasses = null;
    int styleClassStartIndex = -1;
    int length = selector.length();

    for (int i = 0; i < length; i++)
    {
      char c = selector.charAt(i);

      // If we aren't inside of a style class yet,
      // check to see if we are about to enter one.
      // Otherwise, check to see if we are at the
      // end of one.
      if (styleClassStartIndex == -1)
      {
        if (c == '.')
          styleClassStartIndex = i;
      }
      else
      {
        boolean end = _isStyleClassTerminator(c);
        if (!end)
        {
          // Check to see if we are at the end of the string
          if (i == (length - 1))
          {
            // We need to increment our counter to make sure
            // we include this last character when we pull out
            // the substring
            i++;
            end = true;
          }
        }

        // If we're at the end of the style class, add it to the list
        if (end)
        {
          String styleClass = selector.substring(styleClassStartIndex + 1, i);
          if (styleClasses == null)
            styleClasses = new ArrayList<String>(3);

          styleClasses.add(styleClass);
          // if c is ., then this means we've found another styleclass
          if (c == '.')
            styleClassStartIndex = i;
          else
            styleClassStartIndex = -1;
        }
      }
    }

    if (styleClasses == null)
      return null;

    return styleClasses.iterator();
  }

  /**
   * Called when creating the shortened styleclass map.
   * Returns an Iterator of all of the component selectors that begin with the
   * given namespace. All styleclasses and pseudo-classes are ignored and NOT
   * returned.
   * For example, ".OraNav1Enabled af|menuPath" returns
   * a single element Iterator with the string "af|menuPath".
   * "af|menuPath.OraNav1Enabled af|treeTable.text" returns a two
   * element Iterator with "af|menuPath" and "af|treeTable".
   * af|inputText:disabled af|inputText::content returns two
   * "af|inputText" and "af|inputText::content".
   * It also looks at the afSelectorMap to map any special selectors if
   * necessary.
   * e.g., "af|menuPath::step" maps to "af|menuPath A", so
   * ".OraNav1Enabled af|menuPath::step" returns "af|menuPath"
   */
  public static Iterator<String> getNamespacedSelectors(
    String              selector,
    String              namespace,
    Map<String, String> afSelectorMap)
  {
    if (selector == null)
      return null;
    int afIndex = selector.indexOf(namespace);

    // no namespace in the selector, so just return null
    if (afIndex == -1)
      return null;

    ArrayList<String> afUnmappedSelectorList = new ArrayList<String>();

    // now find each af| component selector and map
    // e.g., af|menuPath::step maps to af|menuPath A

    // order the pseudo elements and classes
    String base = selector.substring(afIndex);
    String[] afSelectors =
        _orderPseudoElementsAndClasses(base);

    // loop through each of the af| parts.
    for (int i=0; i < afSelectors.length; i++)
    {
      if (afSelectors[i].startsWith(namespace))
      {
        // get the component selector, which is just the main part, nothing
        // that includes a '.' ':', or a ' '.
        String afComponentSelector =
          _getNSComponentSelector(afSelectors[i], false);
        afUnmappedSelectorList.add(afComponentSelector);
      }
    }

    // ok, now everything in afSelectorList at this point is the unmapped
    // af|xxx component selectors. Now map them, and return the base key
    // in the selector.
    // loop through again and map them
    ArrayList<String> afSelectorList = new ArrayList<String>();

    int size = afUnmappedSelectorList.size();
    for (int i=0; i < size; i++)
    {
      String afComponentSelector = afUnmappedSelectorList.get(i);
      String mappedSelector = null;

      if (afSelectorMap != null)
      {
        mappedSelector = afSelectorMap.get(afComponentSelector);

      }
      if (mappedSelector != null)
      {
        // In case the mapped selector does not start with the namespace,
        // start the string where the namespace starts.
        int namespaceIndex = mappedSelector.indexOf(namespace);
        if (namespaceIndex > -1)
        {
          // get the selector up until the space.
          String[] baseSelector = StyleUtils.splitStringByWhitespace(mappedSelector.substring(namespaceIndex));
          afComponentSelector = baseSelector[0];
          afSelectorList.add(afComponentSelector);
        }
      }
      else
      {
        afSelectorList.add(afComponentSelector);
      }

    }
    return afSelectorList.iterator();

  }

  /**
   * Add to the namespacePrefixes Set any namespace prefixes found in this selector.
   * @param namespacePrefixes
   * @param selector
   */
  public static void getNamespacePrefixes(
    Set<String> namespacePrefixes,
    String selector)
  {

    int length = selector.length();
    int startSubstringIndex = 0;
    // Loop through each character of the selector looking for namespace prefixes.
    for (int i = 0; i < length; i++)
    {
      char c = selector.charAt(i);
      if (c == '|')
      {
        String prefix = selector.substring(startSubstringIndex, i+1);
        startSubstringIndex = i+1;
        // protect against just | in the prefix by checking length.
        if (prefix.length() > 1)
          namespacePrefixes.add(prefix);
      }
      else if(!_isStyleClassTerminator(c))
      {
        // keep going if it isn't a terminating character
      }
      else
      {
        // update the startSubstring index.
        startSubstringIndex = i+1;
      }
    }
    return;
  }

  /**
   * Called from getNamespacedSelectors.
   * Given a single selector that begins with a namespace prefix (most likely
   * af|), return the main portion -- the prefix+component+pseudo-element.
   * Styleclasses and pseudo-classes and anything after a space will be ignored,
   * and won't be returned.
   * @param singleAfSelector
   * @param allowPseudoClass
   * @return
   */
  private static String _getNSComponentSelector(
    String singleAfSelector,
    boolean allowPseudoClass)
  {
    int colonIndex = singleAfSelector.indexOf("::");

    // get the part after the ::.
    // colonIndex will be 0 if there is no ::.
    if (colonIndex != -1)
    {
      colonIndex += 2;
    }
    else
      colonIndex = 0;
    String afterDoubleColon = singleAfSelector.substring(colonIndex);

    // now find the part with a single ':', a ' ', or a '.'. That is where
    // I want to chop it to get my component selector.

    boolean end = false;
    int afterLength = afterDoubleColon.length();
    int endIndex=0;

    for (; ((endIndex < afterLength) && !end); endIndex++)
    {
      char c = afterDoubleColon.charAt(endIndex);
      if (allowPseudoClass)
        end = Character.isWhitespace(c);
      else
        end = _isStyleClassTerminator(c);
    }

    String afComponentSelector = null;
    if (end)
    {
      afComponentSelector =
        singleAfSelector.substring(0, colonIndex + endIndex-1);
    }
    else
    {
      // there was no end, so just take the entire string.
      afComponentSelector = singleAfSelector;
    }

    return afComponentSelector;
  }

  // Converts all style class selectors that begin with '.'
  // to the short version if
  // there is a short version. does not shorten styles that start with the
  // namespace
  // returns null if it can't shorten the selector
  private static String _getShortNonNamespacedSelector(
    String              selector,
    Map<String, String> shortStyleClassMap)
  {
    if (shortStyleClassMap == null)
      return null;

    // This will see if the selector is a single styleClass, and if so,
    // it will run it through the shortStyleClassMap and return the shortened
    // style class. A single style class selector is something like
    // ".AFInstructionText". Examples that are not single style class
    // selectors are "af|inputText" or ".AFFoo .AFBar" or ".foo:hover"
    if (isSingleStyleClassSelector(selector))
    {
      String shortStyleClass = shortStyleClassMap.get(selector.substring(1));

      return (shortStyleClass == null) ? null : "." + shortStyleClass;
    }

    // This will run if we do not have a one class selector definition
    // but a af|XXX class or a command class like .AFXYZ .AFzzz or
    // ".foo:hover" It shortens all the .xxx pieces.
    boolean isShorter = false;
    int length = selector.length();
    StringBuffer buffer = new StringBuffer(length);
    int styleClassStartIndex = -1;

    // loop through each character. If we don't come across a '.' character,
    // return the selector unchanged.
    for (int i = 0; i < length; i++)
    {
      char c = selector.charAt(i);

      if (styleClassStartIndex == -1)
      {
        if (c == '.')
          styleClassStartIndex = i;

        buffer.append(c);
      }
      else
      {
        // We are currently inside of a style class substring.
        // Check to see if the style class has been terminated.
        boolean end = _isStyleClassTerminator(c);
        if (!end)
        {
          // Check to see if we are at the end of the string
          if (i == (length - 1))
          {
            // We need to increment our counter to make sure
            // we include this last character when we pull out
            // the substring
            i++;
            end = true;
          }
        }

        if (end)
        {

          // If we are at the end of the style class, check to
          // see if we've got a shorter version
          String styleClass = selector.substring(styleClassStartIndex + 1, i);
          String shortStyleClass = null;
          // don't shorten the styles that contain a namespace, because we
          // do this in another step.
          if (styleClass.indexOf('|') == -1)
            shortStyleClass = shortStyleClassMap.get(styleClass);


          if (shortStyleClass == null)
          {
            buffer.append(styleClass);
          }
          else
          {
            buffer.append(shortStyleClass);
            isShorter = true;
          }

          // Don't forget the terminator character
          if (i < (length - 1))
            buffer.append(c);
          // if c is ., then this means we've found another styleclass
          if (c == '.')
            styleClassStartIndex = i;
          else
            styleClassStartIndex = -1;
        }
      }
    }

    // return the original selector if this isn't shorter.
    return isShorter ? buffer.toString() : selector;
  }
  
  /**
   * Runs a selector through a map. It returns the selector unchanged (except for converted
   * pseudo-classes) if there is no namespace in the selector.
   * This could be a map to convert the
   * public no-html selector to an internal selector that has html-specifics
   * (e.g., 'af|menuPath::step:hover' -> 'af|menuPath A:hover')
   * or it could be a map to shorten the selector
   * (e.g., 'af|menuPath A:hover' -> '.x11 A:hover')
   * We call this method first with the public->internal map, and then
   * to shorten it.
   * Only the pieces of the selector that start with the namespace are mapped.
   * @param afSelectorMap if shortenPass is true, then this map shortens the
   *                 af| selector. else, it maps the public af| selector
   *                 to the internal selector (a selector that is closer to what is written to the
*                    CSS file. 
*                    e.g., af|inputText:error::content becomes 
*                    af|inputText.p_AFError af|inputText::content
   * @param namespacePrefixArray   most likely, "af|". The selectors with this namespace
   *                               are the ones we map.
   * @param selector    selector to map.
   */
  public static String getMappedSelector (
    Map<String, String> afSelectorMap,
    String[]            namespacePrefixArray,
    String              selector)
  {
    String mappedSelector;
    if (_hasNamespacePrefix(selector, namespacePrefixArray))
    {
      String[] selectorArray =
        _orderPseudoElementsAndClasses(selector);

      // map selectors, if needed
      // any of the selectors that start with a namespace prefix will
      // be mapped.
      // e.g., "af|menuPath::step" maps to "af|menuPath A"
      // if the selector does not need to be 'mapped', it will be returned
      // untouched. e.g., .AFInstructionText maps to .AFInstructionText
      mappedSelector = _getMappedNSSelector(afSelectorMap,
                                            namespacePrefixArray,
                                            selectorArray,
                                            false);
    }
    else
    {
      // there are no namespaces in this selector, but we still need to convert pseudo-classes.
      //Separate pseudoclasses and then call it
      int start = 0;
      StringBuilder b = new StringBuilder();
      for(int i = 0; i < selector.length(); i++)
      {
        char c = selector.charAt(i);
        if (c == ' ' )
        {
          if (start == i)
          {
            //group of spaces
            start = i+1; //Skip space
          }
          else
          {
            String subSelector = selector.substring(start,i);
            subSelector = _convertPseudoClassesInSelector(subSelector, selector);
            start = i+1; //Skip space
            b.append(subSelector);
            b.append(' ');
          }
        }
      } // end for loop

      // We reached the end of the selector, now convert the last bit
      if (start == 0)
      {
        //there is no space in selector, but we still need to map pseudo-classes.
        mappedSelector = _convertPseudoClassesInSelector(selector, selector);
      }
      else
      {
        String subSelector = selector.substring(start);
        subSelector = _convertPseudoClassesInSelector(subSelector, selector);
        b.append(subSelector);
        mappedSelector = b.toString();
      }

    }
    return mappedSelector;
  }

  /**
   * Runs a namespaced selector through a map.
   * This could be a map to convert the
   * public no-html selector to an internal selector that has html-specifics
   * (e.g., 'af|menuPath::step:hover' -> 'af|menuPath A:hover')
   * or it could be a map to shorten the selector
   * (e.g., 'af|menuPath A:hover' -> '.x11 A:hover')
   * We call this method first with the public->internal map, and then
   * to shorten it.
   * Only the pieces of the selector that start with the namespace are mapped.
   * @param map         if shortenPass is true, then this map shortens the
   *                    af| selector. else, it maps the public af| selector
   *                    to the internal selector.
   * @param selectorArray selectorArray is the selector split into pieces based on the ' '
   * @param shorten     if true, then we'll add the "." to the mapped selector.
   * @return            the selector, mapped.
   */
  private static String _getMappedNSSelector (
    Map<String, String> map,
    String[]            nsPrefixArray,
    String[]            selectorArray,
    boolean             shorten)
  {
    // selectorArray is the selector broken into pieces.
    // Map each piece, then put back together.

    for (int i=0; i < selectorArray.length; i++)
    {
      // Assumption is the selector 'piece' only contains one namespace
      // prefix. af|foo.tr|bar is not a valid selector, so this assumption
      // is fine.
      int nsIndex = -1;
      int numNsPrefixes = nsPrefixArray.length;
      for (int j=0; (j <  numNsPrefixes) && (nsIndex == -1); j++)
      {
       nsIndex = selectorArray[i].indexOf(nsPrefixArray[j]);
      }

      if (nsIndex > -1)
      {
        selectorArray[i] = _getEachMappedSelector(map,
                                                  nsIndex,
                                                  selectorArray[i],
                                                  shorten);
      }
    }

    // build back the selectorArray into a string
    return StyleUtils.arrayToStringWithSpaces(selectorArray);
  }

  private static String _getEachMappedSelector(
    Map<String, String> map,
    int                 indexOfNSPrefix,
    String              selector,
    boolean             shorten)
  {
    // break apart the selector into 2 parts:
    // main (af|foo::pseudo-element)
    // end (anything after, ':' or including and after '.')
    // map the main and the ending (pseudo-classes could be mapped to private
    // styleclasses, :disabled -> .p_AFDisabled
    // piece  back together.
    if (indexOfNSPrefix == -1)
     return selector;
    if (map == null)
      return selector;


    String wholeAfSelector = selector.substring(indexOfNSPrefix);

    // get main piece
    int firstDoubleColonIndex = wholeAfSelector.indexOf("::");

    // get the part after the ::.
    // colonIndex will be 0 if there is no ::.
    if (firstDoubleColonIndex != -1)
    {
      int lastDoubleColonIndex = wholeAfSelector.lastIndexOf("::");
      
      // We exceptionally support multiple occurence of double colon for the browser builtin pseudo elements
      if (lastDoubleColonIndex != firstDoubleColonIndex && 
          ! _BUILT_IN_PSEUDO_ELEMENTS.contains(wholeAfSelector.substring(lastDoubleColonIndex)))
      {
        _LOG.warning("UNSUPPORTED_CONSECUTIVE_SUB_ELEMENT_SYNTAX", selector);
      }

      firstDoubleColonIndex += 2;
    }
    else
      firstDoubleColonIndex = 0;
    String afterDoubleColon = wholeAfSelector.substring(firstDoubleColonIndex);

    // now find the part with a single ':', a ' ', a '[', or a '.'. That is where
    // I want to chop it to get my 'main' piece of the component selector.
    boolean end = false;
    int afterLength = afterDoubleColon.length();
    int endIndex=0;
    char c;
    for (; ((endIndex < afterLength) && !end); endIndex++)
    {
      c = afterDoubleColon.charAt(endIndex);
      end = (Character.isWhitespace(c)) || (c == '.') || (c == ':') || (c == '[');
    }

    // Set the main piece in the pieces object
    String mainSelector;
    if (endIndex == afterLength)
    {
      mainSelector = wholeAfSelector.substring(0, firstDoubleColonIndex + endIndex);
    }
    else
    {
      mainSelector = wholeAfSelector.substring(0, firstDoubleColonIndex + endIndex-1);
    }
    String afterMain = null;
    if (endIndex != afterLength)
    {
      // If I got here, that means there are characters after the 'main' part
      // of the selector.
      afterMain = wholeAfSelector.substring(firstDoubleColonIndex + endIndex-1);
      
      // map the afterMain part. It includes styleclasses and pseudo-classes.
      // we don't need to convert the pseudo-classes if we are shortening.
      // Also very likely we are dealing with inbuilt pseudo elements that 
      //  should not be converted, do not process them as pseudo classes.
      if (!shorten && ! _BUILT_IN_PSEUDO_ELEMENTS.contains(afterMain))
        afterMain = _convertPseudoClassesInSelector(afterMain, wholeAfSelector);
    }

    // piece back together the string
    StringBuffer buffer = new StringBuffer();

    // beginning. add '.' if needed.
    if (indexOfNSPrefix > 0)
    {
      buffer.append(selector.substring(0, indexOfNSPrefix));
      if (shorten && selector.charAt(indexOfNSPrefix-1) != '.')
        buffer.append('.');
    }
    else if (shorten)
      buffer.append('.');

    // map the mainSelector, and append the afterMain part.
    buffer.append(_runThroughMap(map, mainSelector));
    if (afterMain != null)
      buffer.append(afterMain);


    return buffer.toString();
  }

  private static boolean _hasNamespacePrefix(
    String   selector,
    String[] nsPrefixArray)
  {
    if (selector == null) return false;
    boolean hasNamespacePrefix = false;
    int numNamespaces = nsPrefixArray.length;
    for (int i=0; (i <  numNamespaces )&& !hasNamespacePrefix; i++)
      if (selector.indexOf(nsPrefixArray[i]) > -1)
        hasNamespacePrefix = true;
    return hasNamespacePrefix;
  }

  /*
   * if I see this: af|foo:class:class::element, return this
   * af|foo:class:class and af|foo::element in a String array.
   * if I see this: af|foo:p-class.StyleClass::element, return this:
   * 'af|foo:p-class.StyleClass' 'af|foo::element'
   * af|foo.StyleClass::element -> 'af|foo.StyleClass' 'af|foo::element'
   * If I see thiss: af|foo::p-element.StyleClass, return this
   * af|foo::p-element.StyleClass (leave alone).
   */
  private static String[]  _orderPseudoElementsAndClasses(
    String selector)
  {
    String[] input = StyleUtils.splitStringByWhitespace(selector);

    List<String> output = new ArrayList<String>();
    for (int i=0; i < input.length; i++)
    {

      int indexOfDoubleColon = input[i].indexOf("::");
      if (indexOfDoubleColon == -1)
      {
        // no double colon (aka pseudo-element)
        output.add(input[i]);
      }
      else
      {
        // you have a double colon index. Now look to see if we need to
        // reorder. We have to reorder if the pseudo-element comes after
        // the pseudo-class or composite style classes.
        int indexOfFirstColon = input[i].indexOf(':');
        int indexOfDot = input[i].indexOf('.');

        boolean pseudoClassBeforePseudoElement =
          (indexOfFirstColon < indexOfDoubleColon);
        boolean styleClassBeforePseudoElement =
          (indexOfDot != -1 && indexOfDot < indexOfDoubleColon);

        if (!(pseudoClassBeforePseudoElement ||
              styleClassBeforePseudoElement))
        {
          output.add(input[i]);
        }
        else
        {
          if (indexOfFirstColon == indexOfDoubleColon)
            indexOfFirstColon = -1;
          int indexOfClass = Math.min(indexOfFirstColon, indexOfDot);
          if (indexOfClass == -1)
            indexOfClass = Math.max(indexOfFirstColon, indexOfDot);

          // we have the condition where pseudo-class or styleClass is before
          // pseudo-element: af|foo:psdo-class::psdo-element
          // e.g.,
          // af|inputText:disabled::content
          // indexOfColon = 12, indexOfDoubleColon = 21
          // main = 'af|inputText'
          // mainPlusClasses = 'af|inputText:disabled'
          // end '::content'
          String main = input[i].substring(0, indexOfClass);
          String mainPlusClasses = input[i].substring(0, indexOfDoubleColon);
          String end = input[i].substring(indexOfDoubleColon);
          output.add(mainPlusClasses);
          output.add(main + end);
        }
      }

    }
    return output.toArray(new String[output.size()]);
  }



  /**
   * get rid of the | and :: that browsers don't like, and add the
   * '.' where needed to make the af| component selector
   * into a style class selector that is valid to be written to the css file.
   * @param selector
   * @return
   */
  public static String getValidFullNameSelector(
    String selector,
    String[] namespacePrefixArray)
  {
    if (selector.indexOf('|') == -1)
      return selector;

    // split on spaces.
    String[] spacerArray = StyleUtils.splitStringByWhitespace(selector);

    for (int i=0; i < spacerArray.length; i++)
    {
      // if this starts with any of our namespaces, then add a '.'
      if (spacerArray[i].indexOf('|') > -1)
      {
        for (int j=0; j < namespacePrefixArray.length; j++)
        {
          String nsPrefix = namespacePrefixArray[j];
          if (spacerArray[i].startsWith(nsPrefix))
          {
            spacerArray[i] = ".".concat(spacerArray[i]);
            break;
          }
        }
      }
    }
    return StyleUtils.convertToValidSelector(
      StyleUtils.arrayToStringWithSpaces(spacerArray));

  }


  // Tests whether the specified character or pseudo-class terminates a
  // style class selector
  private static boolean _isStyleClassTerminator(char c)
  {
    return (Character.isWhitespace(c) || (c == ':') || (c == '.') || (c == '['));
  }

  private static int _computeBuilderSize(PropertyNode[] properties)
  {
    // Compute the StringBuilder size
    int builderSize = 0;
    
    for (int i = 0; i < properties.length; i++)
    {
      PropertyNode property = properties[i];
      String name = property.getName();
      String value = property.getValue();

      if ((name != null) && (value != null))
      {
        builderSize += property.getName().length();
        builderSize += property.getValue().length();

        // Leave room for a separator
        builderSize++;
      }
    }
    
    return builderSize;
  }

  private static String _getSortedPropertyString(PropertyNode[] sortedProperties)
  {
    int builderSize = _computeBuilderSize(sortedProperties);
    
    StringBuilder builder = new StringBuilder(builderSize);
    boolean first = true;

    for (int i = 0; i < sortedProperties.length; i++)
    {
      PropertyNode property = sortedProperties[i];
      String       name     = property.getName();
      String       value    = property.getValue();

      if ((name != null) && (value != null))
      {
        if (!first)
          builder.append(';');
        else
          first = false;

        builder.append(name);
        builder.append(':');
        builder.append(value);
      }
    }

    return builder.toString();
  }
  
  /**
   * Returns the properties of the specified StyleNode in property name sorted order as a String.
   * If the Stylenode contains no properties, the empty String is returned
   */
  private static String _getSortedPropertyString(StyleNode styleNode)
  {
    Collection<PropertyNode> nodeProperties = styleNode.getProperties();
    int propertyCount = nodeProperties.size();
      
    if (propertyCount == 0)
      return "";
    
    // =-= bts the extra step of copying the PropertyNodes into the array is kind of lame.  I'm wondering
    // if it would be better for the StyleNodes to sort the properties for us
    List<PropertyNode> properties       = new ArrayList<PropertyNode>(nodeProperties);
    PropertyNode[]     sortedProperties = properties.toArray(new PropertyNode[propertyCount]);

    // Sort the properties so that the order of the properties won't
    // come into play when comparing property strings.
    Arrays.sort(sortedProperties, PropertyNodeNameComparator.sharedInstance());

    return _getSortedPropertyString(sortedProperties);
  }

  /**
   * Given a selector, convert the pseudo-classes. Non css-2 pseudo-classes
   * get converted to styleclasses so they don't get passed through to the generated css.
   *
   * @param selector String input selector. The assumption is there are no spaces.
   * If the original selector has spaces, then break the selector up into the pieces before
   * calling this method.
   * @param completeSelector to figure out if the selector is AF namespaced
   * @return String the selector with the pseudo-classes converted, if needed.
   * e.g., .StyleClass:error:active -> .StyleClass.p_AFError:active
   */
  private static String _convertPseudoClassesInSelector(String selector, String completeSelector)
  {
    if (selector == null || completeSelector ==null) return selector;

    boolean afNamespacedSelector = _isAFNamespacedSelector(completeSelector);

    StringBuffer completeBuffer = new StringBuffer();
    StringBuffer pseudoClassBuffer = new StringBuffer();
    boolean inPseudoClass = false;

    for (int i=0; i < selector.length(); i++)
    {
      char x = selector.charAt(i);

      if ((x == ':') || (x == '.') || (x == '['))
      {
        if (inPseudoClass)
        {
          // if we are in a pseudo-class already, and we get a ':' or '.' that means
          // this pseudo-class is complete. Get ready for another one.
          String convertedPseudoClass = _convertPseudoClass(completeSelector, pseudoClassBuffer.toString(), afNamespacedSelector);
          completeBuffer.append(convertedPseudoClass);
          pseudoClassBuffer = new StringBuffer();
          inPseudoClass = false;
        }
        if (x == ':')
        {
          inPseudoClass = true;
          pseudoClassBuffer.append(x);
        }
        else if (x == '.' || x == '[')
        {
          completeBuffer.append(x);
        }

      }
      else
      {
        if (!inPseudoClass)
          completeBuffer.append(x);
        else
          pseudoClassBuffer.append(x);
      }

    }
    if (inPseudoClass)
    {
      String mappedPseudoClass = _convertPseudoClass(completeSelector, pseudoClassBuffer.toString(), afNamespacedSelector);
      completeBuffer.append(mappedPseudoClass);

    }
    return completeBuffer.toString();
  }

  /**
   * Returns true if the passed selector is a 'af' selector.
   *
   * @param completeSelector
   * @return
   */
  private static boolean _isAFNamespacedSelector(String completeSelector)
  {

    Set<String> namespaces = new HashSet<String>();
    getNamespacePrefixes(namespaces, completeSelector);

    if (namespaces.isEmpty() && completeSelector.startsWith(_DEFAULT_AF_SELECTOR))
      return true;
    if (namespaces.contains(_DEFAULT_NAMESPACE))
      return true;

    return false;
  }

  // run through the map. If it's not in the map, return the selector unchanged.
  private static String _runThroughMap(Map<String, String> map, String selector)
  {
    String mappedSelector = map.get(selector);
    return (mappedSelector != null) ? mappedSelector : selector;
  }


  // Comparator that sorts PropertyNodes by name
  private static class PropertyNodeNameComparator implements Comparator<PropertyNode>
  {
    public static Comparator<PropertyNode> sharedInstance()
    {
      return _INSTANCE;
    }

    @Override
    public int compare(PropertyNode o1, PropertyNode o2)
    {
      String name1 = (o1 == null) ? null : o1.getName();
      String name2 = (o2 == null) ? null : o2.getName();

      if ((name1 == null) || (name2 == null) )
      {
        if (name1 == name2)
          return 0;
        else if (name1 != null)
          return 1;

        return -1;
      }

      return name1.compareTo(name2);
    }
    
    @Override
    public boolean equals(Object o)
    {
      // we only have our singleton instance
      return (this == o);
    }

    private PropertyNodeNameComparator() {}

    private static final Comparator<PropertyNode> _INSTANCE = new PropertyNodeNameComparator();
  }

  /**
   * Converts the pseudo class into a "p_AF" prefixed styleclass.
   * This is because certain pseudo-classes are internal and are not recognized by any browsers.
   * The renderers are responsible for rendering out the same styleclass format.
   * Some examples are: 
   * :screen-reader becomes .p_AFScreenReader
   * :disabled becomes .p_AFDisabled
   *
   * Exception is CSS built in pseudo classes, which are recognized by browsers. These are rendered as is.
   *
   * @param pseudoClass
   * @param afNamespacedSelector true if the pseudoClass is part of AF namespaced selector
   * @return
   */
  static private String _convertPseudoClass(String completeSelector, String pseudoClass, boolean afNamespacedSelector)
  {
    // The design time needs the browser-supported pseudo-classes to be converted so they
    // can show a preview of the skinned component.
    String builtInPseudoClass = pseudoClass;
    int parenthesesIndex = pseudoClass.indexOf("(");

    // if there are no open parentheses check for closing parentheses
    if (parenthesesIndex == -1)
      parenthesesIndex = pseudoClass.indexOf(")");

    // if at least one parentheses exists strip the pseudoClass off it
    if (parenthesesIndex != -1)
      builtInPseudoClass = pseudoClass.substring(0, parenthesesIndex);

    if (_BUILT_IN_PSEUDO_CLASSES.contains(builtInPseudoClass) && !Beans.isDesignTime())
      return pseudoClass;

    // skip the pseudo selectors in _AT_PAGE_PSEUDO_CLASSES only for @page client rule
    if (completeSelector.contains(_AT_PAGE_SELECTOR) && _AT_PAGE_PSEUDO_CLASSES.contains(builtInPseudoClass))
      return pseudoClass;

    // _BACKWARD_COMPATIBLE_CSS3_PSEUDO_CLASSES is treated differently
    // for namespaced selectors we render it prefixed with "p_AF"
    // for non-namespaced selectors we render it directly
    if (!afNamespacedSelector && _BACKWARD_COMPATIBLE_CSS3_PSEUDO_CLASSES.contains(builtInPseudoClass))
      return pseudoClass;

    StringBuilder builder = new StringBuilder(pseudoClass.length() + 3);
    builder.append(".");
    builder.append(SkinSelectors.STATE_PREFIX);

    for (String content : _DASH_PATTERN.split(pseudoClass.substring(1)))
    {
      if (content.length() > 0)
      {
        builder.append(Character.toUpperCase(content.charAt(0)));
        builder.append(content.substring(1));
      }
    }

    return builder.toString();
  }

  /**
   * encapsulates the propertyString vs StyleNode maps separately for clientRule and normal selectors
   * ClientRule selectors the styles are rendered as:
   * clientRule { selector: {property1: value1, property2: value2 ... } }
   * Normal selectors are rendered as
   * selector: {property1: value1 ... }
   */
  private static final class MatchingStyles
  {
    private MatchingStyles(int styleCount)
    {
      this._nonClientRuleEntry = new MatchingEntry(styleCount);
      this._clientRuleEntries = new LinkedHashMap<String, MatchingEntry>();
    }


    public LinkedHashMap<String, List<StyleNode>> getNonClientRuleMap()
    {
      return _nonClientRuleEntry.getMatchingStyles();
    }

    public LinkedHashMap<String, List<StyleNode>> getClientRuleMap(String clientRule)
    {
      return _clientRuleEntries.get(clientRule).getMatchingStyles();
    }

    public Set<String> getClientRules()
    {
      return _clientRuleEntries.keySet();
    }

    public void addStyle(String propertyString, StyleNode styleNode)
    {
      _nonClientRuleEntry.addMatchingStyle(propertyString, styleNode);
    }

    public void addStyle(String clientRule, String propertyString, StyleNode styleNode)
    {
      MatchingEntry matchingStylesForClientRule = _clientRuleEntries.get(clientRule);

      if (matchingStylesForClientRule == null)
      {
        matchingStylesForClientRule = new MatchingEntry(1);
        _clientRuleEntries.put(clientRule, matchingStylesForClientRule);
      }

      matchingStylesForClientRule.addMatchingStyle(propertyString, styleNode);
    }

    // matching styles map with no client rules
    private final MatchingEntry _nonClientRuleEntry;

    // matching styles map with client rules
    private final LinkedHashMap<String, MatchingEntry> _clientRuleEntries;
  }

  /**
   * encapsulates propertyString vs StyleNode map
   * manages addition of StyleNode for a matching propertyString
   */
  private final static class MatchingEntry
  {
    public MatchingEntry(int initialSize)
    {
      _matchingStyles = new LinkedHashMap<String, List<StyleNode>>(initialSize);
    }

    public void addMatchingStyle(String propertyString, StyleNode styleNode)
    {
      if (propertyString == null || propertyString.equals(""))
        return;


      // See if we already have a StyleNode with the same properties
      List<StyleNode> matchingStyles = _matchingStyles.get(propertyString);

      if (matchingStyles == null)
      {
        // If we don't already have matching StyleNodes, create a new match list and cache it
        matchingStyles = new ArrayList<StyleNode>(1);
        _matchingStyles.put(propertyString, matchingStyles);
      }

      matchingStyles.add(styleNode);
    }

    public LinkedHashMap<String, List<StyleNode>> getMatchingStyles()
    {
      return _matchingStyles;
    }

    @Override
    public boolean equals(Object o)
    {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;

      MatchingEntry that = (MatchingEntry) o;

      if (_matchingStyles != null ? !_matchingStyles.equals(that._matchingStyles) : that._matchingStyles != null)
        return false;

      return true;
    }

    @Override
    public int hashCode()
    {
      return _matchingStyles != null ? _matchingStyles.hashCode() : 0;
    }

    private LinkedHashMap<String, List<StyleNode>> _matchingStyles;
  }

  // We want to output to the css the browser-supported pseudo-classes as is
  static private final Set<String> _BUILT_IN_PSEUDO_CLASSES = new HashSet<String>();
  static private final Set<String> _BACKWARD_COMPATIBLE_CSS3_PSEUDO_CLASSES = new HashSet<String>();

  // We want to output to the css the browser-supported pseudo-elements (HTML5/ CSS3) as is
  static private final Set<String> _BUILT_IN_PSEUDO_ELEMENTS = new HashSet<String>();
  static private final Set<String> _AT_PAGE_PSEUDO_CLASSES = new HashSet<String>();

  static
  {
    /** CSS 2 pseudo classes */
    _BUILT_IN_PSEUDO_CLASSES.add(":first-child");
    _BUILT_IN_PSEUDO_CLASSES.add(":link");
    _BUILT_IN_PSEUDO_CLASSES.add(":visited");
    _BUILT_IN_PSEUDO_CLASSES.add(":hover");
    _BUILT_IN_PSEUDO_CLASSES.add(":active");
    _BUILT_IN_PSEUDO_CLASSES.add(":focus");
    _BUILT_IN_PSEUDO_CLASSES.add(":-moz-placeholder");

    /** Special case CSS2 pseudo-elements used as pseudo-classes
     * for compatibility reasons.
     * Refer: http://www.w3.org/TR/selectors/#pseudo-elements */
    _BUILT_IN_PSEUDO_CLASSES.add(":after");
    _BUILT_IN_PSEUDO_CLASSES.add(":before");
    _BUILT_IN_PSEUDO_CLASSES.add(":first-line");
    _BUILT_IN_PSEUDO_CLASSES.add(":first-letter");

    /** CSS 3 pseudo classes */
    _BUILT_IN_PSEUDO_CLASSES.add(":nth-child");
    _BUILT_IN_PSEUDO_CLASSES.add(":nth-last-child");
    _BUILT_IN_PSEUDO_CLASSES.add(":nth-of-type");
    _BUILT_IN_PSEUDO_CLASSES.add(":nth-last-of-type");
    _BUILT_IN_PSEUDO_CLASSES.add(":last-child");
    _BUILT_IN_PSEUDO_CLASSES.add(":first-of-type");
    _BUILT_IN_PSEUDO_CLASSES.add(":last-of-type");
    _BUILT_IN_PSEUDO_CLASSES.add(":only-child");
    _BUILT_IN_PSEUDO_CLASSES.add(":only-of-type");
    _BUILT_IN_PSEUDO_CLASSES.add(":root");
    _BUILT_IN_PSEUDO_CLASSES.add(":target");
    _BUILT_IN_PSEUDO_CLASSES.add(":enabled");
    _BUILT_IN_PSEUDO_CLASSES.add(":checked");
    _BUILT_IN_PSEUDO_CLASSES.add(":not");
    _BUILT_IN_PSEUDO_CLASSES.add(":lang");
    _BUILT_IN_PSEUDO_CLASSES.add(":indeterminate");

    /** CSS3 pseudo classes for backward compatibility*/
    _BACKWARD_COMPATIBLE_CSS3_PSEUDO_CLASSES.add(":disabled");
    _BACKWARD_COMPATIBLE_CSS3_PSEUDO_CLASSES.add(":empty");

    /** CSS3 pseudo elements */
    _BUILT_IN_PSEUDO_ELEMENTS.add("::outside");
    _BUILT_IN_PSEUDO_ELEMENTS.add("::before");
    _BUILT_IN_PSEUDO_ELEMENTS.add("::after");
    _BUILT_IN_PSEUDO_ELEMENTS.add("::alternate");
    _BUILT_IN_PSEUDO_ELEMENTS.add("::first-line");
    _BUILT_IN_PSEUDO_ELEMENTS.add("::first-letter");
    _BUILT_IN_PSEUDO_ELEMENTS.add("::marker");
    _BUILT_IN_PSEUDO_ELEMENTS.add("::line-marker");
    _BUILT_IN_PSEUDO_ELEMENTS.add("::selection");
    _BUILT_IN_PSEUDO_ELEMENTS.add("::-webkit-input-placeholder");

    /** @page pseudo classes*/
    _AT_PAGE_PSEUDO_CLASSES.add(":first");
    _AT_PAGE_PSEUDO_CLASSES.add(":left");
    _AT_PAGE_PSEUDO_CLASSES.add(":right");
  }
  
  private static final Pattern _DASH_PATTERN =  Pattern.compile("-");
  private static final int _MSIE_SELECTOR_LIMIT = 4095;
  private static final String _DEFAULT_NAMESPACE = "af|";
  private static final String _DEFAULT_AF_SELECTOR = ".AF";
  private static final String _AT_PAGE_SELECTOR = "@page";

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(CSSGenerationUtils.class);
}
