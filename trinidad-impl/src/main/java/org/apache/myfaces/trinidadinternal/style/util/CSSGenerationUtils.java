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

package org.apache.myfaces.trinidadinternal.style.util;

import java.io.File;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.SkinSelectors;
import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.xml.parse.PropertyNode;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleNode;

/**
 * CSS-generation related utilities used when we write out our css-2 stylesheet
 * document based on the skin's css-3 stylesheet document.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/util/CSSUtils.java#0 $) $Date: 10-nov-2005.18:58:49 $
 * @author The Oracle ADF Faces Team
 */
 
public class CSSGenerationUtils
{


  /**
   * Converts the specified set of StyleNodes to CSS.
   *
   * @param context The current StyleContext
   * @param styles The style nodes to convert
   * @param out The PrintWriter to write to
   * @param shortStyleClassMap A Map which maps style
   *   class names to short names.
   * @param selectorNamespace the namespace of our special selectors "af|"
   * @param afSelectorMap A Map which maps the af| component selectors
   *   to their base names (e.g., 'af|menuPath::step' maps to 'af|menuPath A')
   */
  public static void writeCSS(
    StyleContext        context,
    StyleNode[]         styles,
    PrintWriter         out,
    File                outputFile,
    Map<String, String> shortStyleClassMap,
    String              selectorNamespace,
    Map<String, String> afSelectorMap
    )
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

    // We track styles with matching properties in the following HashMap
    // which maps property strings to StyleNode[]s.
    HashMap<String, StyleNode[]> matchingStylesMap = 
      new HashMap<String, StyleNode[]>(101);

    // We also keep an array of the property strings that we generate
    // during this pass, since we need these strings during the second
    // pass to find matching StyleNodes.
    String[] propertyStrings = new String[styles.length];

    for (int i = 0; i < styles.length; i++)
    {
      StyleNode style = styles[i];

      if (style.getSelector() != null)
      {
        // Get the property string (properties are sorted so that
        // the order doesn't affect whether styles match).
        String propertyString = _getSortedPropertyString(style);

        // See if we already have a StyleNode with the same properties
        StyleNode[] matchingStyles = matchingStylesMap.get(propertyString);

        if (matchingStyles == null)
        {
          // If we don't already have matching StyleNodes, add this
          // StyleNode to the map.
          propertyStrings[i] = propertyString;
          matchingStyles = new StyleNode[1];
          matchingStyles[0] = style;
        }
        else
        {
          // If we already have matching StyleNodes, add this StyleNode
          // to the end of the list of matching StyleNodes.
          int length = matchingStyles.length;

          StyleNode[] newMatchingStyles = new StyleNode[length + 1];
          System.arraycopy(matchingStyles,
                           0,
                           newMatchingStyles,
                           0,
                           length);
          newMatchingStyles[length] = style;
          matchingStyles = newMatchingStyles;
        }

        // Rehash with the new value
        matchingStylesMap.put(propertyString, matchingStyles);
      }
    }

    // We'll start writing the CSS file now.  First
    // write out the header with a time stamp
    Date date = new Date();
    out.println("/* CSS file generated on " + date + " */");

    // This is the second pass in which we write out the style rules
    for (int i = 0; i < styles.length; i++)
    {
      StyleNode style = styles[i];
      String propertyString = propertyStrings[i];

      // We only write out styles for which we have a property string.
      // All other entries correspond to styles which don't have selectors -
      // or styles which will be rendered as a "matching" style.
      if (propertyString != null)
      {
        // Get all of the styles which share this property string.
        StyleNode[] matchingStyles = matchingStylesMap.get(propertyString);

        // Actually, we should always have at least one StyleNode here
        assert (matchingStyles != null);

        // Write out all of the style selectors for this property string
        for (int j = 0; j < matchingStyles.length; j++)
        {
          StyleNode matchingStyle = matchingStyles[j];
          String selector = matchingStyle.getSelector();

          // We should always have a selector at this point
          assert (selector != null);

          // map selectors, if needed
          // any of the selectors that start with the selectorNamespace will
          // be mapped.
          // e.g., "af|menuPath::step" maps to "af|menuPath A"
          // if the selector does not need to be 'mapped', it will be returned
          // untouched. e.g., .AFInstructionText maps to .AFInstructionText
          String mappedNSSelector = _getMappedNSSelector(afSelectorMap,
                                                         selectorNamespace,
                                                         selector,
                                                         false);

          // write out the full selector
          String validFullNameSelector =
            _getValidFullNameSelector(mappedNSSelector);
          if (validFullNameSelector != null)
            out.print(validFullNameSelector);


          // shorten all the css-2 style class selectors (those that start with 
          // '.')and return the shortened string.
          // e.g., selector of '.OraBulletedList A' is shortened to '.xj A'
          // e.g., selector of af|inputText::content is not shortened since
          // it has no css-2 style class selector piece that starts with '.'.
          // e.g., selector of af|foo.Bar will shorten the '.Bar' piece 
          // af|foo.xz
          // e.g., .Foo:hover -> .x0:hover
          String shortenedSelector = _getShortSelector(mappedNSSelector,
                                                       shortStyleClassMap,
                                                       selectorNamespace);

          // run it through a shortener one more time to shorten any
          // of the af component selectors.
          // e.g., 'af|menuPath' is shortened to '.x11'
          String shortSelector =
            _getMappedNSSelector(shortStyleClassMap,
                                 selectorNamespace,
                                 shortenedSelector,
                                 true);

          // if the transformed full name is different than the shortSelector
          // then write out the shortSelector, too.
          if (shortSelector != null)
          {
            String validShortSelector =
              _getValidFullNameSelector(shortSelector);
            if (!validFullNameSelector.equals(validShortSelector))
            {
              out.print(",");
              out.print(validShortSelector);
  
            }
          }

          // Write out a separator between matching selectors
          if (j < (matchingStyles.length - 1))
            out.print(",");
        }

        // Now that we have written out the selectors, write out
        // the properties
        out.print(" {");

        // At this point, we could just write out the property string
        // that we already created, but this string contains the properties
        // in sorted order.  We prefer to attempt to preserve the order
        // of the properties as specified in the XSS document.  So,
        // we get the properties from the StyleNode object instead of
        // using the propertyString
        Iterator<PropertyNode> properties = style.getProperties();
        boolean first = true;

        while (properties.hasNext())
        {
          PropertyNode property = properties.next();
          String propName = property.getName();
          String propValue = property.getValue();

          if ((propName != null) &&
              (propValue != null) &&
              (propValue.length() > 0 ))
          {
            if (!first)
              out.print(";");
            else
              first = false;

            out.print(propName);
            out.print(":");
            out.print(propValue);
          }
        }

        out.println("}");
      }
    }
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
   * Returns an
   * Iterator of all of the component selectors that begin with the
   * given namespace.  For example, ".OraNav1Enabled af|menuPath" returns
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
    int afIndex = selector.indexOf(namespace);

    // null or no namespace in the selector, so just return null
    if ((selector == null) || (afIndex == -1))
      return null;

    ArrayList<String> afUnmappedSelectorList = new ArrayList<String>();

    // now find each af| component selector and map
    // e.g., af|menuPath::step maps to af|menuPath A

    // split the string into the spaces
    String base = selector.substring(afIndex);
    String[] afSelectors;
    String[] spacerArray = base.split("\\s");
    afSelectors = 
        _orderPseudoElementsAndClasses(spacerArray, namespace);
        
    // loop through each of the af| parts.
    for (int i=0; i < afSelectors.length; i++)
    {
      if (afSelectors[i].startsWith(namespace))
      {
        // get the component selector, which is just the main part, nothing
        // that includes a '.' ':', or a ' '.
        String afComponentSelector = 
          _getAfComponentSelector(afSelectors[i], false);
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
          String[] baseSelector =
            (mappedSelector.substring(namespaceIndex)).split("\\s");
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


  private static String _getAfComponentSelector(
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
  private static String _getShortSelector(
    String              selector,
    Map<String, String> shortStyleClassMap,
    String              selectorNamespace)
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
          // don't shorten the styles that start with the namespace
          if (!styleClass.startsWith(selectorNamespace))
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
   * Runs a selector through a map. It returns the selector unchanged if 
   * there is no namespace in the selector.
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
   * @param namespace   most likely, "af|". The selectors with this namespace
   *                    are the ones we map.
   * @param selector    selector to map.
   * @param shorten     if true, then we'll add the "." to the mapped selector.
   * @return            the selector, mapped.
   */
  private static String _getMappedNSSelector (
    Map<String, String> map,
    String              namespace,
    String              selector,
    boolean             shorten)
  {
    // break apart by spaces
    // map each piece; if namespace is not in the piece,
    // it will remain unchanged
    // piece back together.


    // quick check; null or no namespace in the selector,
    // return the selector unchanged.
    if ((selector == null) ||
        (selector.indexOf(namespace) == -1))
      return selector;

    // split the string into the spaces
    String[] selectorArray;
    if (!shorten)
    {
      String[] spacerArray = selector.split("\\s");

      selectorArray = 
        _orderPseudoElementsAndClasses(spacerArray, namespace);
    }
    else
    {
      selectorArray = selector.split("\\s"); 
    }

    for (int i=0; i < selectorArray.length; i++)
    {

      if (selectorArray[i].indexOf(namespace) > -1)
      {
        selectorArray[i] = _getEachMappedSelector(map,
                                                  namespace,
                                                  selectorArray[i],
                                                  shorten);
      }
    }

    // build back the selectorArray into a string
    return _arrayToStringWithSpaces(selectorArray);
  }

  private static String _getEachMappedSelector(
    Map<String, String> map,
    String              namespace,
    String              selector,
    boolean             shorten)
  {
    // break apart the selector into 3 parts:
    // main
    // pseudo-classes (e.g., :hover, :disabled, :readOnly)
    // end (anything including and after '.')
    // map the main and the pseudo-classes cuz the pseudo-classes could be
    // our internal pseudo-classes.
    // piece  back together.
    // should I bother mapping pseudo-classes if shorten=true, since they
    // won't be in the map anyway? TODO
    int indexOfNamespace = selector.indexOf(namespace);
    if (indexOfNamespace == -1)
     return selector;
    if (map == null)
      return selector;
    
    
    String wholeAfSelector = selector.substring(indexOfNamespace);    
    SelectorPieces pieces = new SelectorPieces();
    
    // get main piece
    int colonIndex = wholeAfSelector.indexOf("::");

    // get the part after the ::.
    // colonIndex will be 0 if there is no ::.
    if (colonIndex != -1)
    {
      if (_LOG.isWarning() &&
          (wholeAfSelector.lastIndexOf("::") != colonIndex))
      {
        _LOG.warning("Consecutive sub-element (::) syntax used in selector " +
                     selector + ".  This is not supported.");
      }

      colonIndex += 2;
    }
    else
      colonIndex = 0;
    String afterDoubleColon = wholeAfSelector.substring(colonIndex);

    // now find the part with a single ':', a ' ', or a '.'. That is where
    // I want to chop it to get my 'main' piece of the component selector.
    boolean end = false;
    int afterLength = afterDoubleColon.length();
    int endIndex=0;
    char c;
    for (; ((endIndex < afterLength) && !end); endIndex++)
    {
      c = afterDoubleColon.charAt(endIndex);
      end = (Character.isWhitespace(c)) || (c == '.') || (c == ':');
    }
    if (endIndex == afterLength)
    {
      String mainSelector = wholeAfSelector.substring(0, colonIndex + endIndex);
      pieces.setMain(mainSelector);
    }
    else
    {
      // If I got here, that means there are characters after the 'main' part
      // of the selector.
      String mainSelector = wholeAfSelector.substring(0, colonIndex + endIndex-1);
      pieces.setMain(mainSelector);    
      String afterMain = wholeAfSelector.substring(colonIndex + endIndex-1);
      // afterMain includes the : or the .
      // if I get a :, I know I'm in a pseudo-class. Keep getting characters
      // until I'm at the end, or I get another : or .
      boolean inPseudoClass = false;
      StringBuffer pseudoClassBuffer = new StringBuffer();
      for (int i=0; i < afterMain.length(); i++)
      {
        char x = afterMain.charAt(i);
        if (x == '.')
        {
          inPseudoClass = false;
          pieces.setEnd(afterMain.substring(i));
          break;
        }
        else if (x == ':')
        {
          if (inPseudoClass)
          {
            // if I'm in a pseudo-class already, and I get a ':', that means
            // i've got another pseudo-class. End the first one.
            pieces.addPseudoClass(pseudoClassBuffer.toString());
            pseudoClassBuffer = new StringBuffer();           
          }
          inPseudoClass = true;
          pseudoClassBuffer.append(x);
        }
        else
          pseudoClassBuffer.append(x);
      }
      if (pseudoClassBuffer.length() > 0)
        pieces.addPseudoClass(pseudoClassBuffer.toString());
    }

    // piece back together the string
    StringBuffer buffer = new StringBuffer();

    // beginning. add '.' if needed.
    if (indexOfNamespace > 0)
    {
      buffer.append(selector.substring(0, indexOfNamespace));
      if (shorten && selector.charAt(indexOfNamespace-1) != '.')
        buffer.append('.');
    }
    else if (shorten)
      buffer.append('.');
      
    // now I have the pieces of the selector. I'll need to map each piece,
    // then piece back together.
    String mappedMain = map.get(pieces.getMain());
    if (mappedMain != null)
      buffer.append(mappedMain);
    else
      buffer.append(pieces.getMain());
    
    List <String> pseudoClasses = pieces.getPseudoClasses();
    for (String pseudoClass : pseudoClasses )
    {
      String mappedPseudoClass = _convertPseudoClass(pseudoClass);
      buffer.append(mappedPseudoClass);
    }

    String endPiece = pieces.getEnd();
    // don't map end piece. (the piece that starts with '.')
    if(endPiece != null)
    {
      buffer.append(endPiece);
    }

    
    return buffer.toString(); 
  }

  /*
   * Returns a string representation of the contents of the specified array
   * where adjacent elements are separated a space (" ").
   */
  private static String _arrayToStringWithSpaces(
    String[] stringArray
    )
  {
    int length = stringArray.length;
    // if only one thing in the array, just return it to save some time.
    if (stringArray.length == 1) return stringArray[0];
    
    // get the bufferSize
    int bufferSize = 0;
    for (int i=0; i < length; i++)
    {
      bufferSize += stringArray[i].length() + 1;
    }
    // turn the array into a space deliminated String
    StringBuffer returnString = new StringBuffer(bufferSize);
    for (int i=0; i < length; i++)
    {
      returnString.append(stringArray[i]);
      if (i+1 < length)
        returnString.append(' ');
    }
    return returnString.toString();
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
    String[] input,
    String   namespace)
  {
    List<String> output = new ArrayList<String>();
    for (int i=0; i < input.length; i++)
    {
      boolean hasNamespace = (input[i].indexOf(namespace) > -1);

      if (!hasNamespace)
      {
        // not one of ours, so pass through
        output.add(input[i]);
      }
      else
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
    }  
    return output.toArray(new String[output.size()]);
  }

  
  
  /**
   * get rid of the | and :: that browsers don't like, and add the
   * '.' where needed to make the af| component selector
   * into a style class selector that is valid to be written to the css file.
   * @param mappedNSSelector
   * @return
   */
  private static String _getValidFullNameSelector(String mappedNSSelector)
  {
      // get rid of | and :: that the browsers don't like
      String validSelector =
        StyleUtils.convertToValidSelector(mappedNSSelector);

      // if the selector starts with af_, then it is our selector, so
      // add a .
      if (validSelector.startsWith("af_"))
      {
        validSelector = "." + validSelector;
      }
      // now look for any 'space af_' and put a dot in front of the af_
      // this is for the case of af_panelBox_light af_panelBox_body
      // unless we do this, we just have .af_panelBox_light af_panelBox_body
      if (validSelector.indexOf(" af_") > -1)
      {
        validSelector = validSelector.replaceAll(" af_", " .af_");
      }

      return validSelector;

  }


  // Tests whether the specified character or pseudo-class terminates a
  // style class selector
  private static boolean _isStyleClassTerminator(char c)
  {
    return (Character.isWhitespace(c) || (c == ':') || (c == '.'));
  }

  // Gets the properties of the specified StyleNode in sorted
  // order as a String.
  private static String _getSortedPropertyString(StyleNode style)
  {
    // First, pull the properties out of the StyleNode
    // -= Simon Lessard =- 
    // TODO: Check if synchronization is needed, otherwise uses
    //       an ArrayList instead. Even if synchronization is needed 
    //       Collections.synchronizedList(ArrayList) would probably be
    //       a better choice.
    Vector<PropertyNode> v = new Vector<PropertyNode>();
    Iterator<PropertyNode> e = style.getProperties();
    while (e.hasNext())
      v.addElement(e.next());

    PropertyNode[] properties = new PropertyNode[v.size()];
    v.copyInto(properties);

    // Sort the properties so that the order of the properties won't
    // come into play when comparing property strings.
    Arrays.sort(properties,PropertyNodeComparator.sharedInstance());

    // Compute the StringBuffer size
    int bufferSize = 0;
    for (int i = 0; i < properties.length; i++)
    {
      PropertyNode property = properties[i];
      String name = property.getName();
      String value = property.getValue();

      if ((name != null) && (value != null))
      {
        bufferSize += property.getName().length();
        bufferSize += property.getValue().length();

        // Leave room for a separator
        bufferSize++;
      }
    }

    StringBuffer buffer = new StringBuffer(bufferSize);
    boolean first = true;

    for (int i = 0; i < properties.length; i++)
    {
      PropertyNode property = properties[i];
      String name = property.getName();
      String value = property.getValue();

      if ((name != null) && (value != null))
      {
        if (!first)
          buffer.append(";");
        else
          first = false;

        buffer.append(name);
        buffer.append(":");
        buffer.append(value);
      }
    }

    return buffer.toString();
  }
  /* inner class used to store the pieces of a selector:
   * the main piece, the pseudo-classes, and anything after the pseudo-classes
   */
  private static class SelectorPieces
  {
    private String _main;
    private List<String>  _pseudo_classes = new ArrayList<String>();
    private String _end;
    
    public void setMain(String main)
    {
      _main = main;
    }
    public String getMain()
    {
      return _main;
    }  
    public void addPseudoClass(String pseudoClass)
    {
      _pseudo_classes.add(pseudoClass);
    }
    public List <String> getPseudoClasses()
    {
      return _pseudo_classes;
    }  
    public void setEnd(String end)
    {
      _end = end;
    }
    public String getEnd()
    {
      return _end;
    }  
  }
  
  // Comparator that sorts PropertyNodes by name
  private static class PropertyNodeComparator implements Comparator<PropertyNode>
  {
    public static Comparator<PropertyNode> sharedInstance()
    {
      return _sInstance;
    }

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

    private PropertyNodeComparator() {}

    private static final Comparator<PropertyNode> _sInstance = 
      new PropertyNodeComparator();
  }
  
  static private String _convertPseudoClass(String pseudoClass)
  {
    if (_BUILT_IN_PSEUDO_CLASSES.contains(pseudoClass))
      return pseudoClass;
    StringBuilder builder = new StringBuilder(pseudoClass.length() + 3);
    builder.append(".");
    builder.append(SkinSelectors.STATE_PREFIX);

    for (String content : pseudoClass.substring(1).split("-"))
    {
      if (content.length() > 0)
      {
        builder.append(Character.toUpperCase(content.charAt(0)));
        builder.append(content.substring(1));
      }
    }

    return builder.toString();
  }


  // =-=AEW Do we care about built-in pseudo-elements???
  static private final Set<String> _BUILT_IN_PSEUDO_CLASSES =
    new HashSet<String>();
  static
  {
    _BUILT_IN_PSEUDO_CLASSES.add(":first-child");
    _BUILT_IN_PSEUDO_CLASSES.add(":link");
    _BUILT_IN_PSEUDO_CLASSES.add(":visited");
    _BUILT_IN_PSEUDO_CLASSES.add(":hover");
    _BUILT_IN_PSEUDO_CLASSES.add(":active");
    _BUILT_IN_PSEUDO_CLASSES.add(":focus");
  }

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(CSSGenerationUtils.class);
}
