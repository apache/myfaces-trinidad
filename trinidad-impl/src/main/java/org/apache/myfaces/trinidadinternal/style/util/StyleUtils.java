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

import java.util.ArrayList;
import java.util.List;


/**
 * Generic style utilities.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/util/StyleUtils.java#0 $) $Date: 10-nov-2005.18:58:52 $
 */
public class StyleUtils
{

  public static final String RTL_CSS_SUFFIX = ":rtl";
  public static final String LTR_CSS_SUFFIX = ":ltr";


  /**
   * Convert the characters that should not be in a selector
   * e.g., the | that is in the namespace to an _ so the css class name
   * is valid.
   * @param input String to convert.
   * @return the input string with all "|" converted to "_" and all "::" to "_"
   */
  public static String convertToValidSelector(String selector)
  {
    
    if (selector == null) return null;

    selector = selector.replace('|', '_');
    
    // With CSS3 / HTML5 support, we could have more than one double colon sequence in any segment due to the 
    //  browser supported pseudo elements, let those pass as is to the generated css.
    String[] splitSelector = splitStringByWhitespace(selector);
    int splitLength = splitSelector.length;
    if (splitLength == 1)
      return splitSelector[0].replaceFirst("::", "_");

    for (int i=0; i<splitLength; i++)
    {
      splitSelector[i] = splitSelector[i].replaceFirst("::", "_");
    }
    
    return arrayToStringWithSpaces(splitSelector);
  }
  
  
  // returns true if the selectorName indicates that it is an icon.
  public static boolean isIcon(String selectorName)
  {
    if (selectorName == null)
      return false;
    // =-=jmw There is no good way to tell if this is an icon.
    // for now, I look at the selector name.
    // we do have some styles that have -icon- in the name, but it's
    // not at the end which is how icons are determined.
    // our icon names look like .AFWarningIcon:alias
    // AFErrorIconStyle is a style.
    // This supports pseudo-classes on icon definitions (e.g.,
    // foo-icon:hover- or FooIcon:alias:hover)
    // -icon: is a condition because it could be -icon:hover.
    return  (selectorName.endsWith("-icon")  ||
            (selectorName.indexOf("-icon:") > -1) ||
            selectorName.indexOf("Icon:alias") > -1);
  }
  
  /**
   * return the array of strings computed by splitting this string
   * around one or more whitespaces This calls Character.isWhitespace to determine if it is
   * whitespace. This is important because tabs, newlines, etc count as whitespace
   * in the selector strings. This is faster than String's split("\\s")
   * @param selector
   * @return String[] The array of Strings computed by splitting the input String
   * around one or more spaces. e.g, "af|foo    af|bar" returns "af|foo" and "af|bar" in
   * a String Array.
   */
  public static String[] splitStringByWhitespace (
    String  selector)
  {
    // return a String[] with each piece that is deliminated by the inChar.
    int length = selector.length();
    StringBuffer buffer = new StringBuffer(length);
    List<String> splitList = new ArrayList<String>();
    boolean inWhitespace = false;

    for (int i=0; i < length; i++)
    {
      char c = selector.charAt(i);
      if (Character.isWhitespace(c))
      {
        // we hit the whitespace delimiter, so put it in the splitList and start a new buffer.
        // ignore spaces that are in a row
        if (!inWhitespace)
        {
          String bufferString = buffer.toString();
          if (bufferString.length() > 0)
          {
            splitList.add(bufferString);
            buffer = new StringBuffer(length);
            inWhitespace = true;
          }
        }
      }
      else
      {
        buffer.append(c);
        if (inWhitespace)
          inWhitespace = false;
      }
    }
    // we are done with all the characters
    String lastString = buffer.toString();
    if (lastString.length() > 0)
      splitList.add(lastString);

    return splitList.toArray(_EMPTY_STRING_ARRAY);
  }
  
  /*
   * Returns a string representation of the contents of the specified array
   * where adjacent elements are separated a space (" ").
   */
  public static String arrayToStringWithSpaces(
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

  static private final String _DOUBLE_COLON = "::";
  private static final String[] _EMPTY_STRING_ARRAY = new String[0];

}
