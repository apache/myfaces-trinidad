/*
 * Copyright  2002-2006 The Apache Software Foundation.
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

import java.util.Map;
import java.util.regex.Pattern;

import org.apache.myfaces.trinidadinternal.style.StyleContext;
import org.apache.myfaces.trinidadinternal.style.StyleProvider;
import org.apache.myfaces.trinidadinternal.style.cache.FileSystemStyleCache;

/**
 * Generic style utilities.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/util/StyleUtils.java#0 $) $Date: 10-nov-2005.18:58:52 $
 * @author The Oracle ADF Faces Team
 */
public class StyleUtils
{

  public static final String RTL_CSS_SUFFIX = ":rtl";
  public static final String LTR_CSS_SUFFIX = ":ltr";

  /**
   * Returns a Map which maps style class names to
   * equivalent shorter names.
   * <p>
   * Some StyleProvider implementations, such as the FileSystemStyleCache,
   * automatically provide compressed versions style class names.  The
   * short style classes can be used instead of the full style class
   * names to reduce the overall size of generated content.
   * <p>
   * <p>
   * Note: The returned Map uses String keys to represent
   * the full class names.  However, the short style class values
   * may not necessarily be type java.lang.String.  Clients must
   * avoid explicitly casting the values contained in the Map
   * to type String.  Instead, such values should be passed directly
   * to the ResponseWriter API to be rendered.  Or, if the String
   * representation is required, toString() should be called on
   * the value.
   *
   * @param context The StyleContext
   * @param provider The StyleProvider
   *
   * @return A Map which maps the full style class names to
   *   the shorter equivalents.
   */
  public static Map getShortStyleClasses(
    StyleContext  context,
    StyleProvider provider)
  {
    // =-=ags For now, we explicilty cast to FileSystemStyleCache!
    //        It would be better if we could add a method to
    //        StyleProvider, but it is too late for that now.
    //        Alternatively, we could add a new StyleProvider
    //        sub-interface, but we won't pollute our public
    //        API until that proves necessary.
    if (provider instanceof FileSystemStyleCache)
    {
      return ((FileSystemStyleCache)provider).getShortStyleClasses(context);
    }

    return null;
  }

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
    // This is the code that's actually getting called by String.replaceAll();
    // this code is way too heavy.
    if (selector.indexOf("::") > 0)
      selector = _DOUBLE_COLON_PATTERN.matcher(selector).replaceAll("_");

    return selector;
  }

  static private final Pattern _DOUBLE_COLON_PATTERN = Pattern.compile("::");
}
