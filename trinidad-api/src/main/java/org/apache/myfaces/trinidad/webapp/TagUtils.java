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
package org.apache.myfaces.trinidad.webapp;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


final class TagUtils
{
  private TagUtils() {}


  /**
   * Parses a whitespace separated series of name tokens.
   * @param o the full string
   * @return an array of each constituent value, or null
   *  if there are no tokens (that is, the string is empty or
   *  all whitespace)
   * @todo Move to utility function somewhere (ADF Share?)
   */
  @SuppressWarnings("oracle.jdeveloper.java.null-array-return")
  static final String[] parseNameTokens(Object o)
  {
    List<String> list = parseNameTokensAsList(o);

    if (list == null)
      return null;

    return list.toArray(new String[list.size()]);
  }

  @SuppressWarnings("oracle.jdeveloper.java.null-collection-return")
  static final List<String> parseNameTokensAsList(Object o)
  {
    if (o == null)
      return null;

    String stringValue = o.toString();
    ArrayList<String> list = new ArrayList<String>(5);

    int     length = stringValue.length();
    boolean inSpace = true;
    int     start = 0;
    for (int i = 0; i < length; i++)
    {
      char ch = stringValue.charAt(i);

      // We're in whitespace;  if we've just departed
      // a run of non-whitespace, append a string.
      // Now, why do we use the supposedly deprecated "Character.isSpace()"
      // function instead of "isWhitespace"?  We're following XML rules
      // here for the meaning of whitespace, which specifically
      // EXCLUDES general Unicode spaces.
      if (Character.isWhitespace(ch))
      {
        if (!inSpace)
        {
          list.add(stringValue.substring(start, i));
          inSpace = true;
        }
      }
      // We're out of whitespace;  if we've just departed
      // a run of whitespace, start keeping track of this string
      else
      {
        if (inSpace)
        {
          start = i;
          inSpace = false;
        }
      }
    }

    if (!inSpace)
      list.add(stringValue.substring(start));

    if (list.isEmpty())
      return null;

    return list;
  }

  @SuppressWarnings("oracle.jdeveloper.java.null-collection-return")
  static final Set<String> parseNameTokensAsSet (Object o)
  {
    List<String> list = parseNameTokensAsList(o);

    if (list == null)
      return null;
    else
      return new HashSet<String>(list);
  }
}
