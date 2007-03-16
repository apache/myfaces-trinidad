/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidadinternal.share.util;


/**
 * The FastMessageFormat class is a greatly reduced version
 * of the java.text.MessageFormat class.  It's also much faster
 * and much less expensive to create, which is especially
 * valuable when it is created and thrown away many times - 
 * a common use case in web applications.
 * <p>
 * The only syntax supported by this class is simple index-based
 * replacement, namely:
 * <pre>
 *     some{1}text{0}here{2}andthere
 * </pre>
 * as well as escaping using single quotes.  Like MessageFormat,
 * a single quote must be represented using two consecutive single
 * quotes, but the contents of any text between single quotes
 * will not be interpreted.  So, the following pattern could
 * be used to include a left bracket:
 * <pre>
 *     some'{'text{0}
 * </pre>
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/util/FastMessageFormat.java#0 $) $Date: 10-nov-2005.18:59:22 $
 */
public class FastMessageFormat
{
  /**
   * Creates a FastMessageFormat based on the given format string.
   */
  public FastMessageFormat(String formatString)
  {
    if (formatString == null)
      throw new NullPointerException();

    _formatText = formatString.toCharArray();
  }


  /**
   * Formats the given array of strings based on the initial
   * pattern.   It is legal for this array to be shorter
   * than that indicated by the pattern, or to have null
   * entries - these will simply be ignored.
   * <p>
   * @param source an array of strings
   */
  public String format(Object[] source)
  {
    int formatLength = _formatText.length;
    int length = 0;
    int sourceCount = source.length;
    for (int i = 0; i < sourceCount; i++)
    {
      Object sourceString = source[i];
      if (sourceString != null)
      {
        length += sourceString.toString().length();
      }
    }

    StringBuffer buffer = new StringBuffer(length + formatLength);

    int lastStart = 0;
    boolean inQuote = false;
    for (int i = 0; i < formatLength; i++)
    {
      char ch = _formatText[i];
      if (inQuote)
      {
        if (ch == '\'')
        {
          buffer.append(_formatText, lastStart, i - lastStart);
          i++;
          lastStart = i;
          inQuote = false;
        }
      }
      else
      {
        if (ch == '\'')
        {
          buffer.append(_formatText, lastStart, i - lastStart);
          i++;
          lastStart = i;

          // Check for doubled-up quotes
          if ((i < formatLength) && (_formatText[i] == '\''))
          {
            // Do nothing;  we'll add the doubled-up quote later
            ;
          }
          else
          {
            inQuote = true;
          }
        }
        else if (ch == '{')
        {
          buffer.append(_formatText, lastStart, i - lastStart);

          int sourceIndex = 0;
          int j = i + 1;
          for (; j < formatLength; j++)
          {
            char patternChar = _formatText[j];
            if (patternChar == '}')
            {
              break;
            }
            else
            {
              if ((patternChar < '0') ||
                  (patternChar > '9'))
                throw new IllegalArgumentException(
                   "FastMessageFormat only supports numeric arguments");
              sourceIndex = (sourceIndex * 10) + (patternChar - '0');
            }
          }

          if (j == formatLength)
            throw new IllegalArgumentException(
                   "End of pattern not found");
          if (j == i + 1)
            throw new IllegalArgumentException(
                   "FastMessageFormat: empty argument - {} - found");
          if (sourceIndex < sourceCount)
          {
            Object sourceString = source[sourceIndex];
            if (sourceString != null)
              buffer.append(sourceString.toString());
          }
          
          i = j;
          lastStart = i + 1;
        }
        else
        {
          // Do nothing.  The character will be added in later
          ;
        }
      }
    }

    buffer.append(_formatText, lastStart, formatLength - lastStart);

    return new String(buffer);
  }

  private final char[] _formatText;
}
