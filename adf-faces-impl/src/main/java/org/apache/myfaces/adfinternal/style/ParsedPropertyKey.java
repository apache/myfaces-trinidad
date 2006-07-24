/*
 * Copyright  2001-2006 The Apache Software Foundation.
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

package org.apache.myfaces.adfinternal.style;

/**
 * Class for keys used by Style.getParsedProperty()
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/ParsedPropertyKey.java#0 $) $Date: 10-nov-2005.18:57:55 $
 * @author The Oracle ADF Faces Team
 */
public final class ParsedPropertyKey
{
  private ParsedPropertyKey() {}

  /**
   * Creates a parsed property key with the specified name and index.
   */
  ParsedPropertyKey(
    String name,
    int    index
    )
  {
    _name = name;
    _index = index;
  }

  /**
   * Returns the index for the key
   */
  public int getKeyIndex()
  {
    return _index;
  }

  /** 
   * Returns the name for this key
   */
  public String getKeyName()
  {
    return _name;
  }

  private String _name;
  private int    _index;
}
