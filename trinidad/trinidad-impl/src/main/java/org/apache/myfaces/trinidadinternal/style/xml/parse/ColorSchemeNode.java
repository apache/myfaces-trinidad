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

package org.apache.myfaces.trinidadinternal.style.xml.parse;

import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;



/**
 * Private implementation of ColorSchemeNode.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/ColorSchemeNode.java#0 $) $Date: 10-nov-2005.18:58:02 $
 * @author The Oracle ADF Faces Team
 */
public class ColorSchemeNode
{
  /**
   * Creates a ColorSchemeNode
   */
  public ColorSchemeNode(
    String namespaceURI,
    String name,
    ColorNode[] colors
    )
  {
    _namespace = namespaceURI;
    _name = name;

    if (colors != null)
    {
      _colors = new ColorNode[colors.length];
      System.arraycopy(colors, 0, _colors, 0, colors.length);
    }
  }

  /**
   * Returns the name of this ColorSchemeNode.
   */
  public String getName()
  {
    return _name;
  }

  /**
   * Returns the namespace of this ColorSchemeNode.
   */
  public String getNamespaceURI()
  {
    return _namespace;
  }

  /**
   * Returns an Iterator of ColorNodes contained by this ColorSchemeNode
   */
  public Iterator getColors()
  {
    if(_colors!=null)
    {
      return (Arrays.asList(_colors)).iterator();  
    }
    else
      return (Collections.EMPTY_LIST).iterator();
  }

  private String      _namespace;
  private String      _name;
  private ColorNode[] _colors;
}
