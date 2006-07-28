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

/**
 * Private implementation of PropertyNode for color properties.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/ColorPropertyNode.java#0 $) $Date: 10-nov-2005.18:58:02 $
 * @author The Oracle ADF Faces Team
 */
class ColorPropertyNode extends PropertyNode
{
  /**
   * Creates a PropertyNode with the specified name and value
   */
  public ColorPropertyNode(String name, String value)
  {
    super(name, value);
  }

  // Package-level accessor necessary to support deprecated color properties
  boolean __isColorProperty()
  {
    return true;
  }
}
