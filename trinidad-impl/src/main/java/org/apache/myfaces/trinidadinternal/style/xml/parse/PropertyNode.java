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

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
/**
 * Private implementation of PropertyNode.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/PropertyNode.java#0 $) $Date: 10-nov-2005.18:58:09 $
 */
public class PropertyNode
{

  /**
   * Creates a PropertyNode with the specified name and value.
   * @param name name of the propertyNode. Examples are 'font-size', 'background-color'.
   * @param value value of the propertyNode. Examples are '12px', 'red', '0xeaeaea'
   * If name is null or the empty string, an IllegalArgumentException is thrown.
   */
  public PropertyNode(String name, String value)
  {

    if (name == null || "".equals(name))
      throw new IllegalArgumentException(_LOG.getMessage(
        "PROPERTYNODE_NAME_CANNOT_BE_NULL_OR_EMPTY", new Object[]{name, value}));

    // intern() name because many of the property names are the same,
    // like color, background-color, background-image, font-size, etc.
    // This will improve the memory used.
    _name = name.intern();
    _value = value;
  }

  /**
   * Implementation of PropertyNode.getName().
   */
  public String getName()
  {
    return _name;
  }

  /**
   * Implementation of PropertyNode.getValue().
   */
  public String getValue()
  {
    return _value;
  }

  @Override
  public boolean equals(Object obj)
  {
    if (this == obj)
      return true;
    if (!(obj instanceof PropertyNode))
      return false;

    // obj at this point must be an PropertyNode
    PropertyNode test = (PropertyNode)obj;

    return
      (_value == test._value || (_value != null && _value.equals(test._value))) &&
      (_name == test._name || (_name != null && _name.equals(test._name)));
  }

  @Override
  public int hashCode()
  {
    int hash = 17;
    hash = 37*hash + ((null == _name) ? 0 : _name.hashCode());
    hash = 37*hash + ((null == _value) ? 0 : _value.hashCode());
    return hash;
  }

  @Override
  public String toString()
  {
    return
      "[name="   + _name   + ", " +
      "value=" + _value + "]";
  }

  private final String _name;
  private final String _value;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    PropertyNode.class);
}
