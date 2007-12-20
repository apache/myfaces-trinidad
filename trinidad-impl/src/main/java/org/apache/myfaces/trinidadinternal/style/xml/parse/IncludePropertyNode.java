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
package org.apache.myfaces.trinidadinternal.style.xml.parse;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
/**
 * IncludePropertyNode is used to represent a single &lt;includeProperty%gt; element
 * in a parsed XML Style Sheet Language document.
 * The includeProperty element is used to include a single property of one style 
 * within another style. Thus, the includeProperty element is very similar to the 
 * includeStyle element. The only difference is that includeStyle includes all properties 
 * of the referenced style, whereas includeProperty includes only a single property.
 * Currently, the includeProperty element is not yet ported to the skin's CSS syntax.
 * It exists only in the XSS syntax.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/IncludePropertyNode.java#0 $) $Date: 10-nov-2005.18:58:07 $
 */
public class IncludePropertyNode
{
  /**
   * Creates an IncludePropertyNode.  In general, either the name or
   * selector of the included style is specified.
   */
  public IncludePropertyNode(
    String name,
    String selector,
    String propertyName,
    String localPropertyName)
  {

    assert ((name!=null) ||(selector!=null));

    if (propertyName == null)
    {
      throw new NullPointerException(_LOG.getMessage(
        "NULL_PROPERTYNAME"));
    }

    _name = name;
    _selector = selector;
    _propertyName = propertyName;
    _localPropertyName = localPropertyName;
  }

  /**
   * Returns the name of the style to include.
   */
  public String getName()
  {
    return _name;
  }

  /**
   * Returns the selector of the style to include.
   */
  public String getSelector()
  {
    return _selector;
  }

  /**
   * Returns the name of the property to include
   */
  public String getPropertyName()
  {
    return _propertyName;
  }

  /**
   * Returns the name of the property as it should appear in the
   * including style.
   */
  public String getLocalPropertyName()
  {
    if (_localPropertyName == null)
      return _propertyName;

    return _localPropertyName;
  }
  
  @Override 
  public boolean equals(Object obj)
  {
    if (this == obj)
      return true;
    if (!(obj instanceof IncludePropertyNode))
      return false;
     
    // obj at this point must be an IncludePropertyNode
    IncludePropertyNode test = (IncludePropertyNode)obj;
    return
      (_selector == test._selector || (_selector != null && _selector.equals(test._selector))) &&
      (_name == test._name || (_name != null && _name.equals(test._name))) &&
      (_propertyName == test._propertyName || 
        (_propertyName != null && _propertyName.equals(test._propertyName))) &&
      (_localPropertyName == test._localPropertyName || 
        (_localPropertyName != null && _localPropertyName.equals(test._localPropertyName)));       
  }
  
  @Override
  public int hashCode()
  {
    int hash = 17;
    hash = 37*hash + ((null == _name) ? 0 : _name.hashCode());
    hash = 37*hash + ((null == _selector) ? 0 : _selector.hashCode());
    hash = 37*hash + ((null == _propertyName) ? 0 : _propertyName.hashCode());
    hash = 37*hash + ((null == _localPropertyName) ? 0 : _localPropertyName.hashCode());

    return hash; 
  }  
  
  
  @Override
  public String toString()
  {
    return 
      "[name="   + _name   + ", " +
      "selector=" + _selector + ", " +
      "propertyName="  + _propertyName  + ", " +
      "localPropertyName=" + _localPropertyName + "]";
  }

  private final String _name;
  private final String _selector;
  private final String _propertyName;
  private final String _localPropertyName;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    IncludePropertyNode.class);
}
