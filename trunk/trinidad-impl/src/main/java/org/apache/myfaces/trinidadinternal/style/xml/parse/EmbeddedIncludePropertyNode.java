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

package org.apache.myfaces.trinidadinternal.style.xml.parse;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * The EmbeddedIncludePropertyNode node is a data structure to store property values that can
 * have multiple occurrences of -tr-property-ref at random locations in them, examples:
 * border: 1px solid -tr-property-ref(".AFDarkColor:alias",color);
 * background: -ms-linear-gradient(top, -tr-property-ref(".AFRed:alias", "color") 1%, -tr-property-ref(".AFGreen:alias","color") 100%);
 *
 * This is similar to includeProperty element, but it is used on composite css property values.
 *
 * Note that for cases of single occurrence of -tr-property-ref we should be using the simpler
 * IncludePropertyNode.
 * @version $Name:  $ ($Revision: trinidad-impl/src/main/java/org/apache/myfaces/trinidadinternal/style/xml/parse/EmbeddedIncludePropertyNode.java#0 $) $Date: 10-nov-2005.18:58:07 $
 */
public class EmbeddedIncludePropertyNode
{

  /**
   * Creates an EmbeddedIncludePropertyNode.
   * If we have border: 1px solid -tr-property-ref(".AFDarkColor:alias",color); in the css file,
   * then localPropertyName == border, propertyValues == {"1px solid -tr-0"}, and
   * includePropertyNodes will contain map of placeholder token '-tr-0' to 
   * -tr-property-ref(".AFDarkColor:alias",color);
   * @param propertyValues the ordered list of property value chunks that when reconstructed will 
   *                        make up the entire property value
   * @param includePropertyNodes the list of include properties that this embedded node holds
   * @param localPropertyName the name of the property for which this embedded node represents the
   *                          value.
   */
  public EmbeddedIncludePropertyNode(
    List<String>                      propertyValues,
    Map<String, IncludePropertyNode>  includePropertyNodes,
    String                            localPropertyName)
  {
    // The caller of this constructor must have all these values filled out.
    if (propertyValues == null)
      throw new IllegalArgumentException();
    if (includePropertyNodes == null)
      throw new IllegalArgumentException();
    if (localPropertyName == null)
      throw new IllegalArgumentException();
    
    _propertyValues = propertyValues;
    _includePropertyNodes = includePropertyNodes;
    _localPropertyName = localPropertyName;

  }

  /**
   * Returns the name of the style to include.
   */
  public String getLocalPropertyName()
  {
    return _localPropertyName;
  }
  
  /**
   * Returns the property value segments that makes up the entire vlaue for the local property.
   */
  public Iterator<String> getPropertyValues()
  {
    return _propertyValues.iterator();
  }
  
  /**
   * Returns the included property nodes in this embedded node, the key being the placeholder text
   */
  public Map<String, IncludePropertyNode> getIncludedProperties()
  {
    return _includePropertyNodes;
  }

  @Override 
  public boolean equals(Object obj)
  {
    if (this == obj)
      return true;
    if (!(obj instanceof EmbeddedIncludePropertyNode))
      return false;
     
    // obj at this point must be an EmbeddedIncludePropertyNode
    EmbeddedIncludePropertyNode test = (EmbeddedIncludePropertyNode)obj;
    return
      (_localPropertyName == test._localPropertyName 
        || (_localPropertyName != null && _localPropertyName.equals(test._localPropertyName))) 
      && (_propertyValues == test._propertyValues 
        || (_propertyValues != null && _propertyValues.equals(test._propertyValues)))
      && (_includePropertyNodes == test._includePropertyNodes 
        || (_includePropertyNodes != null 
            && _includePropertyNodes.equals(test._includePropertyNodes)));       
  }
  
  @Override
  public int hashCode()
  {
    int hash = 17;
    hash = 37*hash + ((null == _localPropertyName) ? 0 : _localPropertyName.hashCode());
    hash = 37*hash + ((null == _propertyValues) ? 0 : _propertyValues.hashCode());
    hash = 37*hash + ((null == _includePropertyNodes) ? 0 : _includePropertyNodes.hashCode());

    return hash; 
  }  
  
  
  @Override
  public String toString()
  {
    return 
      "[localPropertyName="   + _localPropertyName   + ", " +
      "propertyValues="   + _propertyValues   + ", " +
      "includePropertyNodeMap=" + _includePropertyNodes + "]";
  }

  private final List<String>                      _propertyValues;
  private final Map<String, IncludePropertyNode>  _includePropertyNodes;
  private final String                            _localPropertyName;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    EmbeddedIncludePropertyNode.class);
}
