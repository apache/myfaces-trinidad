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
package org.apache.myfaces.trinidadexhibition.metadata.facescontext;

/**
 * @author Andrew Robinson
 */
public class Property
{
  private String _name;
  private String _propertyClass;
  private String _description;
  private String _defaultValue;
  private String _attributeValues;

  /**
   * @return the name
   */
  public String getName()
  {
    return _name;
  }

  /**
   * @param name the name to set
   */
  public void setName(String name)
  {
    _name = name;
  }

  /**
   * @return the propertyClass
   */
  public String getPropertyClass()
  {
    return _propertyClass;
  }

  /**
   * @param propertyClass the propertyClass to set
   */
  public void setPropertyClass(String propertyClass)
  {
    _propertyClass = propertyClass;
  }

  /**
   * @return the description
   */
  public String getDescription()
  {
    return _description;
  }

  /**
   * @param description the description to set
   */
  public void setDescription(String description)
  {
    _description = description;
  }

  /**
   * @return the defaultValue
   */
  public String getDefaultValue()
  {
    return _defaultValue;
  }

  /**
   * @param defaultValue the defaultValue to set
   */
  public void setDefaultValue(String defaultValue)
  {
    _defaultValue = defaultValue;
  }

  /**
   * @return the attributeValues
   */
  public String getAttributeValues()
  {
    return _attributeValues;
  }

  /**
   * @param attributeValues the attributeValues to set
   */
  public void setAttributeValues(String attributeValues)
  {
    _attributeValues = attributeValues;
  }
}
