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
package org.apache.myfaces.trinidadexhibition.metadata.tld;

public class Attribute
{
  private String _name;
  private String _description;
  private String _deferredValueType;
  private String _deferredValueMethod;
  private boolean _required;

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
   * @return the deferredValueType
   */
  public String getDeferredValueType()
  {
    return _deferredValueType;
  }

  /**
   * @param deferredValueType the deferredValueType to set
   */
  public void setDeferredValueType(String deferredValueType)
  {
    _deferredValueType = deferredValueType;
  }

  /**
   * @return the deferredValueMethod
   */
  public String getDeferredValueMethod()
  {
    return _deferredValueMethod;
  }

  /**
   * @param deferredValueMethod the deferredValueMethod to set
   */
  public void setDeferredValueMethod(String deferredValueMethod)
  {
    _deferredValueMethod = deferredValueMethod;
  }

  /**
   * @return the required
   */
  public boolean isRequired()
  {
    return _required;
  }

  /**
   * @param required the required to set
   */
  public void setRequired(boolean required)
  {
    _required = required;
  }
}