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

import java.util.HashMap;
import java.util.Map;

public class Tag
{
  private String _name;
  private String _description;
  private String _tagClass;
  private Map<String, Attribute> _attributes = new HashMap<String, Attribute>();

  /**
   * @return the tagClass
   */
  public String getTagClass()
  {
    return _tagClass;
  }
  
  /**
   * @param tagClass the tagClass to set
   */
  public void setTagClass(String tagClass)
  {
    _tagClass = tagClass;
  }
  
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
   * @return the attributes
   */
  public Map<String, Attribute> getAttributes()
  {
    return _attributes;
  }
  
  public void addAttribute(Attribute attr)
  {
    _attributes.put(attr.getName(), attr);
  }
}