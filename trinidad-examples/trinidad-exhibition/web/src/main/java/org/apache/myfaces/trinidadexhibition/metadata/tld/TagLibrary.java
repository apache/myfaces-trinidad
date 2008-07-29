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

/**
 *
 * @author Andrew Robinson
 */
public class TagLibrary
{
  private String _shortName;
  private Map<String, Tag> _tags = new HashMap<String, Tag>();

  /**
   * @return the shortName
   */
  public String getShortName()
  {
    return _shortName;
  }

  /**
   * @param shortName the shortName to set
   */
  public void setShortName(String shortName)
  {
    _shortName = shortName;
  }
  
  public void addTag(Tag tag)
  {
    _tags.put(tag.getName(), tag);
  }
  
  /**
   * @return the tags
   */
  public Map<String, Tag> getTags()
  {
    return _tags;
  }
}
