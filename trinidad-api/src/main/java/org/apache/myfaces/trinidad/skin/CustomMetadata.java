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
package org.apache.myfaces.trinidad.skin;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * CustomMetadata represents the metadata associated with a Skin.
 * This API makes it convinient to read the content of metadata tag in trinidad-skins.xml
 * such as:
 * <metadata>
 *    <visibility>deprecated</visibility>
 * </metadata>
 */
public class CustomMetadata
{
  public CustomMetadata()
  {
    _metadata = new HashMap<String, Object>(1);
  }

  public CustomMetadata(Map<String, Object> metadata)
  {
    _metadata = new HashMap<String, Object>(metadata);
  }

  public Map<String, Object> getMetadata()
  {
    return Collections.unmodifiableMap(_metadata);
  }

  public Object getMetadata(String metadataKey)
  {
    return _metadata.get(metadataKey);
  }

  public boolean isEmpty()
  {
    return _metadata.isEmpty();
  }

  public String toString()
  {
    return "CustomMetadata: " + _metadata;
  }

  @Override
  public boolean equals(Object o)
  {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    return _metadata.equals(((CustomMetadata) o)._metadata);
  }


  @Override
  public int hashCode()
  {
    return _metadata.hashCode();
  }

  private final Map<String, Object> _metadata;
}