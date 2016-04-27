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
 * SkinFeatures represent the features supported by a Skin.
 * This API makes it convinient to read the content of features tag in trinidad-skins.xml
 * such as:
 * <features>
 *   <feature name="BORDER_STYLE">simple</feature>
 *   <feature name="COLOR_ALIAS_GROUP">alta</feature>
 * </features>
 */
public class SkinFeatures
{
  public SkinFeatures()
  {
    _features = new HashMap<String, String>(1);
  }

  public SkinFeatures(Map<String, String> features)
  {
    _features = new HashMap<String, String>(features);
  }

  public Map<String, String> getFeatures()
  {
    return Collections.unmodifiableMap(_features);
  }

  public String getFeature(String feature)
  {
    return _features.get(feature);
  }

  public boolean isEmpty()
  {
    return _features.isEmpty();
  }

  public String toString()
  {
    return "SkinFeatures: " + _features;
  }

  @Override
  public boolean equals(Object o)
  {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    return _features.equals(((SkinFeatures) o)._features);
  }


  @Override
  public int hashCode()
  {
    return _features.hashCode();
  }

  private final Map<String, String> _features;
}