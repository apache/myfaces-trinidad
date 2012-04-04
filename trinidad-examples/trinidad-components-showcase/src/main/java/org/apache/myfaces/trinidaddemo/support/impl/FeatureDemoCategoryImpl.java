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
package org.apache.myfaces.trinidaddemo.support.impl;

import org.apache.myfaces.trinidaddemo.support.IFeatureDemoCategory;
import org.apache.myfaces.trinidaddemo.support.FeatureDemoCategoryId;
import org.apache.myfaces.trinidaddemo.support.IFeatureDemo;

import java.util.List;
import java.util.ArrayList;

/**
 * Default implementation of the {@link IFeatureDemoCategory} interface.
 */
public class FeatureDemoCategoryImpl implements IFeatureDemoCategory {

    private static final long serialVersionUID = -7078447662522060454L;

    private FeatureDemoCategoryId id;
  private String name;

  private List<IFeatureDemo> featureDemos;

    /**
   * Constructor.
   *
   * @param id the unique id of this category.
   * @param name the name of this category.
   */
  public FeatureDemoCategoryImpl(FeatureDemoCategoryId id, String name) {
    this.id = id;
    this.name = name;

    featureDemos = new ArrayList<IFeatureDemo>();
  }

  public void addFeatureDemo(IFeatureDemo featureDemo) {
    featureDemo.setCategory(this);
    featureDemos.add(featureDemo);
  }

  public List<IFeatureDemo> getFeatureDemos() {
    return featureDemos;
  }

  public FeatureDemoCategoryId getId() {
    return id;
  }

  public String getName() {
    return name;
  }

    @Override
  public boolean equals(Object obj) {
    if (obj instanceof FeatureDemoCategoryImpl) {
      return getId() == ((FeatureDemoCategoryImpl)obj).getId();
    }

    return false;
  }

  @Override
  public int hashCode() {
    return getId() != null ? getId().hashCode() : super.hashCode();
  }

  @Override
  public String toString() {
    return getName();
  }
}
