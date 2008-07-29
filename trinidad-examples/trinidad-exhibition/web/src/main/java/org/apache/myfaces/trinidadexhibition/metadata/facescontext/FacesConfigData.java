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

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Andrew Robinson
 */
public class FacesConfigData
{
  private Map<String, Component> _components = new HashMap<String, Component>();
  
  public void addData(FacesConfigData data)
  {
    _components.putAll(data.getComponents());
  }
  
  public void addComponent(Component comp)
  {
    _components.put(comp.getComponentType(), comp);
  }
  
  /**
   * @return the components
   */
  public Map<String, Component> getComponents()
  {
    return _components;
  }
}
