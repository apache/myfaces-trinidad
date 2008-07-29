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
 * @author Andrew Robinson
 */
public class Component
{
  private String _componentType;
  private String _description;
  private String _componentClass;
  private String _rendererType;
  private String _family;
  private String _preferredChildren;
  private boolean _acceptsChildComponents;
  private Map<String, Facet> _facets = new HashMap<String, Facet>();
  private Map<String, Property> _properties = new HashMap<String, Property>();

  /**
   * @return the preferredChildren
   */
  public String getPreferredChildren()
  {
    return _preferredChildren;
  }
  
  /**
   * @param preferredChildren the preferredChildren to set
   */
  public void setPreferredChildren(String preferredChildren)
  {
    _preferredChildren = preferredChildren;
  }
  
  /**
   * @return the acceptsChildComponents
   */
  public boolean isAcceptsChildComponents()
  {
    return _acceptsChildComponents;
  }
  
  /**
   * @param acceptsChildComponents the acceptsChildComponents to set
   */
  public void setAcceptsChildComponents(boolean acceptsChildComponents)
  {
    _acceptsChildComponents = acceptsChildComponents;
  }
  
  /**
   * @return the rendererType
   */
  public String getRendererType()
  {
    return _rendererType;
  }
  
  /**
   * @param rendererType the rendererType to set
   */
  public void setRendererType(String rendererType)
  {
    _rendererType = rendererType;
  }
  
  /**
   * @return the family
   */
  public String getFamily()
  {
    return _family;
  }
  
  /**
   * @param family the family to set
   */
  public void setFamily(String family)
  {
    _family = family;
  }
  
  /**
   * @return the type
   */
  public String getComponentType()
  {
    return _componentType;
  }

  /**
   * @param componentType the type to set
   */
  public void setComponentType(String componentType)
  {
    _componentType = componentType;
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
   * @return the componentClass
   */
  public String getComponentClass()
  {
    return _componentClass;
  }
  
  /**
   * @param componentClass the componentClass to set
   */
  public void setComponentClass(String componentClass)
  {
    _componentClass = componentClass;
  }
  
  /**
   * @return the facets
   */
  public Map<String, Facet> getFacets()
  {
    return _facets;
  }
  
  /**
   * @return the properties
   */
  public Map<String, Property> getProperties()
  {
    return _properties;
  }
  
  public void addFacet(Facet facet)
  {
    _facets.put(facet.getName(), facet);
  }
  
  public void addProperty(Property prop)
  {
    _properties.put(prop.getName(), prop);
  }
}
