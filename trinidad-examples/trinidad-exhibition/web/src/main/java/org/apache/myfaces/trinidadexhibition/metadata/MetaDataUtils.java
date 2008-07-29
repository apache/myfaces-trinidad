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
package org.apache.myfaces.trinidadexhibition.metadata;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Enumeration;
import java.util.Properties;

import org.apache.commons.digester.Digester;
import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidadexhibition.metadata.facescontext.Component;
import org.apache.myfaces.trinidadexhibition.metadata.facescontext.FacesConfigData;
import org.apache.myfaces.trinidadexhibition.metadata.facescontext.Facet;
import org.apache.myfaces.trinidadexhibition.metadata.facescontext.Property;
import org.apache.myfaces.trinidadexhibition.metadata.tld.Attribute;
import org.apache.myfaces.trinidadexhibition.metadata.tld.Tag;
import org.apache.myfaces.trinidadexhibition.metadata.tld.TagLibrary;
import org.xml.sax.SAXException;

/**
 *
 * @author Andrew Robinson
 */
public final class MetaDataUtils
{
  private MetaDataUtils() {}
  
  public static String getVersion()
    throws IOException
  {
    Properties pomProps = new Properties();
    
    InputStream stream = MetaDataUtils.class.getClassLoader().getResourceAsStream(
      "/META-INF/maven/org.apache.myfaces.trinidad/trinidad-exhibition/pom.properties");
    
    // the file will not be found when using jetty in-place from maven, so attempt to get the value
    // from the api library instead
    stream = UIXComponent.class.getClassLoader().getResourceAsStream(
      "/META-INF/maven/org.apache.myfaces.trinidad/trinidad-api/pom.properties");
    
    pomProps.load(stream);
    return pomProps.getProperty("version");
  }
  
  public static TagLibrary getTagLibrary(String name)
    throws IOException, SAXException
  {
    String path = new StringBuilder("/META-INF/").append(name).append(".tld").toString();
    // first look in the Trinidad-impl
    InputStream stream = MetaDataUtils.class.getResourceAsStream(path);
    if (stream == null)
    {
      return null;
    }
    
    Digester digester = new Digester();
    digester.setValidating(false);
    
    digester.addObjectCreate("taglib", TagLibrary.class);
    digester.addBeanPropertySetter("taglib/short-name", "shortName");
    
    digester.addObjectCreate("taglib/tag", Tag.class);
    digester.addBeanPropertySetter("taglib/tag/name");
    digester.addBeanPropertySetter("taglib/tag/tag-class", "tagClass");
    digester.addBeanPropertySetter("taglib/tag/description");
    digester.addSetNext("taglib/tag", "addTag");
    
    digester.addObjectCreate("taglib/tag/attribute", Attribute.class);
    digester.addBeanPropertySetter("taglib/tag/attribute/name");
    digester.addBeanPropertySetter("taglib/tag/attribute/description");
    digester.addBeanPropertySetter("taglib/tag/attribute/required");
    digester.addBeanPropertySetter("taglib/tag/attribute/deferred-value/type", "deferredValueType");
    digester.addBeanPropertySetter("taglib/tag/attribute/deferred-value/method-signature",
      "deferredValueMethod");
    
    digester.addSetNext("taglib/tag/attribute", "addAttribute");
    
    return (TagLibrary)digester.parse(stream);
  }
  
  public static FacesConfigData getFacesConfigData()
    throws IOException, SAXException
  {
    FacesConfigData data = new FacesConfigData();
    for (Enumeration<URL> e = Thread.currentThread().getContextClassLoader().getResources(
      "META-INF/faces-config.xml"); e.hasMoreElements(); )
    {
      FacesConfigData d = getFacesConfigData(e.nextElement().openStream());
      if (d != null)
      {
        data.addData(d);
      }
    }
    
    return data;
  }
  
  private static FacesConfigData getFacesConfigData(InputStream stream)
    throws IOException, SAXException
  {
    Digester digester = new Digester();
    digester.setValidating(false);
    
    digester.addObjectCreate("faces-config", FacesConfigData.class);
    
    digester.addObjectCreate("faces-config/component", Component.class);
    digester.addBeanPropertySetter("faces-config/component/description");
    digester.addBeanPropertySetter("faces-config/component/component-type", "componentType");
    digester.addBeanPropertySetter("faces-config/component/component-clas", "componentClass");
    digester.addBeanPropertySetter("faces-config/component/component-extension/accepts-child-components",
      "accepts-child-components");
    digester.addBeanPropertySetter("faces-config/component/component-extension/component-family",
      "family");
    digester.addBeanPropertySetter("faces-config/component/component-extension/renderer-type",
      "rendererType");
    digester.addBeanPropertySetter(
      "faces-config/component/component-extension/component-metadata/preferred-children",
      "preferredChildren");
    digester.addSetNext("faces-config/component", "addComponent");
    
    digester.addObjectCreate("faces-config/component/facet", Facet.class);
    digester.addBeanPropertySetter("faces-config/component/facet/facet-name", "name");
    digester.addBeanPropertySetter("faces-config/component/facet/description");
    digester.addBeanPropertySetter(
      "faces-config/component/facet/facet-extension/facet-metadata/preferred-children",
      "preferredChildren");
    digester.addSetNext("faces-config/component/facet", "addFacet");

    digester.addObjectCreate("faces-config/component/property", Property.class);
    digester.addBeanPropertySetter("faces-config/component/property/property-name", "name");
    digester.addBeanPropertySetter("faces-config/component/property/property-class", "propertyClass");
    digester.addBeanPropertySetter("faces-config/component/property/description");
    digester.addBeanPropertySetter("faces-config/component/property/default-value", "defaultValue");
    digester.addBeanPropertySetter(
      "faces-config/component/property/property-extension/property-metadata/attribute-values",
      "attributeValues");
    digester.addSetNext("faces-config/component/property", "addProperty");
    
    return (FacesConfigData)digester.parse(stream);
  }
}
