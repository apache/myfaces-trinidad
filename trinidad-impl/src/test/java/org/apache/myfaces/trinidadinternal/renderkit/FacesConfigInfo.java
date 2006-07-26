/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.trinidadinternal.renderkit;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;
import java.util.TreeMap;

import javax.faces.application.Application;
import javax.faces.render.RenderKit;
import javax.faces.render.Renderer;

import org.apache.myfaces.trinidad.logging.ADFLogger;

import org.apache.myfaces.trinidadinternal.XMLValidityTestCase;
import org.apache.myfaces.trinidadinternal.share.xml.TreeBuilder;
import java.util.List;

public class FacesConfigInfo
{
  public FacesConfigInfo()
  {
  }

  public void registerConverters(Application appl)
  {
    Iterator convertersByType = _convertersByType.keySet().iterator();
    while (convertersByType.hasNext())
    {
      try
      {
        String type = (String) convertersByType.next();
        String converterType = (String) _convertersByType.get(type);
        appl.addConverter(Class.forName(type), converterType);
      }
      catch (Exception e)
      {
        _LOG.severe(e);
      }
    }

    Iterator convertersById = _convertersById.keySet().iterator();
    while (convertersById.hasNext())
    {
      try
      {
        String id = (String) convertersById.next();
        String converterType = (String) _convertersById.get(id);
        appl.addConverter(id, converterType);
      }
      catch (Exception e)
      {
        _LOG.severe(e);
      }
    }
  }

  public void registerComponents(Application appl)
  {
    Iterator components = _components.keySet().iterator();
    while (components.hasNext())
    {
      String type = (String) components.next();
      ComponentInfo info = (ComponentInfo) _components.get(type);
      String className = info.componentClass;
      appl.addComponent(type, className);
    }
  }

  public Map<String, RenderKit> getRenderKits()
  {
    return _renderKits;
  }

  public void addConverterByType(String type, String converter)
  {
    _convertersByType.put(type, converter);
  }

  public void addConverterById(String id, String converter)
  {
    _convertersById.put(id, converter);
  }

  public void addComponent(
    ComponentInfo info)
  {
    String componentType = info.componentType;
    if (_LOG.isFine())
      _LOG.fine("Found component: type {0}, class {1}",
                new Object[]{componentType, info.componentClass});
    if (_components.containsKey(componentType))
      _LOG.warning("faces-config.xml contains duplicate definitions for " +
                   "component type {0}", componentType);

    _components.put(componentType, info);
  }

  public ComponentInfo getComponentInfo(String componentType)
  {
    return (ComponentInfo) _components.get(componentType);
  }
  
  public void load(String file) throws IOException, SAXException
  {
    TreeBuilder builder = new TreeBuilder();
    XMLValidityTestCase.ER er = new XMLValidityTestCase.ER();
    String publicID = 
      "-//Sun Microsystems, Inc.//DTD JavaServer Faces Config 1.1//EN";
    URL dtdSource = getClass().getResource(
       "/oracle/adfinternal/view/faces/web-facesconfig_1_1.dtd");
    er.registerPublicId(publicID, dtdSource);
    builder.setEntityResolver(er);

    Enumeration resources = getClass().getClassLoader().getResources(file);
    while (resources.hasMoreElements())
    {
      URL resource = (URL) resources.nextElement();
      _LOG.info("PARSING " + resource);
      InputStream inputStream = null;
      // Try to get the inputStream off of a file
      if ("file".equalsIgnoreCase(resource.getProtocol()))
      {
        File resourceFile = new File(resource.getFile().replaceAll("%20", " "));
        if (resourceFile.exists())
        {
          inputStream = new FileInputStream(resourceFile);
        }
      }

      if (inputStream == null)
        inputStream = resource.openStream();

      try
      {
        InputSource source = new InputSource(inputStream);
        source.setSystemId(resource.toExternalForm());
        builder.parse(source, new FacesConfigParser(this));
      }
      finally
      {
        inputStream.close();
      }
    }
  }

  static public class ComponentInfo
  {
    public String componentType;
    public String componentClass;
    // Use a TreeMap so the properties always come back in 
    // sorted order.
    public Map properties = new TreeMap();
    public PropertyInfo getPropertyInfo(String name)
    {
      return (PropertyInfo) properties.get(name);
    }
  }

  static public class PropertyInfo
  {
    public Class type;
    public Object defaultValue;
    public List  enumValues;
  }

  private Map _components = new HashMap();
  private Map<String, RenderKit> _renderKits  = new HashMap<String, RenderKit>();
  private Map _convertersByType  = new HashMap();
  private Map _convertersById  = new HashMap();

  private static final ADFLogger _LOG =
    ADFLogger.createADFLogger(FacesConfigInfo.class);
}