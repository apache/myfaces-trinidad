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
package org.apache.myfaces.trinidadexhibition.webapp;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.faces.FacesException;
import javax.servlet.jsp.tagext.JspTag;

import org.apache.myfaces.trinidad.webapp.UIXComponentTag;
import org.apache.myfaces.trinidadexhibition.metadata.MetaDataUtils;
import org.apache.myfaces.trinidadexhibition.metadata.facescontext.FacesConfigData;
import org.apache.myfaces.trinidadexhibition.metadata.tld.Tag;
import org.apache.myfaces.trinidadexhibition.metadata.tld.TagLibrary;

/**
 *
 * @author Andrew Robinson
 */
public class MetaDataBean
{
  private String _version;
  private Map<String, TagLibrary> _tagLibraries;
  private FacesConfigData _facesConfigData;
  private Map<String, String> _componentTypeToTagName = new HashMap<String, String>();
  private Map<String, String> _tagNameToComponentType = new HashMap<String, String>();
  
  public String getVersion()
    throws IOException
  {
    if (_version == null)
    {
      synchronized (this)
      {
        if (_version == null)
        {
          _version = MetaDataUtils.getVersion();
        }
      }
    }
    return _version;
  }
  
  /**
   * @return the tagLibraries
   */
  public Map<String, TagLibrary> getTagLibraries()
  {
    initTagLibraries();
    return _tagLibraries;
  }
  
  /**
   * @return the facesConfigData
   */
  public FacesConfigData getFacesConfigData()
  {
    initFacesConfigData();
    return _facesConfigData;
  }
  
  /**
   * @return the componentTypeToTagName
   */
  public Map<String, String> getComponentTypeToTagName()
  {
    return _componentTypeToTagName;
  }
  
  /**
   * @return the tagNameToComponentType
   */
  public Map<String, String> getTagNameToComponentType()
  {
    return _tagNameToComponentType;
  }
  
  private void initFacesConfigData()
  {
    if (_facesConfigData != null) return;
    synchronized (this)
    {
      if (_facesConfigData != null) return;
      
      try
      {
        _facesConfigData = MetaDataUtils.getFacesConfigData();
      }
      catch (Exception ex)
      {
        throw new FacesException("Failed to parse faces-config data", ex);
      }
    }
  }

  private void initTagLibraries()
  {
    if (_tagLibraries != null) return;
    synchronized (this)
    {
      if (_tagLibraries != null) return;
      _tagLibraries = new HashMap<String, TagLibrary>();
      
      try
      {
        _tagLibraries.put("tr", MetaDataUtils.getTagLibrary("tr"));
        _tagLibraries.put("trh", MetaDataUtils.getTagLibrary("trh"));
        _tagLibraries.put("trs", MetaDataUtils.getTagLibrary("trs"));
        
        for (TagLibrary library : _tagLibraries.values())
        {
          for (Tag tag : library.getTags().values())
          {
            @SuppressWarnings("unchecked")
            Class<JspTag> tagClass = (Class<JspTag>)Class.forName(tag.getTagClass());
            
            if (UIXComponentTag.class.isAssignableFrom(tagClass))
            {
              UIXComponentTag jspTag = (UIXComponentTag)tagClass.newInstance();
              String tagName = new StringBuilder(library.getShortName())
                .append(':').append(tag.getName()).toString();
              String compType = jspTag.getComponentType();
              _componentTypeToTagName.put(compType, tagName);
              _tagNameToComponentType.put(tagName, compType);
            }
          }
        }
      }
      catch (Exception ex)
      {
        throw new FacesException("Failed to parse TLD information", ex);
      }
    }
  }
}
