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
package org.apache.myfaces.trinidadinternal.ui.laf.xml.parse;

/**
 * Object which represents a single &lt;skin&gt; element in trinidad-skins.xml.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/SkinPropertyNode.java#0 $) $Date: 10-nov-2005.18:50:45 $
 */
public class SkinNode
{
  /**
   * 
   */
  public SkinNode (
    String id,
    String family,
    String renderKitId,
    String skinExtends,
    String styleSheetName,
    String bundleName)
  {
    
    if (id==null)
    {
      throw new NullPointerException("Null id");
    }    
    if (family==null)
    {
      throw new NullPointerException("Null family");
    }

    _id = id;
    _family = family;
    _renderKitId = renderKitId;
    _skinExtends = skinExtends;
    _styleSheetName = styleSheetName;
    _bundleName = bundleName;

  }

  /**
   * Returns the skin id for this node
   */
  public String getId()
  {
    return _id ;
  }

  /**
   * Returns the skin family for this node
   */
  public String getFamily()
  {
    return _family;
  }

  /**
   * Returns the renderKitId for this node.
   */
  public String getRenderKitId()
  {
    return _renderKitId;
  }
  
  /**
   * Returns the skinExtends for this node.
   */
  public String getSkinExtends()
  {
    return _skinExtends;
  } 
  
  /**
   * Returns the styleSheetName for this node.
   */
  public String getStyleSheetName()
  {
    return _styleSheetName;
  }  
  
  /**
   * Returns the bundleName for this node.
   */
  public String getBundleName()
  {
    return _bundleName;
  }  

  
  private String _id;
  private String _family;
  private String _renderKitId;
  private String _skinExtends;
  private String _styleSheetName;
  private String _bundleName;


}
