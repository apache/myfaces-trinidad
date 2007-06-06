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
 * Object which represents a single &lt;skin-addition&gt; element in trinidad-skins.xml.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/SkinPropertyNode.java#0 $) $Date: 10-nov-2005.18:50:45 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class SkinAdditionNode implements Comparable<SkinAdditionNode>
{

  /**
   * 
   */
  public SkinAdditionNode (
    String skinId,
    String styleSheetName
    )
  {
    _styleSheetName = styleSheetName;
    _skinId = skinId;
  }
  
  public String getSkinId()
  {
    return _skinId;
  }  
  
  public void setSkinId(String id)
  {
    _skinId = id;
  }
  
  public String getStyleSheetName()
  {
    return _styleSheetName;
  } 
  
  public void setStyleSheetName(String ssName)
  {
    _styleSheetName = ssName;
  }
  
  // Sort by the name of the stylesheet
  public int compareTo(SkinAdditionNode node)
  {
    return getStyleSheetName().compareTo(node.getStyleSheetName());
  }
  
  private String _skinId;
  private String _styleSheetName;

}
