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
package org.apache.myfaces.trinidadinternal.style.xml.parse;

/**
 * IncludeStyleNode is used to represent a single <includeStyle> element
 * in a parsed XML Style Sheet Language document.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/IncludeStyleNode.java#0 $) $Date: 10-nov-2005.18:58:08 $
 * @author The Oracle ADF Faces Team
 */
public class IncludeStyleNode
{
  /**
   * Creates an IncludeStyleNode.  In general, either the name or
   * selector of the included style is specified.
   */
  public IncludeStyleNode(String name, String selector)
  {
    _name = name;
    _selector = selector;
  }

  /**
   * Returns the name of the style to include.
   */
  public String getName()
  {
    return _name;
  }

  /**
   * Returns the selector of the style to include.
   */
  public String getSelector()
  {
    return _selector;
  }

  private String _name;
  private String _selector;
}
