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

import org.apache.myfaces.trinidadinternal.ui.Renderer;

/**
 * Object which represents a single <renderer> element.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/RendererNode.java#0 $) $Date: 10-nov-2005.18:50:41 $
 * @author The Oracle ADF Faces Team
 */
public class RendererNode
{
  /**
   * Creates a RendererNode.
   * @param namespace The namespace of the component which
   *                  uses the Renderer
   * @param name The name of the component which uses the Renderer
   * @param renderer The Renderer instance
   */
  public RendererNode(
    String   namespace,
    String   name,
    Renderer renderer
    )
  {
    if ((namespace == null)||(name == null)||(renderer == null))
    {
      throw new NullPointerException("Null argument");
    }

    _namespace = namespace;
    _name = name;
    _renderer = renderer;
  }

  /**
   * Returns the namespace of the component which uses
   * the Renderer specified by this RendererNode.
   */
  public String getRendererNamespace()
  {
    return _namespace;
  }

  /**
   * Returns the name of the component which uses
   * the Renderer specified by this RendererNode.
   */
  public String getRendererName()
  {
    return _name;
  }

  /**
   * Returns the Renderer instance for this RendererNode.
   */
  public Renderer getRenderer()
  {
    return _renderer;
  }

  private String      _namespace;
  private String      _name;
  private Renderer    _renderer;
}
