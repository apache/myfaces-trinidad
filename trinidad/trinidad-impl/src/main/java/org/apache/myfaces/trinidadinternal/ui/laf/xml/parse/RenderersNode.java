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

import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;




/**
 * Object which represents a single <renderers> element.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/RenderersNode.java#0 $) $Date: 10-nov-2005.18:50:42 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class RenderersNode
{
  /**
   * Creates a RenderersNode
   */
  public RenderersNode(
    RendererNode[] renderers,
    String[]       facets
    )
  {
          // Make sure that we have some Renderers to register.
      // Otherwise, RenderersNodeParser shouldn't even bother
      // creating the RenderersNode.
      if ((renderers == null) || (renderers.length <= 0))
      {
        throw new IllegalArgumentException(_LOG.getMessage(
          "NULL_RENDERERS_NO_RENDERERS_PASSED"));
      }


    // We could make a copy of these arrays, but RenderersNodeParser
    // is the only creator of RenderersNode objects, so copying the
    // arrays seems pointless.
    _renderers = renderers;
    _facets = facets;
  }

  /**
   * Returns an Iterator which iterates over the RendererNode
   * children of this RenderersNode.
   */
  @SuppressWarnings("unchecked")
  public Iterator<RendererNode> getRendererNodes()
  {
    if (_renderers!=null)
    {
      return (Arrays.asList(_renderers)).iterator();
    }
    else
      return (Collections.EMPTY_LIST).iterator();    
    
  }

  /**
   * Returns an Iterator which iterates over the String
   * facets of this RenderersNode.
   *
   * Important note: This method always returns null if
   * no facets were specified.  This is necessary because it
   * is used to signal LookAndFeelExtensionParser that the
   * Renderers should be registered for all facets.
   */
  public Iterator<String> getFacets()
  {
    if (_facets == null)
      return null;

    return (Arrays.asList(_facets)).iterator();
  }

  private RendererNode[] _renderers;
  private String[]       _facets;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    RenderersNode.class);
}
