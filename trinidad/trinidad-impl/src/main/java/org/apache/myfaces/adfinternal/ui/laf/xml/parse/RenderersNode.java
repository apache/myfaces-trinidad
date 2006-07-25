/*
 * Copyright  2003-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.myfaces.adfinternal.ui.laf.xml.parse;

import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;




/**
 * Object which represents a single <renderers> element.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/RenderersNode.java#0 $) $Date: 10-nov-2005.18:50:42 $
 * @author The Oracle ADF Faces Team
 */
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
        throw new IllegalArgumentException("Null renderers or no renderers passed");
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
  public Iterator getRendererNodes()
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
  public Iterator getFacets()
  {
    if (_facets == null)
      return null;

    return (Arrays.asList(_facets)).iterator();
  }

  private RendererNode[] _renderers;
  private String[]       _facets;
}
