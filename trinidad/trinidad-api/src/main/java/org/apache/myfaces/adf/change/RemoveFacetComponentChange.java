/*
 * Copyright  2005,2006 The Apache Software Foundation.
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

package org.apache.myfaces.adf.change;

import java.util.Map;

import javax.faces.component.UIComponent;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * Change specialization for removal of a facet.
 * While applying this Change, if there were to be a facet with the specified
 *  name, it will be removed.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/change/RemoveFacetComponentChange.java#0 $) $Date: 10-nov-2005.19:10:01 $
 * @author The Oracle ADF Faces Team
 */
public class RemoveFacetComponentChange extends ComponentChange
                                        implements DocumentChange
{
  /**
   * Constructs a RemoveFacetChange with the specified name of the facet.
   * @param facetName The name of facet that needs to be removed.
   * @throws IllegalArgumentException if specified facetName is 
   *          <code>null</code>.
   */
  public RemoveFacetComponentChange(String facetName)
  {
    if ((facetName == null) || (facetName.length() == 0))
      throw new IllegalArgumentException(
        "Cannot construct a RemoveFacetChange with null facetName.");
    _facetName = facetName;
  }
  
  /**
   * Returns the name of facet that needs to be removed.
   */
  public String getFacetName()
  {
    return _facetName;
  }
  
  /**
   * {@inheritDoc}
   */
  public void changeComponent(UIComponent uiComponent)
  {
    Map facets = uiComponent.getFacets();
    facets.remove(_facetName);
  }

  /**
   * {@inheritDoc}
   */
  public void changeDocument(Node componentNode)
  {
    Element facetElement = ChangeUtils.__getFacetElement(componentNode, _facetName);
    
    if (facetElement != null)
    {
      componentNode.removeChild(facetElement);
    }
  }

  /** 
   * Returns true if adding the DocumentChange should force the JSP Document
   * to reload
   */
  public boolean getForcesDocumentReload()
  {
    return false;
  }

  private final String _facetName;
}
