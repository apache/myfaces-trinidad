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
package org.apache.myfaces.adf.render;

import java.util.Iterator;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;

/**
 * TraversingRenderer enhances the general component renderer contract, to
 * control all child component hierarchy traversal and local client id
 * generation.
 *
 * @author The Oracle ADF Faces Team
 */
abstract public class ExtendedRenderer extends Renderer
                                       implements LifecycleRenderer
{
  /**
   * Indicates that this Renderer takes responsibility for encoding
   * its child components.
   *
   * @return true, always
   */
  public final boolean getRendersChildren()
  {
    return true;
  }

  /**
   * Decodes a component and its children.
   *
   * @param context    the Faces context
   * @param component  the component to render
   */
  public void decodeChildren(
    FacesContext context,
    UIComponent  component)
  {
    // Process all the facets and children of this component
    Iterator kids = component.getFacetsAndChildren();
    while (kids.hasNext())
    {
      UIComponent kid = (UIComponent) kids.next();
      kid.processDecodes(context);
    }
  }

  /**
   * Validates a component and its children.
   *
   * @param context    the Faces context
   * @param component  the component to render
   */
  public void validateChildren(
    FacesContext context,
    UIComponent  component)
  {
    // Process all the facets and children of this component
    Iterator kids = component.getFacetsAndChildren();
    while (kids.hasNext())
    {
      UIComponent kid = (UIComponent) kids.next();
      kid.processValidators(context);
    }
  }

  /**
   * Updates a component and its children.
   *
   * @param context    the Faces context
   * @param component  the component to render
   */
  public void updateChildren(
    FacesContext context,
    UIComponent  component)
  {
    // Process all the facets and children of this component
    Iterator kids = component.getFacetsAndChildren();
    while (kids.hasNext())
    {
      UIComponent kid = (UIComponent) kids.next();
      kid.processUpdates(context);
    }
  }
}
