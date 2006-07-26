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

package org.apache.myfaces.adfinternal.skin.icon;

import java.io.IOException;

import javax.faces.context.FacesContext;
import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;

import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.adfinternal.share.io.InputStreamProvider;
import org.apache.myfaces.adfinternal.share.io.ServletNameResolver;

import org.apache.myfaces.adfinternal.style.Style;

/**
 * An Icon implementation for icons which are under the
 * ServletContext root.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/icon/ContextImageIcon.java#0 $) $Date: 10-nov-2005.18:59:03 $
 * @author The Oracle ADF Faces Team
 */
public class ContextImageIcon extends BaseImageIcon
{
  /**
   * Creates a ContextImageIcon which uses the specified image URI
   * regardless of the reading direction.
   *
   * @param uri The URI to the image, relative to the 
   *            servlet context root.
   * @param width An Integer representing the width of the icon, 
   *              or null if the width is not known.
   * @param height An Integer representing the height of the icon, 
   *               or null if the height is not known.
   */
  public ContextImageIcon(
    String  uri,
    Integer width,
    Integer height
    )
  {
    this(uri, null, width, height, null, null);
  }

  /**
   * Creates an ContextImageIcon which has a different image URI 
   * depending on the reading direction. 
   *
   * @param uri The URI of the left-to-right version of the image, 
   *            relative to the servlet context root.
   * @param rtlURI The URI of the right-to-left version of the image, 
   *            relative to the servlet context root.
   * @param width An Integer representing the width of the icon, 
   *              or null if the width is not known.
   * @param height An Integer representing the height of the icon, 
   *               or null if the height is not known.
   */
  public ContextImageIcon(
    String  uri,
    String  rtlURI,
    Integer width,
    Integer height
    )
  {
    this(uri, rtlURI, width, height, null, null);
  }

  /**
   * Creates an ContextImageIcon which has a different image URI 
   * depending on the reading direction. 
   *
   * @param uri The URI of the left-to-right version of the image, 
   *            relative to the servlet context root.
   * @param rtlURI The URI of the right-to-left version of the image, 
   *            relative to the servlet context root.
   * @param width An Integer representing the width of the icon, 
   *              or null if the width is not known.
   * @param height An Integer representing the height of the icon, 
   *               or null if the height is not known.
   * @param styleClass The style class for the image icon
   * @param inlineStyle The inline style for the image icon
   */
  public ContextImageIcon(
    String  uri,
    String  rtlURI,
    Integer width,
    Integer height,
    String  styleClass,
    Style   inlineStyle
    )
  {
    super(uri, rtlURI, width, height, styleClass, inlineStyle);
  }

  /**
   * Override of Icon.getImageIcon().
   */
  public InputStreamProvider getImageData(
    FacesContext        context,
    AdfRenderingContext arc
    ) throws IOException
  {

    ServletRequest servletRequest = 
      (ServletRequest)context.getExternalContext().getRequest();
    ServletContext servletContext = 
      (ServletContext)context.getExternalContext().getContext();

    if ((servletRequest == null) || (servletContext == null))
      return null;

    ServletNameResolver resolver = new ServletNameResolver(servletRequest,
                                                           servletContext);

    return resolver.getProvider(getRelativeURI(context, arc));
  }

  /**
   * Implementation of BaseImageIcon.getBaseURI().
   * The base URI for ContextImageIcon is the ServletContext's
   * base URI.
   * @todo cache the context uri like we used to when we used the
   * RenderingContext. context.getProperty(MARLIN_NAMESPACE, 
   * _TERMINATED_CONTEXT_URI_PROPERTY)
   */
  protected String getBaseURI(
  FacesContext        context,
  AdfRenderingContext arc)
  {
    return context.getExternalContext().getRequestContextPath() + '/';

  }

}
