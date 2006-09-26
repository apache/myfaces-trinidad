/*
 * Copyright  2000-2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.util.Map;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.RequestContext;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;

import org.apache.myfaces.trinidadinternal.context.RequestContextImpl;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.PartialPageContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.ppr.PartialPageContextImpl;

/**
 * Utility methods for Renderers which support partial page rendering.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/PartialPageUtils.java#0 $) $Date: 10-nov-2005.19:01:41 $
 * @author The Oracle ADF Faces Team
 */
public final class PartialPageUtils
{
  private PartialPageUtils()
  {
  }

  public static boolean isPartialRequest(FacesContext context)
  {
    return RequestContext.getCurrentInstance().isPartialRequest(context);
  }


  /**
   * Force partial rendering on for requests that may not have sent
   * a "partial" parameter.
   * @todo This is probably unnecessary.
   */
  @SuppressWarnings("unchecked")
  public static void forcePartialRendering(FacesContext context)
  {
    Map<String, Object> requestMap = 
      context.getExternalContext().getRequestMap();
    
    requestMap.put(RequestContextImpl.FORCED_PARTIAL_KEY, Boolean.TRUE);
  }

  public static PartialPageContext createPartialPageContext(
    FacesContext    context,
    RequestContext afContext)
  {
    if (isPartialRequest(context))
    {
      // Create the PartialPageContext
      return new PartialPageContextImpl(afContext);
    }

    return null;
  }

  /**
   * Returns true if we are performing a partial page render.
   */
  public static boolean isPartialRenderingPass(
    RenderingContext arc
    )
  {
    PartialPageContext pprContext = arc.getPartialPageContext();
    return (pprContext != null);
  }

  /**
   * Tests whether partial page rendering is supported for the
   * current render.
   * <p>
   * Partial page rendering is not supported on all user agents.
   * This method returns false if partial page rendering is not supported
   * by the agent associated with the provided RenderingContext.
   * <p>
   * This method returns false if the disable-partial-rendering configuration 
   * element is set to true. Otherwise, this method returns true.
   * (PPR is considered accessible, so we do not check the accessibility mode)
   */
  public static boolean supportsPartialRendering(
    RenderingContext arc
    )
  {

    // First, make sure the agent supports partial rendering
    Agent agent = arc.getAgent();
    Object capPartial = agent.getCapabilities().get(TrinidadAgent.CAP_PARTIAL_RENDERING);
    if (!Boolean.TRUE.equals(capPartial))
      return false;

    return true;
  }

  public static boolean supportsBlocking(
    RenderingContext arc
    )
  {
    // At the moment we have blocking solved on IE and Mozilla
    if (supportsPartialRendering(arc))
    {
      return (XhtmlRenderer.isIE(arc) || XhtmlRenderer.isGecko(arc));
    }
    return false;
  }


  /**
   * Test if PPR is active during rendering.
   */
  @SuppressWarnings("unchecked")
  public static boolean isPPRActive(FacesContext context)
  {
    Map<String, Object> requestScope =
      context.getExternalContext().getRequestMap();
    
    return Boolean.TRUE.equals(requestScope.get(_PPR_ACTIVE_FLAG_NAME));
  }

  /**
   * Mark that PPR is in fact active during rendering.
   */
  @SuppressWarnings("unchecked")
  public static void markPPRActive(FacesContext context)
  {
    Map<String, Object> requestScope =
      context.getExternalContext().getRequestMap();
    
    requestScope.put(_PPR_ACTIVE_FLAG_NAME, Boolean.TRUE);
  }

  // Flag used to store info on the context about whether
  // an iFrame is built yet.
  private static final String _PPR_ACTIVE_FLAG_NAME =
          "org.apache.myfaces.trinidadinternal.renderkit._pprActiveOnPage";
}
