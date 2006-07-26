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
package org.apache.myfaces.adfinternal.renderkit.core;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.adfinternal.agent.AdfFacesAgent;
import org.apache.myfaces.adfinternal.agent.CapabilityKey;
import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;

public class CoreRendererUtils
{
  private CoreRendererUtils()
  {
  }

  public static String getRelativeId(
    FacesContext context,
    UIComponent  from,
    String       relativeId)
  {
    if (relativeId == null)
      return null;

    UIComponent parentNC = _getParentNamingContainer(from.getParent());
    if (parentNC == null)
      return relativeId;

    return (parentNC.getClientId(context) +
            NamingContainer.SEPARATOR_CHAR + relativeId);
  }


  /**
   * Tests whether partial page rendering is supported for the
   * current render.
   * <p>
   * Partial page rendering is not supported on all user agents.
   * This method returns false if partial page rendering is not supported
   * by the agent associated with the provided rendering context.
   */
  public static boolean supportsPartialRendering(
    AdfRenderingContext context
    )
  {
    return supportsBooleanCapability(context,
                                     AdfFacesAgent.CAP_PARTIAL_RENDERING);
  }

  public static boolean supportsNameIdentification(
    AdfRenderingContext context
    )
  {
    return supportsBooleanCapability(context,
                                     AdfFacesAgent.CAP_NAME_IDENTIFICATION);
  }

  public static boolean supportsBooleanCapability(
    AdfRenderingContext context,
    CapabilityKey cap
    )
  {
    AdfFacesAgent agent = context.getAgent();
    Object capPartial = agent.getCapability(cap);
    if (!Boolean.TRUE.equals(capPartial))
      return false;

    return true;
  }

  private static UIComponent _getParentNamingContainer(UIComponent from)
  {
    while (from != null)
    {
      if (from instanceof NamingContainer)
        return from;
      from = from.getParent();
    }

    return null;
  }
}
