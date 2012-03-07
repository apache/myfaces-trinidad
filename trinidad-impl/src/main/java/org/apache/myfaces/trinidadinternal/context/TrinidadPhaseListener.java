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
package org.apache.myfaces.trinidadinternal.context;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;

import org.apache.myfaces.trinidadinternal.config.xmlHttp.XmlHttpConfigurator;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidadinternal.util.FrameBustingUtils;
import org.apache.myfaces.trinidadinternal.util.FrameBustingUtils.FrameBustingParamValue;

/**
 * Performs some trinidad logic and provides some hooks.
 *
 */
public class TrinidadPhaseListener implements PhaseListener
{
  /**
   * Returns true if the request might be a postback request.
   */
  static public boolean isPostback(FacesContext context)
  {
    
    return !Boolean.FALSE.equals(context.getExternalContext().
                                   getRequestMap().get(_POSTBACK_KEY));
  }
  
  /**
   * Marks that this is a postback request.
   */
  static public void markPostback(FacesContext context)
  {
    context.getExternalContext().getRequestMap().remove(_POSTBACK_KEY);
  }

  @SuppressWarnings("unchecked")
  public void afterPhase(PhaseEvent event)
  {
    FacesContext context = event.getFacesContext();

    if (event.getPhaseId() == PhaseId.RESTORE_VIEW)
    {
      // Store off the current ViewRoot so we can check for a full page
      // render in response to a partial event.
      context.getExternalContext().getRequestMap().put(INITIAL_VIEW_ROOT_KEY,
                                                       context.getViewRoot());
    }
  }

  @SuppressWarnings("unchecked")
  public void beforePhase(PhaseEvent event)
  {
    
    PhaseId phaseId = event.getPhaseId();
    
    // Ensure that the implicit object gets created.  In general,
    // "restore view" would be sufficient, but someone can call
    // renderResponse() before even calling Lifecycle.execute(),
    // in which case RESTORE_VIEW doesn't actually run.
    if (phaseId == PhaseId.RESTORE_VIEW)
    {
      FacesContext context = event.getFacesContext();
      ExternalContext ec = context.getExternalContext();
      // Assume it's not a postback request
      ec.getRequestMap().put(_POSTBACK_KEY, Boolean.FALSE);
    }
    // If we've reached "apply request values", this is definitely a
    // postback (the ViewHandler should have reached the same conclusion too,
    // but make sure)
    else if (phaseId == PhaseId.APPLY_REQUEST_VALUES)
    {
      FacesContext context = event.getFacesContext();
      markPostback(context);
    }
       else if (phaseId == PhaseId.RENDER_RESPONSE)
    {    
      // add response headers for framebusting if needed

      FacesContext context = event.getFacesContext();
      FrameBustingParamValue frameBusting = FrameBustingUtils.getFrameBustingValue(context, RequestContext.getCurrentInstance());        
  
      if (! FrameBustingParamValue.FRAME_BUSTING_NEVER.equals(frameBusting))
      {
        // TODO: support CSP?
        // https://dvcs.w3.org/hg/content-security-policy/raw-file/tip/csp-specification.dev.html 
        
        // the X-Frame-Options header doesn't work on all browsers, but we're adding it anyway
        String xFrameOptions = (FrameBustingParamValue.FRAME_BUSTING_ALWAYS.equals(frameBusting))
                                  ? "deny"
                                  : "sameorigin";

        context.getExternalContext().addResponseHeader("X-Frame-Options", xFrameOptions); 
      }
    }    
  }


  public PhaseId getPhaseId()
  {
    return PhaseId.ANY_PHASE;
  }
  
  static public final String INITIAL_VIEW_ROOT_KEY =
    "org.apache.myfaces.trinidadinternal.InitialViewRoot";

  static private final String _POSTBACK_KEY =
    "org.apache.myfaces.trinidadinternal.context.AdfFacesPhaseListener.POSTBACK";

  private static final long serialVersionUID = 1234567L;
}
