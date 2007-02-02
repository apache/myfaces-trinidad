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

import java.util.Map;

import javax.faces.context.FacesContext;
import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.context.RequestContextFactory;
import org.apache.myfaces.trinidadinternal.webapp.TrinidadFilterImpl;

/**
 * PhaseListener that hacks to ensure that the RequestContext is
 * available even if the filter doesn't execute.
 *
 * @author The Oracle ADF Faces Team
 */
public class TrinidadPhaseListener implements PhaseListener
{
  /**
   * 
   */
  private static final long serialVersionUID = -1249678874100309402L;

  static public final String CACHED_REQUEST_CONTEXT =
    "org.apache.myfaces.trinidadinternal.context.CachedRequestContext";

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

    // If we've finished up Render Response, or for some other
    // reason the response is complete, free up the RequestContext
    // if we created.
    // Note, however, that this code is *not* bulletproof!  There
    // is nothing stopping an "afterPhase()" listener getting called
    // after this one that calls responseComplete(), in which case
    // we'd never get notified.
    if ((event.getPhaseId() == PhaseId.RENDER_RESPONSE) ||
        (event.getFacesContext().getResponseComplete()))
    {
      _releaseContextIfNecessary(event.getFacesContext());
      FacesContextFactoryImpl.endRequestIfNecessary(context);
    }
        
  }

  @SuppressWarnings("unchecked")
  public void beforePhase(PhaseEvent event)
  {
    // Ensure that the implicit object gets created.  In general,
    // "restore view" would be sufficient, but someone can call
    // renderResponse() before even calling Lifecycle.execute(),
    // in which case RESTORE_VIEW doesn't actually run.
    if ((event.getPhaseId() == PhaseId.RESTORE_VIEW) ||
        (event.getPhaseId() == PhaseId.RENDER_RESPONSE))
    {
      if (event.getPhaseId() == PhaseId.RESTORE_VIEW)
      {
        FacesContext context = event.getFacesContext();
        // Assume it's not a postback request
        context.getExternalContext().getRequestMap().put(_POSTBACK_KEY,
                                                         Boolean.FALSE);
        
        //This check doesn't make sense here
        //TrinidadFilterImpl.verifyFilterIsInstalled(context);
      }

      _createContextIfNecessary(event.getFacesContext());
    }
    // If we've reached "apply request values", this is definitely a
    // postback (the ViewHandler should have reached the same conclusion too,
    // but make sure)
    else if (event.getPhaseId() == PhaseId.APPLY_REQUEST_VALUES)
    {
      FacesContext context = event.getFacesContext();
      markPostback(context);
    }
  }


  public PhaseId getPhaseId()
  {
    return PhaseId.ANY_PHASE;
  }

  //
  // Create the RequestContext if necessary;  ideally, this is unnecessary
  // because our filter will have executed - but if not, deal.
  //
  @SuppressWarnings("unchecked")
  static private void _createContextIfNecessary(FacesContext fContext)
  {
    
    Map<String, Object> requestMap = fContext.getExternalContext().getRequestMap();
    Boolean createdContext = (Boolean)
      requestMap.get(_CREATED_CONTEXT_KEY);
    if (createdContext == null)
    {
      RequestContext context = RequestContext.getCurrentInstance();
      // Let our code know if it has to clean up.
      requestMap.put(_CREATED_CONTEXT_KEY,
                     context == null ? Boolean.TRUE : Boolean.FALSE);

      if (context == null)
      {
        Object cachedRequestContext = requestMap.get(CACHED_REQUEST_CONTEXT);
        
        // Catch both the null scenario and the 
        // RequestContext-from-a-different-classloader scenario
        if (cachedRequestContext instanceof RequestContext)
        {
          context = (RequestContext) cachedRequestContext;
          context.attach();
        }
        else
        {
          RequestContextFactory factory = RequestContextFactory.getFactory();
          if (factory == null)
          {
            RequestContextFactory.setFactory(new RequestContextFactoryImpl());
            factory = RequestContextFactory.getFactory();
          }

          assert(factory != null);
          context = factory.createContext(fContext.getExternalContext());
          requestMap.put(CACHED_REQUEST_CONTEXT, context);
        }
      }
    }
  }

  //
  // Release the RequestContext if we created it.
  //
  static private void _releaseContextIfNecessary(FacesContext fContext)
  {
    Boolean createdContext = (Boolean)
      fContext.getExternalContext().getRequestMap().get(_CREATED_CONTEXT_KEY);
    if (Boolean.TRUE.equals(createdContext))
    {
      RequestContext context = RequestContext.getCurrentInstance();
      if (context != null)
        context.release();
    }
  }
  
  static public final String INITIAL_VIEW_ROOT_KEY =
    "org.apache.myfaces.trinidadinternal.InitialViewRoot";

  static private final String _CREATED_CONTEXT_KEY =
    "org.apache.myfaces.trinidadinternal.context.AdfFacesPhaseListener.CREATED_CONTEXT";

  static private final String _POSTBACK_KEY =
    "org.apache.myfaces.trinidadinternal.context.AdfFacesPhaseListener.POSTBACK";
  
    
}
