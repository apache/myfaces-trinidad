package org.apache.myfaces.trinidadinternal.lifecycle;

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.PartialPageUtils;
import org.apache.myfaces.trinidad.render.XhtmlConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreResponseStateManager;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;

import javax.el.MethodExpression;
import javax.faces.FacesException;
import javax.faces.FactoryFinder;
import javax.faces.component.UIComponent;
import javax.faces.component.ContextCallback;
import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;
import javax.faces.lifecycle.Lifecycle;
import javax.faces.lifecycle.LifecycleFactory;
import javax.faces.webapp.FacesServlet;
import java.util.ArrayList;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;
import java.io.IOException;

public class UIViewRoot extends javax.faces.component.UIViewRoot
{
  private static final TrinidadLogger LOG = TrinidadLogger.createTrinidadLogger(UIViewRoot.class);

  private static final ContextCallback RENDER_RESPONSE_CALLBACK = new RenderResponseCallback();
  private static final ContextCallback PROCESS_VALIDATION_CALLBACK = new ProcessValidationsCallback();
  private static final ContextCallback UPDATE_MODEL_VALUES_CALLBACK = new UpdateModelValuesCallback();

  public static final int ANY_PHASE_ORDINAL = PhaseId.ANY_PHASE.getOrdinal();

  private List<FacesEvent> _events;
  private List<PhaseListener> _phaseListeners;

  private transient Lifecycle _lifecycle = null;

  public void addPhaseListener(PhaseListener phaseListener)
  {
    if (phaseListener == null)
    {
      throw new NullPointerException("phaseListener");
    }
    if (_phaseListeners == null)
    {
      _phaseListeners = new ArrayList<PhaseListener>();
    }

    _phaseListeners.add(phaseListener);
  }

  /**
   * Removes a The phaseListeners attached to ViewRoot.
   */
  public void removePhaseListener(PhaseListener phaseListener)
  {
    if (phaseListener == null || _phaseListeners == null)
    {
      return;
    }

    _phaseListeners.remove(phaseListener);
  }

  public void queueEvent(FacesEvent event)
  {
    if (_events == null)
    {
      _events = new ArrayList<FacesEvent>();
    }
    _events.add(event);
  }

  void broadcastForPhase(PhaseId phaseId)
  {
    if (_events == null)
    {
      return;
    }

    boolean abort = false;

    int phaseIdOrdinal = phaseId.getOrdinal();
    for (ListIterator<FacesEvent> listiterator = _events.listIterator(); listiterator
        .hasNext();)
    {
      FacesEvent event = listiterator.next();
      int ordinal = event.getPhaseId().getOrdinal();
      if (ordinal == ANY_PHASE_ORDINAL || ordinal == phaseIdOrdinal)
      {
        UIComponent source = event.getComponent();
        try
        {
          source.broadcast(event);
        }
        catch (AbortProcessingException e)
        {
          // abort event processing
          // Page 3-30 of JSF 1.1 spec: "Throw an
          // AbortProcessingException, to tell the JSF implementation
          // that no further broadcast of this event, or any further
          // events, should take place."
          abort = true;
          break;
        }
        finally
        {
          try
          {
            listiterator.remove();
          }
          catch (ConcurrentModificationException cme)
          {
            int eventIndex = listiterator.previousIndex();
            _events.remove(eventIndex);
            listiterator = _events.listIterator();
          }
        }
      }
    }

    if (abort)
    {
      // TODO: abort processing of any event of any phase or just of any
      // event of the current phase???
      clearEvents();
    }
  }

  private void clearEvents()
  {
    _events = null;
  }

  boolean notifyListeners(FacesContext context, PhaseId phaseId,
      MethodExpression listener, boolean beforePhase)
  {
    boolean skipPhase = false;

    if (listener != null
        || (_phaseListeners != null && !_phaseListeners.isEmpty()))
    {
      PhaseEvent event = createEvent(context, phaseId);

      if (listener != null)
      {
        listener.invoke(context.getELContext(), new Object[]
            {event});
        skipPhase = context.getResponseComplete()
            || context.getRenderResponse();
      }

      if (_phaseListeners != null && !_phaseListeners.isEmpty())
      {
        for (PhaseListener phaseListener : _phaseListeners)
        {
          PhaseId listenerPhaseId = phaseListener.getPhaseId();
          if (phaseId.equals(listenerPhaseId)
              || PhaseId.ANY_PHASE.equals(listenerPhaseId))
          {
            if (beforePhase)
            {
              phaseListener.beforePhase(event);
            } else
            {
              phaseListener.afterPhase(event);
            }
            skipPhase = context.getResponseComplete()
                || context.getRenderResponse();
          }
        }
      }
    }

    return skipPhase;
  }

  private PhaseEvent createEvent(FacesContext context, PhaseId phaseId)
  {
    if (_lifecycle == null)
    {
      LifecycleFactory factory = (LifecycleFactory) FactoryFinder
          .getFactory(FactoryFinder.LIFECYCLE_FACTORY);
      String id = context.getExternalContext().getInitParameter(
          FacesServlet.LIFECYCLE_ID_ATTR);
      if (id == null)
      {
        id = LifecycleFactory.DEFAULT_LIFECYCLE;
      }
      _lifecycle = factory.getLifecycle(id);
    }
    return new PhaseEvent(context, phaseId, _lifecycle);
  }

  public void processDecodes(FacesContext context)
  {
    if (!notifyListeners(context, PhaseId.APPLY_REQUEST_VALUES, getBeforePhaseListener(), true))
    {
      super.processDecodes(context);
      storePartialTargets(context);
      broadcastForPhase(PhaseId.APPLY_REQUEST_VALUES);
    }
    clearEvents(context);
    notifyListeners(context, PhaseId.APPLY_REQUEST_VALUES, getAfterPhaseListener(), false);
  }

  private void storePartialTargets(FacesContext context) {
    // after a normal decode check for partialTargets
    final RequestContext requestContext = RequestContext.getCurrentInstance();
    if (!(context.getResponseComplete() || context.getRenderResponse())
        && requestContext.isPartialRequest(context)) {
      String sourceId = context.getExternalContext().getRequestParameterMap().get("source");
      UIComponent source = findComponent(sourceId);
      if (source != null) {
        List<String> list = new ArrayList<String>();
        Set<UIComponent> components = requestContext.getPartialTargets(source);
        for (UIComponent component : components) {
          list.add(component.getClientId(context));
        }
        if (list.size() > 0) {
          // check if source is a child of the partialTargets or a partialTarget
          if (!list.contains(sourceId)) {
            UIComponent component = source;
            while ((component = component.getParent()) != null) {
              if (list.contains(component.getClientId(context))) {
                break;
              } else if (component instanceof UIViewRoot) {
                // source is not inside partialTargets
                list.add(0, sourceId);
                break;
              }
            }
          }
          PartialLifecycleUtils.setPartialTargets(context, list.toArray(new String[list.size()]));
        } else {
          PartialLifecycleUtils.setPartialTargets(context, sourceId);
        }
      }
    }
  }

  @Override
  // skip invokeOnComponent on UIViewRoot to avoid warning message in myfaces about missing id
  public boolean invokeOnComponent(FacesContext context, String clientId, ContextCallback callback)
      throws FacesException {
    boolean found = false;
    for (Iterator<UIComponent> it = getFacetsAndChildren(); !found && it.hasNext();)
    {
      found = it.next().invokeOnComponent(context, clientId, callback);
    }
    return found;
  }

  void clearEvents(FacesContext context)
  {
    if (context.getRenderResponse() || context.getResponseComplete())
    {
      clearEvents();
    }
  }

  public void processValidators(FacesContext context)
  {
    if (!notifyListeners(context, PhaseId.PROCESS_VALIDATIONS, getBeforePhaseListener(), true))
    {
      String[] partialTargets = PartialLifecycleUtils.getPartialTargets(context);
      if (partialTargets != null)
      {
        for (String clientId : partialTargets)
        {
          invokeOnComponent(context, clientId, PROCESS_VALIDATION_CALLBACK);
        }
      }
      else
      {
        super.processValidators(context);
      }
      broadcastForPhase(PhaseId.PROCESS_VALIDATIONS);
    }
    clearEvents(context);
    notifyListeners(context, PhaseId.PROCESS_VALIDATIONS, getAfterPhaseListener(), false);
  }

  public void processUpdates(FacesContext context)
  {
    if (!notifyListeners(context, PhaseId.UPDATE_MODEL_VALUES, getBeforePhaseListener(), true))
    {
      String[] partialTargets = PartialLifecycleUtils.getPartialTargets(context);
      if (partialTargets != null) {
        for (String clientId : partialTargets)
        {
          invokeOnComponent(context, clientId, UPDATE_MODEL_VALUES_CALLBACK);
        }
      } else {
        super.processUpdates(context);
      }
      broadcastForPhase(PhaseId.UPDATE_MODEL_VALUES);
    }
    clearEvents(context);
    notifyListeners(context, PhaseId.UPDATE_MODEL_VALUES, getAfterPhaseListener(), false);
  }

  public void processApplication(FacesContext context)
  {
    if (!notifyListeners(context, PhaseId.INVOKE_APPLICATION, getBeforePhaseListener(), true))
    {
      broadcastForPhase(PhaseId.INVOKE_APPLICATION);
    }
    clearEvents(context);
    notifyListeners(context, PhaseId.INVOKE_APPLICATION, getAfterPhaseListener(), false);
  }

  public void encodeBegin(FacesContext context) throws java.io.IOException
  {

    boolean skipPhase = false;

    try
    {
      skipPhase = notifyListeners(context, PhaseId.RENDER_RESPONSE,
          getBeforePhaseListener(), true);
    }
    catch (Exception e)
    {
      // following the spec we have to swallow the exception
      LOG.severe("Exception while processing phase listener: " + e.getMessage(), e);
    }

    if (!skipPhase)
    {
      super.encodeBegin(context);
    }
  }

  public void encodeEnd(FacesContext context) throws java.io.IOException
  {
    super.encodeEnd(context);
    try
    {
      notifyListeners(context, PhaseId.RENDER_RESPONSE,
          getAfterPhaseListener(), false);
    }
    catch (Exception e)
    {
      // following the spec we have to swallow the exception
      LOG.severe("Exception while processing phase listener: " + e.getMessage(), e);
    }
  }


  @Override
  public void encodeAll(FacesContext facesContext) throws IOException {
    /*String[] partialTargets = PartialLifecycleUtils.getPartialTargets(facesContext);
    if (partialTargets != null)
    {

      // TODO Messages
      if (facesContext.getMessages().hasNext())
      {
        // add messages to partialTargets
      }

      RenderingContext renderingContext = RenderingContext.getCurrentInstance();
      // ensure mark PPR active
      if (PartialPageUtils.isPartialRenderingPass(renderingContext))
      {
        PartialPageUtils.markPPRActive(facesContext);
      }

      Map<String, String> parameterMap = facesContext.getExternalContext().getRequestParameterMap();
      String formName = parameterMap.get(CoreResponseStateManager.FORM_FIELD_NAME);
      UIComponent form = facesContext.getViewRoot().findComponent(formName);

      // TODO form inside partial Target
      if (form != null)
      {

        form.encodeBegin(facesContext);
        FormData formData = renderingContext.getFormData();
        formData.addNeededValue(TrinidadRenderingConstants.PARTIAL_PARAM);
        formData.addNeededValue(TrinidadRenderingConstants.STATE_PARAM);
        formData.addNeededValue(TrinidadRenderingConstants.VALUE_PARAM);

        // FIXME
        if (renderingContext instanceof CoreRenderingContext)
        {
          Map<String, String> shortStyles = renderingContext.getSkin().getStyleClassMap(renderingContext);
          ((CoreRenderingContext) renderingContext).setStyleMap(shortStyles);
        }
      }

      for (String clientId : partialTargets)
      {
        LOG.info("Rendering partialTarget " + clientId);
        facesContext.getViewRoot().invokeOnComponent(facesContext, clientId, RENDER_RESPONSE_CALLBACK);
      }

      // find a better way
      if (form != null)
      {
        form.encodeEnd(facesContext);
      }  */
   /* } else {  */
      super.encodeAll(facesContext);
    //}
  }

  public Object saveState(FacesContext facesContext)
  {
    Object[] values = new Object[2];
    values[0] = super.saveState(facesContext);
    values[1] = saveAttachedState(facesContext, _phaseListeners);
    return values;
  }

  @Override
  public void restoreState(FacesContext facesContext, Object state)
  {
    Object[] values = (Object[]) state;
    super.restoreState(facesContext, values[0]);
    _phaseListeners = (List) restoreAttachedState(facesContext, values[1]);
  }

}
