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

import javax.faces.FacesException;
import javax.faces.application.Application;
import javax.faces.application.ViewHandler;
import javax.faces.component.ContextCallback;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseId;
import java.io.IOException;

public class RenderResponseExecutor implements PhaseExecutor
{
  private static final TrinidadLogger LOG = TrinidadLogger.createTrinidadLogger(RenderResponseExecutor.class);
  private ContextCallback contextCallback = new RenderResponseCallback();

  public boolean execute(FacesContext facesContext)
  {

    try
    {
      // TODO partial rendering
      /*String[] partialTargets = PartialLifecycleUtils.getPartialTargets(facesContext);
      if (partialTargets != null)
      {
        // Most of this is a copy from ViewHandlerImpl
        // See if there is a possiblity of short-circuiting the current
        // Render Response
        ExtendedRenderKitService service = Service.getService(facesContext.getRenderKit(), ExtendedRenderKitService.class);
        if ((service != null) && service.shortCircuitRenderView(facesContext))
        {
          // Yup, we don't need to do anything
          return false;
        } else
        {
          try
          {
            if (service != null)
            {
              service.encodeBegin(facesContext);
            }
            HttpServletResponse response =
                (HttpServletResponse) facesContext.getExternalContext().getResponse();
            String contentType = "text/html";
            String encoding = "UTF-8";

            // TODO JspUtils.getEncoding(facesContext, "UTF-8");

            response.setContentType(contentType +
                "; charset=" + encoding);

            ResponseWriter responseWriter =
                facesContext.getRenderKit().createResponseWriter(response.getWriter(), contentType, encoding);
            LOG.info("ResponseWriter class: " + responseWriter.getClass().getName());
            facesContext.setResponseWriter(responseWriter);

            ResponseWriter rw = facesContext.getResponseWriter();

            // TODO Messages
            if (facesContext.getMessages().hasNext()) {
              // add messages to partialTargets
            }

            RenderingContext renderingContext = RenderingContext.getCurrentInstance();
            // ensure mark PPR active
            if (PartialPageUtils.isPartialRenderingPass(renderingContext)) {
              PartialPageUtils.markPPRActive(facesContext);
            }
            rw.startDocument();

            Map<String, String> parameterMap = facesContext.getExternalContext().getRequestParameterMap();
            String formName = parameterMap.get(CoreResponseStateManager.FORM_FIELD_NAME);
            UIComponent form = facesContext.getViewRoot().findComponent(formName);

            // TODO form inside partial Target
            if (form != null)
            {

              // TODO find a better way or should form.encodeBegin called?
              //CoreFormData formData = new CoreFormData(formName);
              // FIXME
              //formData.addNeededValue(XhtmlConstants.PARTIAL_PARAM);
              //formData.addNeededValue(XhtmlConstants.STATE_PARAM);
              //formData.addNeededValue(XhtmlConstants.VALUE_PARAM);

              //renderingContext.setFormData(formData);
              form.encodeBegin(facesContext);
              FormData formData = renderingContext.getFormData();
              formData.addNeededValue(XhtmlConstants.PARTIAL_PARAM);
              formData.addNeededValue(XhtmlConstants.STATE_PARAM);
              formData.addNeededValue(XhtmlConstants.VALUE_PARAM);

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
              facesContext.getViewRoot().invokeOnComponent(facesContext, clientId, contextCallback);
            }

            // TODO writeState is performed by patched FormRenderer 
            // find a better way
            if (form != null)
            {
              form.encodeEnd(facesContext);
            }
            rw.endDocument();

            if (service != null)
            {
              service.encodeEnd(facesContext);
            }
          }
          finally
          {
            if (service != null)
            {
              service.encodeFinally(facesContext);
            }
          }
        }
      } else
      { */
        Application application = facesContext.getApplication();
        ViewHandler viewHandler = application.getViewHandler();
        viewHandler.renderView(facesContext, facesContext.getViewRoot());
      //}
    } catch (IOException e)
    {
      throw new FacesException(e.getMessage(), e);
    }
    return false;
  }

  public PhaseId getPhase()
  {
    return PhaseId.RENDER_RESPONSE;
  }
}
