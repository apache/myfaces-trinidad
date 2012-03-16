/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.apache.myfaces.trinidadinternal.skin.pregen;

import java.io.IOException;

import java.util.Map;

import javax.faces.FacesException;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.render.InternalView;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.SkinFactory;
import org.apache.myfaces.trinidad.util.Enums;
import org.apache.myfaces.trinidadinternal.skin.pregen.config.InvalidConfigException;
import org.apache.myfaces.trinidadinternal.skin.pregen.config.PregenConfig;

/**
 * InternalView implementation that provides skin pregeneration service.
 *
 * Skin pregeneration is enabled by specifying the following system
 * property:
 *
 *  -Dorg.apache.myfaces.trinidad.SKIN_PREGENERATION_SERVICE=on
 *
 * When enabled, a request to the "-pregenerate-skins" view id can be
 * used to force pregeneration for specific skins.  Each request must
 * specifiy a single "id" query parameter.  The value of this query parameter
 * must be an id of one of the skins registered with trinidad (eg. via
 * an "id" element in a trinidad-skins.xml file).
 *
 * For example, in an application that uses prefix mapping for the FacesServlet,
 * enabling the skin pregeneration service exposes the following uri:
 *
 *   - /context root/faces/-pregenerate-skins?id=minimal.desktop
 *
 * Which triggers pregeneration of style sheets for the minimal.desktop skin
 * when hit.
 * 
 * By default, only style sheets for common variant values are generated.
 * In particular:
 * 
 * - plaform: android | iphone | linux | macos | windows
 * - agent: ie | gecko | webkit
 * - locale: default (unmatched) locale
 * - reading direction: ltr
 * - accessibility: standard contrast, medium fonts
 *
 * The "variants" request parameter can be used to force generation of all
 * possible variants, eg:
 * 
 *    - "/context root/faces/-pregenerate-skins?id=minimal.desktop?variants=all
 *    
 * The following optional request parameters may also be used to provide contextual
 * information about how the pregeneration should be performed:
 *
 * - containerType: servlet | portlet. (Defaults to "servlet".)
 * - requestType: nonsecure | secure.  (Defaults to "nonsecure".)
 * - styleClassType: compressed | uncompressed.  (Defaults to "compressed".)
 *
 * All of the contextual request parameters support multiple values.  For example,
 * the following request:
 * 
 *  /-pregenerate-skins?id=minimal.desktop&styleClassType=compressed&styleClassType=uncompressed
 *  
 * Pregenerates both servlet and portlet style sheets for the common variants of the the
 * minimal.desktop skin.
 *
 * By default, pregenerated style sheets are written into the web application's
 * style sheet cache directory (typically, tmpdir/adf/styles/cache).  However,
 * a target directory for the pregenerated output can be specified via the
 * following system property:
 * 
 * -Dorg.apache.myfaces.trinidad.SKIN_PREGENERATION_SERVICE_TARGET_DIRECTORY=directory path
 * 
 * 
 * Requests that result in successful skin pregeneration result in status 200
 * responses.  Failed requests (eg. for invalid skin ids) result in non-200
 * status codes.
 * 
 * For security purposes, pregeneration must never be enabled in end user facing,
 * production deployments - ie. skin pregeneration is CPU and I/O intensive, and
 * must not be exposed to arbitrary users.  To avoid the potential for abuse,
 * enabling skin pregeneration has the side effect of disabling the rest of the
 * application.  As such, applications that are enabled for skin pregeneration 
 * can only be used for this single purpose.
 */
public class SkinPregenerationService extends InternalView
{ 
  /**
   * Tests whether the pregeneration service is enabled.
   */
  public static boolean isEnabled()
  {
    return (_getServiceStatus() == ServiceStatus.ON);
  }

  /**
   * Tests whether the view id corresponds to a skin pregeneration request.
   */
  public static boolean isPregenerationRequest(String viewId)
  {
    return _VIEW_ID.equals(viewId);
  }

  @Override
  public UIViewRoot createView(FacesContext context, String viewId)
  {
    return null;
  }

  @Override
  public UIViewRoot restoreView(FacesContext context, String viewId)
  {
    return null;
  }

  @Override
  public void renderView(FacesContext context, UIViewRoot viewToRender)
    throws IOException, FacesException
  {
    if (!isEnabled())
    {
      _sendPregenerationDisabledError(context);
      return;
    }
    
    long duration = _pregenerateSkin(context);    
    
    if (!context.getResponseComplete())
    {
      _renderResponse(context, duration);
    }
  }
  
  // Pregenerates the skin based on the config specified via
  // request parameters and returns the duration.
  private static long _pregenerateSkin(FacesContext context)
  {
    long startTime = System.currentTimeMillis();

    try
    {
      PregenConfig config = PregenConfig.parse(context);
      Skin skin = _parseSkin(context);
      SkinPregenerationUtils.pregenerate(context, skin, config);      
    }
    catch (Exception e)
    {
      _pregenFailed(context, e);
    }

    return (System.currentTimeMillis() - startTime);
  }
  
  // Returns a non-null Skin corresponding based on the "id"
  // request parameter.  Throws InvalidConfigException if no
  // skin is found.
  private static Skin _parseSkin(FacesContext context)
    throws InvalidConfigException
  {
    String skinId = _getSkinId(context);
    if (skinId != null)
    {
      Skin skin = _getSkin(context, skinId);
      if (skin != null)
      {
        return skin;
      }
    }

    String message = _LOG.getMessage("SKIN_PREGEN_REQUESTED_SKIN_INVALID", skinId);
    throw new InvalidConfigException(message);
  }
  
  private static String _getSkinId(FacesContext context)
  {
    ExternalContext external = context.getExternalContext();
    return external.getRequestParameterMap().get(_SKIN_ID_REQUEST_PARAM);        
  }
  
  private static Skin _getSkin(FacesContext context, String skinId)
  {
    SkinFactory factory = SkinFactory.getFactory();
    return factory.getSkin(context, skinId);
  }

  private static void _pregenFailed(FacesContext context, Exception e)
  {
    String message = _LOG.getMessage("SKIN_PREGEN_FAILED", e.getMessage());
    _LOG.severe(message, e);
    _sendError(context, message);
  }

  private static void _sendPregenerationDisabledError(FacesContext context)
  {
    String message = _LOG.getMessage("SKIN_PREGEN_DISABLED",
                                     new Object[] {_SERVICE_PROPERTY,
                                                   _getServicePropertyValues()});

    _sendError(context, message);
  }
  
  private static String _getServicePropertyValues()
  {
    return Enums.patternOf(ServiceStatus.class,
                           Enums.displayNameStringProducer(ServiceStatus.class));
  }


  private static void _sendError(FacesContext context, String message)
  {
    if (context.getResponseComplete())
    {
      return;
    }

    ExternalContext external = context.getExternalContext();
    
    try
    {
      external.responseSendError(500, message);
    }
    catch (IOException e)
    {
      _LOG.warning(e);
    }
    finally
    {
      context.responseComplete();
    }
  }

  // Render the content to send back in the event that skin
  // generation is successful.
  private static void _renderResponse(
    FacesContext context,
    long         duration
    ) throws IOException
  {
    ResponseWriter rw = _getResponseWriter(context);

    rw.startDocument();
    rw.write(_RESPONSE_DOCTYPE);
    rw.startElement("html", null);
    rw.startElement("head", null);
    rw.startElement("title", null);
    rw.writeText(_LOG.getMessage("SKIN_PREGEN_RESPONSE_TITLE"), null);
    rw.endElement("title");
    rw.endElement("head");
    rw.startElement("body", null);
    rw.startElement("p", null);

    String message = _LOG.getMessage("SKIN_PREGEN_RESPONSE", duration);
    rw.writeText(message, null);

    rw.endElement("p");
    rw.endElement("body");
    rw.endElement("html");
    rw.endDocument(); 
  }
  
  private static ResponseWriter _getResponseWriter(FacesContext context)
    throws IOException
  {
    ExternalContext external  = context.getExternalContext();
    String contentType = "text/html";
    String encoding = "UTF-8";

    external.setResponseContentType(contentType);
    external.setResponseCharacterEncoding(encoding);
    
    ResponseWriter rw = context.getRenderKit().createResponseWriter(
      external.getResponseOutputWriter(),
      contentType,
      encoding);
    
    context.setResponseWriter(rw);
    
    return rw;
  }
  
  private static ServiceStatus _getServiceStatus()
  {
    if (_sServiceStatus == null)
    {   
      String serviceProperty = System.getProperty(_SERVICE_PROPERTY);
      if ((serviceProperty != null) && (serviceProperty.length() > 0))
      {
        try
        {
          _sServiceStatus = ServiceStatus.valueOfDisplayName(serviceProperty);
        }
        catch (IllegalArgumentException e)
        {
          _sServiceStatus = ServiceStatus.OFF;

          _LOG.severe("ILLEGAL_SYSTEM_PROPERTY_VALUE",
            new Object[] {
              serviceProperty,
              _SERVICE_PROPERTY,
              _getServicePropertyValues()});
        }
      }
    }
    
    return _sServiceStatus;
  }
  
  // Little utility enum that we use to track the
  // enabled/disabed status of the pregeneration service.
  public static enum ServiceStatus
  {
    ON("on"),
    OFF("off");
    
    ServiceStatus(String displayName)
    {
      _displayName = displayName;
    }
    
    public String displayName()
    {
      return _displayName;
    }

    public static ServiceStatus valueOfDisplayName(String displayName)
    {
      return Enums.stringToEnum(_displayNameMap, displayName, ServiceStatus.class);
    }
    
    private final String _displayName;
    private static final Map<String, ServiceStatus> _displayNameMap;
    
    static
    {
      _displayNameMap = Enums.createDisplayNameMap(ServiceStatus.class);
    }    
  }

  private static volatile ServiceStatus _sServiceStatus;
  
  private static final String _SKIN_ID_REQUEST_PARAM    = "id";

  private static final String _SERVICE_PROPERTY =
    "org.apache.myfaces.trinidad.SKIN_PREGENERATION_SERVICE";

  private static final String _RESPONSE_DOCTYPE =
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">";

  // The view id for the pregeneration service.
  private static final String _VIEW_ID = "/-pregenerate-skins";

  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(SkinPregenerationService.class);
}
