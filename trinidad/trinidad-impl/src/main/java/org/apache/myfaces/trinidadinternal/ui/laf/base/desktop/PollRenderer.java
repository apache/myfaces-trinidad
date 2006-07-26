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
package org.apache.myfaces.adfinternal.ui.laf.base.desktop;

import java.io.IOException;

import org.apache.myfaces.adfinternal.share.url.URLEncoder;
import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;
import org.apache.myfaces.adfinternal.ui.beans.MarlinBean;

import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.XhtmlLafRenderer;
import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.XhtmlLafUtils;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/desktop/PollRenderer.java#0 $) $Date: 10-nov-2005.18:55:33 $
 * @author The Oracle ADF Faces Team
 */
public class PollRenderer extends HtmlLafRenderer
{
  protected void renderContent(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {  
    XhtmlLafUtils.addLib(context, "PollManager()");
    super.renderContent(context, node); 
    
    // A page should not auto-refresh while in screen-reader mode.
    // So only render the setTimeout script if we are in auto-refresh mode.
    if (_isAutoRefreshMode(context) )
    {
      _renderPollingScript(context, node);
    }
    else
    {
      _renderManualRefresh(context, node);
    }
  }

  protected String getElementName(
    RenderingContext context,
    UINode           node
    )
  {
    return "span";
  }

  /**
   * A page should not auto-refresh while in screen-reader mode.
   * In this case, we create a button which when clicked will send the 
   * "poll" event. This is instead of the setTimeout javascript function,
   * which we render when not in screen-reader mode.     
   */
  private void _renderManualRefresh(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    String elementID = _getID(context, node);
    boolean isPartial = _elementSupportsPartial(context, elementID);

    MarlinBean refreshButton = new MarlinBean(BUTTON_NAME);

    refreshButton.setOnClick(
    _getScriptContents(context, node, elementID, isPartial, false));
    
    String buttonText = getTranslatedString(context, "af_poll.MANUAL");
    refreshButton.setShortDesc(buttonText);
    refreshButton.setAttributeValue(TEXT_ATTR, buttonText);
    
    refreshButton.render(context);
  }
      
  /**
   * Renders a script which sends a 'poll' event after a timeout.
   */
  private void _renderPollingScript(
    RenderingContext context,
    UINode           node
    ) throws IOException
  {
    String elementID = _getID(context, node);
    boolean isPartial = _elementSupportsPartial(context, elementID);
    
    String buffer = 
      _getScriptContents(context, node, elementID, isPartial, true);
    // build script bean
    MarlinBean script = new MarlinBean(SCRIPT_NAME);
    script.setAttributeValue(TEXT_ATTR, buffer);
    script.render(context);
  } 

  /**
   * Returns true if the poll element has an id 
   * and the browser supports PPR
   */
  private boolean _elementSupportsPartial(
    RenderingContext context,
    String           id
    )
  {    
    return ((id != null) && 
            XhtmlLafRenderer.supportsPartialRendering(context));
  }
  
  /**
   * Return the element's id in a String, 
   * or null if the element does not have an id
   */
  private String _getID(
    RenderingContext context,
    UINode           node
    )
  {
    Object id = getID(context, node);
    if (id == null)
      return null;
    else
      return id.toString();
  }

  /**
  * get the pollInterval attribute value. Defaults to _POLL_INTERVAL_DEFAULT
  */
  private static Integer _getPollIntervalOrDefault( 
      RenderingContext context,
      UINode           node
    )
  {   
    Integer pollInterval = (Integer)node.getAttributeValue(context, 
                                                     INTERVAL_ATTR);
    if ((pollInterval == null) || (pollInterval.intValue() < 0))
      pollInterval = _POLL_INTERVAL_DEFAULT;

    return pollInterval;
  }

  /**
  * returns a String which is the entire script to be rendered.
  * extract the attributes of the poll element and build
  * a script.
  * the javascript function we build depends on whether the page is refreshed
  * full or partially, and whether or not it is in a form and whether or 
  * not it should be auto-refreshed. For auto-refresh, then
  * the script is a setTimeout script. For manual-refresh
  * this will be the same script we render,
  * but minus the setTimeout call. In manual-refresh mode
  * the user will need to click on a button 
  * to send the 'poll' event
  */
  private String _getScriptContents( 
      RenderingContext context,
      UINode           node,
      String           elementID,
      boolean          isPartial,
      boolean          isAutoRefreshMode
    )
  {
    // We will build a script like one of the following: 
    // (these examples assume isAutoRefreshMode is false, 
    // if isAutoRefreshMode is true, these commands are registered in the
    // _PollManager javascript object, which will manage executing this commands
    // at the specified pollInterval.)
    //
    // Partial page refresh when the poll component is inside of a form:
    // "_submitPartialChange('formName',0,{event:'poll',source:'pollingWidgetId',
    //    partialTargets:'pollingWidgetId', partial='true'})")
    // Partial page refresh when the poll component is not inside of a form:
    // "_firePartialChange('<URL>?event=poll&source=pollingWidgetId
    //    &partialTargets=pollingWidgetId&partial=true')"
    // Full page refresh when the poll component is inside of a from:
    // "submitForm('formName',0,{event:'poll', source:'pollingWidgetId'}), 
    //    timeoutMS)
    // Full page refresh when the poll component is not inside of a from:
    // "document.location.replace('<URL>?event=poll&source=pollingWidgetId
    //    &partialTargets=pollingWidgetId&partial=true')" ->
    /* -------------------------------------------------------- */
    
    // get variables needed to create the javascript function
    Integer pollInterval = null;
    if (isAutoRefreshMode)
    {
      pollInterval = _getPollIntervalOrDefault(context, node);
    }
    String formName = getParentFormName(context);
    String startScript = isPartial? "_pprUpdateMode=true;" : "";

    startScript += _getStartScript(formName,    
                                         isPartial, 
                                         isAutoRefreshMode,
                                         elementID);
    String argumentString = _getArgumentString(context, 
                                               elementID, 
                                               formName, 
                                               isPartial);
    int length = _getScriptBufferLength(startScript, 
                                        argumentString,
                                        pollInterval,
                                        isAutoRefreshMode);

    // build the script:
    StringBuffer buffer = new StringBuffer(length);
    
    buffer.append(startScript);
    
    //pu: In manual-refresh mode, submit is through onclick of button, not an 
    //  auto-scheduled command string, hence no escaping "'" in arguments.
    if (_isAutoRefreshMode(context))
    {
      buffer.append(XhtmlLafUtils.escapeJS(argumentString));
    }
    else
    {
      buffer.append(argumentString);
    }
    
    // auto-refresh is not allowed in accessible mode.
    // So render the pollInterval portion only if in auto-refresh mode.
    if (isAutoRefreshMode)
    {
      buffer.append(_MIDDLE_SCRIPT_AUTO_REFRESH);
      buffer.append(pollInterval);
    }
    
    buffer.append(_END_SCRIPT);

    return buffer.toString();
  }

  /**
  * Returns a String array with encoded event parameter keys and their values.
  * This String array will be used to create the URL or the JS arguments to
  * fire a refresh event.
  */ 
  private static String[] _getKeysAndValues(
    RenderingContext context,
    String           elementID,
    boolean          isPartial
    )
  {
     // get encoded parameters
    URLEncoder encoder = context.getURLEncoder();
    String eventKey  = encoder.encodeParameter(EVENT_PARAM);
    String sourceKey = encoder.encodeParameter(SOURCE_PARAM);
    String partialTargetsKey = encoder.encodeParameter(PARTIAL_TARGETS_PARAM);
    String partialKey = encoder.encodeParameter(PARTIAL_PARAM);
    
    String eventValue = POLL_EVENT;
    String sourceValue = null;
    String partialTargetsValue = null;
    String partialValue = _PARTIAL_VALUE;

    if (elementID != null)
    {
      sourceValue = elementID;
      if (isPartial)
      {
          partialTargetsValue = elementID;       
      }
    }
    String[] keysAndValues = null;
    if (isPartial)
    {
      keysAndValues = new String[]
                      {  eventKey,          eventValue,
                         sourceKey,         sourceValue,
                         partialTargetsKey, partialTargetsValue,
                         partialKey,        partialValue,
                      };
    }
    else
    {
      keysAndValues = new String[]
                      {  eventKey,          eventValue,
                         sourceKey,         sourceValue,
                      };
    }
    return keysAndValues;
  }

  
  /**
  * append to the baseURL the parameters needed, like event=poll.
  */
  private static String _getScriptURL(
    RenderingContext context,
    String           baseURL,
    String           elementID,
    boolean          isPartial
    )
  {
    String[] keysAndValues = _getKeysAndValues(context, elementID, isPartial);
    return appendURLArguments(baseURL, keysAndValues);
  }

  /**
  * Computes the length of the buffer that is needed to build up the script
  */ 
  private static int _getScriptBufferLength(
    String  startScript,
    String  argumentString,
    Integer pollInterval,
    boolean isAutoRefreshMode
    )
  {
    if ((startScript == null) || (argumentString == null))
    {
      return 0;
    }
    
    int length =  startScript.length() + 
                  argumentString.length() + 
                  _MIDDLE_SCRIPT_AUTO_REFRESH.length() +
                  _END_SCRIPT.length();
                  
    if (isAutoRefreshMode && (pollInterval != null))
    {
     length +=  pollInterval.toString().length() + 3;
    }
   
    return length;
  }
  
  /**
  * Returns the start script. The start script depends on whether the poll
  * element is within a form and whether the page is in PPR mode and whether
  * the page is in auto-refresh mode.
  */ 
  private static String _getStartScript(
    String  formName,
    boolean isPartial,
    boolean isAutoRefreshMode,
    String elementID
    )
  {
    String startScript = "";
    if (isAutoRefreshMode)
    {
      startScript = _PRE_START_SCRIPT_AUTO_REFRESH;
      startScript += elementID;
      startScript += "\", \"";
    }

    if (isPartial)
    {
      if (formName == null)
        startScript += _START_SCRIPT_PARTIAL;
      else
        startScript += _START_SCRIPT_PARTIAL_FORM;
    }
    else
    {
      if (formName == null)     
        startScript += _START_SCRIPT_FULL;
      else
        startScript += _START_SCRIPT_FULL_FORM;
    }
    
    return startScript;
  }

  /**
   * Returns a String which will be used as the JavaScript arguments: either
   * a URL with event information or the needed arguments to 
   * submitForm/_submitPartialChange.
   * e.g., if within a form and PPR, the argument string is:
   * 'myform',0,{'event':'poll','source':'polling-widget',
   * 'partialTargets':'polling-widget','partial':'true'}",
  */ 
  private static String _getArgumentString(
    RenderingContext context,
    String           elementID,
    String           formName,
    boolean          isPartial
    )
  {

    String argumentString = null;
    if (formName != null)
    {    
      String[] keysAndValues = _getKeysAndValues(context, elementID, isPartial);
      // Encodes an Enumeration key value pairs as a single Javascript Object
      // initializer, creating any needed form values.
      String jsEventObject = XhtmlLafUtils.encodeJSEventObject(context, 
                                                               formName, 
                                                               keysAndValues);
      int length = formName.length() + 7;
      if (jsEventObject != null)
        length += jsEventObject.length();
      StringBuffer buffer = new StringBuffer(length);
      buffer.append("'");
      buffer.append(formName);
      // 0 means do not validate
      buffer.append("',0,");
      buffer.append(jsEventObject);   
      argumentString = buffer.toString();
    }
    else
    {  
      String baseURL = context.getURLEncoder().getDefaultURL();

      String scriptURL = _getScriptURL(context, baseURL, elementID, isPartial);
      int length = scriptURL.length() + 2;
      StringBuffer buffer = new StringBuffer(length);
      buffer.append("'");
      buffer.append(scriptURL);
      buffer.append("'");
      argumentString = buffer.toString();
    }
    return argumentString;
  }

  /**
   * return true if this element should poll automatically
   * rather than manually.
   */
  private boolean _isAutoRefreshMode(
    RenderingContext context
    )
  {
    // auto poll when NOT in screen-reader mode.
    return (!isScreenReaderMode(context));
  }
  
  // script constants
  // Script for initializing the auto mode for the command
  private static final String _PRE_START_SCRIPT_AUTO_REFRESH = 
  " if (!self._pollManager) _pollManager = new _PollManager(); _pollManager.addAndActivate(\"";

  //=-=pu: We should always be in partial mode. Given this, can we remove
  // the _START_SCRIPT_FULL support ? This condition is not possible, and if
  // forced, initiates refresh (no form submit), hence no
  // chance to decode, hence useless.
  // see _getStartScript()
  private static final String _START_SCRIPT_FULL =
    "document.location.replace(";  
  //=-=pu: This condition again results in URL replacement, but on the iFrame
  // hence equivalent to the one above. Can we force the poll component to
  // be inside of a <af:form> ? If yes, we need not bother of this condition.
  // see _getStartScript()
  private static final String _START_SCRIPT_PARTIAL =
    "_firePartialChange(";
  private static final String _START_SCRIPT_PARTIAL_FORM =
    "_submitPartialChange(";
  private static final String _START_SCRIPT_FULL_FORM =
    "submitForm(";
  private static final String _END_SCRIPT     = ");";
  private static final String _MIDDLE_SCRIPT_AUTO_REFRESH  = ")\", "; 
  
  private static final Integer _POLL_INTERVAL_DEFAULT = new Integer(5000);
  private static final String _PARTIAL_VALUE = "true";
}
