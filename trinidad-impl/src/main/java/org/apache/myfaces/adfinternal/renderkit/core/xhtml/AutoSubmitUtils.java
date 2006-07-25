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

package org.apache.myfaces.adfinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.component.UIParameter;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.jsLibs.Scriptlet;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.jsLibs.XhtmlScriptletFactory;

/**
 * Public utility methods useful for working with AutoSubmits
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/xhtml/AutoSubmitUtils.java#0 $) $Date: 10-nov-2005.19:01:20 $
 * @author The Oracle ADF Faces Team
 */
public class AutoSubmitUtils
{
  public static void writeDependencies(
    FacesContext        context,
    AdfRenderingContext arc
    ) throws IOException
  {
    // Render our scriptlet
    XhtmlUtils.addLib(context, arc, _AUTO_SUBMIT_SCRIPTLET);
  }

  /**
   * Find all UIParameter children of a component, and
   * return it as a JS string of "name1:value1,name2:value2".
   */
  public static String getParameters(UIComponent comp)
  {
    int childCount = comp.getChildCount();
    if (childCount == 0)
      return null;
      
    StringBuffer buffer = null;
    List children = comp.getChildren();
    for (int i = 0; i < childCount; i++)
    {
      UIComponent child = (UIComponent) children.get(i);
      if (child instanceof UIParameter)
      {
        UIParameter param = (UIParameter) child;
        String name = param.getName();
        Object value = param.getValue();
        if ((value == null) || (name == null))
          continue;

        if (buffer == null)
          buffer = new StringBuffer();
          
        // Add a comma if needed
        if (buffer.length() > 0)
          buffer.append(',');
        
        // Add the name and value, and in both cases
        // wrap in single quotes and escape it - we don't
        // know for sure if the name will be a legit JS identifier
        buffer.append('\'');
        buffer.append(XhtmlUtils.escapeJS(name));
        buffer.append("':'");
        buffer.append(XhtmlUtils.escapeJS(value.toString()));
        buffer.append('\'');
      }
    }
    
    if (buffer == null)
      return null;
      
    return buffer.toString();
  }
    
  public static String getFullPageSubmitScript(
     AdfRenderingContext arc,
     String              source,
     boolean             immediate,
     String              event,
     String              extraParams,
     boolean             returnTrue)
  {
    FormData fData = arc.getFormData();
    if (fData == null)
      return null;

    String formName = fData.getName();
    if (formName == null)
      return null;


    String startString = _FULL_PAGE_START;
    String endString = (returnTrue ? _TRUE_END : _FALSE_END);

    int length = (startString.length()
                  + formName.length()
                  + 4 // close quote, comma, validated flag, and one more comma
                  + (event == null ? 0 : event.length() + 9)
                  + (source.length() + 11)
                  + endString.length());

    if (extraParams != null)
    {
      length += (1 + extraParams.length());
    }

    StringBuffer buffer = new StringBuffer(length);
    buffer.append(startString);
    buffer.append(formName);
    buffer.append(immediate ? "',0," : "',1,");

    buffer.append("{source:");
    _appendJSParameter(buffer, source);

    if (event != null)
    {
      buffer.append(",event:");
      _appendJSParameter(buffer, event);
    }

    if (extraParams != null)
    {
      buffer.append(",");
      buffer.append(extraParams);
    }

    buffer.append('}');
    buffer.append(endString);

    return buffer.toString();
  }

  /**
   * Returns a String value which can be used as the onclick handler for
   * an element which fires partial change events.
   *
   * @param destination The destination URL, which contains any
   *   event information, including the partialTargets parameter.
   */
  public static String getPartialGetScript(String destination)
  {
    // Pre-compute StringBuffer size
    int length = _FIRE_PARTIAL_CHANGE_START.length() +
                 _FIRE_PARTIAL_CHANGE_END.length()   +
                 destination.length();

    StringBuffer buffer = new StringBuffer(length);
    buffer.append(_FIRE_PARTIAL_CHANGE_START);
    buffer.append(destination);
    buffer.append(_FIRE_PARTIAL_CHANGE_END);

    return buffer.toString();
  }

  public static String getSubmitScript(
     AdfRenderingContext arc,
     String              source,
     boolean             immediate
     )
  {
    return getSubmitScript(arc, source, immediate, false);
  }

  public static String getSubmitScript(
     AdfRenderingContext arc,
     String              source,
     boolean             immediate,
     boolean             isRadio)
  {
    return getSubmitScript(arc, source, immediate, isRadio, source, null, true);
  }

  /**
   * TODO: remove "isRadio", which shouldn't be necessary
   */
  public static String getSubmitScript(
     AdfRenderingContext arc,
     String              source,
     boolean             immediate,
     boolean             isRadio,
     String              event,
     String              extraParams,
     boolean             returnTrue)
  {
    // Get the formName
    FormData fData = arc.getFormData();
    if (fData == null)
      return null;

    String formName = fData.getName();
    if (formName == null)
      return null;

    String startString = _START;

    if (isRadio)
      startString = _START_RADIO;

    String endString = (returnTrue ? _TRUE_END : _FALSE_END);

    int length = (startString.length()
                  + formName.length()
                  + 4 // close quote, comma, validated flag, and one more comma
                  + (source.length() + 1)
                  + (event == null ? 2 : event.length() + 1)
                  + endString.length());
    if (extraParams != null)
    {
      length += (3 + extraParams.length());
    }

    // Create the buffer
    StringBuffer buffer = new StringBuffer(length);

    // Build up the script
    buffer.append(startString);
    buffer.append(formName);
    buffer.append("\',");
    buffer.append(immediate ? "0" : "1");

    buffer.append(",");
    _appendJSParameter(buffer, event); // eventName
    buffer.append(",");
    _appendJSParameter(buffer, source); // source

    if (extraParams != null)
    {
      buffer.append(",{");
      buffer.append(extraParams);
      buffer.append("}");
    }

    buffer.append(endString);
    return buffer.toString();
  }

  // Appends a parameter to a JavaScript function call buffer
  private static void _appendJSParameter(
    StringBuffer buffer,
    String value
    )
  {
    if (value == null)
    {
      buffer.append("0");
    }
    else
    {
      // double escape in-quotes string
      // e.g. "\'" + escapeJS("a'b") + "\'" -> "\'a\\\'b\'"
      buffer.append("\'");
      XhtmlUtils.escapeJS(buffer, value, true, 2);
      buffer.append("\'");
    }
  }

  // Scriptlet that renders the submitPartialUpdate script
  private static class AutoSubmitScriptlet extends Scriptlet
  {
    static public Scriptlet sharedInstance()
    {
      return _sInstance;
    }

    public Object getScriptletKey()
    {
      return _AUTO_SUBMIT_SCRIPTLET;
    }

    protected void outputScriptletContent(
      FacesContext context,
      AdfRenderingContext arc)
      throws IOException
    {
      // Make sure we have a form
      if (arc.getFormData() == null)
        return;

      String formName = arc.getFormData().getName();
      if (formName == null)
        return;

      // We output a function "_adfspu" (ADF Submit Partial Update)
      // which takes the following arguments:
      // - f:  The form name
      // - v:  The validation flag
      // - e:  The event name (defaults to "update")
      // - s:  The source parameter
      // - o:  Object containing client-defined parameters

      // To support redirecting on IE, we need to check if the parent page is
      // actually responding to a partial update call, so we keep track of it
      // here. We don't worry about re-setting this to false because the only
      // time it comes into play is when the back button is causing a re-render
      // of the iframe and, hence, a re-execution of the redirect script. In
      // that case, we'll have a new javascript context, and _pprUpdateMode will
      // be invalid, or will be initialized to false.

      ResponseWriter writer = context.getResponseWriter();
      writer.writeText("var _pprUpdateMode=false;", null);
      writer.writeText("function _adfspu(f,v,e,s,o){", null);
      writer.writeText("_pprUpdateMode=true;", null);
      writer.writeText("if(!o)o=new Object();", null);
      writer.writeText("o.", null);
      writer.writeText(XhtmlConstants.EVENT_PARAM, null);
      // Should this 'update' value be formEncoded?
      writer.writeText("=(e)?e:\'update\';", null);
      writer.writeText("if(s)o.", null);
      writer.writeText(XhtmlConstants.SOURCE_PARAM, null);
      writer.writeText("=s;_submitPartialChange(f,v,o);}", null);
    }

    private static AutoSubmitScriptlet _sInstance =
      new AutoSubmitScriptlet();
  }

  static
  {
    XhtmlScriptletFactory.registerAllScriptlets();

    // Register our scriptlet
    AutoSubmitScriptlet.sharedInstance().registerSelf();
  }

  // Name for our Scriptlet
  private static final String _AUTO_SUBMIT_SCRIPTLET =
    "org.apache.myfaces.adfinternal.renderkit.core.xhtml.AUTO_SUBMIT_SCRIPTLET";

  private static final String _FULL_PAGE_START = "submitForm(\'";
  private static final String _START = "_adfspu(\'";
  private static final String _START_RADIO = "_radioSet_adfspu(\'";
  private static final String _TRUE_END = ");return true;";
  private static final String _FALSE_END = ");return false;";

  // Partial page rendering scripts
  private static final String _FIRE_PARTIAL_CHANGE_START =
    "_firePartialChange(\'";
  private static final String _FIRE_PARTIAL_CHANGE_END = "\');return false;";
}
