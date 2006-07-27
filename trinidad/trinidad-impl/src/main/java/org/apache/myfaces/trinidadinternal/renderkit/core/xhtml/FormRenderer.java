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

import java.io.IOException;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.faces.component.UIComponent;
import javax.faces.component.UIForm;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.convert.Converter;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXForm;
import org.apache.myfaces.trinidad.component.core.CoreForm;
import org.apache.myfaces.trinidad.context.RequestContext;

import org.apache.myfaces.trinidad.context.Agent;

import org.apache.myfaces.trinidadinternal.renderkit.RenderingContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreResponseStateManager;
import org.apache.myfaces.trinidadinternal.renderkit.core.ppr.PartialPageContext;
import org.apache.myfaces.trinidadinternal.renderkit.uix.SubformRenderer;

// TODO: Remove this class
import org.apache.myfaces.trinidadinternal.share.data.ServletRequestParameters;

import org.apache.myfaces.trinidadinternal.util.IntegerUtils;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/FormRenderer.java#0 $) $Date: 10-nov-2005.18:53:51 $
 * @author The Oracle ADF Faces Team
 */
public class FormRenderer extends XhtmlRenderer
{
  public FormRenderer()
  {
    super(CoreForm.TYPE);
  }

  public void decode(FacesContext context,
                     UIComponent component)
  {
    Map paramMap = context.getExternalContext().getRequestParameterMap();
    Object formName = paramMap.get(CoreResponseStateManager.FORM_FIELD_NAME);
    boolean submitted = false;

    if ( formName != null )
      submitted = formName.equals(getClientId(context, component));

    // We use this decode for both our form and UIForm
    if (component instanceof UIForm)
      ((UIForm) component).setSubmitted(submitted);
    else
      ((UIXForm) component).setSubmitted(submitted);
  }

  public boolean getRendersChildren()
  {
    return false;
  }

  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);

    _usesUploadKey = type.findKey("usesUpload");
    _defaultCommandKey = type.findKey("defaultCommand");
    _onsubmitKey = type.findKey("onsubmit");
    _targetFrameKey = type.findKey("targetFrame");
  }


  protected void encodeBegin(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();

    String formName = getClientId(context, comp);

    FormData fData = new FormData(formName);
    arc.setFormData(fData);

    String defaultCommand = getDefaultCommand(bean);
    if (defaultCommand != null)
    {
      fData.setDefaultCommandId(defaultCommand);
    }

    if (formName != null)
    {
      // =-=AEW This should get removed in favor of solely using FormData;
      // but keep it around for now
      arc.getProperties().put(XhtmlConstants.FORM_NAME_PROPERTY, formName);
    }

    if (supportsScripting(arc))
    {
      // we depend on the form submission library
      XhtmlUtils.addLib(context, arc, "submitForm()");
      XhtmlUtils.addLib(context, arc, "_submitOnEnter()");

      // if the onSubmit attribute is set, generate the JavaSCript variable
      // with the source code to execute when submitting.  We do this,
      // because the normal onSubmit handler on forms is NOT called if
      // the form is submitted programatically
      String onsubmit = getOnsubmit(bean);

      if (onsubmit != null)
      {
        rw.startElement("script", null);
        renderScriptDeferAttribute(context, arc);

        // Bug #3426092:
        // render the type="text/javascript" attribute in accessibility mode
        renderScriptTypeAttribute(context, arc);

        rw.write("var _");
        rw.write(XhtmlUtils.getJSIdentifier(formName));
        rw.write("_Submit=\"");
        rw.writeText(onsubmit, null);
        rw.write('"');

        rw.endElement("script");
      }
    }


    rw.startElement("form", comp);
    renderId(context, comp);
    renderAllAttributes(context, arc, bean);

    // Render the method.  It's POST if they want file upload, and they
    // can actually upload files otherwise we'll just use whatever they request
    if (getUsesUpload(bean))
    {
      rw.writeAttribute("enctype", "multipart/form-data", "usesUpload");
    }

    rw.writeAttribute("method", "POST", null);

    rw.writeAttribute("onkeypress",
                      getFullOnkeypress(context,
                                        comp,
                                        bean,
                                        formName),
                      "onkeypress");

    // render the autocomplete attribute
    if (supportsAutoCompleteFormElements(arc))
    {
      // TODO: support autocomplete
      boolean noAutocomplete = getNoAutocomplete(bean);
      if (noAutocomplete)
      {
        rw.writeAttribute("autocomplete", "off", "noAutocomplete");
      }
    }

    String viewId = context.getViewRoot().getViewId();
    String action =
      context.getApplication().getViewHandler().getActionURL(context, viewId);

    rw.writeURIAttribute("action", action, null);

    if (supportsTarget(arc))
    {
      rw.writeAttribute("target", getTargetFrame(bean), "targetFrame");
    }
  }


  protected void encodeEnd(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         comp,
    FacesBean           bean) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    //VAC isPIE variable added for bug 4526850
    boolean isPIE = Agent.PLATFORM_PPC.equalsIgnoreCase(
                        arc.getAgent().getPlatformName());

    String formName = arc.getFormData().getFormName();
    PartialPageContext pprContext = arc.getPartialPageContext();

    boolean isXMLDOM = supportsXMLDOM(arc);

    // Write out the hidden form field that identifies which
    // form is the one being submitted
    writer.startElement("input", null);
    writer.writeAttribute("type", "hidden", null);
    writer.writeAttribute("name", CoreResponseStateManager.FORM_FIELD_NAME, null);
    writer.writeAttribute("value", formName, null);
    writer.endElement("input");

    // Check to see if this is a partial page render.  If so, we need
    // to push the ID of the postscript onto the partial target stack
    String postscriptId = _getPostscriptId(arc, formName);
    if (pprContext != null)
    {
      _startPartialPostscriptRender(pprContext, postscriptId);
      if (isXMLDOM)
      {
        writer.startElement("ppr",null);
        writer.writeAttribute("target_id", postscriptId,null);
        writer.write("<![CDATA[");
      }
    }

    if (postscriptId != null)
    {
      // We wrap all of our values/scripts inside of a span so that
      // they can be easily updated during a partial page render.
      // Note: it is essential that we call context.getResponseWriter()
      // *after* calling _startPartialPostscriptRender().  Otherwise,
      // we'll end up writing to the null output method.
      writer.startElement("span", null);
      writer.writeAttribute("id", postscriptId, null);
    }

    // Include JSF state.
    context.getApplication().getViewHandler().writeState(context);

    // Render any needed values
    //VAC this condition is needed for bug 4526850- It ensures that only
    //state token and form name parameters are overwritten when there is
    //a partial page submission.
    if (!isPIE)
    {
      _renderNeededValues(context, arc);
    }


    // Render any validation scripts
    _renderValidationScripts(context, arc);

    // Render reset calls
    _renderResetCalls(context, arc);

    // Close up our postscript span if we have one
    if (postscriptId != null)
      writer.endElement("span");

    //this condition is needed for bug 4526850- It ensures that only
    //state token and form name parameters are overwritten when there is
    //a partial page submission.
    if (isPIE && pprContext == null)
    {
      _renderNeededValues(context, arc);
    }

    // Pop the partial target stack if this is a partial page render
    if (pprContext != null)
    {
      if (isXMLDOM)
      {
        writer.write("]]>");
        writer.endElement("ppr");
      }
      _endPartialPostscriptRender(pprContext, postscriptId);
    }

    // Render submitFormCheck js function --
    // checks if submitForm was rejected because form was incomplete
    // when it was called, and thus calls submitForm again.
    if (pprContext == null)
      _renderSubmitFormCheck(context, arc);


    // Close up the form
    writer.endElement("form");

    // Clear out the form name property
    arc.getProperties().remove(XhtmlConstants.FORM_NAME_PROPERTY);

    // clear out form data:;
    arc.clearFormData();
  }


  /**
   * Returns the inline Style used to render this node.
   */
  protected String getInlineStyle(FacesBean bean)
  {
    String inlineStyle = super.getInlineStyle(bean);
    if (inlineStyle == null)
      return "margin:0px";

    return inlineStyle + ";margin:0px";
  }


  /**
   * Render the client ID as both an "id" and a "name"
   */
  protected void renderId(
    FacesContext context,
    UIComponent  component) throws IOException
  {
    String clientId = getClientId(context, component);

    context.getResponseWriter().writeAttribute("id", clientId, "id");
    context.getResponseWriter().writeAttribute("name", clientId, "id");
  }


  /**
   * All editable components need IDs.
   */
  protected boolean shouldRenderId(
    FacesContext context,
    UIComponent  component)
  {
    return true;
  }

  // Renders reset call code
  private static void _renderResetCalls(
    FacesContext context,
    RenderingContext arc) throws IOException
  {

    // if scripting isn't supported, no need to do the rest
    if (!supportsScripting(arc))
      return;

    //
    // Write the array of reset calls
    //
    FormData fData = arc.getFormData();
    List resetCallList = fData.getResetCalls(false);

    int resetCallCount = (resetCallList != null)
                            ? resetCallList.size()
                            : 0;

    if (resetCallCount != 0)
    {
      String jsID = XhtmlUtils.getJSIdentifier(arc.getFormData().getFormName());

      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("script", null);
      renderScriptDeferAttribute(context, arc);
      // Bug #3426092:
      // render the type="text/javascript" attribute in accessibility mode
      renderScriptTypeAttribute(context, arc);

      writer.write("var _");
      writer.write(jsID);
      writer.write("_Reset=[");

      boolean firstCall = true;

      for (int i = 0; i < resetCallCount; i++)
      {
        String currCall = (String)resetCallList.get(i);

        if (firstCall)
        {
          firstCall = false;
        }
        else
        {
          // write the separator every time except the first time
          writer.write(",");
        }

        // write the error format
        // use single quotes since embedded single quotes
        // are automatically escaped
        writer.write("\'");
        writer.write(XhtmlUtils.escapeJS(currCall));
        writer.write("\'");
      }

      writer.write("];");
      writer.endElement("script");
    }

  }


  // Renders validation code
  private static void _renderValidationScripts(
    FacesContext        context,
    RenderingContext arc
    ) throws IOException
  {
    // if scripting isn't supported, no need to do the rest
    if (!supportsScripting(arc))
      return;

    //
    // Output validation-related JavaScript
    //
    ResponseWriter writer = context.getResponseWriter();
    FormData       fData = arc.getFormData();

    // Fix up the form name for use as a Javascript identifier
    String jsID = XhtmlUtils.getJSIdentifier(arc.getFormData().getFormName());

    writer.startElement("script", null);
    renderScriptDeferAttribute(context, arc);
    renderScriptTypeAttribute(context, arc);

    //
    // Write the array of client dependencies
    // Whether or not client side validation is enabled,
    // The dependencies may be needed - see bug
    // 4409339 TURNING OFF CLIENT SIDE VALIDATION CAUSES
    //                                ERRORS IN SELECTINPUTCOLOR & DATE
    List clientDependencies = fData.getClientDependencies( false);
    if (clientDependencies != null)
    {
      for (int d = 0; d < clientDependencies.size(); d++)
      {
        writer.writeText(clientDependencies.get(d),null);
      }
    }

    // TODO - when there are immediate components validate only those on the
    // client?
    // When there is an immediate component do server side validation,
    // see bug 4697440 CLIENT VALIDATION HANDLES
    //                 EDITABLEVALUEHOLDER IMMEDIATE INCORRECTLY
    boolean hasImmediateComponent = fData.hasImmediateComponent();

    boolean isClientValidationDisabled =
      RequestContext.getCurrentInstance().isClientValidationDisabled();

    if (isClientValidationDisabled || hasImmediateComponent)
    {
      writer.writeText("function _", null);
      writer.writeText(jsID, null);
      // no validation, so validation always succeeds
      writer.writeText("Validator(){return true;}", null);
      writer.endElement("script");
      return;
    }



    //
    // Write the array of validation calls
    //

    Iterator validationIterator = fData.getValidationIterator();

    if (validationIterator != null)
    {
      writer.writeText("var _", null);
      writer.writeText(jsID, null);
      writer.writeText("_Validations=[", null);

      boolean firstValidation = true;

      while(validationIterator.hasNext())
      {
        String currValidation = (String)validationIterator.next();

        if (firstValidation)
        {
          firstValidation = false;
        }
        else
        {
          // write the separator every time except the first time
          writer.writeText(",", null);
        }

        // write the error format
        // use single quotes since embedded single quotes
        // are automatically escaped
        writer.writeText("\'", null);
        writer.writeText(XhtmlUtils.escapeJS(currValidation), null);
        writer.writeText("\'", null);
      }

      writer.writeText("];", null);
    }


    //
    // write the validation function for this form
    //
    writer.writeText("function _", null);
    writer.writeText(jsID, null);

    // get the form validators
    List validatorInfoList = fData.getFormValidatorsInfo(false);

    if (validatorInfoList == null)
    {
      // no validation, so validation always succeeds
      writer.writeText("Validator(){return true;}", null);
    }
    else
    {
      writer.writeText("Validator(f,s){var fl = _multiValidate(f,s,[", null);

      boolean firstFormInfo = true;

      for (int j = 0; j < validatorInfoList.size(); j++)
      {

        if (firstFormInfo)
        {
          firstFormInfo = false;
        }
        else
        {
          // write the separator every time except the first time
          writer.writeText("],", null);
        }

        FormData.ConvertValidate convertValidate =
          (FormData.ConvertValidate)validatorInfoList.get(j);

        writer.writeText("\"", null);

        // write the element name of the element to be validated
        writer.writeText(convertValidate.clientId, null);
        writer.writeText("\",", null);

        // write out whether or not this element is required
        writer.writeText(convertValidate.required? "1" : "0", null);
        writer.writeText(",", null);

        if (convertValidate.requiredFormatIndex != null)
        {
          // write out the index of the required error message
          writer.writeText(convertValidate.requiredFormatIndex, null);
        }

        writer.writeText(",", null);

        Object[] converterInfo = convertValidate.converter;

        if (converterInfo != null)
        {
          writer.writeText("[", null);
          writer.writeText(converterInfo[0], null);
          writer.writeText(",", null);
          writer.writeText(converterInfo[1], null);
          writer.writeText("],", null);
        }
        else
        {
          writer.writeText("(void 0),", null);
        }

        writer.writeText("[", null);

        ArrayList validatorInfo = convertValidate.validators;

        if (validatorInfo != null)
        {
          boolean firstValidator = true;

          int i = 0;
          while (i < validatorInfo.size())
          {
            if (firstValidator)
            {
              firstValidator = false;
            }
            else
            {
              // write the separator every time except the first time
              writer.writeText(",", null);
            }

            // write the validation string for the validater
            writer.writeText(validatorInfo.get(i).toString(), null);


            writer.writeText(",", null);

            // write the index of the format for handling errors
            Integer formatIndex = (Integer)validatorInfo.get(i + 1);
            if (formatIndex != null)
            {
              writer.writeText(IntegerUtils.getString(formatIndex.intValue()),
                               null);
            }

            i = i + 2;
          }
        }
      }

      writer.writeText("]", null);

      writer.writeText("]);if(fl.length>0){_validationAlert('", null);
      writer.writeText(XhtmlUtils.escapeJS(
                          arc.getTranslatedString("af_form.SUBMIT_ERRORS")),
                       null);
        writer.writeText("'+fl);return false;}else{return true;}}", null);
    }

    //
    // Render the labels used by validated fields in this form
    //

    // list of labels used for validation on this form
    List inputList = fData.getValidatedInputList(false);

    int inputCount = (inputList != null)
                         ? inputList.size()
                         : 0;

    if (inputCount > 0)
    {
      Map labelMap = fData.getLabelMap(false);

      if (labelMap != null)
      {
        writer.writeText("var _", null);
        writer.writeText(jsID, null);
        writer.writeText("_Labels={", null);

        boolean firstLabel = true;

        for (int i = 0; i < inputCount; i++)
        {
          String currID = (String)inputList.get(i);

          // remove the ID entry to prevent multiple labels from
          // being written
          String currLabel = (String)labelMap.remove(currID);

          if (currLabel != null)
          {
            if (firstLabel)
            {
              firstLabel = false;
            }
            else
            {
              // write the separator every time except the first time
              writer.writeText(",", null);
            }

            // write the ID of the validated field as the key
            writer.writeText("\'", null);
            writer.writeText(currID, null);
            writer.writeText("\':\'", null);

            // write the label of the validated field as the value
            writer.writeText(XhtmlUtils.escapeJS(currLabel), null);
            writer.writeText("\'", null);
          }
        }

        writer.writeText("};", null);

      }

      // =-= jrf: optimize pattern reuse?
      // BUG 2024773

      Map patternMap = fData.getPatternMap(false);

      if (patternMap != null)
      {
        writer.writeText("var _", null);
        writer.writeText(jsID, null);
        writer.writeText("_Patterns={", null);

        boolean firstPattern = true;

        for (int i = 0; i < inputCount; i++)
        {
          String currID = (String)inputList.get(i);

          // remove the ID entry to prevent multiple labels from
          // being written
          String currPattern = (String)patternMap.remove(currID);

          if (currPattern != null)
          {
            if (firstPattern)
            {
              firstPattern = false;
            }
            else
            {
              // write the separator every time except the first time
              writer.writeText(",", null);
            }

            // write the ID of the validated field as the key
            writer.writeText("\'", null);
            writer.writeText(currID, null);
            writer.writeText("\':\'", null);

            // write the label of the validated field as the value
            writer.writeText(XhtmlUtils.escapeJS(currPattern), null);
            writer.writeText("\'", null);
          }
        }

        writer.writeText("};", null);

      }
    }

    //
    // Render the error format list for this form
    //

    // list of error formats used for validation on this form
    Iterator errorFormatIterator = fData.getErrorFormatIterator();

    if (errorFormatIterator != null)
    {
      writer.writeText("var _", null);
      writer.writeText(jsID, null);
      writer.writeText("_Formats=[", null);

      boolean firstFormat = true;

      while(errorFormatIterator.hasNext())
      {
        String currErrorFormat = (String)errorFormatIterator.next();

        if (firstFormat)
        {
          firstFormat = false;
        }
        else
        {
          // write the separator every time except the first time
          writer.writeText(",", null);
        }

        // write the error format
        writer.writeText("'", null);
        writer.writeText(XhtmlUtils.escapeJS(currErrorFormat), null);
        writer.writeText("'", null);
      }

      writer.writeText("];", null);
    }

    _renderSubformLists(context, jsID);

    writer.endElement("script");
  }


  private static void _renderSubformLists(
    FacesContext      context,
    String           jsID
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    List subforms =
      SubformRenderer.getSubformList(context, false, false);
    writer.writeText("var ", null);
    writer.writeText(jsID, null);
    writer.writeText("_SF={", null);
    if ((subforms != null) && !subforms.isEmpty())
    {
      List defaultSubforms =
        SubformRenderer.getSubformList(context, true, false);
      Iterator ids = subforms.iterator();
      while (ids.hasNext())
      {
        String id = (String) ids.next();
        writer.writeText("\"", null);
        writer.writeText(id, null);
        writer.writeText("\":", null);
        if ((defaultSubforms != null) && defaultSubforms.contains(id))
          writer.writeText("2", null);
        else
          writer.writeText("1", null);

        if (ids.hasNext())
          writer.writeText(",", null);
      }
    }

    writer.writeText("};", null);
  }


  // render script which will check if submitForm script has been called before
  // the form has rendered in its entirety, and if so, it will re-call
  // submitForm.
  // this script should be rendered at the very end of the form.
  private static void _renderSubmitFormCheck(
    FacesContext        context,
    RenderingContext arc
    ) throws IOException
  {
    // if scripting isn't supported, no need to do the rest
    if (!supportsScripting(arc))
      return;

    ResponseWriter writer = context.getResponseWriter();
    writer.startElement("script", null);
    renderScriptDeferAttribute(context, arc);
    // Bug #3426092:
    // render the type="text/javascript" attribute in accessibility mode
    renderScriptTypeAttribute(context, arc);

    writer.writeText("_submitFormCheck();", null);
    writer.endElement("script");
  }



  /**
   * @param call a function call.
   * "eval(call)" will be called on the client when resetting.
   */
  public static void addResetCall(
    String           call
    )
  {
    FormData fData = RenderingContext.getCurrentInstance().getFormData();
    fData.addResetCall(call);
  }

 public static void addOnSubmitConverterValidators(
    UIComponent      component,
    Converter        converter,
    Iterator         validators,
    String           clientId,
    boolean          immediate,
    boolean          required,
    String           requiredMessageKey
    ) throws IOException
  {
    FormData fData = RenderingContext.getCurrentInstance().getFormData();

    fData.addOnSubmitConverterValidators(component,
                                         converter,
                                         validators,
                                         clientId,
                                         immediate,
                                         required,
                                         requiredMessageKey);
  }



  /**
   * Add a mapping of an input element ID to a label String. If there is a
   * client-side error regarding the form element with the given ID, the given
   * label will be used in the client-side error message.
   * @param targetID the ID of the form element
   * @param label the label that describes the form element
   * <code>targetID</code>
   */
  public static void addLabelMapping(
    String           targetID,
    String           label
    )
  {
    FormData fData = RenderingContext.getCurrentInstance().getFormData();
    fData.addLabel(targetID, label);
  }


  /**
   * Add a mapping of an input element ID to a pattern String. If there is a
   * client-side error regarding the form element with the given ID, the given
   * pattern will be used in the client-side error message.
   * @param targetID the ID of the form element
   * @param pattern the pattern that describes the form element
   * <code>targetID</code>
   */
  public static void addPatternMapping(
    String           targetID,
    String           pattern
    )
  {
    FormData fData = RenderingContext.getCurrentInstance().getFormData();
    fData.addPattern(targetID, pattern);
  }

  public static String getDefaultCommandId(
  )
  {
    FormData fData = RenderingContext.getCurrentInstance().getFormData();
    return fData.getDefaultCommandId();
  }


  public static int getInputTextCount(
  )
  {
    FormData fData = RenderingContext.getCurrentInstance().getFormData();
    return fData.getInputTextCount();
  }

  public static void incrementInputTextCount(
  )
  {
    FormData fData = RenderingContext.getCurrentInstance().getFormData();
    fData.incrementInputTextCount();
  }

  // Returns the ID to use for our postscript if PPR is supported
  private static String _getPostscriptId(
    RenderingContext arc,
    String formName
    )
  {
    if (PartialPageUtils.supportsPartialRendering(arc))
      return "_" + formName + "_Postscript";

    return null;
  }

  // Starts rendering the postscript partial target
  private static void _startPartialPostscriptRender(
    PartialPageContext pprContext,
    String             postscriptId
    )
  {
    // Notify the PartialPageContext that we are about to
    // start rendering our postscript target
    pprContext.pushRenderedPartialTarget(postscriptId);
    pprContext.addRenderedPartialTarget(postscriptId);
  }

  // Ends rendering the postscript partial target
  private static void _endPartialPostscriptRender(
    PartialPageContext pprContext,
    String             postscriptId
    )
  {
    // Pop the PartialPageContext targets stack when we are
    // done rendering the postscript
    if (pprContext.isPartialTarget(postscriptId))
      pprContext.popRenderedPartialTarget();
  }

  protected String getDefaultCommand(FacesBean bean)
  {
    return toString(bean.getProperty(_defaultCommandKey));
  }

  protected String getOnsubmit(FacesBean bean)
  {
    return toString(bean.getProperty(_onsubmitKey));
  }

  protected String getTargetFrame(FacesBean bean)
  {
    return toString(bean.getProperty(_targetFrameKey));
  }


  protected boolean getUsesUpload(FacesBean bean)
  {
    Object o = bean.getProperty(_usesUploadKey);
    if (o == null)
      o = _usesUploadKey.getDefault();

    return Boolean.TRUE.equals(o);
  }


  protected String getFullOnkeypress(
     FacesContext context,
     UIComponent  component,
     FacesBean    bean,
     String       clientId)
  {
    String onKeypress = super.getOnkeypress(bean);

    String defaultCommand = getDefaultCommand(bean);

    String submitFunc;

    UIComponent defaultCommandComponent = null;
    if (defaultCommand != null)
    {
      defaultCommandComponent
        = component.findComponent(defaultCommand);
    }

    if (defaultCommandComponent != null)
    {
      // Get the true clientId
      String defaultCommandId =
        defaultCommandComponent.getClientId(context);
      submitFunc = "return _submitOnEnter"
                     + "(event,'"  + clientId
                     + "'," + "'" + defaultCommandId + "');";
    }
    else
    {
      submitFunc = "return _submitOnEnter(event,'" +
                             clientId +
                             "');";
    }

    onKeypress = XhtmlUtils.getChainedJS(onKeypress, submitFunc, true);

    return onKeypress;
  }

  protected String getOnkeypress(FacesBean bean)
  {
    // Back out the default keypress, since we need more info
    return null;
  }

  protected boolean getNoAutocomplete(FacesBean bean)
  {
    // TODO: Support disabling autocomplete
    return false;
  }



  private static void _renderHiddenField(
    ResponseWriter writer,
    Object       name,
    Object       value
    ) throws IOException
  {
    writer.startElement("input", null);
    writer.writeAttribute("type", "hidden", null);
    writer.writeAttribute("name", name, null);
    writer.writeAttribute("value", value, null);
    writer.endElement("input");
  }

  /**
   * Render each "needed" FormValue that hasn't already
   * been rendered.  Called by FormRenderer.postrender().
   * @param formName the name of the form. Only hidden form values "needed"
   *  by this form are rendered.
   * @see FormRenderer#postrender(RenderingContext, UINode)
   */
  static private void _renderNeededValues(
    FacesContext        context,
    RenderingContext arc
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();
    FormData fData = arc.getFormData();

    if (fData.useCompoundNames())
    {
      // We are rendering compound names instead of rendering
      // hidden fields without values
      _renderHiddenField(writer,
                         ServletRequestParameters.HAS_COMPOUND_NAME,
                         "a");
    }
    else
    {
      int realNeededIndex = 0;
      List neededValues = fData.getNeededValues(false);

      //
      // loop over the list of needed names, creating hidden fields
      // for any that have not already been rendered
      //
      int neededCount = (neededValues != null)
                          ? neededValues.size()
                          : 0;

      if (neededCount > 0)
      {
        // if we support scripting use null as the value for
        // the needed values, as we will be filling them
        // in.  For non-js platforms, use a value that will
        // result in the value being passed to the server
        String neededValue = (supportsScripting(arc))
                                ? null
                                : "a";

        Set renderedValues = fData.getRenderedValues(true);

        for (int i = 0; i < neededCount; i++)
        {
          Object currName = neededValues.get(i);

          // if the needed name hasn't been rendered, add it to our
          // list of unrendered elements
          if (!renderedValues.contains(currName))
          {
            // move this item to be the last actually needed item
            neededValues.set(realNeededIndex, currName);
            realNeededIndex++;

            // generate the hidden form field for this needed item
            _renderHiddenField(writer, currName, neededValue);
            fData.addRenderedValue(currName.toString());
          }
        }

        //
        // Output the javascript array of fields that need to be reset
        //
        if (realNeededIndex > 0)
        {
          if (supportsScripting(arc))
          {
            writer.startElement("script", null);
            renderScriptDeferAttribute(context, arc);
            // Bug #3426092:
            // render the type="text/javascript" attribute in accessibility mode
            renderScriptTypeAttribute(context, arc);

            writer.writeText("var _reset", null);
            writer.writeText(XhtmlUtils.getJSIdentifier(fData.getFormName()),
                             null);
            writer.writeText("Names=[\"", null);
            writer.writeText(neededValues.get(0).toString(), null);

            for (int i = 1; i < realNeededIndex; i++)
            {
              writer.writeText("\",\"", null);
              writer.writeText(neededValues.get(i).toString(), null);
            }

            writer.writeText("\"];", null);
            writer.endElement("script");
          }
        }
      }
    }
  }


  // key used to indicate whether or not usesUpload is used:
  public static final Object USES_UPLOAD_KEY = new Object();

  private PropertyKey _usesUploadKey;
  private PropertyKey _defaultCommandKey;
  private PropertyKey _onsubmitKey;
  private PropertyKey _targetFrameKey;


  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FormRenderer.class);
}
