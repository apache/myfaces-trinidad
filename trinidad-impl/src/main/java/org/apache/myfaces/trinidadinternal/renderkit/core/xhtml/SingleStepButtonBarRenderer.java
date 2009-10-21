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
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXSingleStep;
import org.apache.myfaces.trinidad.component.core.nav.CoreSingleStepButtonBar;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.IntegerUtils;
import org.apache.myfaces.trinidad.render.XhtmlConstants;

import org.apache.myfaces.trinidadinternal.util.nls.StringUtils;

/**
 * Renderer for singleStepButtonBar components
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/uix/SingleStepRenderer.java#0 $) $Date: 10-nov-2005.19:00:37 $
 */
public class SingleStepButtonBarRenderer extends XhtmlRenderer
{
  public SingleStepButtonBarRenderer()
  {
    super(CoreSingleStepButtonBar.TYPE);
  }
  
  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);
    _selectedStepKey = type.findKey("selectedStep");
    _maxStepKey = type.findKey("maxStep");
    _textKey = type.findKey("text");
    _backButton = new Button(type, false);
    _nextButton = new Button(type, true);
  }

  @SuppressWarnings("unchecked")
  @Override
  public void decode(FacesContext context, UIComponent component)
  {
    Map<String, String> parameters =  
      context.getExternalContext().getRequestParameterMap();
    
    String source = parameters.get(TrinidadRenderingConstants.SOURCE_PARAM);
    String id = getClientId(context, component);
    if ((source != null) && source.startsWith(id))
    {
      // queue the action on the singleStep component
      // set immediate to true when going back (no validation)
      // and set to false when going forward (validation).
      String suffix = source.substring(id.length());
      UIXSingleStep singleStep = (UIXSingleStep) component;
      if (suffix.equals(_NEXT_ID_SUFFIX))
        singleStep.setActionType(UIXSingleStep.NEXT_ACTION_TYPE);
      else if (suffix.equals(_BACK_ID_SUFFIX))
        singleStep.setActionType(UIXSingleStep.PREVIOUS_ACTION_TYPE);
      else
        return;

      // queue an action event
      // This must be added to queue AFTER the actionType is set
      (new ActionEvent(component)).queue();
    }
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }
  
  @Override
  protected String getDefaultStyleClass(FacesBean bean)
  {
    return SkinSelectors.AF_SINGLE_STEP_BUTTON_BAR;
  }

  @Override
  protected void encodeAll(
    FacesContext        context,
    RenderingContext    arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    if (canSkipRendering(context, arc, component))
      return;

    long currentValue = getSelectedStep(bean);

    // get max value
    long totalItems = getMaxStep(bean);

    boolean showBackButton = (currentValue > 1);
    boolean showNextButton = ((totalItems == _MAX_VALUE_UNKNOWN) ||
                               (currentValue < totalItems));

    // bail if no buttons shown or values are bogus
    if ((!showBackButton && !showNextButton) ||
        ((currentValue > totalItems) && (totalItems != _MAX_VALUE_UNKNOWN)) ||
        (currentValue < _MAX_VALUE_UNKNOWN))
      return;


    String formName = arc.getFormData().getName();
    if (formName == null)
    {
      _LOG.warning("SINGLE_STEP_MUST_INSIDE_FORM");
      return;
    }

    if (!supportsNavigation(arc))
      return;

    // If we don't support navigation (e.g., printable pages),
    // lie and claim we support scripting (even though we probably don't).
    // This will give us the highest fidelity output - that is,
    // we avoid creating submit buttons.
    boolean renderAsTable = SelectRangeChoiceBarRenderer.__renderAsTable(component);
    
    // start the rendering
    ResponseWriter writer = context.getResponseWriter();

    // FIXME: when inside a buttonbar, etc., PPR will be badly broken
    if (renderAsTable)
    {
      writer.startElement("table", component);
      renderAllAttributes(context, arc, bean);
      renderId(context, component);
      OutputUtils.renderLayoutTableAttributes(context, arc, "0", null);
      writer.startElement("tr", null);
    }

    writer.startElement("td", null);

    // don't render back button on first step
    if (showBackButton)
    {
      delegateRenderer(context, arc, component, bean, _backButton);
      writer.endElement("td");

      _renderSpacerCell(context, arc);
      writer.startElement("td", null);
    }

    //
    // create the label and render it
    //
    writer.writeAttribute("nowrap", Boolean.TRUE, null);

    // No "Step 1 of X" when there's 1 or two steps
    if (totalItems > 2)
    {
      // the string to be displayed between buttons
      String rangeString = _getRangeString(arc,
                                           bean,
                                           currentValue,
                                           totalItems);

      writer.startElement("span", null);
      renderStyleClass(context, arc,
                       SkinSelectors.AF_SINGLE_STEP_BUTTON_BAR_LABEL);
      writer.writeText(rangeString, "text");
      writer.endElement("span");
    }

    // don't render the next button on last step
    if (showNextButton)
    {
      writer.endElement("td");
      
      _renderSpacerCell(context, arc);

      writer.startElement("td", null);
      delegateRenderer(context, arc, component, bean, _nextButton);
    }

    writer.endElement("td");

    if (renderAsTable)
    {
      writer.endElement("tr");
      writer.endElement("table");
    }
  }


  protected long getSelectedStep(FacesBean bean)
  {
    Object o = bean.getProperty(_selectedStepKey);
    if (o == null)
      o = _selectedStepKey.getDefault();
    return toLong(o);
  }


  protected long getMaxStep(FacesBean bean)
  {
    Object o = bean.getProperty(_maxStepKey);
    if (o == null)
      o = _maxStepKey.getDefault();
    return toLong(o);
  }


  protected String getText(FacesBean bean)
  {
    return toString(bean.getProperty(_textKey));
  }

  /**
   * Writes the separator between two elements
   */
  protected void renderItemSpacer(
   FacesContext context,
   RenderingContext arc) throws IOException
  {
    if (isPDA(arc))
    {
      context.getResponseWriter().writeText(XhtmlConstants.NBSP_STRING, null);
    }
    else
    {
      renderSpacer(context, arc, "5", "1");
    }
  }

  /**
   * Writes the separator between two elements
   */
  private void _renderSpacerCell(
    FacesContext       context,
    RenderingContext arc
    ) throws IOException
  {
    ResponseWriter writer = context.getResponseWriter();

    writer.startElement("td", null);
    renderItemSpacer(context, arc);
    writer.endElement("td");
  }

  private String _getRangeString(
    RenderingContext arc,
    FacesBean        bean,
    long             start,
    long             total)
  {
    String text = getText(bean);
    
    if (text == null)
        text = arc.getTranslatedString(_STEP_TEXT_KEY);
    String pattern;
    String[] parameters;

    if (total == _MAX_VALUE_UNKNOWN)
    {
      // =-= right now you can't ever get here because we don't show the
      // step number if maxStep is unknown, but just in case...
      pattern = arc.getTranslatedString(_SINGLE_RANGE_FORMAT_NO_TOTAL_STRING);
      parameters = new String[]
                     {
                       text,
                       IntegerUtils.getString(start)
                     };
      
    }
    else
    {
      pattern = arc.getTranslatedString(_SINGLE_RANGE_FORMAT_TOTAL_STRING);
      parameters = new String[]
                     {
                       text,
                       IntegerUtils.getString(start),
                       IntegerUtils.getString(total)
                     };
    }
    
    return XhtmlUtils.getFormattedString(pattern, parameters);
  }

  private PropertyKey _selectedStepKey;
  private PropertyKey _maxStepKey;
  private PropertyKey _textKey;
  private Button      _nextButton;
  private Button      _backButton;

  // resource keys
  static private final String _SINGLE_BACK_TEXT_KEY =
    "af_singleStepButtonBar.BACK";
  static private final String _SINGLE_NEXT_TEXT_KEY =
    "af_singleStepButtonBar.NEXT";
  static private final String _SINGLE_CONTINUE_TEXT_KEY =
    "af_singleStepButtonBar.CONTINUE";
  static private final String _SINGLE_RANGE_FORMAT_TOTAL_STRING =
    "af_singleStepButtonBar.FORMAT_TOTAL";
  static private final String _SINGLE_RANGE_FORMAT_NO_TOTAL_STRING =
    "af_singleStepButtonBar.FORMAT_NO_TOTAL";
  static private final String _STEP_TEXT_KEY =
    "af_singleStepButtonBar.STEP";
  static private final String _NEXT_ID_SUFFIX = "::next";
  static private final String _BACK_ID_SUFFIX = "::back";

  static private class Button extends CommandButtonRenderer
  {
    public Button(FacesBean.Type type, boolean next)
    {
      super(type);
      _next = next;
    }

    @Override
    protected String getClientId(FacesContext context, UIComponent component)
    {
      String clientId = super.getClientId(context, component);
      return clientId + (_next ? _NEXT_ID_SUFFIX : _BACK_ID_SUFFIX);
    }

    @Override
    protected String getShortDesc(FacesBean bean)
    {
      return null;
    }

    @Override
    protected String getStyleClass(FacesBean bean)
    {
      return null;
    }
    
    @Override
    protected String getInlineStyle(FacesBean bean)
    {
      return null;
    }

    @Override
    protected String getText(FacesBean bean)
    {
      String textAndAccessKey = _getTextAndAccessKey();
      return StringUtils.stripMnemonic(textAndAccessKey);
    }

    @Override
    protected char getAccessKey(FacesBean bean)
    {
      String textAndAccessKey = _getTextAndAccessKey();
      int index = StringUtils.getMnemonicIndex(textAndAccessKey);
      if (index < 0)
        return (char) 0;

      return textAndAccessKey.charAt(index + 1);
    }

    @Override
    protected void renderEventHandlers(
      FacesContext context,
      FacesBean    bean) throws IOException
    {
      context.getResponseWriter().writeAttribute("onclick",
                                                 getOnclick(bean),
                                                 null);
    }

    @Override
    protected String getComponentOnclick(FacesBean bean)
    {
      return null;
    }

    @Override
    protected boolean getImmediate(FacesBean bean)
    {
      return !_next;
    }

    @Override
    protected boolean getPartialSubmit(FacesBean bean)
    {
      return false;
    }

    @Override
    protected boolean getDisabled(FacesBean bean)
    {
      return false;
    }
    
    @Override
    protected String getIcon(FacesBean bean)
    {
      return null;
    }

    private String _getTextAndAccessKey()
    {
      RenderingContext rc = RenderingContext.getCurrentInstance();
      return rc.getTranslatedString(
                _next ? _SINGLE_NEXT_TEXT_KEY : _SINGLE_BACK_TEXT_KEY);
    }


    private boolean _next;
  }

  static private final long _MAX_VALUE_UNKNOWN =
    ((Number) UIXSingleStep.MAX_STEP_KEY.getDefault()).longValue();
  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(SingleStepButtonBarRenderer.class);
}
