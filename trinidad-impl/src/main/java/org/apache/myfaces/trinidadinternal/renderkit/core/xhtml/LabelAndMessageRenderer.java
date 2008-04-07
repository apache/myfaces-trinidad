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

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.component.UIXGroup;
import org.apache.myfaces.trinidad.component.UIXPanel;
import org.apache.myfaces.trinidad.component.html.HtmlTableLayout;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.table.TableRenderingContext;


/**
 * @todo Support "end" facet???
 */
public abstract class LabelAndMessageRenderer extends XhtmlRenderer
{
  static public final String INLINE_MESSAGE_DEFAULT_GAP = "12";
  static public final String INLINE_MESSAGE_PDA_GAP     = "2";

  public LabelAndMessageRenderer(FacesBean.Type type)
  {
    super(type);
  }

  @Override
  public boolean getRendersChildren()
  {
    return true;
  }

  @Override
  protected void findTypeConstants(FacesBean.Type type)
  {
    super.findTypeConstants(type);

    _labelKey        = type.findKey("label");
    _requiredKey = type.findKey("required");
    _showRequiredKey = type.findKey("showRequired");

    _message = new Message(type);
    _label = new Label(type, false);
    _labelInTable = new Label(type, true);
  }
  
  private boolean _needsTableTag(UIComponent component)
  {
    // Find the first content-generating parent
    UIComponent parent = XhtmlUtils.getStructuralParent(component);
    if (parent == null)
      return true;

    // =-=AEW We should review this code.
    // Either the parent should mark down that it rendered
    // a table, or we should lean on the ResponseWriter
    // to tell us if a table had been used.

    // Hardcoding some packages 'cause I need this code to change!
    String family = parent.getFamily();
    if (UIXPanel.COMPONENT_FAMILY.equals(family))
    {
      String rendererType = parent.getRendererType();
      if (_isFormRendererType(rendererType))
        return false;
    }
    else if (HtmlTableLayout.COMPONENT_FAMILY.equals(family))
    {
      return false;
    }

    return true;
  }

  private boolean _isParentPanelForm(UIComponent component)
  {
    // We must know if the immediate parent is a PanelForm because if there is
    // even just one other component inbetween this component and a PanelForm,
    // we must render different DOM--the same DOM as if there were no parent
    // PanelForm.
    UIComponent parentComponent = component.getParent();
    String family = parentComponent.getFamily();
    // FIXME: OK... This is another strong coupling
    //        We could add a an interface instead like ComponentHolder or something
    //        instead of checking against a specific component family.
    while (UIXGroup.COMPONENT_FAMILY.equals(family))
    {
      // Special case:
      // Since UIXGroup components are purely organizational, it is valid to
      // have them inbetween panelForm-friendly components and the panelForm,
      // so we need to look further up the chain:
      parentComponent = parentComponent.getParent();
      if (parentComponent == null)
      {
        return false;
      }
      family = parentComponent.getFamily();
    }
    if (UIXPanel.COMPONENT_FAMILY.equals(family))
    {
      String rendererType = parentComponent.getRendererType();
      if (_isFormRendererType(rendererType))
        return true;
    }
    return false;
  }

   // put the outer style class here, like af_selectManyRadio, styleClass,
  // inlineStyle, 'state' styles like p_AFDisabled, etc.  
  protected void renderRootDomElementStyles(
   FacesContext        context,
   RenderingContext arc,
   UIComponent         component,
   FacesBean           bean) throws IOException
  {
    // do nothing for now
  } 

  @Override
  protected boolean shouldRenderId(
    FacesContext context,
    UIComponent  component)
  {
    return true;
  }

  private boolean _isInTable()
  {
    TableRenderingContext tContext =
      TableRenderingContext.getCurrentInstance();
    if (tContext != null)
    {
      return TableRenderingContext.isInsideContentOfTable();
    }

    return false;
  }

  /**
   */
  @Override
  protected void encodeAll(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    String clientId = component.getClientId(context);
    
    // If we're a leaf component, see if we can skip our rendering
    if (isLeafRenderer() && canSkipRendering(arc, clientId))
    {
      // Except we really do have a "help" facet, so render that one...
      UIComponent help = getFacet(component, "help");
      if (help != null)
        encodeChild(context, help);
      return;
    }

    String saved = arc.getCurrentClientId();
    arc.setCurrentClientId(clientId);


    boolean isInTable = _isInTable();

    if (hasOwnLabel(bean) || isInTable)
    {
      String value = getLabel(bean);
      FormData fd = arc.getFormData();
      if (fd != null)
        fd.addLabel(clientId, value);
    }

    RequestContext rc = RequestContext.getCurrentInstance();
    boolean isInline = (rc.getClientValidation() ==
                        RequestContext.ClientValidation.INLINE);
    if (isInTable)
    {
      ResponseWriter rw = context.getResponseWriter();
      delegateRenderer(context, arc, component, bean, _labelInTable);
      renderFieldCellContents(context, arc, component, bean);

      if (isInline || hasMessage(context, arc, component, bean))
      {
        rw.startElement("div", null);
        rw.endElement("div");
        _renderMessageCellContents(context, arc, component, bean);
      }
    }
    else
    {
      ResponseWriter rw = context.getResponseWriter();

      boolean needsPanelFormLayout = _isParentPanelForm(component);
      boolean needsTableTag = false;

      if (!needsPanelFormLayout)
      {
        // Start encoding of the non-panelForm-friendly wrappers:
        needsTableTag = _needsTableTag(component);
        if (needsTableTag)
        {
          rw.startElement("table", component);
          // =-=AEW THIS DOESN'T SEEM RIGHT - IT SHOULD GO ON THE INPUT FIELD
          // ONLY, RIGHT?  Matching UIX 2.2 behavior here.
          rw.writeAttribute("title", getShortDesc(bean), "title");
          if (!isDesktop(arc))
          {
            // On PDA browsers label and message pair is always
            // rendered in full width.
            rw.writeAttribute("width", "100%", null);
          }
          renderId(context, component);
          
          // put the outer style class here, like af_inputText, styleClass,
          // inlineStyle, 'state' styles like p_AFDisabled, etc.
          renderRootDomElementStyles(context, arc, component, bean);
          
          OutputUtils.renderLayoutTableAttributes(context, arc, "0", null);
        }
        
        rw.startElement("tr", component);
        if (!needsTableTag)
        {
          // =-=AEW SHORT DESC?
          renderId(context, component);
        }
      }
      else
      {
        // -= Simon =- HACK
        // It's ugly, I hate it, but it works and it's better than pushing
        // Cannot use a span either because td is not a valid span child.
        //rw.startElement("td", component);
        //rw.startElement("table", component);
        //OutputUtils.renderLayoutTableAttributes(context, arc, "0", null);
        //rw.startElement("tbody", component);
        //rw.startElement("tr", component); 
        //renderRootDomElementStyles(context, arc, component, bean);
        
        // Basically, we're screwed unless we specify panelForm to keep a 
        // class-less container opened to receive the rootDomStyles.
        // Even if this is the case we need a way to detect if a new 
        // element get opened from encodeBetweenLabelAndFieldCells call.
        // Since the above option is so ugly, I'll assume that.
        // FIXME: That's too strongly coupled to my taste. Even being stuck
        //        with a parent tr is too strongly coupled to my taste.
        renderRootDomElementStyles(context, arc, component, bean);
      }
      
      boolean labelExists = (getLabel(bean) != null);
      
      _renderLabelCell(context, arc, component, bean, labelExists);
      
      if (needsPanelFormLayout)
      {
        if(PanelFormLayoutRenderer.encodeBetweenLabelAndFieldCells(context, arc, rw))
        {
          renderRootDomElementStyles(context, arc, component, bean);
        }
      }
      
      //This part is necessary to make work hspace on tr:tableFormLayout
      Map<String, Object> requestMap = context.getExternalContext()
          .getRequestMap();
      
      Integer hspaceObject = (Integer) requestMap.get(
              "org.apache.myfaces.trinidadinternal.TableFormHspace");

      Boolean percentWidthObject = (Boolean) requestMap.get(
              "org.apache.myfaces.trinidadinternal.TableFormPercentWidth");
      
      if (hspaceObject != null){
          
          rw.startElement("td", null);
          if (percentWidthObject != null && percentWidthObject == true){
              rw.writeAttribute("width", hspaceObject +"%", null);
          }else{
              rw.writeAttribute("width", hspaceObject, null);              
          }
          rw.endElement("td");
      }
      
      _renderFieldCell(context, arc, component, bean, labelExists,
                       needsPanelFormLayout, isInline);
      
      if (!needsPanelFormLayout)
      {
        // End encoding of the non-panelForm-friendly wrappers:
        rw.endElement("tr");

        if (isInline || hasMessage(context, arc, component, bean))
        {
          // =-=AEW PPR PROBLEM!!!  We should always be
          // rendering the "tr", and always rendering an ID, if we ever
          // want it to be PPR replaceable
          rw.startElement("tr", component);
          rw.startElement("td", null);
          rw.endElement("td");
          
          rw.startElement("td", null);
          renderStyleClass(context, arc,
                           SkinSelectors.AF_COMPONENT_MESSAGE_CELL_STYLE_CLASS);
          _renderMessageCellContents(context, arc, component, bean);
          rw.endElement("td");
          
          rw.endElement("tr");
        }

        if (needsTableTag)
          rw.endElement("table");
      }
    }
    
    arc.setCurrentClientId(saved);
  }

  // subclasses should override this
  protected String getRootStyleClass(FacesBean bean)
  {
    return null;
  }

  
  /**
   * @todo Get cell alignment from skin property.
   */
  private void _renderLabelCell(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean,
    boolean             labelExists) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("td", null);
    
    // render labelStyleClass and defaultStyleClass.
    renderStyleClasses(context, arc, new String[]{
                       getLabelStyleClass(bean),
                       _getDefaultLabelStyleClass(arc, 
                          SkinSelectors.AF_LABEL_TEXT_STYLE_CLASS)});

    String labelInlineStyle = getLabelInlineStyleKey(bean);
    rw.writeAttribute("style", labelInlineStyle, null);

    String valign = getDefaultLabelValign(bean);

    rw.writeAttribute("valign", valign, null);
    if (isDesktop(arc))
    {
      // On PDA browsers where width is limited, the label is allowed to wrap.
      rw.writeAttribute("nowrap", Boolean.TRUE, null);
    }

    if (labelExists)
    {
      rw.writeAttribute("width",
                        arc.getProperties().get(_LABEL_CELL_WIDTH_KEY),
                        null);
    }

    delegateRenderer(context, arc, component, bean, _label);
    rw.endElement("td");
  }

  /**
   * @todo see if bug 2484841 still applies!
   */
  private void _renderFieldCell(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean,
    boolean             labelExists,
    boolean             needsPanelFormLayout,
    boolean             isInline) throws IOException
  {
    ResponseWriter rw = context.getResponseWriter();
    rw.startElement("td", null);

    rw.writeAttribute("valign", "top", null);
    if (isDesktop(arc))
    {
      // On PDA browsers where width is limited, the field data is
      // allowed to wrap.
      rw.writeAttribute("nowrap", Boolean.TRUE, null);
    }

    renderStyleClass(context, arc, SkinSelectors.AF_CONTENT_CELL_STYLE_CLASS );

    if (labelExists)
      rw.writeAttribute("width",
                        arc.getProperties().get(_FIELD_CELL_WIDTH_KEY),
                        null);

    renderFieldCellContents(context, arc, component, bean);

    // The panelForm places messages below the fields, not on a separate
    // row:
    if (needsPanelFormLayout)
    {
      // =-= mcc PPR PROBLEM!!!  We should always be rendering the "div",
      //     and always rendering an ID, if we ever want it to be PPR
      //     replaceable:
      if (isInline || hasMessage(context, arc, component, bean))
      {
        rw.startElement("div", null);
        renderStyleClass(context, arc,
                         SkinSelectors.AF_COMPONENT_MESSAGE_CELL_STYLE_CLASS);
        _renderMessageCellContents(context, arc, component, bean);
        rw.endElement("div");
      }
    }

    // bug 2484841: PDA: TOO MUCH WHITESPACE BETWEEN
    //                   INPUT ELEMENTS IN LABELEDFIELD
    // This is a browser bug workaround, hopefully we can remove it eventually
    if (isPDA(arc) && isIE(arc))
    {
      rw.startElement("div", null);
      renderSpacer(context, arc, "1", "0");
      rw.endElement("div");
    }

    rw.endElement("td");
  }

  static String __getCachedClientId(RenderingContext arc)
  {
    String clientId = arc.getCurrentClientId();
    assert(clientId != null);
    return clientId;
  }

  protected boolean hasMessage(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean)
  {
    UIComponent help = getFacet(component, "help");
    if (help != null)
      return true;

    String id  = getLabelFor(context, arc, component, bean);
    return context.getMessages(id).hasNext();
  }

  private void _renderMessageCellContents(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    delegateRenderer(context, arc, component, bean, _message);
  }

  /**
   * Returns the client ID.
   */
  @Override
  protected String getClientId(
    FacesContext context,
    UIComponent  component)
  {
    return (super.getClientId(context, component) +
            XhtmlConstants.COMPOSITE_ID_EXTENSION);
  }

  protected boolean isLeafRenderer()
  {
    return true;
  }

  protected String getDefaultLabelValign(FacesBean bean)
  {
    return null;
  }


  abstract protected void renderFieldCellContents(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean) throws IOException;

  /**
   * If it's known that the field content is not editable, return false.
   * Otherwise, assume it is editable and return true
   */
  protected boolean isContentEditable(FacesBean bean)
  {
    return true;
  }

  protected boolean isIndented()
  {
    return false;
  }

  /**
   * Override and return "true" to indicate that the component
   * has its own internal label - and that therefore there
   * shouldn't be an HTML <label> tag, for instance.
   */
  protected boolean hasOwnLabel(FacesBean bean)
  {
    return false;
  }

  protected boolean showAccessKeyOnLabel(FacesBean bean)
  {
    // By default, if we have our own label, don't show the
    // access key on the label (but that's not always true)
    return !hasOwnLabel(bean);
  }

  /**
   * Returns the ID (clientId) of the component that
   * should receive the label.
   */
  abstract protected String getLabelFor(
    FacesContext        context,
    RenderingContext arc,
    UIComponent         component,
    FacesBean           bean);

  static void __setLabelWidth(RenderingContext arc, Object width)
  {
    arc.getProperties().put(_LABEL_CELL_WIDTH_KEY, width);
  }

  static void __setFieldWidth(RenderingContext arc, Object width)
  {
    arc.getProperties().put(_FIELD_CELL_WIDTH_KEY, width);
  }


  static void __clearProperties(RenderingContext arc)
  {
    arc.getProperties().remove(_LABEL_CELL_WIDTH_KEY);
    arc.getProperties().remove(_FIELD_CELL_WIDTH_KEY);
  }

  private class Label extends OutputLabelRenderer
  {
    public Label(FacesBean.Type type, boolean inTable)
    {
      super(type);
      _inTable = inTable;
    }

    @Override
    protected boolean shouldRenderId(
      FacesContext context,
      UIComponent  component)
    {
      return false;
    }

    @Override
    protected boolean needComponentInStartElement()
    {
      // Because we're not rendering the ID on the label (at this time)
      // there's no point in passing the component to startElement() - it
      // just makes PPR unhappy to do so
      return false;
    }

    @Override
    protected void renderAllAttributes(
      FacesContext        context,
      RenderingContext arc,
      FacesBean           bean) throws IOException
    {
      // Block everything
    }

    @Override
    protected String getDefaultValign(FacesBean bean)
    {
      // get the defaultLabelValign from the form component.
      return getDefaultLabelValign(bean);
    }


    @Override
    protected String getConvertedString(
      FacesContext context,
      UIComponent  component,
      FacesBean    bean)
    {
      if (_inTable)
        return null;

      return LabelAndMessageRenderer.this.getLabel(bean);
    }

    /**
     * Only display the required icon indicator if we're required
     * or showRequired is on.
     */
    @Override
    protected boolean getShowRequired(FacesBean bean)
    {
      // Inside the table, never show the required icon.
      if (_inTable)
        return false;

      return (LabelAndMessageRenderer.this.labelShowRequired(bean));
    }

    @Override
    protected char getAccessKey(FacesBean bean)
    {
      if (LabelAndMessageRenderer.this.showAccessKeyOnLabel(bean))
        return super.getAccessKey(bean);

      return CHAR_UNDEFINED;
    }

    @Override
    protected String getShortDesc(FacesBean bean)
    {
      String shortDesc = super.getShortDesc(bean);
      // =-=AEW Apparently, we're supposed to do this
      // for screenReader selectOneRadio and selectBooleanRadio!?!
      if ((shortDesc == null) && _inTable)
      {
        shortDesc = LabelAndMessageRenderer.this.getLabel(bean);
      }

      return shortDesc;
    }

    @Override
    protected String getForId(
      FacesContext context,
      UIComponent  component,
      FacesBean    bean)
    {
      return getLabelFor(context,
                         RenderingContext.getCurrentInstance(),
                         component,
                         bean);
    }

    @Override
    protected boolean isLabelTagNeeded(
      RenderingContext arc,
      FacesBean           bean,
      String              forId,
      int                 accessKeyIndex
    )
    {
      if (LabelAndMessageRenderer.this.hasOwnLabel(bean))
        return false;

      return super.isLabelTagNeeded(arc, bean, forId, accessKeyIndex);
    }

    private final boolean _inTable;
  }

  private class Message extends MessageRenderer
  {
    public Message(FacesBean.Type type)
    {
      super(type);
    }

    @Override
    protected boolean shouldRenderId(
      FacesContext context,
      UIComponent  component)
    {
      return false;
    }

    @Override
    protected String getShortDesc(FacesBean bean)
    {
      return null;
    }

    @Override
    protected boolean getIndented(FacesBean bean)
    {
      return LabelAndMessageRenderer.this.isIndented();
    }

    @Override
    protected void renderAllAttributes(
      FacesContext        context,
      RenderingContext arc,
      FacesBean           bean) throws IOException
    {
    }

    @Override
    protected String getForId(
      FacesContext context,
      UIComponent  component,
      FacesBean    bean)
    {
      return getLabelFor(context,
                         RenderingContext.getCurrentInstance(),
                         component,
                         bean);
    }
  }


  protected String getLabelInlineStyleKey(FacesBean bean)
  {
    return null; // overridden by PanelLabelAndMessageRenderer
  }

  /**
   * Hook for resolving whether we should show the "required" icon.
   */
  protected boolean labelShowRequired(FacesBean bean)
  {
    // If we're required (from the input side of things),
    // or showRequired is true, then show the "required" icon.
    // Unless we're read-only, in which case never show it.
    if (getRequired(bean) || getShowRequired(bean))
    {
      return isContentEditable(bean);
    }

    return false;
  }

  protected boolean getShowRequired(FacesBean bean)
  {
    if(_showRequiredKey == null)
    { // showRequired is not supporte on the element
      return false;
    }
    
    Object o = bean.getProperty(_showRequiredKey);
    if (o == null)
    {
      o = _showRequiredKey.getDefault();
    }

    return Boolean.TRUE.equals(o);
  }

  protected boolean getRequired(FacesBean bean)
  {
    Object o = bean.getProperty(_requiredKey);
    if (o == null)
      o = _requiredKey.getDefault();

    return Boolean.TRUE.equals(o);
  }


  protected String getLabel(FacesBean bean)
  {
    return toString(bean.getProperty(_labelKey));
  }

  /**
   * This gets the rootStyleClass from the bean, appends ::label to it,
   * @param bean
   * @return
   */
  protected String getLabelStyleClass(FacesBean bean)  
  {
    String styleClass = getRootStyleClass(bean);
    if(styleClass != null)
    {
      styleClass += _LABEL_PSEUDO_ELEMENT;
    }
    
    return styleClass;
  } 
  

  // If we have mapped this style (like in panelForm), 
  // then return the style, otherwise return null
  private String _getDefaultLabelStyleClass(
    RenderingContext arc,
    String              styleClass)  
  {
    Map<String, String> keyMap = arc.getSkinResourceKeyMap();
    return (keyMap != null) ?
            keyMap.get(styleClass) :
            null;
  }
  
  private boolean _isFormRendererType(String rendererType)
  {
    return "org.apache.myfaces.trinidad.Form".equals(rendererType) ||
        "org.apache.myfaces.trinidad.FormLayout".equals(rendererType) ||
        "org.apache.myfaces.trinidad.rich.Form".equals(rendererType) ||
        "org.apache.myfaces.trinidad.TableLayout".equals(rendererType);
  }

  
  // THESE VALUES MUST MATCH THOSE IN INLINEMESSAGERENDERER
  // (at least for as long as both classes exist)
  static private final Object _LABEL_CELL_WIDTH_KEY = "_imLCWidth";
  static private final Object _FIELD_CELL_WIDTH_KEY = "_imFCWidth";
  
  private static final String _LABEL_PSEUDO_ELEMENT = "::label";

  private PropertyKey   _labelKey;
  private PropertyKey   _requiredKey;
  private PropertyKey   _showRequiredKey;
  private XhtmlRenderer _message;
  private XhtmlRenderer _label;
  private XhtmlRenderer _labelInTable;
}
