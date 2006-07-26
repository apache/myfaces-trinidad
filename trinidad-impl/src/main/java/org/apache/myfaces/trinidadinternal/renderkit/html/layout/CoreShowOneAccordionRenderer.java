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
package org.apache.myfaces.adfinternal.renderkit.html.layout;

import java.io.IOException;

import java.util.List;
import java.util.ListIterator;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.adf.logging.ADFLogger;
import org.apache.myfaces.adf.component.UIXShowDetail;

import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.adfinternal.renderkit.RenderUtils;
import org.apache.myfaces.adfinternal.renderkit.core.xhtml.XhtmlRenderer;
import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UIConstants;
import org.apache.myfaces.adfinternal.ui.laf.base.desktop.HideShowUtils;
import org.apache.myfaces.adfinternal.ui.laf.base.xhtml.XhtmlLafConstants;
import org.apache.myfaces.adfinternal.ui.partial.PartialPageRendererUtils;
import org.apache.myfaces.adfinternal.uinode.UINodeRendererBase;

/**
 * Renderer for ShowOneAccordion
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/html/layout/CoreShowOneAccordionRenderer.java#0 $) $Date: 10-nov-2005.19:01:13 $
 * @author The Oracle ADF Faces Team
 */
public class CoreShowOneAccordionRenderer extends UINodeRendererBase
{
  /**
   *  If nothing is disclosed, makes the first child disclosed.
   *
   *  Makes sure that the child being disclosed has rendered = true
   *  and is not disabled.
   *
   * @param context the faces context object
   * @param component the UIComponent object
   * @throws IOException when some issues while writing output
   */
  public void encodeBegin(FacesContext context, UIComponent component)
    throws IOException
  {
    _LOG.finer("Entering CoreShowOneAccordionRenderer.encodeBegin()");
    List children = component.getChildren();
    int numChildren = children.size();
    UIComponent disclosedChild = null;
    UIXShowDetail renderableChild = null;

    for (int indxChild = 0; indxChild < numChildren ; indxChild++ )
    {
      UIComponent child =  (UIComponent) children.get(indxChild);
      if (! (child instanceof UIXShowDetail) )
      {
        continue;
      }

      UIXShowDetail detailChild =  (UIXShowDetail) children.get(indxChild);

      if (detailChild.isRendered())
      {
        // Mark the first renderable child
        Object disabled =
          detailChild.getAttributes().get(
            UIConstants.DISABLED_ATTR.getAttributeName());
        if (Boolean.TRUE.equals(disabled))
        {
          continue;
        }
        if (renderableChild == null)
        {
          renderableChild = detailChild;
        }
        if (detailChild.isDisclosed())
        {
          disclosedChild = detailChild;
          // A diclosed child found. return.
          break;
        }
      }
    }

    // If we have a minimum of 1 disclosed child and none have been disclosed
    // yet, disclose the first rendered one:
    if ( (disclosedChild == null) && !_isDiscloseNone(component) &&
      (renderableChild != null) )
    {
      renderableChild.setDisclosed(true);
    }

    _LOG.finer("Exiting CoreShowOneAccordionRenderer.encodeBegin()");
  }

  /**
   *  Renders a vertical panel bar and children in individual panels.
   *
   *  For the panel bar, draws a DIV that forms the outline of panels.
   *  Within the DIV, iteratively calls the encodeBegin, encodeChildren and
   *  encodeEnd on the panel children (if they have rendered and disclosed
   *  set to true).
   *
   *  Non UIXShowDetail children are ignored.
   *  The title of each of the panels is the same as the text assigned to
   *  UIXShowDetail child. When text attribute is not specified,
   *  title remains blank.
   *
   * @param context the faces context object
   * @param component the UIComponent object
   * @throws IOException when some issues while writing output
   */
  public void encodeChildren(FacesContext context, UIComponent component)
    throws IOException
  {
    if (! component.isRendered())
    {
      return;
    }

    ResponseWriter out = context.getResponseWriter();

    ListIterator iter = component.getChildren().listIterator();

    if (iter == null)
    {
      return;
    }

    String compId = component.getClientId(context);

    // This will only render UIXShowDetail children.
    // Non UIXShowDetail children are ignored.
    _LOG.finest("CoreShowOneAccordionRenderer.encodeChildren: compId: {0}",
                compId);

    out.startElement("div", component);
    out.writeAttribute("id", compId, null);

    RenderingContext rCtx = getRenderingContext(context, component);

    String styleClass = (String) component.getAttributes().get("styleClass");
    AdfRenderingContext arc = AdfRenderingContext.getCurrentInstance();
    if (styleClass != null)
    {
      XhtmlRenderer.renderStyleClasses(context, arc, new String [] {
        getContainerStyleClass(), styleClass});
    }
    else
      XhtmlRenderer.renderStyleClass(context, arc, getContainerStyleClass());

    ShowOneUtils.renderGenericAttributes(rCtx, component, out);

    String formName = RenderUtils.getFormId(context, component);

    boolean discloseMany = isDiscloseMany();
    boolean discloseNone = _isDiscloseNone(component);
    boolean disclosedFixed = false;
    if (discloseMany && !discloseNone) // must keep at least one item disclosed
    {
      // This is a special case where we must determine if we have to fix the
      // disclosure state of one of the items.
      int disclosedCount = 0;
      while (iter.hasNext())
      {
        UIXShowDetail detailItem = _getNextShowDetailChild(iter);
        if (detailItem == null)
        {
          continue;
        }
        if (detailItem.isDisclosed())
        {
          disclosedCount++;
          if (disclosedCount > 1)
          {
            break; // we have enough information at this point to stop counting
          }
        }
      }
      if (disclosedCount <= 1)
      {
        disclosedFixed = true;
      }

      // Reset the iterator for the 2nd pass
      iter = component.getChildren().listIterator();
    }

    boolean childAlreadyRendered = false;
    while (iter.hasNext())
    {
      UIXShowDetail detailItem = _getNextShowDetailChild(iter);
      if (detailItem == null)
      {
        continue;
      }

      Boolean disabledObj =
        (Boolean) detailItem.getAttributes().get(
          UIConstants.DISABLED_ATTR.getAttributeName());
      boolean disabled = false; // by default is enabled.
      if (disabledObj != null)
      {
        disabled = disabledObj.booleanValue();
      }

      String titleText = (String) detailItem.getAttributes().get("text");
      boolean disclosed = detailItem.isDisclosed();

      if (childAlreadyRendered)
      {
        // The detail child should be disclosed only when all three criteria met
        // 1. is marked as disclosed
        // 2. is not disabled and
        // 3. if a child is not already disclosed. This occurs when more than
        //    one showDetail child has it's disclosed property set to true.
        disclosed = false;
      }

      // Header renderer section.
      out.startElement("div", component);
      String detailItemId = detailItem.getClientId(context);
      out.writeAttribute("id", compId + detailItemId, null);

      if (disabled)
      {
        XhtmlRenderer.renderStyleClass(context, arc,
          getHeaderDisabledStyleClass());
      }
      else if (disclosed)
      {
        XhtmlRenderer.renderStyleClass(context, arc,
          getHeaderExpanedStyleClass());
      }
      else
      {
        XhtmlRenderer.renderStyleClass(context, arc,
            getHeaderCollapsedStyleClass());
      }

      out.startElement("a", null);
      out.writeAttribute("name", detailItemId, null);

      if (disabled)
      {
        XhtmlRenderer.renderStyleClass(context, arc,
          getLinkDisabledStyleClass());
      }
      else
      {
        XhtmlRenderer.renderStyleClass(context, arc,
          getLinkEnabledStyleClass());
      }

      // If the child is disclosable and enabled...
      boolean disclosable =
        discloseNone || (! disclosed) || (discloseMany && !disclosedFixed);
      if ( disclosable && (! disabled) )
      {
        if (formName == null)
        {
          // =-=rbaranwa: Do we need to handle this case any other way?
          _LOG.warning("Page does not contain any form element." +
                       "Page will not behave properly.");
        }
        else
        {
          boolean isImmediate = detailItem.isImmediate();
          String event;
          if (disclosed)
          {
            event = "hide";
          }
          else
          {
            event = "show";
          }
          String onClickHandler = _getFormSubmitScript(component,
                                                       rCtx,
                                                       event,
                                                       detailItemId,
                                                       formName,
                                                       compId,
                                                       isImmediate);
          out.writeAttribute("onclick", onClickHandler, null);
        }

        out.writeAttribute("href", "#", null);
      }

      // =-=rbaranwa Per the UI Review, no icon to be rendered when
      // panel is disabled.
      if (! disabled)
      {
        HideShowUtils.renderDisclosedStateSymbol(rCtx,
                                                 disclosed,
                                                 getDisclosedTipKey(),
                                                 getUndisclosedTipKey());
      }

      out.writeText(titleText, null);
      out.endElement("a");

      out.endElement("div"); // Ending div for an individual panel

      // The detail child should be disclosed only when all three criteria met
      // 1. is marked as disclosed
      // 2. is not disabled and
      // 3. if a child is not already disclosed. This occurs when more than
      //    one showDetail child has it's disclosed property set to true.
      if (disclosed && (! disabled) && (! childAlreadyRendered) )
      {
        _encodeDetailItem(context, component, detailItem, out);
        if (!discloseMany)
        {
          childAlreadyRendered = true;
        }
      }
    }
    out.endElement("div");
    _LOG.finest("Exited CoreShowOneAccordionRenderer.encodeChildren");
  }

  /**
   *  Dummy method to prevent base class call.
   *
   * @param context the faces context object
   * @param component the UIComponent object
   * @throws IOException when some issues while writing output
   */
  public void encodeEnd(FacesContext context,
                        UIComponent component)
    throws IOException
  {
    // This is done so that the UIXComponentUINode does not
    // go looking for a UIX 22 style renderer for this component
  }

  String getContainerStyleClass()
  {
    return XhtmlLafConstants.AF_SHOWONEACCORDION_CONTAINER_STYLE_CLASS;
  }

  String getContentStyleClass()
  {
    return XhtmlLafConstants.AF_SHOWONEACCORDION_CONTENT_STYLE_CLASS;
  }

  String getHeaderDisabledStyleClass()
  {
    return XhtmlLafConstants.AF_SHOWONEACCORDION_HEADER_DISABLED_STYLE_CLASS;
  }

  String getHeaderExpanedStyleClass()
  {
    return XhtmlLafConstants.AF_SHOWONEACCORDION_HEADER_EXPANDED_STYLE_CLASS;
  }

  String getHeaderCollapsedStyleClass()
  {
    return XhtmlLafConstants.AF_SHOWONEACCORDION_HEADER_COLLAPSED_STYLE_CLASS;
  }

  String getLinkDisabledStyleClass()
  {
    return XhtmlLafConstants.AF_SHOWONEACCORDION_TITLE_LINK_DISABLED_STYLE_CLASS;
  }

  String getLinkEnabledStyleClass()
  {
    return XhtmlLafConstants.AF_SHOWONEACCORDION_TITLE_LINK_STYLE_CLASS;
  }

  String getDisclosedTipKey()
  {
    return _DISCLOSED_TIP_KEY;
  }

  String getUndisclosedTipKey()
  {
    return _UNDISCLOSED_TIP_KEY;
  }

  boolean isDiscloseMany()
  {
    return false;
  }

  /**
   *  Encodes the disclosed child.
   *
   *  Generates the required markup for the disclosed child.
   *
   * @param context the faces context object
   * @param component the UIComponent object
   * @param detailItem the UIXShowDetailItem thats disclosed
   * @param out the response writer object
   * @throws IOException when some issues while writing output
   */
  private void _encodeDetailItem(FacesContext context,
                                 UIComponent component,
                                 UIXShowDetail detailItem,
                                 ResponseWriter out)
    throws IOException
  {
    out.startElement("table", component);
    out.writeAttribute("cellSpacing", "0", null);
    out.writeAttribute("cellPadding", "0", null);
    out.writeAttribute("summary", "", null);

    AdfRenderingContext arc = AdfRenderingContext.getCurrentInstance();
    XhtmlRenderer.renderStyleClass(context, arc, getContentStyleClass());

    out.startElement("tr", component);
    out.startElement("td", component);

    RenderUtils.encodeRecursive(context, detailItem);

    out.endElement("td");
    out.endElement("tr");

    out.endElement("table"); // Ending table for the contained child
  }


  /**
   *  Creates javascript used to submit the page.
   *
   * @param component the UIComponent object
   * @param rCtx the ADF Faces rendering context object
   * @param detailItemId Id of the item that will be disclosed
   * @param formName id of the containing form
   * @param compId id of the showOneAccordion component
   * @param isImmediate the value of immediate attribute on child component
   */
  private String _getFormSubmitScript(UIComponent component,
                                      RenderingContext rCtx,
                                      String event,
                                      String detailItemId,
                                      String formName,
                                      String compId,
                                      boolean isImmediate)
  {
    // Check if PPR enabled, do a _submitPartialChange, else do a formSubmit.
    String onClickHandler = "";
    boolean pprEnabled =
      PartialPageRendererUtils.supportsPartialRendering(rCtx);

    String validate = "1";
    if (isImmediate)
    {
      validate = "0";
    }

    if (pprEnabled)
    {
      // encode PPR targets first.
      String encodedPartialTargets =
        ShowOneUtils.getEncodedPartialTargets(component, compId);

      StringBuilder onClickHandlerBuff =
          new StringBuilder("_submitPartialChange('")
          .append(formName)
          .append("',")
          .append(validate)
          .append(", {partialTargets:'")
          .append(encodedPartialTargets)
          .append("', event:'")
          .append(event)
          .append("',source:'")
          .append(detailItemId)
          .append("'});return false;");

      onClickHandler = onClickHandlerBuff.toString();
    }
    else
    {
      StringBuilder onClickHandlerBuff = new StringBuilder("submitForm('")
                                  .append(formName)
                                  .append("',")
                                  .append(validate)
                                  .append(", {event:'show',source:'")
                                  .append(detailItemId)
                                  .append("'});return false;");

      onClickHandler = onClickHandlerBuff.toString();
    }
    return onClickHandler;
  }

  private boolean _isDiscloseNone(UIComponent component)
  {
    Boolean discloseNoneObj =
      (Boolean) component.getAttributes().get(
        UIConstants.DISCLOSE_NONE_ATTR.getAttributeName());
    boolean discloseNone = false; // default is that at least 1 is disclosed.
    if (discloseNoneObj != null)
    {
      discloseNone = discloseNoneObj.booleanValue();
    }
    return discloseNone;
  }

  private UIXShowDetail _getNextShowDetailChild(ListIterator iter)
  {
    UIComponent child = (UIComponent)iter.next();
    if (! child.isRendered() )
    {
      return null;
    }

    if (! (child instanceof UIXShowDetail) )
    {
      return null;
    }

    return (UIXShowDetail)child;
  }


  private static final ADFLogger _LOG =
    ADFLogger.createADFLogger(CoreShowOneAccordionRenderer.class);

  private static final String _DISCLOSED_TIP_KEY =
    "af_showOneAccordion.DISCLOSED_TIP";
  private static final String _UNDISCLOSED_TIP_KEY =
    "af_showOneAccordion.UNDISCLOSED_TIP";

}
