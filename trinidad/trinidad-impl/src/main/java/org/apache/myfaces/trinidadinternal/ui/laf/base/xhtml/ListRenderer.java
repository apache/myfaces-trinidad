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
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.io.IOException;

import java.util.List;

import javax.faces.component.UIComponent;

import javax.faces.context.ResponseWriter;

import org.apache.myfaces.trinidadinternal.ui.action.ClientAction;
import org.apache.myfaces.trinidadinternal.ui.action.ClientActionUtils;
import org.apache.myfaces.trinidadinternal.util.IntegerUtils;

import org.apache.myfaces.trinidadinternal.renderkit.uix.SelectItemSupport;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;

/**
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/ListRenderer.java#0 $) $Date: 10-nov-2005.18:54:01 $
 * @author The Oracle ADF Faces Team
 */
public class ListRenderer extends FormSelectRenderer
{
  protected void renderAttributes(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    super.renderAttributes(context, node);

    Object multiple = node.getAttributeValue(context, MULTIPLE_ATTR);
    renderAttribute(context, MULTIPLE_ATTRIBUTE, multiple);

    Object size = node.getAttributeValue(context, SIZE_ATTR);

    // Get (and cache) the list of SelectItems
    UIComponent component = OptionContainerRenderer.__getUIComponent(context,
                                                                     node);
    List selectItems;
    if (component != null)
    {
        selectItems = 
          SelectItemSupport.getSelectItems(
            component, SelectItemSupport.getConverter(component));
    }
    else
    {
      selectItems = null;
    }
    context.setLocalProperty(_SELECT_ITEMS_PROPERTY, selectItems);

    // must specify size or we might render a Choice
    if (size == null || ((Integer)size).intValue() < 1)
    {
      // If we're using "selectItems", use the count there.
      // Otherwise, use the number of indexed children
      int itemCount = ((selectItems != null) &&
                       (!selectItems.isEmpty()))
                         ? selectItems.size()
                         : node.getIndexedChildCount(context);

      int defaultSize = Math.min(8,
                                 Math.max(2, itemCount));

      size = IntegerUtils.getInteger(defaultSize);
    }
    // Multiple-selection lists of height 1 do not work in many
    // browsers.  Block this combination.
    else if (Boolean.TRUE.equals(multiple) &&
             (IntegerUtils.getInteger(1).equals(size) ||
              "1".equals(size)))
    {
      size = "2";
    }
    renderAttribute(context, SIZE_ATTRIBUTE, size);
  }

  protected void prerender(
    UIXRenderingContext context,
    UINode           node) throws IOException
  {
    // If we've got a ClientAction, let it write its dependencies
    // before we start rendering the link
    ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                   node);
    if (action != null)
      action.writeDependencies(context, node);

    super.prerender(context, node);
  }

  protected void renderContent(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {


    super.renderContent(context, node);
    
    UIComponent component = OptionContainerRenderer.__getUIComponent(context,
                                                                     node);

    List selectItems = (List) context.getLocalProperty(0,
                                                       _SELECT_ITEMS_PROPERTY,
                                                       null); 

  
    if (component != null)
    {
      // try to get it if it is null; this can happen if renderAttributes
      // was not called. It wouldn't be called if the selectOne component
      // is readOnly (renderAsNonElement gets called instead, which in
      // turn calls renderContent.
      if (selectItems == null)
      {

        selectItems = 
          SelectItemSupport.getSelectItems(component,
                                           SelectItemSupport.getConverter(
                                           component));          
      }
      renderSelectItemOptions(context,
                              node,
                              component,
                              selectItems);
    }
    
  }  

  protected Boolean isMultipleSelection(
    UIXRenderingContext context,
    UINode           node)
  {
    // list can be multiple selection
    if (Boolean.TRUE.equals(node.getAttributeValue(context, MULTIPLE_ATTR)))
      return Boolean.TRUE;
    return Boolean.FALSE;
  }

  /**
   * Called to render between each set of rendered indexed children.
   * @param context the rendering context
   * @param node the current UINode
   */
  protected void renderBetweenIndexedChildren(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {
    // disabled && !supportsDisabled && !readOnly && wasOptionSelected
    if (!renderAsElement(context, node) &&
        wasOptionSelected(context, node))
    {
      ResponseWriter writer = context.getResponseWriter();
      writer.startElement("br", null);
      writer.endElement("br");
    }
  }

  protected Object getOnChange(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object onChange = node.getAttributeValue(context, ON_CHANGE_ATTR);

    ClientAction action = ClientActionUtils.getPrimaryClientAction(context,
                                                                   node);

    if ((action != null) && !action.isTriggerRequired(context, node))
    {
      String actionScript = action.getScript(context, node, Boolean.TRUE);

      if (actionScript != null)

      onChange  = XhtmlLafUtils.getChainedJS(onChange,
                                             actionScript,
                                             true);
    }
    return onChange;
  }


  protected boolean wasOptionSelected(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Integer oldSelectedCount = (Integer)
      context.getLocalProperty(0, _SELECTED_COUNT_PROPERTY,
                               IntegerUtils.getInteger(0));

    OptionInfo info = getOptionInfo(context);

    if (info.selectedCount == oldSelectedCount.intValue())
      return false;

    context.setLocalProperty(_SELECTED_COUNT_PROPERTY,
                             IntegerUtils.getInteger(info.selectedCount));

    return true;
  }

  private static final Object _SELECTED_COUNT_PROPERTY = new Object();
  private static final Object _SELECT_ITEMS_PROPERTY = new Object();
}
