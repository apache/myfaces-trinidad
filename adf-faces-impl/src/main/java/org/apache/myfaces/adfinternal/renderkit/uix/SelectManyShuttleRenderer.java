/*
 * Copyright  2003-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.renderkit.uix;

import java.io.IOException;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import javax.faces.component.UIComponent;
import javax.faces.component.UISelectItem;
import javax.faces.component.UISelectItems;
import javax.faces.context.FacesContext;
import javax.faces.model.SelectItem;

import org.apache.myfaces.adf.logging.ADFLogger;
import org.apache.myfaces.adf.component.UIXSelectMany;
import org.apache.myfaces.adf.component.core.input.CoreSelectItem;
import org.apache.myfaces.adf.util.ArrayMap;

import org.apache.myfaces.adfinternal.agent.AdfFacesAgent;
import org.apache.myfaces.adfinternal.agent.AgentUtil;
import org.apache.myfaces.adfinternal.ui.AttributeKey;
import org.apache.myfaces.adfinternal.uinode.UINodeFacesBean;
import org.apache.myfaces.adfinternal.util.IntegerUtils;

/**
 * Renderer used both for <uix:selectManyShuttle> and <uix:selectOrderShuttle>.
 *
 * @author The Oracle ADF Faces Team
 */
public class SelectManyShuttleRenderer extends SelectManyRenderer
{
  // UIX2 key used to store the List of SelectItems available (for
  // the leading list)
  static public final AttributeKey SELECT_ITEMS_ATTR =
    AttributeKey.getAttributeKey("selectItems");

  // UIX2 key used to store the List of SelectItems already picked
  // (for the "following" list)
  static public final AttributeKey VALUE_ITEMS_ATTR =
    AttributeKey.getAttributeKey("valueItems");

  public void encodeBegin(FacesContext context, UIComponent component)
    throws IOException
  {
    UIXSelectMany selectMany = (UIXSelectMany) component;
    boolean isValuePassThru = getValuePassThru(selectMany);
    // allItems will store all the items in the shuttle; all the children
    // regardless of which list, the trailing or following, that they are in.
    // We store this in a map, with the key
    // being a SelectItem's value and the value is the index.

    List selectItems = _getSelectItems(selectMany, context);
    Map allItems = null;

    if (!isValuePassThru)
    {
      // we must create the allItems list immediately after we create the
      // selectItems list, before the selectItems list is mutated.
       allItems = _getAllItems(selectItems);
    }

    List valueItems;

    // The submitted values are in the form of a string array;
    // currently, we simply "toString()" the outgoing values,
    // so if we're going to match up the submitted values, we
    // have to toString() as well.
    Object submittedValue = selectMany.getSubmittedValue();

    if (submittedValue != null)
    {
      valueItems = _transferValueSelectItems(selectItems,
                                             submittedValue,
                                             true);
    }
    else
    {
      valueItems = _transferValueSelectItems(selectItems,
                                             selectMany.getValue(),
                                             false);
    }

    // if valuePassThru is false, then we need to use the index for the value
    // not the value object itself.
    if (!isValuePassThru)
    {
      // modify the List of SelectItems to use the index of the original
      // list instead of the item's value object.
      _convertSelectItemValueToIndex(allItems, selectItems);
      _convertSelectItemValueToIndex(allItems, valueItems);
    }

    UINodeFacesBean uinode = (UINodeFacesBean) selectMany.getFacesBean();
    uinode.setUINodeAttribute(SELECT_ITEMS_ATTR, selectItems);
    uinode.setUINodeAttribute(VALUE_ITEMS_ATTR, valueItems);

    super.encodeBegin(context, component);
  }


  public void encodeEnd(FacesContext context, UIComponent component)
    throws IOException
  {
    super.encodeEnd(context, component);

    UINodeFacesBean uinode = (UINodeFacesBean)
      ((UIXSelectMany) component).getFacesBean();
    uinode.setUINodeAttribute(SELECT_ITEMS_ATTR, null);
    uinode.setUINodeAttribute(VALUE_ITEMS_ATTR, null);

  }

  /**
   *
   * @todo Support ordinary UIXSelectItems?
   * @todo Support UISelectItems?
   */
  private List _getSelectItems(
    UIXSelectMany many,FacesContext context)
  {

    List childList = many.getChildren();
    int childCount = childList.size();
    List list = new ArrayList(childCount);
    for (int i = 0; i < childCount; i++)
    {
      UIComponent child = (UIComponent) childList.get(i);
      if (child instanceof CoreSelectItem)
      {
        CoreSelectItem uiSelectItem = (CoreSelectItem) child;
        Object itemValue = uiSelectItem.getValue();
        String itemLabel = uiSelectItem.getLabel();

        // The UIX 2 rendering layer doesn't support "disabled" items.
        // So remove 'em instead of showing them as enabled items.
        // remove only if the item is disabled and agent doesnt support disabled
        // options
        boolean agentSupportsDisabledOptions = Boolean.TRUE.equals(AgentUtil
            .getAgent(context).getCapability(
                AdfFacesAgent.CAP_SUPPORTS_DISABLED_OPTIONS));
        boolean isItemDisabled = uiSelectItem.isDisabled();
        if (!isItemDisabled || agentSupportsDisabledOptions)
        {
          SelectItem item = null;
          // this allows values to be bound to SelectItems for af:selectItem.
          if (itemValue instanceof SelectItem)
          {
            item = (SelectItem)itemValue;
          }
          else
          {
          // JSF throws a null pointer exception for empty values
          // and empty labels
            item = new SelectItem(itemValue == null ? "" : itemValue,
                                  itemLabel == null ? "" : itemLabel,
                                  uiSelectItem.getLongDesc(),
                                  isItemDisabled);
          }
          list.add(item);
        }
      }
      else if (child instanceof UISelectItem)
      {
        // @todo get the index instead of value if valuePassThru is false
        SelectItemSupport.addSelectItem((UISelectItem) child, list);
      }
      else if (child instanceof UISelectItems)
      {
        // @todo get the index instead of value if valuePassThru is false
        SelectItemSupport.addSelectItems((UISelectItems) child, list);
      }
    }

    return list;
  }

  private List _transferValueSelectItems(
    List         selectItems,
    Object       value,
    boolean      toStringFirst)
  {
    List valueItemsList;
    if (value == null)
    {
      valueItemsList = Collections.EMPTY_LIST;
    }
    else if (value.getClass().isArray())
    {
      int length = Array.getLength(value);
      valueItemsList = new ArrayList(length);
      for (int i = 0; i < length; i++)
        _transferSelectItem(selectItems,
                            valueItemsList,
                            Array.get(value, i),
                            toStringFirst);
    }
    else if (value instanceof List)
    {
      List valueList = (List) value;
      int length = valueList.size();
      valueItemsList = new ArrayList(length);
      for (int i = 0; i < length; i++)
        _transferSelectItem(selectItems,
                            valueItemsList,
                            valueList.get(i),
                            toStringFirst);
    }
    else
    {
      valueItemsList = new ArrayList(1);
      _transferSelectItem(selectItems, valueItemsList, value, toStringFirst);
    }

    return valueItemsList;
  }

  private void _transferSelectItem(
    List from,
    List to,
    Object value,
    boolean toStringFirst)
  {
    int length = from.size();
    for (int i = 0; i < length; i++)
    {
      SelectItem selectItem = (SelectItem) from.get(i);
      Object selectItemValue = selectItem.getValue();
      if (selectItemValue == null)
      {
        if (value == null)
        {
          from.remove(i);
          to.add(selectItem);
          return;
        }
      }
      else
      {
        // The submitted values are in the form of a string array;
        // currently, we simply "toString()" the outgoing values,
        // so if we're going to match up the submitted values, we
        // have to toString() as well.
        if (toStringFirst)
          selectItemValue = selectItemValue.toString();

        if (selectItemValue.equals(value))
        {
          from.remove(i);
          to.add(selectItem);
        return;
        }
      }
    }

    if (_LOG.isFine())
      _LOG.fine("Could not find item " + value +
                " in list of available items.");
  }

  public Object getSubmittedValue(
    FacesContext context,
    UIComponent  component)
  {


    String clientId = component.getClientId(context);
    String trailingId = clientId + ":trailing:items";
    String paramValue = (String) context.getExternalContext().
                                getRequestParameterMap().get(trailingId);
    if ((paramValue == null) || "".equals(paramValue))
      return new String[0];

    List list = new ArrayList();
    StringTokenizer tokenizer = new StringTokenizer(paramValue, ";");

    // don't let the submitted list get any bigger than the number of
    // total items in the shuttle
    int availableItems = SelectItemSupport.getSelectItemCount(component);
    int numItems = 0;
    while (tokenizer.hasMoreElements())
    {
      numItems++;
      if (numItems > availableItems)
      {
        _LOG.severe("The number of items selected for shuttle '" +
                   clientId +
                   "' exceeds the total number of items in the shuttle." +
                   " No selected items will be returned.");
        return new String[0];
      }

      list.add(tokenizer.nextElement());
    }

    if (_LOG.isFiner())
    {
      _LOG.finer("Found " + list.size() + " entries for shuttle " + clientId);
    }

    return list.toArray(new String[list.size()]);

  }

  static private Map _getAllItems(List allSelectItems)
  {
    int length = allSelectItems.size();
    Map allItems = new ArrayMap(length);
    for (int i=0; i < length; i++)
    {
      SelectItem selectItem = (SelectItem) allSelectItems.get(i);
      Object selectItemValue = selectItem.getValue();
      allItems.put(selectItemValue, IntegerUtils.getInteger(i));

    }
    return allItems;
  }

  /**
   * Given a list of SelectItem Objects, get the SelectItem's value. Next
   * convert that value to its index, using the allItems array to figure out
   * the index.
   * This is called when valuePassThru is false.
   * @param allItems Map which contains the SelectItem Object's value as the
   *                 key and the index as the value.
   *                 This contains all the items in the shuttle, whether
   *                 they are in the leading or following lists.
   * @param itemsToConvert  This is a List of SelectItem Objects. Convert
   *                        each SelectItem's value to be an index by
   *                        looking at the allItems to get the index.
   *
   */
  static private void _convertSelectItemValueToIndex(
    Map  allItems,
    List itemsToConvert)
  {
    if (allItems == null || itemsToConvert == null)
      return;


    int length = itemsToConvert.size();
    // loop through each item to convert.
    for (int j=0; j < length; j++)
    {
      SelectItem selectItem = (SelectItem) itemsToConvert.get(j);
      Object selectItemValue = selectItem.getValue();

      Integer index = (Integer)allItems.get(selectItemValue);
      selectItem.setValue(index);
    }

  }

  static private final ADFLogger _LOG = ADFLogger.createADFLogger(
    SelectManyShuttleRenderer.class);
}
