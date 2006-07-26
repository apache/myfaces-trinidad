/*
 * Copyright  2004-2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.renderkit.uix;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.component.UISelectItem;
import javax.faces.component.UISelectItems;
import javax.faces.component.ValueHolder;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.el.ValueBinding;
import javax.faces.model.SelectItem;
import org.apache.myfaces.trinidad.component.UIXSelectItem;
import org.apache.myfaces.trinidadinternal.convert.ConverterUtils;

public class SelectItemSupport
{
  private SelectItemSupport()
  {
  }

   /**
   * 
   * @param component  UIComponent
   * @param converter For UISelectItem and UIXSelectItem children of the 
   *                  component, use the converter to convert itemValue Strings 
   *                  when creating the javax.faces.model.SelectItem Object if
   *                  the child's value is not an instanceof SelectItem.
   * @return a List of javax.faces.model.SelectItem Objects that we get or 
   *         create from the component's children.
   *         OR 
   *         java.util.Collections.EMPTY_LIST if component has no children or
   *         the component isn't a javax.faces.component.ValueHolder. else
   */
  static public List getSelectItems(
    UIComponent  component,
    Converter    converter)
  { 
    
    int childCount = component.getChildCount();
    if (childCount == 0)
      return Collections.EMPTY_LIST;

    // Make sure we haven't accidentally stumbled outside of
    // the UIXSelectXXX world.
    if (!(component instanceof ValueHolder))
      return Collections.EMPTY_LIST;

    FacesContext context = FacesContext.getCurrentInstance();
    List items = null;
    List children = component.getChildren();
    for (int i = 0; i < childCount; i++)
    {
      UIComponent child = (UIComponent) children.get(i);
      // f:selectItem
      if (child instanceof UISelectItem)
      {
        if (items == null)
          items = new ArrayList(childCount);
        _addSelectItem(context, 
                       component, 
                       (UISelectItem) child, 
                       items, 
                       converter);
      }
      // f:selectItems
      else if (child instanceof UISelectItems)
      {
        if (items == null)
          items = new ArrayList(childCount);
        addSelectItems((UISelectItems) child, items);
      }
      // af:selectItem
      else if (child instanceof UIXSelectItem)
      {
        if (items == null)
          items = new ArrayList(childCount);
        _addUIXSelectItem(context,
                          component,
                          (UIXSelectItem) child, 
                          items, 
                          converter);
        

        
      }
    }

    if (items == null)
      return Collections.EMPTY_LIST;

    return items;
  }
  
  /**
   * 
   * @param component  UIComponent
   * @return item count
   */
  static public int getSelectItemCount(
    UIComponent  component)
  { 
    int itemCount = 0;
    int childCount = component.getChildCount();
    if (childCount == 0)
      return itemCount;

    // Make sure we haven't accidentally stumbled outside of
    // the UIXSelectXXX world.
    if (!(component instanceof ValueHolder))
      return itemCount;

    List children = component.getChildren();
    for (int i = 0; i < childCount; i++)
    {
      UIComponent child = (UIComponent) children.get(i);
      
      if (child instanceof UISelectItem ||
          child instanceof UIXSelectItem)
      {
        itemCount++;
      }
      // f:selectItems
      else if (child instanceof UISelectItems)
      {        
        Object value = ((UISelectItems)child).getValue();
        if (value instanceof SelectItem)
        {
          itemCount++;
        }
        else if (value instanceof Object[])
        {
          Object[] array = (Object[]) value;
          itemCount = itemCount + array.length;
        }
        else if (value instanceof Collection)
        {
          itemCount = itemCount + ((Collection) value).size();  
        }
        else if (value instanceof Map)
        {        
          itemCount = itemCount + ((Map) value).size();  
        }                        
      }
    }
    
    return itemCount;
  }

  
  
  /**
   * Adds a SelectItem object derived from
   * a UISelectItem component into the items List.
   * @param context    FacesContext
   * @param component  UIComponent (the parent of the uixSelectItem)
   * @param uiItem     The UISelectItem component we are deriving the 
   *                   javax.faces.model.SelectItem Object from.
   * @param items      The List to which we are adding the 
   *                   javax.faces.model.SelectItem Object.
   * @param converter  The converter for the component. This is used to
   *                   create the SelectItem Object
   *                   if the UISelectItem's value attribute's value 
   *                   is NOT an instance of SelectItem and the UISelectItem's
   *                   itemValue is a String.
   */
  static private void _addSelectItem(
    FacesContext  context,
    UIComponent   component,
    UISelectItem  uiItem,
    List          items,
    Converter     converter)
  {
    if (!uiItem.isRendered())
    {
      items.add(null);
      return;
    }

    Object value = uiItem.getValue();
    SelectItem item;

    if (value instanceof SelectItem)
    {
      item = (SelectItem) value;
    }
    else
    {
      Object itemValue = uiItem.getItemValue();
      if ((converter != null) &&  (itemValue instanceof String))
      {

        // convert to model class if it isn't a String, too.
        itemValue = converter.getAsObject(context, 
                                          component, 
                                          (String)itemValue);
      }
      String itemLabel = uiItem.getItemLabel();
      item = new SelectItem(itemValue == null ? "" : itemValue,
                            itemLabel == null ? "" : itemLabel,
                            uiItem.getItemDescription(),
                            uiItem.isItemDisabled());
    }

    items.add(item);
  }

  /**
   * Adds a SelectItem object derived from
   * a UISelectItem component into the items List.
   */
  static public void addSelectItem(
    UISelectItem  uiItem,
    List          items)
  {
    Object value = uiItem.getValue();
    SelectItem item;

    if (value instanceof SelectItem)
    {
      item = (SelectItem) value;
    }
    else
    {
      Object itemValue = uiItem.getItemValue();
      String itemLabel = uiItem.getItemLabel();
      // JSF throws a null pointer exception for null values and labels,
      // which is a serious problem at design-time.
      item = new SelectItem(itemValue == null ? "" : itemValue,
                            itemLabel == null ? "" : itemLabel,
                            uiItem.getItemDescription(),
                            uiItem.isItemDisabled());
    }

    items.add(item);
  }

  /**
   * Adds SelectItem objects derived from
   * a UISelectItems component into the items List.
   */
  static public void addSelectItems(
    UISelectItems uiItems,
    List          items)
  {
    if (!uiItems.isRendered())
    {
      items.add(null);
      return;
    }

    Object value = uiItems.getValue();
    if (value instanceof SelectItem)
    {
      items.add((SelectItem) value);
    }
    else if (value instanceof Object[])
    {
      Object[] array = (Object[]) value;
      for (int i = 0; i < array.length; i++)
      {
        items.add((SelectItem) array[i]);
      }
    }
    else if (value instanceof Collection)
    {
      Iterator iter = ((Collection) value).iterator();
      while (iter.hasNext())
      {
        items.add((SelectItem) iter.next());
      }
    }
    else if (value instanceof Map)
    {
      Iterator entries = ((Map) value).entrySet().iterator();
      while (entries.hasNext())
      {
        Map.Entry entry = (Map.Entry) entries.next();
        Object label = entry.getKey();
        SelectItem item =
          new SelectItem(entry.getValue(),
                         label == null ? (String) null : label.toString());

        items.add(item);
      }
    }
  }
 
  /**
   * Adds a SelectItem object derived from
   * a UIXSelectItem component into the items List.
   * @param context       FacesContext
   * @param component     UIComponent (the parent of the uixSelectItem)
   * @param uixSelectItem  The UIXSelectItem component we are deriving the 
   *                   javax.faces.model.SelectItem Object from.
   * @param items      The List to which we are adding the 
   *                   javax.faces.model.SelectItem Object.
   * @param converter  The converter for the component. This is used to
   *                   create the SelectItem Object
   *                   if the UIXSelectItem's value attribute's value 
   *                   is NOT an instance of SelectItem and the UIXSelectItem's
   *                   itemValue is a String.
   */   
  static private void _addUIXSelectItem(
    FacesContext  context,
    UIComponent   component,
    UIXSelectItem uixSelectItem,
    List          items,
    Converter     converter)
  {  
    // check if rendered="false". If so, add null to the list.
    if (!uixSelectItem.isRendered())
    {
      items.add(null);
      return;
    }
    
    Object label = uixSelectItem.getAttributes().get("label");
    Object value = uixSelectItem.getValue();    
    boolean disabled = 
      (Boolean.TRUE.equals(uixSelectItem.getAttributes().get("disabled")));  
        
    Object description = uixSelectItem.getAttributes().get("shortDesc");
    SelectItem selectItem = null;
    if (value instanceof SelectItem)
    {
      selectItem = (SelectItem)value;
    }
    else
    {
      if ((converter != null) &&  (value instanceof String))
      {

        // convert to model class if it isn't a String, too.
        value = converter.getAsObject(context, 
                                      component, 
                                      (String)value);
      }    
      selectItem = 
        new SelectItem(value == null ? "" : value, 
                       label == null ? "" : label.toString(),
                       description == null ? "" : (String)description,
                       disabled);
    }
    items.add(selectItem);

  }
  
  static public Converter getConverter(
    UIComponent component)
  {  
    FacesContext fContext = FacesContext.getCurrentInstance();
  
    Converter converter = null;
    Class modelClass = null;
    
    ValueBinding binding = component.getValueBinding("value");
    if (binding != null)
    {
      modelClass = binding.getType(fContext);
      if (modelClass == null)
      {
        Object o = binding.getValue(fContext);
        if (o != null)
        {
          modelClass = o.getClass();
        }
      }
    }

    if ((modelClass != null) && 
        ( modelClass.isArray() ||  modelClass.isAssignableFrom(List.class)))
    {
        // get the itemClass in the case where modelClass is an array or List
        // for instance, in the case of selectManyListbox
        Class itemClass = modelClass.getComponentType();
        if (itemClass != null)
        {
           converter = ConverterUtils.createConverter(fContext, itemClass);           
        }      
    }
    else
    {
      converter = ConverterUtils.createConverter(fContext, modelClass);
    }


    return converter;
  }
  
}
