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
package org.apache.myfaces.trinidadinternal.renderkit.uix;

import java.lang.reflect.Array;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;
import javax.faces.el.ValueBinding;
import javax.faces.model.SelectItem;
import org.apache.myfaces.trinidad.component.UIXSelectMany;

import org.apache.myfaces.trinidad.util.MessageFactory;
import org.apache.myfaces.trinidadinternal.convert.ConverterUtils;
/**
 * @todo Support primitive arrays (=-=AEW Already does?)
 */
public class SelectManyRenderer extends InputRenderer
{
  /**
   * Support the following types of values:
   * Multiple-selection: Object[] (that contains strings),
   *   String[], List (that contains strings). (selected values)
   * Single-selection: String (selected value)
   * @todo Add support for all types supported by UISelectMany
   * @todo This throws a ConverterException on the first unconvertable
   *  value;  it should wait
   */
  public Object getConvertedValue(
    FacesContext context,
    UIComponent  component,
    Object       submittedValue) throws ConverterException
  {
    // If "submittedValue" is null, something's gone wrong;  that
    // should be caught by editable component
    assert(submittedValue != null);

    UIXSelectMany selectMany = (UIXSelectMany)component;

    Class modelClass = null;
    ValueBinding binding = component.getValueBinding("value");
    if (binding != null)
    {
      modelClass = binding.getType(context);
      if (modelClass == null)
      {
        Object o = binding.getValue(context);
        if (o != null)
        {
          modelClass = o.getClass();
        }
      }
    }


    boolean valuePassThru = getValuePassThru(component);

    if (!valuePassThru)
    {
      return _convertIndexedSubmittedValue(context,
                                           component,
                                           submittedValue,
                                           modelClass);

    }
    else
    {

      Converter converter = selectMany.getConverter();

      String[] values = (String[]) submittedValue;
      if ((modelClass == null) || modelClass.isAssignableFrom(List.class))
      {
        if (converter == null)
        {
          return Arrays.asList(values);
        }

        ArrayList newList = new ArrayList(values.length);
        for (int i = 0; i < values.length; i++)
        {
          // Note - any error will result in an immediate ConverterException
          newList.add(converter.getAsObject(context, component, values[i]));
        }

        return newList;
      }
      else if (modelClass.isArray())
      {
        Class itemClass = modelClass.getComponentType();
        if (converter == null)
        {
          converter = ConverterUtils.createConverter(context, itemClass);
          // We'd better be able to coerce each entry appropriately
          if (converter == null)
          {
            if ((itemClass == String.class) ||
                (itemClass == Object.class) ||
                (itemClass == null))
            {
              return values;
            }
            else
            {
              // Don't throw a ConverterException for something a user can't fix!
              throw new IllegalStateException(
                "Couldn't coerce submitted entries to type " + itemClass);
            }
          }
        }

        // Use Array API instead of Object[] to support primitive types
        Object convertedArray = Array.newInstance(itemClass, values.length);
        for (int i = 0; i < values.length; i++)
        {
          // Note - any error will result in an immediate ConverterException
          Array.set(convertedArray, i,
                    converter.getAsObject(context, component, values[i]));
        }

        return convertedArray;
      }
      else
      {
        throw new ConverterException(
          MessageFactory.getMessage(context,
                                    UIXSelectMany.UNSUPPORTED_MODEL_TYPE_MESSAGE_ID,
                                    new Object[]{modelClass}, component));
      }
    }
  }



  public Object getSubmittedValue(
    FacesContext context,
    UIComponent  component)
  {
    String clientId = component.getClientId(context);
    Object o = context.getExternalContext().
       getRequestParameterValuesMap().get(clientId);
    if (o == null)
      return _EMPTY_ARRAY;

    return o;

  }

  /**
   * Call this method only when the valuePassThru attribute on the component
   * is not set to true. This indicates that the client-side value
   * is an index. We need to convert that index into its real value.
   * @param component
   * @param submittedValue the submittedValue. Since this method is only
   *  called when the valuePassThru attribute on the selectMany component is
   *  not true, then the submittedValue in this case is an index into a List.
   * @return the Object value at that index specified in submittedValue.
   * * we convert it if the value should be a primitive.
   * @throws ConverterException if the index as sepcified by the
   * submittedValue is out of bounds.
   */
  static private Object _convertIndexedSubmittedValue(
    FacesContext context,
    UIComponent  component,
    Object       submittedValue,
    Class        modelClass) throws ConverterException
  {
    Converter converter = null;
    Class itemClass = null;
    // getComponentType returns the component type of an array, or null
    // if it isn't an array.
    if ( modelClass != null )
      itemClass = modelClass.getComponentType();

    if (itemClass != null)
      converter = ConverterUtils.createConverter(context, itemClass);
    List selectItems = SelectItemSupport.getSelectItems(component, converter);

    if ((selectItems == null) || (selectItems.isEmpty()))
    {
      return submittedValue;
    }

    // if we are here, then the submittedValue
    // is an index value, not a String value.
    // we need to convert the index value into the actual value
    // get each value that is in the submittedValue
    // this is the index value
    // use this index value to get at the selectItem
    // now add this selectItem's value to a List,
    // and return a now-transformed submittedValues array.
    assert (submittedValue instanceof String[]);

    String[] submittedValueArray = (String[])submittedValue;
    List submittedValuesList = new ArrayList();


    for (int i=0; i < submittedValueArray.length; i++)
    {
      try
      {
        int index =  Integer.parseInt(submittedValueArray[i]);

        if (( -1 < index) && (selectItems.size() > index))
        {
          SelectItem item = (SelectItem)selectItems.get(index);

          if (item != null)
            submittedValuesList.add(item.getValue());
        }
        else
        {
          throw new IndexOutOfBoundsException(
            "SelectMany submittedValue's index " +
            index + " is out of bounds. It should be between -1 and " +
            selectItems.size());
        }
      }
      catch (NumberFormatException ne)
      {
        throw new NumberFormatException(
          "SelectMany could not convert submittedValue's index " +
          submittedValue.toString() + " into int " + ne);
      }
    }
    // convert to modelClass
    if ((modelClass == null) || modelClass.isAssignableFrom(List.class))
    {
      return submittedValuesList;
    }
    else if (modelClass.isArray())
    {
      Object[] values = submittedValuesList.toArray();

      // Use Array API instead of Object[] to support primitive types
      Object convertedArray = Array.newInstance(itemClass, values.length);
      for (int i = 0; i < values.length; i++)
        // Note - any error will result in an immediate ConverterException
        Array.set(convertedArray, i, values[i]);

      return convertedArray;
    }
    else
    {
      throw new ConverterException(
        MessageFactory.getMessage(context,
                                  UIXSelectMany.UNSUPPORTED_MODEL_TYPE_MESSAGE_ID,
                                  new Object[]{modelClass}, component));
    }
  }

  /*
   * get the valuePassThru attribute value from the component.
   * defaults to false.
   * When true, the value is set on the client; otherwise we use an index on
   * the client, and convert it back to its true value in getConvertedValue
   * 
   */
  protected boolean getValuePassThru(UIComponent component)
  {
    Object valuePassThruObj =
      component.getAttributes().get("valuePassThru");

    // default valuePassThru to false.
    return Boolean.TRUE.equals(valuePassThruObj);
  }



  static private final String[] _EMPTY_ARRAY = new String[0];

}
