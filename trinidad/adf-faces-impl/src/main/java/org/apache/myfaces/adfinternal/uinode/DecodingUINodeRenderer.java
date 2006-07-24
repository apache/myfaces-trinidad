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
package org.apache.myfaces.adfinternal.uinode;

import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.adf.logging.ADFLogger;

import org.apache.myfaces.adfinternal.convert.ConverterUtils;

/**
 * Renderer for EditableValueHolder components.
 * <p>
 * @todo Rename this class
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/uinode/DecodingUINodeRenderer.java#0 $) $Date: 10-nov-2005.18:49:14 $
 * @author The Oracle ADF Faces Team
 */
public class DecodingUINodeRenderer extends UINodeRendererBase
{
  public void decode(FacesContext context, UIComponent component)
  {
    if (skipDecode(context))
      return;

    Object submittedValue;
    if (!wasSubmitted(context, component))
      submittedValue = null;
    else
      submittedValue = getSubmittedValue(context, component);

    if (_LOG.isFinest())
    {
      _LOG.finest("Value submitted for ID {0} is {1}",
                  new Object[]{component.getClientId(context),
                               submittedValue});
    }

    EditableValueHolder evh = (EditableValueHolder) component;
    evh.setSubmittedValue(submittedValue);
  }

  public Object getSubmittedValue(
    FacesContext context,
    UIComponent  component)
  {
    String clientId = component.getClientId(context);
    return context.getExternalContext().
                                getRequestParameterMap().get(clientId);
  }

  /**
   * Converts a string value into the component's value
   * @param context the FacesContext
   * @param component the component
   * @param newValue the unconverted string value
   */
  public Object getConvertedValue(
    FacesContext context,
    UIComponent  component,
    Object       submittedValue) throws ConverterException
  {
    EditableValueHolder evh = (EditableValueHolder) component;

    Converter converter = __getConverter(context, evh);
    if (converter != null)
    {
      return converter.getAsObject(context,
                                   component,
                                   submittedValue.toString());
    }

    return submittedValue;
  }

  /**
   * Override this method to return "false" if the component was
   * not actually submitted (if, for instance, it was disabled
   * or "read-only".
   */
  protected boolean wasSubmitted(
    FacesContext context,
    UIComponent  component)
  {
    return true;
  }

  /**
   */
  static Converter __getConverter(
    FacesContext context,
    EditableValueHolder component)
  {
    Converter converter = component.getConverter();
    if (converter != null)
      return converter;

    ValueBinding binding = ((UIComponent) component).getValueBinding("value");
    if (binding != null)
    {
      Class modelClass = binding.getType(context);
      return ConverterUtils.createConverter(context, modelClass);
    }

    return null;
  }

  static private final ADFLogger _LOG = ADFLogger.createADFLogger(DecodingUINodeRenderer.class);
}
