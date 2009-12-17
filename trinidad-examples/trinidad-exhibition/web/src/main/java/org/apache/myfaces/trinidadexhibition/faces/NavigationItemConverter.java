/**
 * 
 */
package org.apache.myfaces.trinidadexhibition.faces;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

/**
 *
 * @author Andrew Robinson
 */
public class NavigationItemConverter
  implements Converter
{

  /**
   * @see javax.faces.convert.Converter#getAsObject(javax.faces.context.FacesContext,
   * javax.faces.component.UIComponent, java.lang.String)
   */
  public NavigationItem getAsObject(FacesContext context, UIComponent component,
    String value) throws ConverterException
  {
    String[] str = value.split("\\s*,\\s*", 2);
    if (str.length < 2)
    {
      throw new ConverterException("NavigationItem string must have a comma to separate the view ID and the " +
      		"bundle key");
    }
    return new NavigationItem(str[0], str[1]);
  }

  /**
   * @see javax.faces.convert.Converter#getAsString(javax.faces.context.FacesContext,
   * javax.faces.component.UIComponent, java.lang.Object)
   */
  public String getAsString(FacesContext context, UIComponent component,
    Object value) throws ConverterException
  {
    if (value instanceof NavigationItem)
    {
      NavigationItem item = (NavigationItem)value;
      return String.format("%s,%s", item.getMessageKey(), item.getViewId());
    }
    return value == null ? "" : value.toString();
  }

}
