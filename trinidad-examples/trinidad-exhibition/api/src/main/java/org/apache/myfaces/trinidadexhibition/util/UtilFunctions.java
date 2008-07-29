/**
 * 
 */
package org.apache.myfaces.trinidadexhibition.util;

import java.text.MessageFormat;
import java.util.ResourceBundle;

import javax.faces.context.FacesContext;

/**
 *
 * @author Andrew Robinson
 */
public final class UtilFunctions
{
  private UtilFunctions() {}
  
  public static String getBundleString(String key, Object... parms)
  {
    FacesContext facesContext = FacesContext.getCurrentInstance();
    ResourceBundle bundle = facesContext.getApplication().getResourceBundle(facesContext, "msgs");
    
    String str = bundle.getString(key);
    if (parms != null && parms.length > 0)
    {
      str = MessageFormat.format(str, parms);
    }
    return str;
  }
  
  public static Object evaluateEl(String el)
  {
    return evaluateEl(el, Object.class);
  }
  
  @SuppressWarnings("unchecked")
  public static <T> T evaluateEl(String el, Class<T> returnType)
  {
    FacesContext facesContext = FacesContext.getCurrentInstance();
    return (T)facesContext.getApplication().getExpressionFactory().createValueExpression(el, returnType)
      .getValue(facesContext.getELContext());
  }
}
