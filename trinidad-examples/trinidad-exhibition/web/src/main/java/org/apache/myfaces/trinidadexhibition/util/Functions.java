/**
 * 
 */
package org.apache.myfaces.trinidadexhibition.util;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.model.ChildPropertyMenuModel;
import org.apache.myfaces.trinidad.model.MenuModel;

/**
 *
 * @author Andrew Robinson
 */
public final class Functions
{
  private Functions() {}
  
  public static String getActionURL(String viewId)
  {
    FacesContext facesContext = FacesContext.getCurrentInstance();
    return facesContext.getApplication().getViewHandler().getActionURL(
      facesContext, viewId);
  }

  public static MenuModel asMenuModel(Object value, String childProperty, Object focusRowKey)
  {
    return new ChildPropertyMenuModel(value, childProperty, focusRowKey);
  }
}
