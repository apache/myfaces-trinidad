/**
 * 
 */
package org.apache.myfaces.trinidadexhibition.util;

import javax.faces.context.FacesContext;

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
}
