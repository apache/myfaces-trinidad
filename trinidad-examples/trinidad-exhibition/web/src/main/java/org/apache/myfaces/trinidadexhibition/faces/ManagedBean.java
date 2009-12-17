/**
 * 
 */
package org.apache.myfaces.trinidadexhibition.faces;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidadexhibition.util.UtilFunctions;

/**
 *
 * @author Andrew Robinson
 */
public class ManagedBean
{
  protected FacesContext getFacesContext()
  {
    return FacesContext.getCurrentInstance();
  }
  
  protected <T> T evaluateExpression(String expression, Class<T> expectedClass)
  {
    return UtilFunctions.evaluateEl(expression, expectedClass);
  }
  
  protected void setExpression(String expression, Object value)
  {
    UtilFunctions.setExpression(expression, value);
  }
  
  protected void navigate(String outcome)
  {
    navigate(outcome, outcome);
  }
  
  protected void navigate(String action, String outcome)
  {
    FacesContext fctx = getFacesContext();
    fctx.getApplication().getNavigationHandler().handleNavigation(fctx, action, action);
  }
}
