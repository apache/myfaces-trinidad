/**
 * 
 */
package org.apache.myfaces.trinidadexhibition.util;

import java.io.IOException;
import java.text.MessageFormat;
import java.util.ResourceBundle;

import javax.el.ValueExpression;
import javax.faces.FacesException;
import javax.faces.application.ViewHandler;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.context.RequestContext;

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
  
  public static <T> T evaluateEl(String el, Class<T> returnType)
  {
    FacesContext facesContext = FacesContext.getCurrentInstance();
    
    @SuppressWarnings("unchecked")
    T t = (T)facesContext.getApplication().getExpressionFactory().createValueExpression(el, returnType)
      .getValue(facesContext.getELContext());
    
    return t;
  }
  
  public static void setExpression(String expression, Object value)
  {
    FacesContext facesContext = FacesContext.getCurrentInstance();
    ValueExpression el = facesContext.getApplication().getExpressionFactory().createValueExpression(facesContext,
      value == null ? Object.class : value.getClass());
    
    el.setValue(facesContext.getELContext(), value);
  }
  
  public static void navigateToViewId(String viewId, boolean forceRedirect)
  {
    FacesContext facesContext = FacesContext.getCurrentInstance();
    ViewHandler viewHandler = facesContext.getApplication().getViewHandler();
    ExternalContext externalContext = facesContext.getExternalContext();
    
    RequestContext requestContext = RequestContext.getCurrentInstance();
    
    if (forceRedirect || requestContext.isPartialRequest(facesContext))
    {
      // if PPR, then we need to redirect
      String path = viewHandler.getActionURL(facesContext, viewId);
      try
      {
        externalContext.redirect(externalContext.encodeActionURL(path));
      }
      catch (IOException ex)
      {
        throw new FacesException(ex);
      }
    }
    else
    {
      // navigate using "normal" JSF view ID navigation
      //create new view
      UIViewRoot viewRoot = viewHandler.createView(facesContext, viewId);
      facesContext.setViewRoot(viewRoot);
      facesContext.renderResponse();
    }
  }
}
