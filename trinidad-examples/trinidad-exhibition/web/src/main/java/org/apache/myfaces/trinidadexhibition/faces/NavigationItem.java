/**
 * 
 */
package org.apache.myfaces.trinidadexhibition.faces;

import javax.faces.event.ActionEvent;

import org.apache.myfaces.trinidadexhibition.util.UtilFunctions;

/**
 *
 * @author Andrew Robinson
 */
public class NavigationItem
{
  private String _messageKey;
  private String _viewId;
  
  public NavigationItem()
  {
  }
  
  public NavigationItem(String messageKey, String viewId)
  {
    _messageKey = messageKey;
    _viewId = viewId;
  }

  /**
   * @return the key
   */
  public String getMessageKey()
  {
    return _messageKey;
  }

  /**
   * @param messageKey the key to set
   */
  public void setMessageKey(String messageKey)
  {
    _messageKey = messageKey;
  }

  /**
   * @return the viewId
   */
  public String getViewId()
  {
    return _viewId;
  }
  
  /**
   * @param viewId the viewId to set
   */
  public void setViewId(String viewId)
  {
    _viewId = viewId;
  }
  
  public void actionPerfomed(ActionEvent event)
  {
    UtilFunctions.navigateToViewId(getViewId(), true);
  }
}
