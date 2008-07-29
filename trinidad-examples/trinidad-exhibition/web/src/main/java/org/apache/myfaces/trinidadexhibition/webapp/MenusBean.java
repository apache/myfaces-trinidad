/**
 * 
 */
package org.apache.myfaces.trinidadexhibition.webapp;

import javax.faces.model.SelectItem;

import org.apache.myfaces.trinidadexhibition.util.UtilFunctions;

/**
 *
 * @author Andrew Robinson
 */
public class MenusBean
{
  private final SelectItem[] _mainMenuItems;
  
  public MenusBean()
  {
    _mainMenuItems = new SelectItem[] {
      buildItem("menus_component_demo"),
      buildItem("menus_feature_demo"),
      buildItem("menus_example_apps"),
      buildItem("menus_resources"),
    };
  }
  
  /**
   * @return the mainMenuItems
   */
  public SelectItem[] getMainMenuItems()
  {
    return _mainMenuItems;
  }
  
  private SelectItem buildItem(String key)
  {
    return new SelectItem(key, UtilFunctions.getBundleString(key));
  }
}
