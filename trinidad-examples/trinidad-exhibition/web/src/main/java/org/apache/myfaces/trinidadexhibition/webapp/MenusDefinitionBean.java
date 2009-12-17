/**
 * 
 */
package org.apache.myfaces.trinidadexhibition.webapp;

import java.util.List;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidadexhibition.faces.ManagedBean;
import org.apache.myfaces.trinidadexhibition.faces.NavigationItem;
import org.apache.myfaces.trinidadexhibition.faces.NavigationItemConverter;

/**
 *
 * @author Andrew Robinson
 */
public class MenusDefinitionBean
  extends ManagedBean
{
  private NavigationItem[] _mainMenu;

  /**
   * @return the mainMenu
   */
  public NavigationItem[] getMainMenu()
  {
    return _mainMenu;
  }

  /**
   * @param mainMenu the mainMenu to set
   */
  public void setMainMenu(NavigationItem[] mainMenu)
  {
    _mainMenu = mainMenu;
  }
  
  public void setMainMenuFromString(List<String> items)
  {
    setMainMenu(convert(items));
  }
  
  private NavigationItem[] convert(List<String> items)
  {
    NavigationItemConverter converter = new NavigationItemConverter();
    FacesContext facesContext = getFacesContext();
    NavigationItem[] navItems = new NavigationItem[items.size()];
    for (int i = 0; i < navItems.length; ++i)
    {
      navItems[i] = converter.getAsObject(facesContext, null, items.get(i));
    }
    return navItems;
  }
}
