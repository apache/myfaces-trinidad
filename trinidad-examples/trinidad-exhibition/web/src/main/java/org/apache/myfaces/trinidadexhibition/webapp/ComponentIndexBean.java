/**
 * 
 */
package org.apache.myfaces.trinidadexhibition.webapp;

import java.util.HashMap;
import java.util.Map;

import org.apache.myfaces.trinidad.model.ChildPropertyMenuModel;
import org.apache.myfaces.trinidad.model.MenuModel;
import org.apache.myfaces.trinidadexhibition.metadata.tld.TagLibrary;
import org.apache.myfaces.trinidadexhibition.util.UtilFunctions;

/**
 *
 * @author Andrew Robinson
 */
public class ComponentIndexBean
{
  private Map<String, Integer> _numRowsMap = null;
  private final MenuModel _tagLibrariesMenuModel;
  
  public ComponentIndexBean()
  {
    MetaDataBean metaData = getMetaDataBean();
    _tagLibrariesMenuModel = new ChildPropertyMenuModel(metaData.getTagLibraries().values().toArray(
      new TagLibrary[metaData.getTagLibraries().size()]), null, null);
  }
  
  /**
   * @return the tagLibrariesMenuModel
   */
  public MenuModel getTagLibrariesMenuModel()
  {
    return _tagLibrariesMenuModel;
  }
  
  /**
   * @return the numRowsMap
   */
  public Map<String, Integer> getNumRowsMap()
  {
    if (_numRowsMap == null)
    {
      synchronized (this)
      {
        if (_numRowsMap == null)
        {
          _numRowsMap = new HashMap<String, Integer>(3);
          MetaDataBean metaData = getMetaDataBean();
          for (String str : new String[] { "tr", "trh", "trs" })
          {
            int count = metaData.getTagLibraries().get(str).getTags().size();
            _numRowsMap.put(str, (int)Math.ceil(count / 3f));
          }
        }
      }
    }
    return _numRowsMap;
  }
  
  private MetaDataBean getMetaDataBean()
  {
    return UtilFunctions.evaluateEl("#{metaData}", MetaDataBean.class);
  }
}
