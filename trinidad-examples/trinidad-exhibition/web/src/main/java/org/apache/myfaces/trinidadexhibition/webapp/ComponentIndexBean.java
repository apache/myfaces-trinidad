/**
 * 
 */
package org.apache.myfaces.trinidadexhibition.webapp;

import java.util.HashMap;
import java.util.Map;

import org.apache.myfaces.trinidadexhibition.util.UtilFunctions;

/**
 *
 * @author Andrew Robinson
 */
public class ComponentIndexBean
{
  private Map<String, Integer> _numRowsMap = null;
  
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
          MetaDataBean metaData = UtilFunctions.evaluateEl("#{metaData}", MetaDataBean.class);
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
}
