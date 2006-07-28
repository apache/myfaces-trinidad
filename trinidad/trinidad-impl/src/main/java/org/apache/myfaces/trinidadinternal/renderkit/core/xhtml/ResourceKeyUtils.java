/*
 * Copyright  2005,2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;



/**
 * Utilities for dealing with Resource Keys
 */
public class ResourceKeyUtils
{
  /**
   * Replaces all the resource key aliases. For example, this can be
   * used to replace all af_table.styleXYZ values into 
   * af_treeTable.styleXYZ values.
   * @param original the original resource key map
   * @param fromComponent eg: table
   * @param toComponent eg: treeTable
   * @return A new map with the replacesment values.
   */
  public static Map convertResourceKeyMap(
    Map original, 
    String fromComponent, 
    String toComponent)
  {
    String pattern1 = "af_"+fromComponent+".";
    String replace1 = "af_"+toComponent+".";
    String pattern2 = "af|"+fromComponent+":";
    String replace2 = "af|"+toComponent+":";
    
    Map result = new HashMap(original.size());
    Iterator entries = original.entrySet().iterator();
    while(entries.hasNext())
    {
      Map.Entry en = (Map.Entry) entries.next();
      String value = (String) en.getValue();
      if (value.startsWith(pattern1))
        value = _replace(value, pattern1, replace1);
      else if (value.startsWith(pattern2))
        value = _replace(value, pattern2, replace2);
      result.put(en.getKey(), value);
    }
    return result;
  }
  
  private static String _replace(String data, String search, String replace)
  {
    return replace + data.substring(search.length());
  }
}
