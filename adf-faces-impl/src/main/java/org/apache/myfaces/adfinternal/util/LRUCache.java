/*
 * Copyright  2004-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.util;


import java.util.LinkedHashMap;
import java.util.Map;


import org.apache.myfaces.adf.logging.ADFLogger;

/**
 * A VERY simple LRU cache.
 * <p>
 * @author The Oracle ADF Faces Team
 */
public class LRUCache extends LinkedHashMap
{
  public LRUCache(int maxSize)
  {
    super(16, 0.75f, true /*prioritize by access order*/);
    _maxSize = maxSize;
  }

  protected void removing(Object key)
  {
  }

  protected boolean removeEldestEntry(Map.Entry eldest)
  {
    if (size() > _maxSize)
    {
      Object key = eldest.getKey();
      removing(key);

      _LOG.finer("Discarding cached value for key {0}", key);

      return true;
    }
    return false;
  }

  private final int _maxSize;

  private static final ADFLogger _LOG = ADFLogger.createADFLogger(LRUCache.class);

}
