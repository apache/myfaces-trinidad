package org.apache.myfaces.trinidad.style;

/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

/**
 *
 */
public abstract class Styles
{

  /**
   * Returns a Map containing the selector String as the key and the Style Object
   * (contains all the css property names/values) as the value. This Map can then be used
   * to get all the selector keys, or to get the Style Object for a Selector, or to 
   * get all the selectors that match some criteria, like they contain a certain simple selector.
   * 
   * @return unmodifiableMamp of the resolved Selector -> Style map.
   */
  public abstract Map<String, Style> getSelectorStyleMap();
  
  /**
   * Given a simple selector (as opposed to a complex selector) like "af|inputText", this method
   * will return a Set of the selectors that contain the simple selector.
   * The recommended usage is to call this for  root dom selectors, like "af|inputText" or "af|treeTable" 
   * (and not selectors like "af|inputText::content")
   * and this will return a Set of all the selectors that contain the the simple selector.
   * @param simpleSelector
   * @return
   */
  public Set<String> getSelectorsForSimpleSelector(String simpleSelector)
  {
    Map<String, Style> selectorStyleMap = getSelectorStyleMap();
    Set<String> set = null;
    
    // loop through each entry and find all simple selectors
    for (Map.Entry<String, Style> entry : selectorStyleMap.entrySet())
    {
      String completeSelector = entry.getKey();
      if (completeSelector.indexOf(simpleSelector) > -1)
      {
        // split based on . and space and :?
        String[] selectorsSplit = _SPACE_PATTERN.split(completeSelector);
        
        for(String selector : selectorsSplit)
        {
          if (selector.indexOf(simpleSelector) > -1)
          {
            if (set == null)
              set = new HashSet<String>(); 
            set.add(completeSelector);
            break;
          }
        }
      }
    }
    if (set == null)
      set = Collections.emptySet();
    return set;
    
  }
  
  private static final Pattern _SPACE_PATTERN = Pattern.compile("[.\\n\\t\\s]");
}
