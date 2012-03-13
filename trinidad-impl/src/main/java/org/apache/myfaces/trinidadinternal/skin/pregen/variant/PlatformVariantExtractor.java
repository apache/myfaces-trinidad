/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidadinternal.skin.pregen.variant;

import java.util.ArrayList;
import java.util.Collection;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;
import org.apache.myfaces.trinidadinternal.style.util.NameUtils;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetNode;

 /**
  * An @-rule processor for extracting @platform rule metadata.
  */
final class PlatformVariantExtractor implements SkinVariantExtractor<Integer>
{
   public PlatformVariantExtractor()
   {
     _platforms = new HashSet<Integer>();
   }

   @Override
   public void visit(StyleSheetNode node)
   {
     Collection<Integer> nodePlatforms = node.getPlatforms();
     
     for (Integer platform : nodePlatforms)
     {
       _platforms.add(platform);
     }     
   }

  /**
   * Returns un unmodifiable list containing platforms corresponding
   * to all processed @platform rules.
   */
  public List<Integer> getVariants()
  {
    ArrayList<Integer> platforms = new ArrayList<Integer>(_platforms.size() + 1);

    // The "unknown" platform shouldn't be found by the document search,
    // but does need to be included during pregeneration.    
    if (!_platforms.contains(TrinidadAgent.OS_UNKNOWN))
    {
      platforms.add(TrinidadAgent.OS_UNKNOWN);      
    }
    
    platforms.addAll(_platforms);

    // Sort by name to make logger output/progress easier to monitor    
    _sortByPlatformName(platforms);

    return Collections.unmodifiableList(platforms);
  }
  
  private static void _sortByPlatformName(List<Integer> platforms)
  {
    Collections.sort(platforms,
      new Comparator<Integer>() {
        @Override
        public int compare(Integer platform1, Integer platform2)
        {
          String name1 = NameUtils.getPlatformName(platform1);
          String name2 = NameUtils.getPlatformName(platform2);
          assert(name1 != null);
          assert(name2 != null);
          
          return name1.compareTo(name2);
        }
      });
  }

  private final Collection<Integer> _platforms;
}
