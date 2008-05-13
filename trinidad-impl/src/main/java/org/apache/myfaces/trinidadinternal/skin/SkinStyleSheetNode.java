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
package org.apache.myfaces.trinidadinternal.skin;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.myfaces.trinidad.context.Version;

/** Stores information about the .css skin file.
 * namespaceMap, a List of SkinSelectorPropertiesNodes, and direction.
 * @todo honor the namespaces that are set in the css file. For now, we ignore
 * them. We need to honor them for icons, properties, and styles at the same
 * time so they are consistent. By honoring, I mean if the namespace is
 * af http://uix.faces.abc and the style name is af|breadCrumbs, we store the
 * icon or property as http://uix.faces.abc|navigationPath. We need to do something
 * similar with styles.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/SkinStyleSheetNode.java#0 $) $Date: 10-nov-2005.18:58:59 $

 */
class SkinStyleSheetNode
{
  SkinStyleSheetNode(
    List<SkinSelectorPropertiesNode> skinSelectorNodeList,
    Map<String, String>              namespaceMap,
    int                              direction,
    Map<Integer, Set<Version>>       agentVersions,
    Set<String>                      accProperties)
  {
    _skinSelectorNodeList = skinSelectorNodeList;
    _namespaceMap = namespaceMap;
    _direction = direction;
    _agentVersions = agentVersions;
    _accProperties = accProperties;
  }

  SkinStyleSheetNode(
    Map<String, String>        namespaceMap,
    int                        direction,
    Map<Integer, Set<Version>> agentVersions,
    int[]                      platforms,
    Set<String>                accProperties)
  {
    _namespaceMap = namespaceMap;
    _direction = direction;
    _agentVersions = agentVersions;
    _platforms = platforms;
    _accProperties = accProperties;
  }

  public void add(SkinSelectorPropertiesNode node)
  {
    if (_skinSelectorNodeList == null)
    {
      _skinSelectorNodeList = new ArrayList<SkinSelectorPropertiesNode>();
    }

    _skinSelectorNodeList.add(node);
  }

  /**
   *
   * @return Map containing Strings keys/values of namespace prefix and
   * namespaces specified in the style sheet.
   */
  public Map<String, String> getNamespaceMap()
  {
    return _namespaceMap;
  }

  /**
   *
   * @return List containing SkinSelectorPropertiesNodes
   */
  public List<SkinSelectorPropertiesNode> getSelectorNodeList()
  {
    return _skinSelectorNodeList;
  }

  public int getDirection()
  {
    return _direction;
  }

  /**
   * @return a set of the supported agent versions
   */
  public Map<Integer, Set<Version>> getAgentVersions()
  {
    return _agentVersions;
  }

  public int[] getPlatforms()
  {
    return _platforms;
  }

  public Set<String> getAcessibilityProperties()
  {
    return _accProperties;
  }

  public boolean matches(
    int                        direction,
    Map<Integer, Set<Version>> agentVersions,
    int[]                      platforms,
    Set<String>                accProperties)
  {
    if (direction == _direction)
    {
      boolean agentsMatch = _mapsEqual(agentVersions, _agentVersions);

      if (agentsMatch)
      {
        boolean platformsMatch = _intArraysEqual(platforms, _platforms);
        if (platformsMatch)
        {
          boolean accMatch = _setsEqual(accProperties, _accProperties);

          if (accMatch)
            return true;
        }
      }
    }
    return false;
  }

  private boolean _intArraysEqual(int[] a1, int[] a2)
  {
    if (a1 != null)
      Arrays.sort(a1);
    if (a2 != null)
      Arrays.sort(a2);
    return Arrays.equals(a1, a2);
  }

  private boolean _setsEqual(Set<?> s1, Set<?> s2)
  {
    return (s1 == null)? (s2 == null): (s1.equals(s2));
  }

  private boolean _mapsEqual(Map<?, ?> m1, Map<?, ?> m2)
  {
    return (m1 == null)? (m2 == null): (m1.equals(m2));
  }

  private Map<String, String> _namespaceMap;
  private List<SkinSelectorPropertiesNode> _skinSelectorNodeList;
  private int _direction; // reading direction
  private Map<Integer, Set<Version>> _agentVersions;
  private int[] _platforms;
  private Set<String> _accProperties;
}
