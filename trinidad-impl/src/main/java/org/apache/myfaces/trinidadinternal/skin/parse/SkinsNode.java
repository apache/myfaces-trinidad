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
package org.apache.myfaces.trinidadinternal.skin.parse;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.apache.myfaces.trinidad.skin.SkinAddition;
import org.apache.myfaces.trinidad.skin.SkinMetadata;

/**
 * Object which represents the skin and skin-addition nodes in trinidad-skins.xml.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/SkinPropertyNode.java#0 $) $Date: 10-nov-2005.18:50:45 $
 */
public class SkinsNode
{
  /**
   *
   */
  public SkinsNode(
    List<SkinMetadata> skinNodes,
    List<SkinAddition> skinAdditionNodes)
  {
    _skinAdditionNodes = skinAdditionNodes;
    _skinNodes = skinNodes;
  }

  /**
   */
  public List<SkinAddition> getSkinAdditionNodes()
  {
    return _skinAdditionNodes;
  }
  /**
   *
   */
  public List<SkinMetadata> getSkinNodes()
  {
    return _skinNodes;
  }

  private List<SkinAddition> _skinAdditionNodes;
  private List<SkinMetadata> _skinNodes;

}
