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
package org.apache.myfaces.trinidaddemo;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A basic implementation of TreeNode that exposes the extra magic
 * keys that will be requested by the tree renderer.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-demo/src/main/java/oracle/adfdemo/view/faces/TreeNodeImpl.java#1 $) $Date: 16-aug-2005.15:12:29 $
 */
public class TreeNodeImpl implements Serializable
{
  public String getText()
  {
    return _text;
  }

  public void setText(String text)
  {
    _text = text;
  }


  public String getIcon()
  {
    return _icon;
  }

  public void setIcon(String icon)
  {
    _icon = icon;
  }

  public String getDestination()
  {
    return _destination;
  }


  public void setDestination(String destination)
  {
    _destination = destination;
  }

  public void setChildren(List<TreeNodeImpl> nodes)
  {
    // Clone on the way in.
    _nodes = new ArrayList<TreeNodeImpl>(nodes);
  }

  public List<TreeNodeImpl> getChildren()
  {
    if (_nodes == null)
      return null;

    return Collections.unmodifiableList(_nodes);
  }

  public String getNodeType()
  {
    return _nodeType;
  }

  public void setNodeType(String nodeType)
  {
    this._nodeType = nodeType;
  }

  private String _text = null;
  private String _destination = null;
  private String _icon = null;
  private String _nodeType = null;
  private List<TreeNodeImpl> _nodes;
}
