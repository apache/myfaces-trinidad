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

import java.util.ArrayList;

/**
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-demo/src/main/java/oracle/adfdemo/view/faces/DemoTreeData.java#1 $) $Date: 16-aug-2005.15:12:27 $
 */

public class DemoTreeData extends ArrayList<TreeNodeImpl>
{

  // data

  /**
   * @param text the text label of the tree node
   */
  private static TreeNodeImpl _createNode(
    String text
    )
  {
    TreeNodeImpl data = new TreeNodeImpl();
    data.setText(text);
    data.setDestination( "http://www.oracle.com");
    data.setNodeType("document");
    return data;
  }

  public DemoTreeData()
  {
    TreeNodeImpl node_0 = _createNode("node_0");
    TreeNodeImpl node_0_0 = _createNode("node_0_0");
    TreeNodeImpl node_0_0_0 = _createNode("node_0_0_0");
    TreeNodeImpl node_0_0_0_0 = _createNode("node_0_0_0_0");
    TreeNodeImpl node_0_0_1 = _createNode("node_0_0_1");
    TreeNodeImpl node_0_1 = _createNode("node_0_1");
    TreeNodeImpl node_0_1_0 = _createNode("node_0_1_0");
    TreeNodeImpl node_0_1_1 = _createNode("node_0_1_1");
    TreeNodeImpl node_0_2 = _createNode("node_0_2");
    TreeNodeImpl node_0_3 = _createNode("node_0_3");
    TreeNodeImpl node_0_4 = _createNode("node_0_4");
    TreeNodeImpl node_0_5 = _createNode("node_0_5");

    add(node_0);

    ArrayList<TreeNodeImpl> list_0 = new ArrayList<TreeNodeImpl>();
    list_0.add(node_0_0);
    list_0.add(node_0_1);
    list_0.add(node_0_2);
    list_0.add(node_0_3);
    list_0.add(node_0_4);
    list_0.add(node_0_5);
    node_0.setChildren(list_0);
    node_0.setNodeType("folder");

    ArrayList<TreeNodeImpl> list_0_0 = new ArrayList<TreeNodeImpl>();
    list_0_0.add(node_0_0_0);
    list_0_0.add(node_0_0_1);
    node_0_0.setChildren(list_0_0);
    node_0_0.setNodeType("folder");

    ArrayList<TreeNodeImpl> list_0_0_0 = new ArrayList<TreeNodeImpl>();
    list_0_0_0.add(node_0_0_0_0);
    node_0_0_0.setChildren(list_0_0_0);
    node_0_0_0.setNodeType("folder");

    ArrayList<TreeNodeImpl> list_0_1 = new ArrayList<TreeNodeImpl>();
    list_0_1.add(node_0_1_0);
    list_0_1.add(node_0_1_1);
    node_0_1.setChildren(list_0_1);
    node_0_1.setNodeType("folder");
  }
}
