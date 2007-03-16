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
package org.apache.myfaces.trinidad.change;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;
import org.w3c.dom.Node;
import org.w3c.dom.NamedNodeMap;

/**
 * Change specialization for re-ordering of children.
 * While applying this Change, the specified order of children is  restored.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/change/ReorderChildrenComponentChange.java#0 $) $Date: 10-nov-2005.19:10:01 $
 */
public class ReorderChildrenComponentChange extends ComponentChange
                                            implements DocumentChange
{
  /**
   * Constructs a ReorderChange with the given List of identifiers for children.
   * @param childIds An in-order collection (List) of Ids (as java.lang.String) 
   *         of child components.
   *        This List implementation should be of type java.io.Serializable in
   *         order to be persisted.
   * @throws IllegalArgumentException if supplied childIds were to be null.
   */
  public ReorderChildrenComponentChange(
    List<String> childIds
    )
  {
    if (childIds == null)
      throw new IllegalArgumentException(
        "Cannot construct a ReorderChange with null childIds.");
  
    // make serializable copy of list        
    _childIds = Collections.unmodifiableList(new ArrayList<String>(childIds));
  }
  
  /**
   * Returns an unmodifiable List of the identifiers for the  children.
   */
  public List<String> getChildIds()
  {
    return _childIds;
  }
  
  /**
   * {@inheritDoc}
   * In case children were to be removed between the time when this Change was
   *  added, and the time when it was applied, maybe due to application of a
   *  RemoveChildrenChange, such children are not re-instated.
   * In case children were to be added between the time when this Change was
   *  added, and the time when it was applied, maybe due to application of an 
   *  AddChildChange, such children are appended to the end of the list in
   *  preserving the order in which they were added (that is they appear at 
   *  the end).
   */
  @SuppressWarnings("unchecked")
  @Override
  public void changeComponent(UIComponent uiComponent)
  {
    int childCount = uiComponent.getChildCount();
    if (childCount == 0)
      return;
 
    // build order map of of current Nodes, keyed by id
    Map<String, UIComponent> childrenMap = new LinkedHashMap<String, UIComponent>();
    
    List<UIComponent> children = uiComponent.getChildren();
    for(UIComponent child : children)
    {
      childrenMap.put(child.getId(), child);
    }

    // remove the children so that we can add them back in
    children.clear();

    //
    // put children back in, in order
    //
    for(String currReorderID : _childIds)
    {
      UIComponent currChild = childrenMap.remove(currReorderID);
      
      if (currChild != null)
      {
        children.add(currChild);
      }
    }
    
    // add in all of the rest of the children in
    // relative order they originally appeared
    children.addAll(childrenMap.values());
  }

  /**
   * {@inheritDoc}
   * In case children were to be removed between the time when this Change was
   *  added, and the time when it was applied, maybe due to application of a
   *  RemoveChildrenChange, such children are not re-instated.
   * In case children were to be added between the time when this Change was
   *  added, and the time when it was applied, maybe due to application of an 
   *  AddChildChange, such children are appended to the end of the list in
   *  preserving the order in which they were added (that is they appear at 
   *  the end).
   */
  public void changeDocument(
    Node componentNode)
  {
    // build order map of of current Nodes, keyed by id
    LinkedHashMap<String, Node> currChildrenMap = new LinkedHashMap<String, Node>(13);
        
    Node currChild = componentNode.getFirstChild();
    
    while (currChild != null)
    {
      int fakeIndex = 0;
      NamedNodeMap attributes = currChild.getAttributes();
      
      String currKey = null;
      if (attributes != null)
      {
        Node idAttr = attributes.getNamedItem("id");
        
        if (idAttr != null)
        {
          currKey = idAttr.getNodeValue();
        }
      }
      
      // create a dummy key to maintain order of non-ided children
      if (currKey == null)
      {
        // =-= bts What about insignificant whitespace?
        currKey = new Integer(fakeIndex++).toString();
      }

      currChildrenMap.put(currKey, currChild);
      
      // remove the children so that we can add them back in
      componentNode.removeChild(currChild);
      
      // next node is first node again
      currChild = componentNode.getFirstChild();
    }

    //
    // put children back in, in order
    //
    for(String currReorderID : _childIds)
    {
      currChild = currChildrenMap.remove(currReorderID);
      if (currChild != null)
      {
        componentNode.appendChild(currChild);
      }
    }
    
    // add in all of the rest of the children in
    // relative order they originally appeared
    for(Map.Entry<String, Node> entry : currChildrenMap.entrySet())
    {
      componentNode.appendChild(entry.getValue());
    } 
  }
  
  /** 
   * Returns true if adding the DocumentChange should force the JSP Document
   * to reload
   */
  public boolean getForcesDocumentReload()
  {
    return false;
  }

  private final List<String> _childIds;
}
