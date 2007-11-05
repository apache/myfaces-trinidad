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

import java.util.List;

import javax.faces.component.UIComponent;

import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * Utility functions for use by Changes.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/change/ChangeUtils.java#0 $) $Date: 10-nov-2005.19:09:58 $
 */
class ChangeUtils 
{
  private ChangeUtils()
  {
  }

  /**
   * Given a parent component and the identifier for the child, looks up among
   *  the children for a child with the specified identifier and returns.
   * Returns null if there were to be no such child
   * @param parent the parent UIComponent
   * @param childId the 'id' identifier value of child to be searched in the parent's 
   *        children.
   */
  @SuppressWarnings("unchecked")
  public static UIComponent getChildForId(UIComponent parent, String childId)
  {
    return getChildForId(parent, childId, "id");
  }
  
  /**
   * Given a parent component and the identifier value for the child, looks up among
   * the children for a child with the specified identifier and returns.
   * Returns null if there were to be no such child
   * @param parent the parent UIComponent
   * @param childId the identifier value of child to be searched in the parent's 
   *        children.
   * @param identifier the identifier type 
   */
  @SuppressWarnings("unchecked")
  public static UIComponent getChildForId(
    UIComponent parent, 
    String childId,
    String identifier)
  {
    if (parent == null)
      return null;

    int numChildren = parent.getChildCount();
    if (numChildren == 0)
      return null;

    List<UIComponent> children = parent.getChildren();
    
    for (int i=0; i<numChildren; i++)
    {
      UIComponent child = children.get(i);
      Object attrVal = child.getAttributes().get(identifier);
      
      if ( childId.equals(attrVal) )
        return child;
    }
    return null;
  }
  
  /**
   * Given a parent component and the identifier for the child, looks up among
   * the children for a child with the specified identifier and returns the index
   * of the child
   * Returns -1 if there were to be no such child
   * @param parent
   * @param childId the identifier of child to be searched in the parent's 
   * children
   */
  @SuppressWarnings("unchecked")
  public static int getChildIndexForId(UIComponent parent, String childId)
  {
    if (parent == null)
      throw new NullPointerException(_LOG.getMessage(
        "PARENT_CANNOT_BE_NULL"));

    int numChildren = parent.getChildCount();
    if (numChildren == 0)
      return -1;

    List<UIComponent> children = parent.getChildren();      
    UIComponent child;    
    for (int i=0; i<numChildren; i++)
    {
      child = children.get(i);
      if ( childId.equals(child.getId()) )
        return i;
    }
    return -1;
  }
  
  /**
   * Given a node representing a component, returns the named facet's Element.
   * @param componentNode The node to search for a facet contained in it.
   * @param facetName The name of the facet to search for.
   * @return
   */
  static Element __getFacetElement(
    Node componentNode,
    String facetName)
  {
    assert componentNode != null;
    assert (facetName != null) && (facetName.length() > 0);
    
    Node currChild = componentNode.getFirstChild();
    
    while (currChild != null)
    {
      // check for local name match
      if ("facet".equals(currChild.getLocalName()))
      {
        // check for namespace match
        if (__JSF_CORE_NAMESPACE.equals(currChild.getNamespaceURI()))
        {
          NamedNodeMap attributes = currChild.getAttributes();

          if (facetName.equals(attributes.getNamedItem("name").getNodeValue()))
          {
            return (Element)currChild;
          }
        }
      }

      currChild = currChild.getNextSibling();
    }
    
    return null;
  }

  /**
   * Removes all of the children from the parent Node.
   * @param parentNode 
   */
  static void __removeAllChildren(Node parentNode)
  {
    Node nukeChild = parentNode.getFirstChild();
    
    while (nukeChild != null)
    {
      parentNode.removeChild(nukeChild);
      nukeChild = parentNode.getFirstChild();
    }
  }

  static final String __JSF_CORE_NAMESPACE = "http://java.sun.com/jsf/core";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    ChangeUtils.class);
}
