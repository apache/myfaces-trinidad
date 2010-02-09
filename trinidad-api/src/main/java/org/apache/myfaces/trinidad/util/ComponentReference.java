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
package org.apache.myfaces.trinidad.util;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

/**
 * A utility to store a reference to an <code>UIComponent</code>. Application developers
 * should use this tool if they need to have a reference to an instance of the
 * <code>UIComponent</code> class in <code>managed beans</code> that are <b>session scoped</b>.
 * 
 * Use <code>newUIComponentReference()</code> to create a <code>ComponentReference</code> and
 * use the <code>getComponent()</code> to look up the referenced <code>UIComponent</code>.
 * 
 * <p><b>Please note:</b>
 * <ul>
 * <li>This class is <b>not</b> thread-safe, since it depends on <code>UIComponent</code> APIs.
 * <li>The passed in <code>UIComponent</code> is <b>required</b> to have an <code>ID</code>
 * <li>The reference will break if the <code>UIComponent</code> is moved between <code>NamingContainer</code>s <b>or</b>
 * if any of the ancestor <code>NamingContainer</code>s have their IDs changed.
 * <li>The refrence is persistable. <b>However</b> <code>UIComponent</code>s are not <code>Serializable</code> and therefore
 * can not be used at any scope longer than request.
 * 
 * </ul>
 * 
 * @see ComponentReference#newUIComponentReference(UIComponent)
 * @see ComponentReference#getComponent()
 */
public final class ComponentReference<T extends UIComponent> implements Serializable
{
  /**
   * Private constructor, used by <code>ComponentReference.newUIComponentReference</code> 
   * @param uicomponent the <code>UIComponent</code> we want to store the path for
   */
  private ComponentReference(T uicomponent)
  {
    // create the "component path" to remember the position of the uicomponent and begin
    // the work to figureout the scopedID
    _scopedId = _createComponentPathAndScopedId(uicomponent, null);
  }

  /**
   * Factory method to create an instance of the <code>ComponentReference</code> class, which wraps the given
   * <code>UIComponent</code>. The component must be in the component tree when this method
   * is called and that we will throw an <code>IllegalArgumentException</code> if it is not.
   * 
   * @param uicomponent the <code>UIComponent</code> to wrap.
   * @throws IllegalArgumentException if component is not in the component tree
   * @throws IllegalArgumentException if component does <b>not</b> have an <code>Id</code>
   * @return <code>ComponentReference</code> that wrap the given component
   */
  public static <T extends UIComponent> ComponentReference<T> newUIComponentReference(T uicomponent)
  {
    return new ComponentReference<T>(uicomponent);
  }

  /**
   * This method will use a calculated "component path" to walk down to the <code>UIComponent</code>
   * that is referenced by this class. If the component can not be found, the <code>getComponent()</code>
   * will return <code>null</code>. 
   * 
   * @return the referenced <code>UIComponent</code> or <code>null</code> if it can not be found.
   * @see ComponentReference#newUIComponentReference(UIComponent)
   */
  @SuppressWarnings("unchecked")
  public T getComponent()
  {
    UIComponent foundComponent = null;

    // In order to find the component with its
    // calculated path, we need to start at the ViewRoot;
    UIViewRoot root = FacesContext.getCurrentInstance().getViewRoot();


    if (_componentPath != null) 
    {
      // Walk down the component tree, to the component we are looking for.
      // We start at the ViewRoot and use the previous calculated "component path"
      foundComponent = _walkPathToComponent(root, _componentPath);
    }

    // Check if we really found it with the previously created "component path"
    if (foundComponent == null || (!_componentId.equals(foundComponent.getId())))
    {
      // OK, we were not luck with the calculated "component path", let's
      // see if we can find it by using the "scoped ID" and the regular 
      // findComponent();
      foundComponent = root.findComponent(_scopedId);

      // was the regular findComponent() successful ?
      if (foundComponent != null)
      {        
        // OK, now let's rebuild the path
        _createComponentPathAndScopedId(foundComponent, _scopedId);
      }
    }

    return (T) foundComponent;
  }

  /**
   * Utility to (re) create the component Id from the <code>_scopedId</code>.
   * 
   * @return the extracted <code>UIComponentId</code>
   */
  private String _calculateComponentIdFromScopedId(String scopedId)
  {
    // String.substring() is optimized to return this if the entire string
    // is the substring so that no one tries to optimize it later
    return scopedId.substring(scopedId.lastIndexOf(NamingContainer.SEPARATOR_CHAR)+1);
  }

  /**
   * Transform the <code>scopedIdList</code> of "important" component IDs to 
   * generate the <code>scopedID</code> for the referenced <code>UIComponent</code>.
   * 
   * Uses the <code>scopedIdLength</code>
   */
  private String _createScopedId(int scopedIdLength, List<String> scopedIdList)
  {
    StringBuilder builder = new StringBuilder(scopedIdLength);

    for (int i = scopedIdList.size()-1; i >= 0 ; i--)
    {
      builder.append(scopedIdList.get(i));

      // if there are more IDs in the list, we need to append a colon
      if (i > 0)
      {
        builder.append(NamingContainer.SEPARATOR_CHAR);
      }
    }

    // store the (final) scopedId
    return builder.toString();
  }

  /**
   * Creates the "component path" started by the given <code>UIComponent</code> up the <code>UIViewRoot</code>
   * of the underlying component tree. The hierarchy is stored in a <code>List</code> of <code>Object</code>s
   * (the <code>componentHierarchyList</code> parameter). If the given <code>UIComponent</code> is nested in a 
   * <code>Facet</code> of a <code>UIComponent</code>, we store the name of the actual <code>facet</code> in
   * the list. If it is a regular child, we store its position/index. 
   * 
   * <p> 
   * To calculate the <code>scopedID</code> we add the ID of every <code>NamingContainer</code> that we hit,
   * while walking up the component tree, to the <code>scopedIdList</code>. 
   * 
   * @param uicomponent The <code>UIComponent</code> for this current iteration
   * @param scopedId , or null if neeed to build it
   * 
   * @see ComponentReference#newUIComponentReference()
   */
  private String _createComponentPathAndScopedId(UIComponent component, String scopedId)
  {
    // setUp of list that stores information about the FACET name or the COMPONENT index
    List<Object> componentHierarchyList = new ArrayList<Object>();

    // setUp of list that contains the IDs for all NamingContainers
    // on the way up to the <code>UIViewRoot</code>
    List<String> scopedIdList = null;

    // extract the component Id and
    // start to calculate its length... IF needed...
    boolean needToCalculateScopedId = (scopedId == null);

    int scopedIdLength = 0;
    if (scopedId == null)
    {
      // store the id of the component as a transient field since we can grab it from the scoped id
      // but want it available to validate the component we found from the path
      String currCompId = component.getId();

      if (currCompId == null)
      {
        //TODO i18n
        throw new IllegalArgumentException("The UIComponent needs to have an ID");
      }
      else
      {
        _componentId = currCompId;
      }

      scopedIdList = new ArrayList<String>();
      scopedIdLength += _componentId.length();
      // add the ID of the given UIComponent to (the beginning of) the list
      scopedIdList.add(_componentId);
    }
    else
    {
      // extract componentId from scopedId
      _componentId = _calculateComponentIdFromScopedId(scopedId);
    }


    // stash the component and parent , for the loop
    UIComponent currComponent = component;
    UIComponent currParent = currComponent.getParent();

    // enter the loop, if there is a parent for the current component
    while(currParent != null)
    {

      // When current parent is a NamingContainer we want to scope the ID
      // but ONLY if that has not been done (needToCalculateScopedId:true)
      if (needToCalculateScopedId && (currParent instanceof NamingContainer))
      {
        // extract the parent-component Id and
        // continue to calculate the final length of the scopedId
        // the "+1" takes the SEPARATOR_CHAR into account
        String namingContainerId = currParent.getId();
        scopedIdLength += namingContainerId.length() + 1;  
        // as we hit a NamingContainer we add its ID to
        // the beginning of the "scopedId" list
        scopedIdList.add(namingContainerId);
      }
      // end of scopedID business, for this call;


      // is the given component a child of the parent?
      if (currParent.getChildren().contains(currComponent))
      {          
        // if so, add the INDEX (type: int) at the beginning of the list
        componentHierarchyList.add(currParent.getChildren().indexOf(currComponent));
      }
      else
      {
        // If the component is not a child, it must be a facet.
        // When the component is nested in a facet, we need to find
        // the name of the embedding FACET
        Set<Map.Entry<String, UIComponent>> entries = currParent.getFacets().entrySet();
        for(Map.Entry<String, UIComponent> entry : entries)
        {
          if (currComponent.equals(entry.getValue()))
          {
            // once we identified the actual component/facet,
            // we store the name (type: String)at the
            // beginning of the list and quite the loop afterwards
            componentHierarchyList.add(entry.getKey());
            break;
          }
        }
      }

      // set references for the next round of the loop
      currComponent = currParent;
      currParent = currParent.getParent();
    }

    // done with the loop as >currComponent< has no own parent. Which
    // means we must talk to <code>UIViewRoot</code> here.
    // Otherwise the component is not connected to the tree,
    // so we thrown an IllegalArgumentException
    if (!(currComponent instanceof UIViewRoot))
    {
      //TODO i18n
      throw new IllegalArgumentException("The component " + component + " is NOT connected to the component tree");
    }

    // once we are done with loop to the UIViewRoot, the "componentHierarchyList" as our "component path"
    _componentPath = componentHierarchyList;
    
    // trigger the generation of the final scopedID
    // OR return NULL (as we already have the scopedId
    // when "needToCalculateScopedId" is false...
    if (needToCalculateScopedId)
    {
      scopedId = _createScopedId(scopedIdLength, scopedIdList);
    }

    return scopedId;
  }

  /**
   * Starts to walk down the component tree by the given <code>UIViewRoot</code>. It
   * uses the <code>hierarchyInformationList</code> to check if the it needs to
   * walk into a FACET or an INDEX of the component.
   * 
   * @see ComponentReference#_createComponentPathAndScopedId(UIComponent)
   */
  private UIComponent _walkPathToComponent(UIViewRoot root, List<Object> hierarchyInformationList)
  {
    UIComponent currFound = root;

    // iterate backwards since we appending the items starting from the component
    for (int i = hierarchyInformationList.size() - 1; i >= 0 ; i--)
    {
      Object location = hierarchyInformationList.get(i);

      // integer means we need to get the kid at INDEX obj
      // but let's not try to lookup from a component with
      // no kids
      if (location instanceof Integer)
      {
        int childIndex = ((Integer)location).intValue();
        
        List<UIComponent> children = currFound.getChildren();

        // make sure there is actually a child at this index
        if (childIndex < children.size())
        {
          currFound = children.get(childIndex);
        }
        else
        {
          // something changed, there aren't enough children so give up
          return null;
        }
      }
      else
      {
        // there is only ONE child per facet! So get the
        // component of FACET "obj"
        String facetName = location.toString();

        currFound = currFound.getFacets().get(facetName);

        // component isn't under the same facet anymore, so give up
        if (currFound == null)
          return null;
      }
    }
    return currFound;
  }

  private transient List<Object> _componentPath;
  private transient String _componentId;
  private final String _scopedId;
  private static final long serialVersionUID = -6803949269368863899L;
}