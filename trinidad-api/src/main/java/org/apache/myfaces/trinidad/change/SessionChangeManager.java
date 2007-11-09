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
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.faces.context.FacesContext;

import org.w3c.dom.Document;

/**
 * A ChangeManager implementation that manages
 *  persisting the added Changes at the session. This means
 *  the lifetime of Changes added such is within the session scope.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/change/SessionChangeManager.java#0 $) $Date: 10-nov-2005.19:06:35 $
 */
public class SessionChangeManager extends BaseChangeManager
{
  /**
   * The Map used to store the Changes.  The Map is stored as
   * key=ComponentCompositeId, value=ChangesList (List)
   * 
   * @param facesContext FacesContext for request
   * @param viewId viewID for request
   * @param createIfNecessary <code>true</code> if Map should be created if not
   *        already present
   * @return Synchronized Map of componentID tokens to Lists of Changes
   */
  @SuppressWarnings("unchecked")
  @Override
  protected Map<String, List<ComponentChange>> getComponentToChangesMapForView(
    FacesContext facesContext,
    String viewId,
    boolean createIfNecessary)
  {
    Map<String, Object> sessMap = facesContext.getExternalContext().getSessionMap();
    //pu: Get datastructure #1 described at the end of this file.
    Map<String, Map<String, List<ComponentChange>>> viewToChangesMap = 
      (Map<String, Map<String, List<ComponentChange>>>)sessMap.get(_CHANGE_KEY);
    if (viewToChangesMap == null)
    {
      if (!createIfNecessary)
        return null;
      viewToChangesMap = new ConcurrentHashMap<String, Map<String, List<ComponentChange>>>();
      sessMap.put(_CHANGE_KEY, viewToChangesMap);
    }
    
    //pu: Get datastructure #2 described at the end of this file.
    Map<String, List<ComponentChange>> componentToChangesMap = viewToChangesMap.get(viewId);
    if (componentToChangesMap == null)
    {
      if (!createIfNecessary)
        return null;
      componentToChangesMap = new ConcurrentHashMap<String, List<ComponentChange>>();
      viewToChangesMap.put(viewId, componentToChangesMap);
    }
    return componentToChangesMap;
  }

  /** 
   * We don't support DocumentAspect persistence
   */
  @Override
  protected Document getDocument(FacesContext context)
  {
    return null;
  }

  
  static private final String _CHANGE_KEY =
    "org.apache.myfaces.trinidadinternal.Change";
    
  //pu: DataStructure:
  //  1. Session holds a Map instance named by _CHANGE_KEY 
  //      [ key=viewId(String), value=ComponenChangesMap(Map) ]
  //  2. ComponenChangesMap is a Map 
  //      [ key=ComponentCompositeId, value=ChangesList (List) ]
  //  3. ChangesList is a List of Changes for the given component.
}
