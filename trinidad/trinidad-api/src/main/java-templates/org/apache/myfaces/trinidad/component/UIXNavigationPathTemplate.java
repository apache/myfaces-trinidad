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
package org.apache.myfaces.trinidad.component;

import java.util.ArrayList;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseId;


/**
 * Base class for the NavigationPath component.
 *
 * @version $Name:  $ ($Revision$) $Date$
 * @author The Oracle ADF Faces Team
 */
abstract public class UIXNavigationPathTemplate extends UIXNavigationHierarchy
{
	
  @Override
  protected void processFacetsAndChildren(
    FacesContext context,
    PhaseId phaseId)
  {
    Object oldPath = getRowKey();
    
    Object focusPath = getFocusRowKey();
    
    if (focusPath != null )
    {
      List<Object> paths = 
        new ArrayList<Object>(getAllAncestorContainerRowKeys(focusPath));
      
      paths.add(focusPath);
      int focusPathSize = paths.size();
      UIComponent nodeStamp = getFacet("nodeStamp");
      
      if (nodeStamp != null)
      {
        for (int i = 0; i < focusPathSize; i++)
        {
          setRowKey(paths.get(i));
          processComponent(context, nodeStamp, phaseId);
        }
      }
    }
    
    setRowKey(oldPath);
    
    // process the children
    TableUtils.__processChildren(context, this, phaseId);
  }  

    
}
