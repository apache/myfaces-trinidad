/*
 * Copyright  2004-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.adf.component;

import java.util.List;

import javax.faces.context.FacesContext;
import javax.faces.event.PhaseId;


/**
 * Base class for the NavigationLevel component.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java-templates/oracle/adf/view/faces/component/UIXNavigationLevelTemplate.java#0 $) $Date: 10-nov-2005.19:07:37 $
 * @author The Oracle ADF Faces Team
 */
abstract public class UIXNavigationLevelTemplate extends UIXNavigaitonHierarchy
{
/**/ // Abstract methods implemented by code gen
/**/  abstract public int getLevel();


  protected void processFacetsAndChildren(
    FacesContext context,
    PhaseId phaseId)
  {
    Object oldPath = getRowKey();
    HierarchyUtils.__setStartDepthPath(this, getLevel());
    // process stamp for one level
    HierarchyUtils.__processLevel(context, phaseId, this);
    setRowKey(oldPath);


    // process the children
    TableUtils.__processChildren(context, this, phaseId);
  }





}
