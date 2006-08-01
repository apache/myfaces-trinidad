/*
 * Copyright  2005,2006 The Apache Software Foundation.
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

package org.apache.myfaces.trinidad.change;

import java.util.List;

import javax.faces.component.UIComponent;

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

/**
 * Change specialization for removal of a child.
 * While applying this Change, if there were to be a child with the specified
 *  identifier, it will be removed.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/change/RemoveChildComponentChange.java#0 $) $Date: 10-nov-2005.19:10:00 $
 * @author The Oracle ADF Faces Team
 */
public class RemoveChildComponentChange extends ComponentChange
                                         implements DocumentChange
{
  /**
   * Constructs a RemoveChildChange with the specified identifier of the child.
   * @param childId The identifier of the child component that needs to be 
   *         removed.
   * @throws IllegalArgumentException if specified childId were to be null.
   */
  public RemoveChildComponentChange(String childId)
  {
    if ((childId == null) || (childId.length() == 0))
      throw new IllegalArgumentException(
        "Cannot construct a RemoveChildChange with null childId.");
    _childId = childId;
  }
  
  /**
   * Returns the identifier of child component that needs to be removed.
   */
  public String getChildId()
  {
    return _childId;
  }
  
  /**
   * {@inheritDoc}
   */
  @SuppressWarnings("unchecked")
  @Override
  public void changeComponent(UIComponent uiComponent)
  {
    if (uiComponent.getChildCount() == 0)
      return;
      
    List<UIComponent> children = uiComponent.getChildren();
    children.remove(ChangeUtils.getChildForId(uiComponent, _childId));
  }

  /**
   * {@inheritDoc}
   */
  public void changeDocument(Node componentNode)
  {
    Node currChild = componentNode.getFirstChild();
    
    while (currChild != null)
    {
      NamedNodeMap attributes = currChild.getAttributes();
      
      if (attributes != null)
      {
        Node idAttr = attributes.getNamedItem("id");
        
        if (idAttr != null)
        {
          if (_childId.equals(idAttr.getNodeValue()))
          {
            currChild.getParentNode().removeChild(currChild);
            break;
          }
        }
      }
      
      currChild = currChild.getNextSibling();
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

  private final String _childId;
}
