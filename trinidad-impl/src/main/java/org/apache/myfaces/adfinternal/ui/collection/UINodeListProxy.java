/*
 * Copyright  2000-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.ui.collection;

import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.UINode;

/**
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/collection/UINodeListProxy.java#0 $) $Date: 10-nov-2005.18:57:37 $
 * @author The Oracle ADF Faces Team
 */
public abstract class UINodeListProxy implements UINodeList
{
  protected abstract UINodeList getUINodeList(RenderingContext context);
  
  public int size(
    RenderingContext context
    )
  {
    UINodeList nodeList = getUINodeList(context);
    
    if (nodeList != null)
    {
      return nodeList.size(context);
    }
    else
    {
      return 0;
    }
  }
  
  public UINode getUINode(
    RenderingContext context,
    int              index
    )
  {
    UINodeList nodeList = getUINodeList(context);
    
    if (nodeList != null)
    {
      return nodeList.getUINode(context, index);
    }
    else
    {
      return null;
    }
  }
  
  
  public UINode setUINode(
    int    index,
    UINode node
    )
  {
    UINodeList nodeList = getUINodeList(null);
    
    if (nodeList != null)
    {
      return nodeList.setUINode(index, node);
    }
    else
    {
      return null;
    }
  }
  
  public void addUINode(
    int    index,
    UINode node
    )
  {
    UINodeList nodeList = getUINodeList(null);
    
    if (nodeList != null)
    {
      nodeList.addUINode(index, node);
    }
  }
  
  public void addUINode(
    UINode node
    )
  {
    UINodeList nodeList = getUINodeList(null);
    
    if (nodeList != null)
    {
      nodeList.addUINode(node);
    }
  }
  
  public UINode removeUINode(
    int index
    )
  {
    UINodeList nodeList = getUINodeList(null);
    
    if (nodeList != null)
    {
      return nodeList.removeUINode(index);
    }
    else
    {
      return null;
    }
  }
  
  public void clearUINodes()
  {
    UINodeList nodeList = getUINodeList(null);
    
    if (nodeList != null)
    {
      nodeList.clearUINodes();
    }
  }
  
  public Object clone()
  {
    try
    {
      return super.clone();
    }
    catch (CloneNotSupportedException cnse)
    {
      // this should never happen
      throw new InternalError();
    }
  }
} 
