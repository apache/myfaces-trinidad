/*
 * Copyright  2001-2006 The Apache Software Foundation.
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
 * Unmodifiable UINodeList that merges the results of two other node lists.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/collection/UnmodifiableCompoundNodeList.java#0 $) $Date: 10-nov-2005.18:57:38 $
 * @author The Oracle ADF Faces Team
 */
public class UnmodifiableCompoundNodeList implements UINodeList
{
  public UnmodifiableCompoundNodeList(
    UINodeList first,
    UINodeList second
    )
  {
    _first  = first;
    _second = second;
  }
  
  public int size(
    RenderingContext context
    )
  {
    int total = 0;
    
    if (_first != null)
      total = _first.size(context);
    
    if (_second != null)
      return total += _second.size(context);
    
    return total;
  }
  
  
  public UINode getUINode(
    RenderingContext context,
    int              index
    )
  {
    if (_first != null)
    {
      int firstSize = _first.size(context);
      
      if (index < firstSize)
        return _first.getUINode(context, index);
        
      index -= firstSize;
    }
    
    if (_second != null)
    {
      return _second.getUINode(context, index);
    }
    else
    {
      throw new IndexOutOfBoundsException();
    }
  }
  
  public UINode setUINode(
    int    index,
    UINode node
    )
  {
    throw new UnsupportedOperationException(
      "It is illegal to set children on an UnmodifiableCompoundNodeList");
  }
  
  
  public void addUINode(
    int    index,
    UINode node
    )
  {
    throw new UnsupportedOperationException(
      "It is illegal to add children to an UnmodifiableCompoundNodeList");
  }
  
  
  public void addUINode(
    UINode node
    )
  {
    throw new UnsupportedOperationException(
      "It is illegal to add children to an UnmodifiableCompoundNodeList");
  }
  
  public UINode removeUINode(
    int index
    )
  {
    throw new UnsupportedOperationException(
      "It is illegal to remove children from an UnmodifiableCompoundNodeList");
  }
  
  public void clearUINodes()
  {
    throw new UnsupportedOperationException(
      "It is illegal to clear children from an UnmodifiableCompoundNodeList");
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
  
  private UINodeList _first;
  private UINodeList _second;
}
