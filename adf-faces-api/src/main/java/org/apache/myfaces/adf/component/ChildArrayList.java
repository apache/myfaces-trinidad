/*
 * Copyright  2003-2006 The Apache Software Foundation.
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

import javax.faces.component.UIComponent;

import java.util.Collection;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


/**
 * List for storing children.
 *
 * @author The Oracle ADF Faces Team
 */
class ChildArrayList extends ArrayList
{
  public ChildArrayList(UIComponent parent)
  {
    _parent = parent;
  }


  public void add(int index, Object element)
  {
    if (element == null)
      throw new NullPointerException();

    if ((index < 0) || (index > size()))
      throw new IndexOutOfBoundsException("index:"+index+" size:"+size());

    UIComponent child = (UIComponent) element;
    if (child.getParent() != null)
    {
      index = __removeFromParent(child, index);
    }

    child.setParent(_parent);
    super.add(index, child);
  }

  
  public boolean add(Object element)
  {
    add(size(), element);
    return true;
  }
  
  
  public boolean addAll(Collection collection)
  {
    return addAll(size(), collection);
  }

  public boolean addAll(int index, Collection collection)
  {
    Iterator elements = collection.iterator();
    boolean changed = false;
    while (elements.hasNext())
    {
      UIComponent element = (UIComponent) elements.next();
      if (element == null)
        throw new NullPointerException();

      add(index++, element);
      changed = true;
    }
    
    return changed;
  }

  public Object remove(int index)
  {
    UIComponent child = (UIComponent) get(index);
    super.remove(index);
    child.setParent(null);

    return child;
  }

  public boolean remove(Object element)
  {
    if (element == null)
      throw new NullPointerException();
    
    if (!(element instanceof UIComponent))
      return false;
  
    if (super.remove(element))
    {
      UIComponent child = (UIComponent) element;
      child.setParent(null);
      return true;
    }

    return false;
  }
  
  public boolean removeAll(Collection collection)
  {
    boolean result = false;
    Iterator elements = collection.iterator();
    while (elements.hasNext())
    {
      if (remove(elements.next()))
        result = true;
    }
    
    return result;
  }

  public Object set(int index, Object element)
  {
    if (element == null)
      throw new NullPointerException();
    
    if (!(element instanceof UIComponent))
      throw new ClassCastException();
    
    if ((index < 0) || (index >= size()))
      throw new IndexOutOfBoundsException();

    UIComponent child = (UIComponent) element;
    UIComponent previous = (UIComponent) get(index);

    previous.setParent(null);
    
    child.setParent(_parent);
    super.set(index, element);
    return previous;
  }

  static int __removeFromParent(
    UIComponent component,
    int index)
  {
    UIComponent parent = component.getParent();
    assert(parent != null);

    if (parent.getChildCount() > 0)
    {
      List children = parent.getChildren();
      int size = children.size();
      for  (int i = 0; i < size; i++)
      {
        if  (children.get(i) == component)
        {
          children.remove(i);
          if (index > i)
            index--;
          return index;
        }
      }
    }
    
    Collection facets = parent.getFacets().values();
    if (facets.contains(component))
    {
      facets.remove(component);
      return index;
    }

    // Not good - the child thought it was in a parent,
    // but it wasn't.
    assert(false);
    return index;
  }


  private final UIComponent _parent;
}
