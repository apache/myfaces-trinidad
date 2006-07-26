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
package org.apache.myfaces.trinidadinternal.application;

import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;

import org.apache.myfaces.trinidad.component.UIXComponentBase;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

/**
 * Utility class for storing the structure of a UIComponent tree.
 * <p>
 * @author The Oracle ADF Faces Team
 */
final class Structure implements Externalizable
{
  /**
   * Zero-arg constructor for Externalizable contract.
   */
  public Structure()
  {
  }


  /**
   * Create the structure of an existing component.
   */
  public Structure(UIComponent component)
  {
    _class = component.getClass().getName();
    _id = component.getId();
    _facets = _getFacets(component);
    _children = _getChildren(component);
  }

  /**
   * Re-create a component from a structure object
   */
  public UIComponent createComponent()
    throws ClassNotFoundException, InstantiationException,
           IllegalAccessException
  {
    Class clazz = ClassLoaderUtils.loadClass(_class);
    UIComponent component = (UIComponent) clazz.newInstance();
    if (_id != null)
      component.setId(_id);
    // Create any facets
    if (_facets != null)
    {
      Map facets = component.getFacets();
      for (int i = 0 ; i < _facets.size(); i += 2)
      {
        UIComponent facet = ((Structure) _facets.get(i + 1)).
                                 createComponent();
        facets.put(_facets.get(i), facet);
      }
    }

    // Create any children
    if (_children != null)
    {
      List children = component.getChildren();
      for (int i = 0 ; i < _children.size(); i++)
      {
        UIComponent child = ((Structure) _children.get(i)).createComponent();
        children.add(child);
      }
    }

    return component;
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(_abbreviateClass(_class));
    out.writeObject(_id);

    // Write the facets
    if (_facets == null)
    {
      out.writeShort(0);
    }
    else
    {
      out.writeShort(_facets.size());
      for (int i = 0; i < _facets.size(); i += 2)
      {
        out.writeObject(_facets.get(i));
        ((Structure) _facets.get(i + 1)).writeExternal(out);
      }
    }

    // Write the children
    if (_children == null)
    {
      out.writeShort(0);
    }
    else
    {
      out.writeShort(_children.size());
      for (int i = 0; i < _children.size(); i++)
      {
        ((Structure) _children.get(i)).writeExternal(out);
      }
    }
  }

  public void readExternal(ObjectInput in)
   throws IOException, ClassNotFoundException
  {
    _class = _unabbreviateClass((String) in.readObject());
    _id = (String) in.readObject();

    // Read the facets
    short facetCount = in.readShort();
    if (facetCount > 0)
    {
      _facets = new ArrayList(facetCount);
      for (int i = 0; i < facetCount; i += 2)
      {
        _facets.add(in.readObject());
        Structure newStruct = new Structure();
        newStruct.readExternal(in);
        _facets.add(newStruct);
      }
    }

    // Read the children
    short childCount = in.readShort();
    if (childCount > 0)
    {
      _children = new ArrayList(childCount);
      for (int i = 0; i < childCount; i++)
      {
        Structure newStruct = new Structure();
        newStruct.readExternal(in);
        _children.add(newStruct);
      }
    }
  }

  /**
   * Store the structure of all the children.
   */
  private List _getChildren(UIComponent component)
  {
    if (component.getChildCount() == 0)
      return null;

    List children = component.getChildren();
    ArrayList list = new ArrayList(children.size());

    Iterator childIterator = children.iterator();
    while (childIterator.hasNext())
    {
      UIComponent child = (UIComponent) childIterator.next();
      if ((child != null) && !child.isTransient())
      {
        list.add(new Structure(child));
      }
    }

    if (list.isEmpty())
      return null;

    return list;
  }


  /**
   * Store the structure of all the facets.
   */
  private List _getFacets(UIComponent component)
  {
    Iterator facetNames;
    if (component instanceof UIXComponentBase)
    {
      facetNames = ((UIXComponentBase) component).getFacetNames();
    }
    else
    {
      facetNames = component.getFacets().keySet().iterator();
    }

    if (!facetNames.hasNext())
      return null;

    Map facets = component.getFacets();
    ArrayList list = new ArrayList(facets.size() * 2);
    while (facetNames.hasNext())
    {
      Object name = facetNames.next();
      UIComponent facet = (UIComponent) facets.get(name);
      if ((facet != null) && !facet.isTransient())
      {
        list.add(name);
        list.add(new Structure(facet));
      }
    }

    if (list.isEmpty())
      return null;

    return list;
  }

  // Trim down a class name for any very common package prefixes
  static private String _abbreviateClass(String clazz)
  {
    if (clazz.startsWith(_COMMON_CLASS_PREFIX))
      clazz = clazz.substring(_COMMON_CLASS_PREFIX_LENGTH);
    return clazz;
  }

  // Reverse of _abbreviateClass()
  static private String _unabbreviateClass(String clazz)
  {
    if (clazz.charAt(0) == '.')
      return _COMMON_CLASS_PREFIX + clazz;
    return clazz;
  }

  private String _class;
  private String _id;
  private List _facets;
  private List _children;


  static private final String _COMMON_CLASS_PREFIX =
    "org.apache.myfaces.trinidad.component";
  static private final int _COMMON_CLASS_PREFIX_LENGTH =
    _COMMON_CLASS_PREFIX.length();
}

