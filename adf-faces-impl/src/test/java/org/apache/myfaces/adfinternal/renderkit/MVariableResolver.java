/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.adfinternal.renderkit;

import java.awt.Color;

import java.beans.IntrospectionException;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.faces.context.FacesContext;
import javax.faces.el.VariableResolver;

import org.apache.myfaces.adf.logging.ADFLogger;
import org.apache.myfaces.adf.model.ChildPropertyTreeModel;
import org.apache.myfaces.adf.model.DefaultBoundedRangeModel;
import org.apache.myfaces.adf.model.ProcessMenuModel;
import org.apache.myfaces.adf.model.RowKeySet;
import org.apache.myfaces.adf.model.RowKeySetImpl;
import org.apache.myfaces.adf.model.RowKeySetTreeImpl;
import org.apache.myfaces.adf.model.TreeModel;

import org.apache.myfaces.adfinternal.renderkit.testData.Person;

public class MVariableResolver extends VariableResolver
{
  public Object resolveVariable(FacesContext context, String name)
  {
    Object o =  context.getExternalContext().getRequestMap().get(name);
    TreeModel modelTree = null;
    if (o != null)
      return o;

    if ("pageList".equals(name))
    {
      try
      {
        if (_pageList == null)
        {
          List al = _createPageList();
          _pageList = new MenuModelImpl(al, "viewId", "/1.jspx");
        }

        return _pageList;
      }
      catch(IntrospectionException e)
      {
        _LOG.severe(e);
        return null;
      }
    }
    else if ("menu".equals(name))
    {
      try
      {
        if (_menu == null)
        {
          List al = _createPageList();
          TreeModel treeModel = new ChildPropertyTreeModel(al, "children");
          _menu = new MenuModelImpl(treeModel, "viewId", "/4.jspx");
        }

        return _menu;
      }
      catch(IntrospectionException e)
      {
        _LOG.severe(e);
        return null;
      }
    }
    else if ("navigationpath".equals(name))
    {
      try
      {
        if (_navigationpath == null)
        {
          List al = _createPageList();
          TreeModel treeModel = new ChildPropertyTreeModel(al, "children");
          _navigationpath = new MenuModelImpl(treeModel, "viewId", "/7.jspx");
        }

        return _navigationpath;
      }
      catch(IntrospectionException e)
      {
        _LOG.severe(e);
        return null;
      }
    }
    else if ("simpleList".equals(name))
    {
      if (_simpleList == null)
      {
        _simpleList = new ArrayList();
        _simpleList.add(_createHashMap("First", 1));
        _simpleList.add(_createHashMap("Second", 2));
        _simpleList.add(_createHashMap("Three", 3));
        _simpleList.add(_createHashMap("Four", 4));
        _simpleList.add(_createHashMap("Five", 5));
        _simpleList.add(_createHashMap("Six", 6));
      }

      return _simpleList;
    }
    else if("iteratorList".equals(name))
    {
      if (_iteratorList == null)
      {
        _iteratorList = new ArrayList();
        _iteratorList.add(new HashMap().put("data","One"));
        _iteratorList.add(new HashMap().put("data","Two"));
        _iteratorList.add(new HashMap().put("data","Three"));
        _iteratorList.add(new HashMap().put("data","Four"));
        _iteratorList.add(new HashMap().put("data","Five"));
      }

      return _iteratorList;
    }
    else if("colorList".equals(name))
    {
      ArrayList colorList = new ArrayList();
      colorList.add(new Color(255, 0, 0));
      colorList.add(new Color(0, 0, 255));
      colorList.add(new Color(255, 255, 0));
      colorList.add(new Color(0, 255, 0));
      return colorList;
    }
    else if("treeModel".equals(name))
    {
      if (_treeModel == null)
      {
        Person john = new Person("John Smith");
        Person kim = new Person("Kim Smith");
        Person tom = new Person("Tom Smith");
        Person zoe = new Person("Zoe Smith");
        Person ira = new Person("Ira Wickrememsinghe");
        Person mallika = new Person("Mallika Wickremesinghe");

        john.getKids().add(kim);
        john.getKids().add(tom);
        tom.getKids().add(zoe);
        ira.getKids().add(mallika);

        List people = new ArrayList();
        people.add(john);
        people.add(ira);

        _treeModel = new ChildPropertyTreeModel(people, "kids");
      }

      return _treeModel;
    }
    else if("pathSet".equals(name))
    {
      if (_treeState == null)
      {
        _treeState = new RowKeySetTreeImpl(true);

      }
      return _treeState;
    }
    else if("disclosureState".equals(name))
    {
      if(_disclosureState == null)
        _disclosureState = new RowKeySetImpl(true);
      return _disclosureState;
    }
    else if("arrayString".equals(name))
    {
      String stringArray[] = {"id1","id2","id3"};
      return stringArray;
    }
    else if("rangeModel".equals(name))
    {
      return new DefaultBoundedRangeModel(2,4);
    }
    else if ("bigList".equals(name))
    {
      return new BigList();
    }
    else if("rangeChoiceBarModel".equals(name))
    {
      return new RangeChoiceBarModelImpl();
    }
    else if ("oldDate".equals(name))
    {
      return new Date(70, 5, 10);
    }
    else if ("midDate".equals(name))
    {
      return new Date(105, 6, 27);
    }
    else if ("newDate".equals(name))
    {
      return new Date(130, 0, 5);
    }

    return null;
  }

  public class MenuModelImpl extends ProcessMenuModel
  {
    public MenuModelImpl(
      Object instance,
      String viewIdProperty,
      String currentViewId
    )throws IntrospectionException
    {
      super(instance, viewIdProperty, null);
      _currentViewId = currentViewId;
    }

    protected String getCurrentViewId()
    {

      return _currentViewId;
    }

    private String _currentViewId;
  }

  static private Map _createHashMap(String s, int i)
  {
    HashMap m = new HashMap();
    m.put("string", s);
    m.put("int", new Integer(i));
    return m;
  }

  public static class PageImpl
  {
    public PageImpl(String viewId, String label, boolean disabled)
    {
      _viewId = viewId;
      _label = label;
      _disabled = disabled;
    }

    public void setViewId(String viewId)
    {
      _viewId = viewId;
    }

    public void setLabel(String label)
    {
      _label = label;
    }

    public void setDisabled(boolean disabled)
    {
      _disabled = disabled;
    }


    public void setChildren(List children)
    {
      _children = children;
    }


    public String getViewId()
    {
      return _viewId;
    }


    public String getLabel()
    {
      return _label;
    }


    public boolean isDisabled()
    {
      return _disabled;
    }

    public List getChildren()
    {
      return _children;
    }

    private String _viewId;
    private String _label;
    private boolean _disabled;
    private List _children;
  }

  public static class BigList extends AbstractList
  {
    public int size()
    {
      return 10000;
    }

    public Object get(int i)
    {
      return new Integer(i);
    }
  }

  private static List _createPageList()
  {
    ArrayList al = new ArrayList();
    PageImpl page1 = new PageImpl("/1.jspx", "First", false);

    al.add(page1);
    al.add(new PageImpl("/2.jspx", "Second", false));
    al.add(new PageImpl("/3.jspx", "Third", false));

    PageImpl page5 = new PageImpl("/5.jspx", "fifth", false);
    ArrayList p1 = new ArrayList();
    p1.add(new PageImpl("/4.jspx", "fourth", false));
    p1.add(page5);

    ArrayList p2 = new ArrayList();
    p2.add(new PageImpl("/6.jspx", "sixth", false));
    p2.add(new PageImpl("/7.jspx", "seventh", false));

    page1.setChildren(p1);
    page5.setChildren(p2);

    return al;
  }

  private static class RangeChoiceBarModelImpl
  {
    private List _names;
    private int _start;
    private int _end;

    public RangeChoiceBarModelImpl()
    {
      _names = new ArrayList();
      _names.add("vox");
      _names.add("populi");
      _names.add("en");
      _names.add("vogue");
    }
    public void setNames(List names)
    {
      this._names = names;
    }

    public List getNames()
    {
      return _names;
    }

    public void setStart(int start)
    {
      this._start = start;
    }

    public int getStart()
    {
      return _start;
    }

    public void setEnd(int end)
    {
      this._end = end;
    }

    public int getEnd()
    {
      return _end;
    }
  }

  private MenuModelImpl _pageList;
  private MenuModelImpl _menu;
  private MenuModelImpl _navigationpath;

  private List _iteratorList;
  private List _simpleList;

  private ChildPropertyTreeModel _treeModel;

  private RowKeySet _treeState = null;
  private RowKeySet _disclosureState = null;

  private static final ADFLogger _LOG =
    ADFLogger.createADFLogger(MVariableResolver.class);
}
