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
package org.apache.myfaces.trinidaddemo;

import java.beans.BeanInfo;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;

import java.lang.reflect.Method;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import javax.faces.component.UIComponent;

import org.apache.myfaces.trinidad.context.RequestContext;

public class ComponentEditorHandler
{
  public String update()
  {
    boolean rendered = _editedComponent.isRendered();

    List<PropertyOfComponent> list = _list;
    if (list != null)
    {
      for(PropertyOfComponent prop : list)
      {
        prop.flushToComponent();
      }
    }

    RequestContext rc = RequestContext.getCurrentInstance();
    
    // If we toggled rendered, we'd better toggle the parent
    if (rendered != _editedComponent.isRendered())
      rc.addPartialTarget(_editedComponent.getParent());
    else
      rc.addPartialTarget(_editedComponent);

    return null;
  }

  public void setComponent(UIComponent component)
  {
    _editedComponent = component;
  }

  public UIComponent getComponent()
  {
    return _editedComponent;
  }

  public boolean isJavascriptShown()
  {
    return _javascriptShown;
  }

  public void setJavascriptShown(boolean javascriptShown)
  {
    _javascriptShown = javascriptShown;
  }

  public List<PropertyOfComponent> getAttributes()
  {
    if (_list != null)
      return _list;

    UIComponent comp = getComponent();
    if (comp == null)
      return null;

    List<PropertyOfComponent> list = new ArrayList<PropertyOfComponent>();
    try
    {
      BeanInfo beanInfo = Introspector.getBeanInfo(comp.getClass());
      PropertyDescriptor[] descriptors = beanInfo.getPropertyDescriptors();
      for (int i = 0; i < descriptors.length; i++)
      {
        PropertyDescriptor descriptor = descriptors[i];
        // "Write-only" properties - no go
        if (descriptor.getReadMethod() == null)
          continue;

        PropertyOfComponent poc = null;

        boolean readOnly = descriptor.getWriteMethod() == null;
        if (readOnly)
          continue;

        // For now, skip any attributes with ValueBindings
        String name = descriptor.getName();
        if (comp.getValueBinding(name) != null)
          continue;
                        

        Class<?> type = descriptor.getPropertyType();
        if ((type == String.class) ||
            (type == Object.class))
        {
          if (!isJavascriptShown() &&
              name.startsWith("on"))
            continue;

          poc = new StringProperty(comp, descriptor);
        }
        else if ((type == Integer.class) ||
                 (type == Integer.TYPE))
        {
          poc = new IntegerProperty(comp, descriptor);
        }
        else if ((type == Boolean.class) ||
                 (type == Boolean.TYPE))
        {
          poc = new BooleanProperty(comp, descriptor);
        }
        else if (type == Date.class)
        {
          poc = new DateProperty(comp, descriptor);
        }

        if (poc != null)
          list.add(poc);
      }

      // Sort the list by property name
      Collections.sort(list);

      _list = list;

      return list;
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }

    return null;
  }

  static public class PropertyOfComponent implements Comparable<PropertyOfComponent>
  {
    public PropertyOfComponent(UIComponent component,
                               PropertyDescriptor descriptor)
    {
      _component = component;
      _descriptor = descriptor;
    }

    // Sort by the name of the property
    public int compareTo(PropertyOfComponent poc)
    {
      return getName().compareTo(poc.getName());
    }

    public String getName()
    {
      return _descriptor.getName();
    }

    public String getType()
    {
      return null;
    }

    @SuppressWarnings("unchecked")
    public void flushToComponent()
    {
      if (_valueSet)
        _component.getAttributes().put(getName(), _value);
    }

    protected Object getBeanProperty()
    {
      Method method = _descriptor.getReadMethod();
      try
      {
        return method.invoke(_component, (Object[])null);
      }
      catch (Exception e)
      {
        e.printStackTrace();
      }

      return getProperty();
    }


    protected Object getProperty()
    {
      return _component.getAttributes().get(getName());
    }

    protected void setProperty(Object value)
    {
      if ("".equals(value))
        value = null;

      _valueSet = true;
      _value = value;
    }

    private boolean _valueSet = false;
    private Object  _value    = null;
    private final PropertyDescriptor _descriptor;
    private final UIComponent        _component;
  }

  static public class IntegerProperty extends PropertyOfComponent
  {
    public IntegerProperty(UIComponent component, PropertyDescriptor descriptor)
    {
      super(component, descriptor);
    }

    public Integer getValue()
    {
      return (Integer) getProperty();
    }

    public void setValue(Integer i)
    {
      setProperty(i);
    }

    @Override
    public String getType()
    {
      return "integer";
    }
  }


  static public class DateProperty extends PropertyOfComponent
  {
    public DateProperty(UIComponent component, PropertyDescriptor descriptor)
    {
      super(component, descriptor);
    }

    public Date getValue()
    {
      return (Date) getProperty();
    }

    public void setValue(Date i)
    {
      setProperty(i);
    }

    @Override
    public String getType()
    {
      return "date";
    }
  }


  static public class StringProperty extends PropertyOfComponent
  {
    public StringProperty(UIComponent component, PropertyDescriptor descriptor)
    {
      super(component, descriptor);
    }

    public String getValue()
    {
      Object o = getProperty();
      if (o == null)
        return null;
      return o.toString();
    }

    public void setValue(String s)
    {
      setProperty(s);
    }

    @Override
    public String getType()
    {
      return "string";
    }
  }



  static public class BooleanProperty extends PropertyOfComponent
  {
    public BooleanProperty(UIComponent component, PropertyDescriptor descriptor)
    {
      super(component, descriptor);
    }

    public Boolean getValue()
    {
      return (Boolean) getBeanProperty();
    }

    public void setValue(Boolean b)
    {
      setProperty(b);
    }

    @Override
    public String getType()
    {
      return "boolean";
    }
  }


  private UIComponent               _editedComponent;
  private boolean                   _javascriptShown = true;
  private List<PropertyOfComponent> _list;
}
