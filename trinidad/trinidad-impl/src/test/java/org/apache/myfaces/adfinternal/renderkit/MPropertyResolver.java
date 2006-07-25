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

import java.beans.BeanInfo;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Method;
import java.util.Map;

import javax.faces.context.FacesContext;
import javax.faces.el.PropertyNotFoundException;
import javax.faces.el.PropertyResolver;

public class MPropertyResolver extends PropertyResolver
{
  public Object getValue(Object o , Object key)
  {
    if (o == null)
      return null;
      
    if (o instanceof Map)
      return ((Map) o).get(key);
    
    if (key == null)
      return null;
      
    String propertyName = key.toString();
    try
    {
      BeanInfo info = Introspector.getBeanInfo(o.getClass());
      PropertyDescriptor[] properties = info.getPropertyDescriptors();
      for (int i = 0; i < properties.length; i++)
      {
        if (propertyName.equals(properties[i].getName()))
        {
          Method m = properties[i].getReadMethod();
          return m.invoke(o);
        }
      }
    }
    catch (Exception e)
    {
      throw new PropertyNotFoundException(e);
    }
    
    throw new PropertyNotFoundException("Couldn't find getter for " + propertyName);
  }

  public Object getValue(Object o , int index )
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public void setValue(Object o , Object key, Object value)
  {
    throw new UnsupportedOperationException("Should not be called when rendering");
  }

  public void setValue(Object o , int index , Object value )
  {
    throw new UnsupportedOperationException("Should not be called when rendering");
  }

  public boolean isReadOnly(Object o , Object key )
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public boolean isReadOnly(Object o , int index )
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public Class getType(Object o , Object key)
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public Class getType(Object o , int index )
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

}
