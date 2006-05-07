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
package org.apache.myfaces.adfbuild.test;

import java.beans.*;

import java.util.HashMap;
import java.util.Map;

import javax.faces.application.Application;
import javax.faces.application.MockApplication;
import javax.faces.component.UIComponent;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.MockExternalContext;
import javax.faces.context.MockFacesContext;
import javax.faces.el.EvaluationException;
import javax.faces.el.PropertyNotFoundException;
import javax.faces.el.PropertyResolver;
import javax.faces.render.MockRenderKit;
import javax.faces.render.MockRenderer;
import javax.faces.render.RenderKit;
import javax.faces.render.Renderer;

/**
 * Mock faces context for use with unit tests
 * @author Arjuna Wijeyekoon
 */
public class MockFContext extends MockFacesContext
{
  public MockFContext()
  {
    setCurrentInstance(this);
  }

  public static void clearContext()
  {
    FacesContext.setCurrentInstance(null);
  }

  public ExternalContext getExternalContext()
  {
    // this method is called a lot, so we don't want to use the "mock"
    // implementations as those expect a specific number of calls:
    return _eContext;
  }

  public RenderKit getRenderKit()
  {
    // this method is called a lot, so we don't want to use the "mock"
    // implementations as those expect a specific number of calls:
    return _kit;
  }

  public Application getApplication()
  {
    return _app;
  }

  private final ExternalContext _eContext = new MockEContext();
  private final RenderKit _kit = new MockRKit();
  private final Application _app = new MApplication();

  private static final class MApplication extends MockApplication
  {
    public PropertyResolver getPropertyResolver()
    {
      return _resolver;
    }
    
    private PropertyResolver _resolver = new PropertyResolverImpl();
  }

  private static final class PropertyResolverImpl extends PropertyResolver
  {
    public Class getType(Object base, int index)
    {
      throw new UnsupportedOperationException();
    }

    public Class  getType(Object base, Object property)
    {
      throw new UnsupportedOperationException();
    }
    
    public  Object  getValue(Object base, int index)
    {
      throw new UnsupportedOperationException();
    }

    public  Object  getValue(Object base, Object property)
    {
      if (base instanceof Map)
        return ((Map) base).get(property);

      try
      {
        BeanInfo info = Introspector.getBeanInfo(base.getClass());
        PropertyDescriptor[] props =  info.getPropertyDescriptors();
        for (int i = 0; i < props.length; i++)
        {
          if (property.equals(props[i].getName()))
          {
            return props[i].getReadMethod().invoke(base, (Object[]) null);
          }
        }

        throw new PropertyNotFoundException(property.toString());
      }
      catch (Exception e)
      {
        throw new EvaluationException(e);
      }
    }

    public  boolean  isReadOnly(Object base, int index)
    {
      throw new UnsupportedOperationException();
    }

    
    public boolean  isReadOnly(Object base, Object property)
    {
      throw new UnsupportedOperationException();
    }

    public  void  setValue(Object base, int index, Object value)
    {
      throw new UnsupportedOperationException();
    }

    public  void  setValue(Object base, Object property, Object value)
    {
      throw new UnsupportedOperationException();
    }
  }

  private static final class MockEContext extends MockExternalContext
  {
    public Map getRequestMap()
    {
      // this method is called a lot, so we don't want to use the "mock"
      // implementations as those expect a specific number of calls:
      return _requestMap;
    }

    public Map getApplicationMap()
    {
      return _appMap;
    }

    private final Map _requestMap = new HashMap(2);
    private final Map _appMap = new HashMap(2);
  }

  private static final class MockRKit extends MockRenderKit
  {
    public Renderer getRenderer(String family, String type)
    {
      // this method is called a lot, so we don't want to use the "mock"
      // implementations as those expect a specific number of calls:
      return _renderer;
    }

    private final Renderer _renderer = new MockR();
  }

  private static final class MockR extends MockRenderer
  {
    // this method is called a lot, so we don't want to use the "mock"
    // implementations as those expect a specific number of calls:
    public void decode(FacesContext fc, UIComponent comp)
    {
      // do nothing
    }

    // this method is called a lot, so we don't want to use the "mock"
    // implementations as those expect a specific number of calls:
    public String convertClientId(FacesContext fc, String id)
    {
      return id;
    }
  }
}
