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
package org.apache.myfaces.trinidadinternal.renderkit;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import javax.faces.FacesException;
import javax.faces.application.Application;
import javax.faces.application.NavigationHandler;
import javax.faces.application.StateManager;
import javax.faces.application.ViewHandler;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.el.MethodBinding;
import javax.faces.el.PropertyResolver;
import javax.faces.el.ValueBinding;
import javax.faces.el.VariableResolver;
import javax.faces.event.ActionListener;
import javax.faces.validator.Validator;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.bean.util.StateUtils;

public class MApplication extends Application 
{
  static public MApplication sharedInstance()
  {
    return _INSTANCE;
  }
  
  private MApplication()
  {
  }

  public ActionListener getActionListener()
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public void setActionListener(ActionListener listener)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public Locale getDefaultLocale()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public void setDefaultLocale(Locale locale)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public String getDefaultRenderKitId()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public void setDefaultRenderKitId(String renderKitId)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public String getMessageBundle()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public void setMessageBundle(String bundle)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public NavigationHandler getNavigationHandler()
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public void setNavigationHandler(NavigationHandler handler)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public PropertyResolver getPropertyResolver()
  {
    return _propertyResolver;
  }

  public void setPropertyResolver(PropertyResolver resolver)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public VariableResolver getVariableResolver()
  {
    return _variableResolver;
  }

  public void setVariableResolver(VariableResolver resolver)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public ViewHandler getViewHandler()
  {
    return _viewHandler;
  }

  public void setViewHandler(ViewHandler viewHandler)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public StateManager getStateManager()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public void setStateManager(StateManager stateManager)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public void addComponent(String type, String className)
  {
    _components.put(type, className);
  }

  public UIComponent createComponent(String type) throws FacesException
  {
    String s = (String) _components.get(type);
    if (s == null)
      throw new IllegalArgumentException("No component for type " + type);
    try
    {
      Class c = Class.forName(s);
      return (UIComponent) c.newInstance();
    }
    catch (Exception e)
    {
      throw (FacesException)
        (new FacesException("Could not create component of type " +
                            type + " with class " + s).initCause(e));
    }
  }

  public UIComponent createComponent(ValueBinding binding, FacesContext context, String type)
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public Iterator getComponentTypes()
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public void addConverter(String id, String className)
  {
    _converterIdMap.put(id,className);
  }

  public void addConverter(Class type, String className)
  {
    _converterIdMap.put(type,className);
  }

  public Converter createConverter(String className)
  {
    //copy the RI code
    Converter converter = null;
    try
    {
      converter = (Converter) _mapLookUp(className, _converterIdMap);
      return converter;
    }
    catch (Exception e)
    {
      _LOG.log(e.getMessage());
      throw new FacesException(e.getMessage());
    }
    
  }
  
  private Object _mapLookUp(Object key, Map map) throws InstantiationException,
      IllegalAccessException, ClassNotFoundException
  {
    Object result;
    Object value;
    Class klass;
    value = map.get(key);
    if (value == null)
      return null;
    if (value instanceof String)
    {      
      klass = Class.forName((String)value);
      map.put(key, klass);
    }
    else
      klass = (Class) value;
    result = klass.newInstance();
    return result;
  }

  public Converter createConverter(Class type)
  {
    Converter converter = null;
    try
    {
      converter = (Converter) _mapLookUp(type, _converterTypeMap);
      if (converter != null)
        return converter;
    }
    catch (Exception e)
    {
      _LOG.log(e.getMessage());
    }
    Class[] interfaces = type.getInterfaces();
    for (int i = 0; i < interfaces.length; i++)
    {
      converter = createConverter(interfaces[i]);
      if (converter != null)
        return converter;
    }
    Class superclass = type.getSuperclass();
    if (superclass != null)
    {
      converter = createConverter(superclass);
      if (superclass != null)
        return converter;
    }
    return converter;
  }

  public Iterator getConverterIds()
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public Iterator getConverterTypes()
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public MethodBinding createMethodBinding(String expr, Class[] paramTypes)
  {
    return null;
  }

  public Iterator getSupportedLocales()
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public void setSupportedLocales(Collection locales)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public void addValidator(String id, String className)
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public Validator createValidator(String validatorId)
  {
    throw new UnsupportedOperationException("Not implemented yet");
  }

  public Iterator getValidatorIds()
  {
    throw new UnsupportedOperationException("Should not be called during rendering");
  }

  public ValueBinding createValueBinding(String expression)
  {
    if (!expression.startsWith("#{") ||
        !expression.endsWith("}"))
      throw new UnsupportedOperationException("Haven't implemented that much yet!");
    return new MValueBinding(expression);
  }

  private Map _components = new HashMap();
  private ViewHandler _viewHandler = new MViewHandler();
  private VariableResolver _variableResolver = new MVariableResolver();
  private PropertyResolver _propertyResolver = new MPropertyResolver();
  static private MApplication _INSTANCE = new MApplication();
  private Map _converterIdMap = new HashMap();
  private Map _converterTypeMap = new HashMap();
  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(StateUtils.class);
}
