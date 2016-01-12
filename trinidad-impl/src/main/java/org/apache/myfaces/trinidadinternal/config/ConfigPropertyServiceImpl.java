/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidadinternal.config;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CopyOnWriteArraySet;

import javax.faces.context.ExternalContext;

import org.apache.myfaces.trinidad.config.ConfigPropertyService;
import org.apache.myfaces.trinidad.config.PropertyValueProvider;
import org.apache.myfaces.trinidad.config.TestPropertyValueProvider;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

/**
 *  This class allows access to configuration properties supplied by a plugable
 *  property value provider.  Using a plugable property value provider separates
 *  access to property values from where the values are stored, allowing different
 *  property storage and configuration schemes to be used without impacting code
 *  that uses the property values.
 *  <p>
 *  See {@link PropertyValueProvider PropertyValueProvider} for information
 *  on configuring a property value provider.
 */
public final class ConfigPropertyServiceImpl
  extends ConfigPropertyService
{
  /**
   * Constructor.
   */
  private ConfigPropertyServiceImpl(List<PropertyValueProvider> valueProviders)
  {
    _valueProviders = new CopyOnWriteArrayList<PropertyValueProvider>(valueProviders);
  }

  @Override
  public String getProperty(ExternalContext externalContext, String name)
  {
    if (name == null)
    {
      throw new NullPointerException(_LOG.getMessage("NULL_CONFIG_PROPERTY_NAME"));
    }

    //
    //  Iterate through the list of value providers and return the first non-null value
    //  returned.
    //
    String result = null;
    for (PropertyValueProvider provider: _valueProviders)
    {
      result = provider.getValue(externalContext, name);
      if (result != null)
      {
        break;
      }
    }
    return result;
  }

  @Override
  public void addPropertyChangeListener(PropertyChangeListener listener)
  {
    _changeListeners.add(listener);
  }

  @Override
  public void removePropertyChangeListener(PropertyChangeListener listener)
  {
    _changeListeners.remove(listener);
  }

  @Override
  protected synchronized TestPropertyValueProvider getTestProvider()
  {
    if (_testProvider == null)
    {
      _testProvider = new TestPropertyValueProvider();

      //
      //  Add the test provider to the front of the providers list so it's
      //  checked first.
      //
      _valueProviders.add(0, _testProvider);
    }
    return _testProvider;
  }

  @Override
  protected void notifyValueChange(ExternalContext externalContext, String name, String oldValue)
  {
    String newValue = getProperty(externalContext, name);
    PropertyChangeEvent event = new PropertyChangeEvent(this, name, oldValue, newValue);
    for (PropertyChangeListener listener: _changeListeners)
    {
      listener.propertyChange(event);
    }
  }

  /**
   * This method initializes the collection of value providers.  The caller must ensure
   * this method is called only once during the application's lifecycle to initialize the
   * application's instance.
   */
  static void initialize(ExternalContext externalContext)
  {
    ArrayList<PropertyValueProvider> valueProviders = new ArrayList<PropertyValueProvider>(5);

    //
    //  The first value provider is always the servlet config value provider.
    //
    valueProviders.add(new ServletConfigValueProvider());

    //
    //  Next see if there is a value provider SPI registered on the classpath.
    //
    List<PropertyValueProvider> list = ClassLoaderUtils.getServices(_PROPERTY_PROVIDER_URL);
    if (list.size() > 1)
    {
      throw new RuntimeException(_LOG.getMessage("MULTIPLE_CONFIG_PROPERTY_PROVIDERS_FOUND"));
    }
    else if (list.size() == 1)
    {
      valueProviders.add(list.get(0));
    }

    //
    //  The last value provider is always the default value provider.
    //
    valueProviders.add(new DefaultValueProvider());

    //
    //  Create the instance and place it in the applicationScope map.
    //
    ConfigPropertyServiceImpl instance = new ConfigPropertyServiceImpl(valueProviders);
    externalContext.getApplicationMap().put(ConfigPropertyService.CONFIG_PROPERTY_SERVICE_KEY, instance);
  }

  private TestPropertyValueProvider _testProvider = null;

  private final List<PropertyValueProvider> _valueProviders;

  private final Set<PropertyChangeListener> _changeListeners = new CopyOnWriteArraySet<PropertyChangeListener>();

  static private final String _PROPERTY_PROVIDER_URL = "org.apache.myfaces.trinidad.config.PropertyValueProvider";

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ConfigPropertyServiceImpl.class);
}
