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
package org.apache.myfaces.trinidad.config;

import java.beans.PropertyChangeListener;

import javax.faces.context.ExternalContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 *  This class allows access to configuration properties supplied by a plugable
 *  property value provider.  Using a plugable property value provider separates
 *  access to property values from where the values are stored, allowing different
 *  property storage and configuration schemes to be used without impacting code
 *  that uses the property values.
 *  <p>
 *  See {@link PropertyValueProvider PropertyValueProvider} for information
 *  on configuring a property value provider.
 *  <p>
 *  Implementation os this class are required to be thread safe allowing multiple
 *  threads to invoke methods on an instance concurrently.
 */
public abstract class ConfigPropertyService
{
  /**
   * Obtain a reference to the service.
   *
   * @param externalContext  container context object.
   * @return  service object for obtaining property values.
   */
  public static ConfigPropertyService getInstance(ExternalContext externalContext)
  {
    ConfigPropertyService service =
      (ConfigPropertyService) externalContext.getApplicationMap().get(CONFIG_PROPERTY_SERVICE_KEY);
    if (service == null)
    {
      throw new IllegalStateException(_LOG.getMessage("CONFIG_PROPERTY_SERVICE_NOT_INITIALIZED"));
    }
    return service;
  }

  /**
   * @param externalContext  container context object.
   * @param name  name of the requested property.
   * @return  the value of the property or <code>null</code> if the property was not found.
   * @throws NullPointerException is the specified name is null.
   */
  public abstract String getProperty(ExternalContext externalContext, String name);

  /**
   * Register a listener to be informed if/when a property value is changed.
   * <p>
   * The {@link ConfigPropertyService ConfigPropertyService} will be specified as the
   * 'source' attribute of any change events the listener receives.
   *
   * @param listener  the listener to be notified of property value changes.
   */
  public abstract void addPropertyChangeListener(PropertyChangeListener listener);

  /**
   * Unregister a property change listener.
   * @param listener  the listener to unregister.
   */
  public abstract void removePropertyChangeListener(PropertyChangeListener listener);

  /**
   * Get the test value provider.  If one is not currently in the collection of value providers a
   * new instance will be created and added to the collection.  An implementation of this class
   * should not allow more than one instance of {@ TestPropertyValueProvider TestPropertyValueProvider}
   * in its collection of value providers.
   *
   * @return  a test value provider.
   */
  protected abstract TestPropertyValueProvider getTestProvider();

  /**
   * Notify value change listeners of a config property value change.
   * @param name  the name of the property who's value changed.
   * @param oldValue  the previous value of the property.
   * @param newValue  the new value of the property.
   */
  //
  //  Note: this method is not public because it's expected to only be invoked by
  //        PropertyValueProvider which is in the same package.
  //
  protected abstract void notifyValueChange(ExternalContext externalContext, String name, String oldValue);

  /**
   * Key used to store the application's ConfigPropertyService instance in the applicationScope map.
   */
  protected static final String CONFIG_PROPERTY_SERVICE_KEY =
    "org.apache.myfaces.trinidad.config.CONFIG_PROPERTY_SERVICE_INSTANCE";

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ConfigPropertyService.class);
}
