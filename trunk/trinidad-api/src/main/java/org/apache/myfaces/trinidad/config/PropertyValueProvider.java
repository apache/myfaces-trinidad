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

import javax.faces.context.ExternalContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Abstract base class for configurable property value providers.
 * <p>
 * A PropertyValueProvider can be registered by placing
 * <code>/META-INF/services/org.apache.myfaces.trinidad.config.PropertyValueProvider</code>
 * on the classpath containing the fully qualified name of the implementation class.
 * <p>
 * Note: at this time the framework only supports one PropertyValueProvider being registered
 * to avoid provider ordering issues.
 *  <p>
 *  Implementation os this class are required to be thread safe allowing multiple
 *  threads to invoke methods on an instance concurrently.
 */
public abstract class PropertyValueProvider
{
  /**
   * Constructor.
   */
  protected PropertyValueProvider()
  {
  }

  /**
   * Fetch a property value.  There is no concurrency control provided by the property service so
   * all implementations of this method are required to be thread safe or to perform their own
   * concurrency control.
   * @param externalContext  container context object.
   * @param name  name of the requested property.
   * @return  the value of the property or <code>null</code> if the property was not found.
   * @throws NullPointerException if the supplied property name is null.
   */
  public abstract String getValue(ExternalContext externalContext, String name);

  /**
   * Implementations of this class are required to call this method when a property value is changed.
   * @param externalContext  container context object.
   * @param name  the name of the property who's value changed.
   * @param oldValue  the previous value of the property.
   * @param newValue  the new value of the property.
   */
  protected final void notifyValueChanged(ExternalContext externalContext, String name, String oldValue)
  {
    ConfigPropertyService service = ConfigPropertyService.getInstance(externalContext);
    if (service == null)
    {
      throw new IllegalStateException(_LOG.getMessage("CONFIG_PROPERTY_SERVICE_NOT_INITIALIZED"));
    }
    service.notifyValueChange(externalContext, name, oldValue);
  }

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(PropertyValueProvider.class);
}
