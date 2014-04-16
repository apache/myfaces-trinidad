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

import java.util.concurrent.ConcurrentHashMap;

import javax.faces.context.ExternalContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 *  A {@link PropertyValueProvider PropertyValueProvider} that allows runtime modification
 *  of property values to support unit testing code.  By using this class a unit test can
 *  exercise a piece of code with a number of different property values without needing to
 *  change property values in their underlaying source.
 *  <p>
 *  This class maintains an in-memory collection of property values supplied by calling the
 *  {@link TestPropertyValueProvider#setProperty setProperty} method.  When a value is requested
 *  by the {@link ConfigPropertyService ConfigPropertyService} the in-memory collection of
 *  values are consulted first.  If a value has been supplied that will be returned, otherwise
 *  the property value will be fetched from the configured property value provider.
 *  <p>
 *  When this class is used it will coordinate with the {@link ConfigPropertyService ConfigPropertyService}
 *  to ensure values are obained from this class.
 */
public final class TestPropertyValueProvider
  extends PropertyValueProvider
{
  public TestPropertyValueProvider()
  {
  }

  /**
   * Obtain a reference to the test value provider.  Calling this method will also result
   * in the test property value provider being installed with the
   * {@link ConfigPropertyService ConfigPropertyService}.
   *
   * @return  a test value provider.
   */
  public static TestPropertyValueProvider getInstance(ExternalContext externalContext)
  {
    ConfigPropertyService service = ConfigPropertyService.getInstance(externalContext);
    if (service == null)
    {
      throw new IllegalStateException(_LOG.getMessage("CONFIG_PROPERTY_SERVICE_NOT_INITIALIZED"));
    }
    return service.getTestProvider();
  }

  /**
   *  Used by the {@link ConfigPropertyService ConfigPropertyService} to obtain a property value.
   *  If a value has been specified by calling the {@link TestPropertyValueProvider#setProperty setProperty}
   *  method that value will be returned, otherwise the request will be delegated to the configured
   *  property value provider.
   *
   *  @param extCtx {@inheritDoc}
   *  @param name {@inheritDoc}
   *  @return {@inheritDoc}
   */
  @Override
  public String getValue(ExternalContext extCtx, String name)
  {
    String value;
    if (name != null)
    {
      value = _testValues.get(name);
    }
    else
    {
      throw new NullPointerException(_LOG.getMessage("NULL_CONFIG_PROPERTY_NAME"));
    }
    return value;
  }

  /**
   *  Set the value of a property.  Calling this method only updates the in-memory value used for
   *  test purposes.  The value is not propagated to any underlying storage.
   *  <p>
   *  Any PropertyChangeListener reqiested with the {@link ConfigPropertyService ConfigPropertyService}
   *  will be notified of the value change when this method is called.
   *
   *  @param externalContext  container context object.
   *  @param name name of the property.
   *  @param value value of the property.
   *  @throws NullPointerException if the property name is null.
   */
  public void setProperty(ExternalContext externalContext, String name, String value)
  {
    if (name == null)
    {
      throw new NullPointerException(_LOG.getMessage("NULL_CONFIG_PROPERTY_NAME"));
    }
    String oldValue = ConfigPropertyService.getInstance(externalContext).getProperty(externalContext, name);
    _testValues.put(name, value);
    notifyValueChanged(externalContext, name, oldValue);
  }

  /**
   *  Remove a property value from the test value provider.
   *  @param name  name of the property.
   *  @throws NullPointerException if the property name is null.
   */
  public void removeProperty(ExternalContext externalContext, String name)
  {
    if (name == null)
    {
      throw new NullPointerException(_LOG.getMessage("NULL_CONFIG_PROPERTY_NAME"));
    }
    String oldValue = ConfigPropertyService.getInstance(externalContext).getProperty(externalContext, name);
    _testValues.remove(name);
    notifyValueChanged(externalContext, name, oldValue);
  }

  /**
   *  Removes all in-memory property values specified by calling
   *  {@link TestPropertyValueProvider#setProperty setProperty}, returning them to their original values.
   */
  public void resetValues()
  {
    _testValues.clear();
  }

  private final ConcurrentHashMap<String, String> _testValues = new ConcurrentHashMap<String, String>();

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(TestPropertyValueProvider.class);
}
