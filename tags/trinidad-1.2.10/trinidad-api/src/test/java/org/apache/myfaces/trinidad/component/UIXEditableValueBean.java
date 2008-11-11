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
package org.apache.myfaces.trinidad.component;

import org.apache.myfaces.trinidad.bean.PropertyKey;

public class UIXEditableValueBean extends UIXComponentBean
{
  /**
   * Subclass for editable components;  beans here have an
   * additional responsibility, which is to automatically
   * set "localValueSet" any time the value is set.
   */
  @Override
  public void setProperty(PropertyKey key, Object value)
  {
    super.setProperty(key, value);
    if (key == UIXEditableValue.VALUE_KEY)
      setProperty(UIXEditableValue.LOCAL_VALUE_SET_KEY, Boolean.TRUE);
  }
}
