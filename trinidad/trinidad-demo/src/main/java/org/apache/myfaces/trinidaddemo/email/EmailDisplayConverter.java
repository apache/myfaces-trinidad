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
package org.apache.myfaces.trinidaddemo.email;

import javax.faces.component.UIComponent;
import javax.faces.convert.Converter;
import javax.faces.context.FacesContext;

/**
 * Converter that displays just the name, or the full
 * e-mail address if the name is not available.
 */
public class EmailDisplayConverter implements Converter
{
  public EmailDisplayConverter()
  {
  }

  public String getAsString(FacesContext context, UIComponent component,
                            Object value)
  {
    if (value == null)
      return null;

    String val = value.toString();
    int lessThanIndex = val.indexOf('<');
    if (lessThanIndex < 0)
      return val;

    return val.substring(0, lessThanIndex).trim();
  }

  public Object getAsObject(FacesContext context, UIComponent component,
                            String value)
  {
    throw new UnsupportedOperationException();
  }
}
