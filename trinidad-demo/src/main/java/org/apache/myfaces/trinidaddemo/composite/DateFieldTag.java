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
package org.apache.myfaces.trinidaddemo.composite;

import org.apache.myfaces.trinidadinternal.taglib.UIXEditableValueTag;


/**
 * NOTE: a clients may not extend UIXEditableValueTag (or
 * any other tag classes), as these are not part of the public
 * API (note the package);  I'm doing it for expedience here.
 */
public class DateFieldTag extends UIXEditableValueTag
{
  @Override
  public String getComponentType()
  {
    return "org.apache.myfaces.trinidaddemo.DateField";
  }

  @Override
  public String getRendererType()
  {
    return null;
  }
}
