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
package org.apache.myfaces.trinidadinternal.el;

import javax.faces.context.FacesContext;
import javax.faces.el.VariableResolver;

import org.apache.myfaces.trinidad.context.RequestContext;

/**
 * VariableResolver decorator used by Trinidad.
 * <p>
 */
public class TrinidadVariableResolver extends VariableResolver
{
  static public final String PAGE_FLOW_SCOPE_VARIABLE_NAME = "pageFlowScope";

  public TrinidadVariableResolver(VariableResolver decorated)
  {
    _decorated = decorated;
  }

  @Override
  public Object resolveVariable(FacesContext context, String name)
  {
    if (RequestContext.VARIABLE_NAME.equals(name))
    {
      return RequestContext.getCurrentInstance();
    }
    // Support both "pageFlowScope" and "processScope"
    // as EL variables to give developers a time to migrate
    else if (PAGE_FLOW_SCOPE_VARIABLE_NAME.equals(name) ||
             "processScope".equals(name))
    {
      return RequestContext.getCurrentInstance().getPageFlowScope();
    }

    return _decorated.resolveVariable(context, name);
  }

  private VariableResolver _decorated;
}
