/*
 * Copyright  2004-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.adfinternal.el;

import javax.faces.context.FacesContext;
import javax.faces.el.VariableResolver;

import org.apache.myfaces.adf.context.AdfFacesContext;

/**
 * VariableResolver decorator used by ADF Faces.
 * <p>
 * @author The Oracle ADF Faces Team
 */
public class AdfFacesVariableResolver extends VariableResolver
{
  static public final String PAGE_FLOW_SCOPE_VARIABLE_NAME = "pageFlowScope";

  public AdfFacesVariableResolver(VariableResolver decorated)
  {
    _decorated = decorated;
  }

  public Object resolveVariable(FacesContext context, String name)
  {
    if (AdfFacesContext.VARIABLE_NAME.equals(name))
    {
      return AdfFacesContext.getCurrentInstance();
    }
    // Support both "pageFlowScope" and "processScope"
    // as EL variables to give developers a time to migrate
    else if (PAGE_FLOW_SCOPE_VARIABLE_NAME.equals(name) ||
             "processScope".equals(name))
    {
      return AdfFacesContext.getCurrentInstance().getPageFlowScope();
    }

    return _decorated.resolveVariable(context, name);
  }

  private VariableResolver _decorated;
}
