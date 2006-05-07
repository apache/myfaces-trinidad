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
package org.apache.myfaces.adfinternal.agent;

import org.apache.myfaces.adf.context.Agent;

import javax.faces.context.FacesContext;


/**
 * Factory to create Agent. Allows implementations to plug in their own agent
 * detection, without having to override AdfFacesContext.
 * //@todo: Right now this not public API, but will be when adf faces cofiguration is sorted out
 */
public interface AgentFactory
{
  /**
   * @param facesContext
   * @return Agent for the current request/context
   */
  public Agent createAgent(FacesContext facesContext);
}
