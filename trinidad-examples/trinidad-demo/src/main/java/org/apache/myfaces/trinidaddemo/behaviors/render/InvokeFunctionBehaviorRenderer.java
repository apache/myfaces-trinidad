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
package org.apache.myfaces.trinidaddemo.behaviors.render;

import javax.faces.component.behavior.ClientBehavior;
import javax.faces.component.behavior.ClientBehaviorContext;
import javax.faces.render.ClientBehaviorRenderer;

import org.apache.myfaces.trinidaddemo.behaviors.InvokeFunctionBehavior;


// Not using annotation so that Jetty can be used un-exploded
//@FacesBehaviorRenderer(
//  renderKitId = "org.apache.myfaces.trinidad.core",
//  rendererType = "invoke-function")
public class InvokeFunctionBehaviorRenderer
  extends ClientBehaviorRenderer
{
  @Override
  public String getScript(
    ClientBehaviorContext clientBehaviorContext,
    ClientBehavior        clientBehavior)
  {
    InvokeFunctionBehavior behavior = (InvokeFunctionBehavior)clientBehavior;
    String function = behavior.getFunction();
    return new StringBuilder(15 + function.length())
      .append("return ")
      .append(function)
      .append("(event);")
      .toString();
  }
}
