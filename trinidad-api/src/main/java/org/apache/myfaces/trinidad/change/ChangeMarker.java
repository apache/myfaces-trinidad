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
package org.apache.myfaces.trinidad.change;

/**
 * Interface for Marking a change on a different component.
 */
public interface ChangeMarker
{
  /**
   * Get the identifier for the target component on which a change needs to be applied. This id
   * should be a absolute id scoped from the UIViewRoot and the generation algorithm should reverse 
   * match that of UIComponent.findComponent(). In other words, call to findComponent() on any 
   * component in the view tree should be able to successfully return given the id that this method 
   * returns.
   * @return The unique absolute identifier for target component.
   */
  public String getChangeTargetComponentScopedId();
}