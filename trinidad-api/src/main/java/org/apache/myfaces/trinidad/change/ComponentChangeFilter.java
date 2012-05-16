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

package org.apache.myfaces.trinidad.change;

import java.io.Serializable;

import javax.faces.component.UIComponent;

/**
 * Filter class used to filter the ComponentChanges that gets added to the ChangeManager through calls to
 * UIXComponentBase.addComponentChange() method. Clients that wish to supress certain changes for
 * certain components would provide the concrete implementations.
 * @see org.apache.myfaces.trinidad.component.UIXComponentBase#addComponentChange(ComponentChange)
 * @see org.apache.myfaces.trinidad.component.UIXComponentBase#addComponentChangeFilter(ComponentChangeFilter)
 */
public abstract class ComponentChangeFilter implements Serializable
{
  /**
   * Indicates whether this filter accepts or rejects a supplied change for the supplied component for which the 
   * change is targeted
   * @param componentChange The ComponentChange to check acceptance for
   * @param changeTargetComponent The intended target component for the change
   * @return Result Indicates acceptance or rejection of the change
   */
  public abstract Result accept(ComponentChange componentChange, UIComponent changeTargetComponent);

  /**
   * Enum for filter result
   */
  public enum Result
  {
     ACCEPT,
     REJECT
  }
  
  private static final long serialVersionUID = 1L;
}
