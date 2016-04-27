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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

/**
 * Strategy implemented for <em>components</em> that insert document fragments to return the URL of
 *  the fragment currently inserted by this component. When determining the URL of the
 *  document that defines the tag corresponding to a target component, clients will call
 *  <code>getFramentUrlForInsertedComponent</code> for each registered
 *  <code>InsertedComponentFragmentLocator</code> on each UIComponent starting from the target
 *  component and upto the UIViewRoot and each registered InsertingComponentFragmentLocator on each
 *  component from the parent of the target component to the UIViewRoot. If a non-null URL string
 *  is returned, the walk of the ancestor chain halts and the returned value is considered the
 *  URL string for the document for the target component. If the registered listeners return
 *  <code>null</code> for every component in the ancestor chain, the containing URL is assumed to be
 *  the URL of the enclosing page.
 *
 * Implementations of InsertingComponentFragmentLocator are registered using the normal
 *  Service Provider Interface pattern.  A text file named
 *  "org.apache.myfaces.trinidad.change.InsertingComponentFragmentLocator" is placed in the
 *  META-INF/services directory. This file contains the fully qualified class names of all the
 *  InsertingComponentFragmentLocator strategy to register.
 *
 * @see InsertedComponentFragmentLocator
 */
public abstract class InsertingComponentFragmentLocator
{
  /**
   * Returns the URL string of the fragment inserted by <code>componentToTest</code>, or 
   *  <code>null</code> if this InsertingComponentFragmentLocator could not determine the URL.
   * @param context         The FacesContext instance for current request
   * @param componentToTest The component that possibly inserted the targetComponent. This component
   *                         will be used to determine the fragment URL for targetComponent.
   *                         <code>componentToTest</code> will be an ancestor of the 
   *                         <code>targetComponent</code>. EL context will NOT be setup for 
   *                         <code>componentToTest</code> when this method is called.
   * @param targetComponent The component to determine the fragment URL for
   */
  public abstract String getInsertedFragmentUrl(
    FacesContext context, 
    UIComponent  componentToTest, 
    UIComponent  targetComponent);
}
