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
 * Strategy implemented for <em>tags</em> that insert document fragments, returning the URL of the
 *  fragment for any top-level UIComponents inserted by the tag. When determining the URL of the
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
 * Implementations of InsertedComponentFragmentLocator are registered using the normal
 *  Service Provider Interface pattern.  A text file named
 *  "org.apache.myfaces.trinidad.change.InsertedComponentFragmentLocator" is placed in the
 *  META-INF/services directory. This file contains the fully qualified class names of all the
 *  InsertedComponentFragmentLocator strategy to register.
 *
 * @see InsertingComponentFragmentLocator
 */
public abstract class InsertedComponentFragmentLocator
{
  /**
   * Returns the URL string of the fragment that contains tag corresponding to 
   *  <code>componentToTest</code>, or <code>null</code> if this InsertedComponentFragmentLocator 
   *  could not determine the URL.
   * @param context         The FacesContext instance for current request
   * @param componentToTest The component to determine the fragment URL for. 
   *                         <code>componentToTest</code> will be the <code>targetComponent</code>, 
   *                         or one of its ancestors. EL context will NOT be setup for 
   *                         <code>componentToTest</code> when this method is called.
   * @param targetComponent The target component for which we are ultimately trying to determine the 
   *                         fragment URL. It is assumed that this URL will be same as the URL 
   *                         obtained for <code>componenToTest</code>
   */
  public abstract String getFragmentUrlForInsertedComponent(
    FacesContext context, 
    UIComponent  componentToTest, 
    UIComponent  targetComponent);
}
