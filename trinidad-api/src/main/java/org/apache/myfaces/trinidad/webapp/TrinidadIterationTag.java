/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.trinidad.webapp;

import javax.faces.component.UIComponent;

import javax.servlet.jsp.JspException;


/**
 * A base class for Trinidad tags that iterate over their content bodies.
 * This class provides functionality to interact with the components within
 * an iteration of the tag so that there may be interaction between the
 * tag and the JSF components that are created within it.
 */
public abstract class TrinidadIterationTag
  extends TrinidadTagSupport
{
  /**
   * Default constructor.
   */
  public TrinidadIterationTag()
  {
    super();
  }

  @Override
  public final int doStartTag()
    throws JspException
  {
    // Register this tag for callbacks from JSF component tags within
    // the body.
    TagComponentBridge bridge = TagComponentBridge.getInstance(pageContext);
    bridge.registerTag(this);

    return doStartTagImpl();
  }

  @Override
  public final int doEndTag()
    throws JspException
  {
    // Unregister this tag to avoid further callbacks from the component
    // tag.
    TagComponentBridge bridge = TagComponentBridge.getInstance(pageContext);
    bridge.unregisterTag(this);

    return doEndTagImpl();
  }

  /**
   * Call back method from
   * {@link org.apache.myfaces.trinidad.webapp.UIXComponentELTag} to
   * notify an iteration tag that a component has been created, or found
   * within the body of the tag.
   *
   * @param component The subject component.
   */
  public void childComponentProcessed(
    @SuppressWarnings("unused") UIComponent component)
  {
    // No code necessary, this is a hook for sub-classes to be able
    // to override.
  }

  /**
   * Call back method from
   * {@link org.apache.myfaces.trinidad.webapp.UIXComponentELTag} to
   * notify an iteration tag that a component tag has ended (corresponds to the doEndTag call).
   *
   * @param component The subject component.
   */
  public void afterChildComponentProcessed(
    @SuppressWarnings("unused") UIComponent component)
  {
    // No code necessary, this is a hook for sub-classes to be able
    // to override.
  }

  /**
   * Method to customize logic performed during {@link #doStartTag()}.
   */
  protected int doStartTagImpl()
    throws JspException
  {
    return super.doStartTag();
  }

  /**
   * Method to customize logic performed during {@link #doStartTag()}.
   */
  protected int doEndTagImpl()
    throws JspException
  {
    return super.doEndTag();
  }

  /**
   * Pushes a suffix to be used onto a stack.
   * By applying a component suffix, an iterating tag may control how
   * component IDs are generated. This control allows a controlled and
   * documented ID to be generated rather than relying on the ID
   * generation that the JSF tag implements. By default,
   * {@link javax.faces.webapp.UIComponentClassicTagBase} appends a
   * "j_id_" and a counter to component IDs after the first loop to avoid
   * ID collisions, but this suffix is not part of the specification and
   * therefore cannot be developed against.
   * <p>
   * Note that this suffix is only applied up to, and including, a Trinidad
   * naming container component. The children of a Trinidad naming container
   * component will not have a suffix appended to them.
   * </p>
   *
   * @param suffix The suffix applied to components.
   */
  protected final void pushComponentSuffix(String suffix)
  {
    ComponentIdSuffixStack stack =
      ComponentIdSuffixStack.getInstance(pageContext);

    stack.push(suffix);
  }

  /**
   * Pops the component ID suffix from the stack.
   * This removes the last suffix pushed.
   * <p>
   * Tag authors must call this method when
   * {@link #pushComponentSuffix(String)} is used to ensure that the
   * stack is kept properly in-sync.
   * </p>
   *
   * @see #pushComponentSuffix(String)
   */
  protected final void popComponentSuffix()
  {
    ComponentIdSuffixStack stack =
      ComponentIdSuffixStack.getInstance(pageContext);

    stack.pop();
  }
}
