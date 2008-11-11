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
package org.apache.myfaces.trinidad.component;

import java.io.IOException;

import javax.faces.component.UIComponent;

import javax.el.MethodExpression;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.event.AttributeChangeListener;

/**
 * Pure abstract base class for all UIX components.
 */
abstract public class UIXComponent extends UIComponent
{
  /**
   * Helper function called by Renderers to iterate over a flattened view of a group of
   * potentially FlattenedComponent instances rooted at a single child of the component that
   * the Renderer is encoding, invoking the <code>childProcessor</code> with its
   * <code>callbackContext</code> on each renderable instance.
   * <p>
   * If the child is a FlattenedComponent, the <code>childProcessor</code> will
   * be called on each of that FlattenedComponent's children, recursing if those children are
   * themselves FlattenedComponents, otherwise, the <code>childProcessor</code> will be called on
   * the child itself.
   * <p>
   * This method is typically used to flatten the contents of a facet on the FlattenedComponent to
   * be encoded.  If the Renderer accidentally passes in the component to be encoded instead of one
   * of its children, the result will almost certainly be an infinite recursion and stack overflow.
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessor, Iterable, Object)
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, UIComponent, Object)
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, Iterable, Object)
   * @see FlattenedComponent
   */
  public static <S> boolean processFlattenedChildren(
    FacesContext context,
    ComponentProcessor<S> childProcessor,
    UIComponent child,
    S callbackContext) throws IOException
  {
    return processFlattenedChildren(context,
                                    new ComponentProcessingContext(),
                                    childProcessor,
                                    child,
                                    callbackContext);
  }

  /**
   * Helper function called by FlattenedComponent to iterate over a flattened view of a group of
   * potentially FlattenedComponent instances rooted at a single child of the FlattenedComponent,
   * invoking the <code>childProcessor</code> with its
   * <code>callbackContext</code> on each renderable instance.
   * <p>
   * If the child is a FlattenedComponent, the <code>childProcessor</code> will
   * be called on each of that FlattenedComponent's children, recursing if those children are
   * themselves FlattenedComponents, otherwise, the <code>childProcessor</code> will be called on
   * the child itself.
   * <p>
   * This method is typically used to flatten the contents of a facet of the FlattenedComponent.
   * If the FlattenedComponent accidentally passes in itself instead of one
   * of its children, the result will almost certainly be an infinite recursion and stack overflow.
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessor, UIComponent, Object)
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessor, Iterable, Object)
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, Iterable, Object)
   * @see FlattenedComponent
   */
    public static <S> boolean processFlattenedChildren(
    FacesContext context,
    ComponentProcessingContext cpContext,
    ComponentProcessor<S> childProcessor,
    UIComponent child,
    S callbackContext) throws IOException
  {
    if (child.isRendered())
    {      
       // component is an action FlattenedComponent.  Ask it to flatten its children
      if ((child instanceof FlattenedComponent) &&
          ((FlattenedComponent)child).isFlatteningChildren(context))
      {
        return ((FlattenedComponent)child).processFlattenedChildren(context,
                                                                    cpContext,
                                                                    childProcessor,
                                                                    callbackContext);
      }
      else
      {
        // not a FlattenedComponent, pass the component directly to the ComponentProcessor
        try
        {
          childProcessor.processComponent(context, cpContext, child, callbackContext);
        }
        finally
        {
          // if startDepth is > 0, only the first visible child will be marked as starting a group
          cpContext.resetStartDepth();
        }
        
        return true;
      }
    }
    else
    {
      // component not rendered
      return false;
    }
  }

  /**
   * Helper function called by Renderers to iterate over a flattened view of the
   * children, potentially containing FlattenedComponents, of the component the Renderer is
   * encoding, invoking the <code>childProcessor</code> with its
   * <code>callbackContext</code> on each renderable instance.
   * <p>
   * For each FlattenedComponent child, the <code>childProcessor</code> will
   * be called on each of that FlattenedComponent's children, recursing if those children are
   * themselves FlattenedComponents, otherwise, the <code>childProcessor</code> will be called on
   * the child itself.
   * <p>
   * This method is typically used to flatten the children of the FlattenedComponent to
   * be encoded.
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessor, UIComponent, Object)
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, UIComponent, Object)
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, Iterable, Object)
   * @see FlattenedComponent
   */
  public static <S> boolean processFlattenedChildren(
    FacesContext context,
    ComponentProcessor<S> childProcessor,
    Iterable<UIComponent> children,
    S callbackContext) throws IOException
  {
    return processFlattenedChildren(context,
                                    new ComponentProcessingContext(),
                                    childProcessor,
                                    children,
                                    callbackContext);
  }

  /**
   * Helper function called by FlattenedComponents to iterate over a flattened view of their
   * children, potentially themselves FlattenedComponents, invoking the <code>childProcessor</code>
   * with its <code>callbackContext</code> on each renderable instance.
   * <p>
   * For each FlattenedComponent child, the <code>childProcessor</code> will
   * be called on each of that FlattenedComponent's children, recursing if those children are
   * themselves FlattenedComponents, otherwise, the <code>childProcessor</code> will be called on
   * the child itself.
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessor, UIComponent, Object)
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessor, Iterable, Object)
   * @see UIXComponent#processFlattenedChildren(FacesContext, ComponentProcessingContext, ComponentProcessor, UIComponent, Object)
   * @see FlattenedComponent
   */
  public static <S> boolean processFlattenedChildren(
    FacesContext context,
    ComponentProcessingContext cpContext,
    ComponentProcessor<S> childProcessor,
    Iterable<UIComponent> children,
    S callbackContext) throws IOException
  {
    // we haven't processed a child yet
    boolean processedChild = false;
    
    for (UIComponent currChild : children)
    {
      // latch processed child to the first child processed
      processedChild |= processFlattenedChildren(context,
                                                 cpContext, childProcessor,
                                                 currChild,
                                                 callbackContext);
    }
    
    return processedChild;
  }

  /**
   * Returns the FacesBean used for storing the component's state.
   */
  abstract public FacesBean getFacesBean();

  /**
   * Adds an AttributeChangeListener.  Attribute change events are not
   * delivered for any programmatic change to a property.  They are only
   * delivered when a renderer changes a property without the application's
   * specific request.  An example of an attribute change events might
   * include the width of a column that supported client-side resizing.
   */
  abstract public void addAttributeChangeListener(AttributeChangeListener acl);

  /**
   * Removes an AttributeChangeListener.  Attribute change events are not
   * delivered for any programmatic change to a property.  They are only
   * delivered when a renderer changes a property without the application's
   * specific request.  An example of an attribute change events might
   * include the width of a column that supported client-side resizing.
   */
  abstract public void removeAttributeChangeListener(AttributeChangeListener acl);

  /**
   * Gets the registered AttributeChangeListeners.
   */ 
  abstract public AttributeChangeListener[] getAttributeChangeListeners();

  /**
   * Sets a method binding to an AttributeChangeListener.  Attribute
   * change events are not
   * delivered for any programmatic change to a property.  They are only
   * delivered when a renderer changes a property without the application's
   * specific request.  An example of an attribute change events might
   * include the width of a column that supported client-side resizing.
   */
  abstract public void setAttributeChangeListener(MethodExpression me);

  /**
   * Gets the method binding to an AttributeChangeListener.  Attribute
   * change events are not
   * delivered for any programmatic change to a property.  They are only
   * delivered when a renderer changes a property without the application's
   * specific request.  An example of an attribute change events might
   * include the width of a column that supported client-side resizing.
   */
  abstract public MethodExpression getAttributeChangeListener();

  abstract public void markInitialState();
  
  /**
   * Provides additional context (the target child component for which the container 
   * client ID is requested) to a naming container for constructing a client ID.
   * This is useful for components such as @link UIXTable and @link UIXTreeTable which need 
   * to return different container client IDs for stamped and non-stamped child components.
   * @see UIXComponentBase#getClientId(FacesContext context)
   */
  abstract public String getContainerClientId(FacesContext context, UIComponent child);
}
