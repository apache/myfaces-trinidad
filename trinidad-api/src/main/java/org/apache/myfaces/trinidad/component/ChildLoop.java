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
package org.apache.myfaces.trinidad.component;

import java.io.IOException;

import java.util.Collection;

import javax.faces.FacesException;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;

/**
 */
abstract class ChildLoop implements ComponentProcessor<Object>
{
  public ChildLoop()
  {
    this(null);
  }

  public ChildLoop(ComponentProcessingContext cpContext)
  {
    _cpContext = cpContext;
  }
  
  public final void run(FacesContext context, UIXCollection comp)
  {
    run(context, comp, comp);
  }

  public final void run(FacesContext context, 
                        UIXCollection comp, UIComponent child)
  {
    if (shouldRun(comp))
      runAlways(context, child);
  }
  
  @SuppressWarnings("unchecked")
  public final void runAlways(FacesContext context, UIComponent comp)
  {
    runAlways(context, comp.getChildren());
  }

  public final void runAlways(FacesContext context, Collection<UIComponent> kids)
  {
    try
    {
      if (_cpContext != null)
        UIXComponent.processFlattenedChildren(context, _cpContext, this, kids, null);
      else
        UIXComponent.processFlattenedChildren(context, this, kids, null);
    }
    catch (IOException ioe)
    {
      throw new FacesException(ioe);
    }
  }
  
  /**
   * Sets up the context for the child and processes it
   */
  public void processComponent(
    FacesContext context,
    ComponentProcessingContext cpContext,
    UIComponent component,
    Object callbackContext) throws IOException
  {
    process(context, component, cpContext);
  }
  
  /**
   * See if we need to run at all (which is usually only
   * if there is available data on this row)
   */
  protected boolean shouldRun(UIXCollection comp)
  {
    return comp.isRowAvailable();
  }

  protected final ComponentProcessingContext getComponentProcessingContext()
  {
    return _cpContext;
  }
    
  protected abstract void process(FacesContext context, UIComponent comp, ComponentProcessingContext cpContext);

  private final ComponentProcessingContext _cpContext;                            
  
  static final class Update extends ChildLoop
  {
    @Override
    protected void process(FacesContext context, UIComponent comp, ComponentProcessingContext cpContext)
    {
      comp.processUpdates(context);
    }
  }

  static final class Validate extends ChildLoop
  {
    @Override
    protected void process(FacesContext context, UIComponent comp, ComponentProcessingContext cpContext)
    {
      comp.processValidators(context);
    }
  }

  static final class Decode extends ChildLoop
  {
    @Override
    protected void process(FacesContext context, UIComponent comp, ComponentProcessingContext cpContext)
    {
      comp.processDecodes(context);
    }
  }  
}
