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
package org.apache.myfaces.trinidad.component;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.faces.component.UIComponent;

import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.el.ValueBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.FacesListener;
import javax.faces.render.Renderer;
import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.event.AttributeChangeListener;

/**
 * Pure abstract base class for all UIX components.
 * @author The Oracle ADF Faces Team
 * @todo Remove UIComponent methods when mock maker walks up hierarchy during 
 *       mock generation of abstract classes to ensure all abstract methods
 *       have mock implementations.
 * @mock
 */
abstract public class UIXComponent extends UIComponent
{
  abstract public FacesBean getFacesBean();
  abstract public void addAttributeChangeListener(AttributeChangeListener acl);
  abstract public void removeAttributeChangeListener(AttributeChangeListener acl);
  abstract public AttributeChangeListener[] getAttributeChangeListeners();

  abstract public void setAttributeChangeListener(MethodBinding mb);
  abstract public MethodBinding getAttributeChangeListener();

  abstract public void markInitialState();

  // JSF 1.2 methods that we're adding up front
  abstract public int getFacetCount();
  abstract public void encodeAll(FacesContext context) throws IOException;

  
  // Everything below here is a UIComponent method

  @SuppressWarnings("unchecked")
  @Override
  public abstract Map getAttributes();
  
  @Override
	public abstract ValueBinding getValueBinding(String name);
  
  @Override
  public abstract void setValueBinding(String name, ValueBinding binding);

  @Override
  public abstract String getClientId(FacesContext context);

  @Override
  public abstract String getFamily();

  @Override
  public abstract String getId();

  @Override
  public abstract void setId(String id);

  @Override
  public abstract UIComponent getParent();

  @Override
  public abstract void setParent(UIComponent parent);

  @Override
  public abstract boolean isRendered();

  @Override
  public abstract void setRendered(boolean rendered);

  @Override
  public abstract String getRendererType();

  @Override
  public abstract void setRendererType(String rendererType);

  @Override
  public abstract boolean getRendersChildren();

  @SuppressWarnings("unchecked")
  @Override
  public abstract List getChildren();

  @Override
  public abstract int getChildCount();

  @Override
  public abstract UIComponent findComponent(String expr);

  @SuppressWarnings("unchecked")
  @Override
  public abstract Map getFacets();

  @Override
  public abstract UIComponent getFacet(String name);

  @SuppressWarnings("unchecked")
  @Override
  public abstract Iterator getFacetsAndChildren();

  @Override
  public abstract void broadcast(FacesEvent event)
      throws AbortProcessingException;

  @Override
  public abstract void decode(FacesContext context);

  @Override
  public abstract void encodeBegin(FacesContext context) throws IOException;

  @Override
  public abstract void encodeChildren(FacesContext context) throws IOException;

  @Override
  public abstract void encodeEnd(FacesContext context) throws IOException;

  @Override
  protected abstract void addFacesListener(FacesListener listener);

  @SuppressWarnings("unchecked")
  @Override
  protected abstract FacesListener[] getFacesListeners(Class clazz);

  @Override
  protected abstract void removeFacesListener(FacesListener listener);

  @Override
  public abstract void queueEvent(FacesEvent event);

  @Override
  public abstract void processRestoreState(FacesContext context,
                                           Object state);

  @Override
  public abstract void processDecodes(FacesContext context);

  @Override
  public abstract void processValidators(FacesContext context);

  @Override
  public abstract void processUpdates(FacesContext context);

  @Override
  public abstract Object processSaveState(FacesContext context);

  @Override
  protected abstract FacesContext getFacesContext();

  @Override
  protected abstract Renderer getRenderer(FacesContext context);

  public abstract Object saveState(FacesContext context);
  public abstract void restoreState(FacesContext context, Object state);
  public abstract boolean isTransient();
  public abstract void setTransient(boolean trans);
}
