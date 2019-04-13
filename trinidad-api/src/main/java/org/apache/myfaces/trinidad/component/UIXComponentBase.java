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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectOutputStream;

import java.net.URL;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Properties;

import javax.el.ELContext;
import javax.el.ELException;
import javax.el.MethodExpression;
import javax.el.ValueExpression;

import javax.faces.FacesException;
import javax.faces.component.ContextCallback;
import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.EvaluationException;
import javax.faces.el.MethodBinding;
import javax.faces.el.ValueBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;
import javax.faces.event.FacesListener;
import javax.faces.render.RenderKit;
import javax.faces.render.Renderer;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.FacesBeanFactory;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.bean.util.StateUtils;
import org.apache.myfaces.trinidad.bean.util.ValueMap;
import org.apache.myfaces.trinidad.change.AttributeComponentChange;
import org.apache.myfaces.trinidad.change.ComponentChange;
import org.apache.myfaces.trinidad.change.ComponentChangeFilter;
import org.apache.myfaces.trinidad.change.RowKeySetAttributeChange;
import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.event.AttributeChangeEvent;
import org.apache.myfaces.trinidad.event.AttributeChangeListener;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.render.CoreRenderer;
import org.apache.myfaces.trinidad.render.ExtendedRenderer;
import org.apache.myfaces.trinidad.render.LifecycleRenderer;
import org.apache.myfaces.trinidad.util.CollectionUtils;
import org.apache.myfaces.trinidad.util.ThreadLocalUtils;


/**
 * Base implementation of components for all of Trinidad.  UIXComponentBase
 * offers a number of features not supplied by the standard UIComponentBase
 * class:
 * <ul>
 * <li>Use of FacesBean for better and easier state saving</li>
 * <li>Support of the LifecycleRenderer class for greater Renderer
 *  control over the lifecycle</li>
 * <li>Built-in support for both the "partialTriggers" attribute
 *   (declarative support for being a PPR target) and for triggering
 *   such components (for being a the source of a PPR-causing event).</li>
 * </ul>
 * <h3>FacesBean and UIXComponentBase</h3>
 * <p>
 * UIXComponentBase differs from UIXComponent most particularly
 * in its use of FacesBeans to store all state.  This offers
 * a number of advantages:
 * <ul>
 * <li>Subclassers - if they use FacesBean for their state as well -
 *   do not need to write overrides of saveState() and restoreState().
 *   </li>
 * <li>State is optimized by default</li>
 * <li>Future optimizations - partly exposed today with
 *    markInitialState() - can offer major state saving improvements.
 * </ul>
 * </p>
 */
// TODO Write Class Javadoc
// TODO Thorough review against UIComponentBase
abstract public class UIXComponentBase extends UIXComponent
{
  // Created up top to ensure it's present while we're processing
  // class initialization code.
  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(UIXComponentBase.class);

  static public final FacesBean.Type TYPE = _createType();
  static public final PropertyKey ID_KEY =
    TYPE.registerKey("id", String.class, PropertyKey.CAP_NOT_BOUND);
  static private final PropertyKey _GENERATED_ID_KEY =
    TYPE.registerKey("_genId", String.class, PropertyKey.CAP_NOT_BOUND);
  static public final PropertyKey RENDERED_KEY =
    TYPE.registerKey("rendered", Boolean.class, Boolean.TRUE);
  static public final PropertyKey BINDING_KEY =
    TYPE.registerKey("binding");
  static public final PropertyKey TRANSIENT_KEY =
    TYPE.registerKey("transient", Boolean.class,
                     PropertyKey.CAP_NOT_BOUND |
                     PropertyKey.CAP_TRANSIENT);
  static public final PropertyKey RENDERER_TYPE_KEY =
    TYPE.registerKey("rendererType", String.class, PropertyKey.CAP_NOT_BOUND);
  static private final PropertyKey _LISTENERS_KEY =
    TYPE.registerKey("listeners", FacesListener[].class, PropertyKey.CAP_LIST);
  static private final PropertyKey _ATTRIBUTE_CHANGE_LISTENER_KEY =
    TYPE.registerKey("attributeChangeListener", MethodExpression.class);
  static private final PropertyKey _COMPONENT_CHANGE_FILTERS_KEY =
    TYPE.registerKey("componentChangeFilters", ComponentChangeFilter[].class, PropertyKey.CAP_LIST);
  // =-=AEW "parent", "rendersChildren", "childCount", "children",
  // "facets", "facetsAndChildren", "family" all are technically
  // bean properties, but they aren't exposed here...

  static
  {
    // Register a couple of PropertyKeys against names that
    // the RI's UIComponentTag implementation is shoving
    // into all components.  This is purely an optimization, but
    // a very useful one.
    TYPE.registerKey("javax.faces.webapp.COMPONENT_IDS",
                     List.class,
                     PropertyKey.CAP_NOT_BOUND);
    TYPE.registerKey("javax.faces.webapp.FACET_NAMES",
                     List.class,
                     PropertyKey.CAP_NOT_BOUND);
    TYPE.lock();
  }

  public UIXComponentBase()
  {
  }

  public UIXComponentBase(String rendererType)
  {
    setRendererType(rendererType);
  }

  protected FacesBean createFacesBean(
    String rendererType)
  {
    FacesBean bean = FacesBeanFactory.createFacesBean(getClass(),
                                                      rendererType);
    UIXFacesBean uixBean = (UIXFacesBean) bean;
    uixBean.init(this, getBeanType());
    return uixBean;
  }

  protected PropertyKey getPropertyKey(String name)
  {
    PropertyKey key = getBeanType().findKey(name);
    if (key == null)
      key = PropertyKey.createPropertyKey(name);

    return key;
  }

  protected FacesBean.Type getBeanType()
  {
    return TYPE;
  }

  @Override
  public FacesBean getFacesBean()
  {
    if (_facesBean == null)
      _init(null);

    return _facesBean;
  }

  @Override
  public String getContainerClientId(FacesContext context, UIComponent child)
  {
    return getContainerClientId(context);
  }

  @Override
  public void addAttributeChangeListener(AttributeChangeListener acl)
  {
    addFacesListener(acl);
  }

  @Override
  public void removeAttributeChangeListener(AttributeChangeListener acl)
  {
    removeFacesListener(acl);
  }

  @Override
  public AttributeChangeListener[] getAttributeChangeListeners()
  {
    return (AttributeChangeListener[])
      getFacesListeners(AttributeChangeListener.class);
  }

  @Override
  public void setAttributeChangeListener(MethodExpression mb)
  {
    setProperty(_ATTRIBUTE_CHANGE_LISTENER_KEY, mb);
  }

  @Deprecated
  public void setAttributeChangeListener(MethodBinding mb)
  {
    setAttributeChangeListener(adaptMethodBinding(mb));
  }

  @Override
  public MethodExpression getAttributeChangeListener()
  {
    return (MethodExpression) getProperty(_ATTRIBUTE_CHANGE_LISTENER_KEY);
  }


  @Override
  public ValueExpression getValueExpression(String name)
  {
    if (name == null)
      throw new NullPointerException();

    PropertyKey key = getPropertyKey(name);

    // Support standard RI behavior where getValueBinding()
    // doesn't complain about being asked for a ValueBinding -
    // but continue supporting strict behavior at FacesBean layer.
    if (!key.getSupportsBinding())
      return null;

    return getFacesBean().getValueExpression(key);
  }

  @Override
  public void setValueExpression(String name,
                                 ValueExpression expression)
  {
    if (name == null)
      throw new NullPointerException();

    if ((expression != null) && expression.isLiteralText())
    {
      ELContext context =
          FacesContext.getCurrentInstance().getELContext();
      getAttributes().put(name, expression.getValue(context));
    }
    else
    {
      PropertyKey key = getPropertyKey(name);
      getFacesBean().setValueExpression(key, expression);
    }
  }



  /**
   */
  @Override
  public ValueBinding getValueBinding(String name)
  {
    if (name == null)
      throw new NullPointerException();

    PropertyKey key = getPropertyKey(name);

    // Support standard RI behavior where getValueBinding()
    // doesn't complain about being asked for a ValueBinding -
    // but continue supporting strict behavior at FacesBean layer.
    if (!key.getSupportsBinding())
      return null;

    return getFacesBean().getValueBinding(key);
  }


  @Override
  public void setValueBinding(String name, ValueBinding binding)
  {
    if (name == null)
      throw new NullPointerException();

    PropertyKey key = getPropertyKey(name);
    getFacesBean().setValueBinding(key, binding);
  }


  @Override
  public Map<String, Object> getAttributes()
  {
    if (_attributes == null)
      _init(null);

    return _attributes;
  }

  /**
   * Adds a change for a Component, or the Component's subtree, returning the change actually added,
   * or <code>null</code>, if no change was added.  The proposed change may be rejected by the
   * component itself, one of its ancestors, or the ChangeManager implementation.
   * @param change     The change to add for this component
   * @return The ComponentChange actually added, or
   * <code>null</code> if no change was added.
   * @see #addComponentChange(UIComponent, ComponentChange)
   */
  public final ComponentChange addComponentChange(ComponentChange change)
  {
    return addComponentChange(this, change);
  }
  
  /**
   * Add a component change filter to this component.
   * When <code>addComponentChange(ComponentChange)</code> method on this component is called, the ComponentChange will
   * be added only if it is accepted by all the component change filters attached to this component as well as those
   * attached to all its ancestors. 
   * @param componentChangeFilter The ComponentChangeFilter instance to add to this component
   * @see #addComponentChange(ComponentChange)
   */
  public final void addComponentChangeFilter(ComponentChangeFilter componentChangeFilter)
  {
    if (componentChangeFilter == null)
      throw new NullPointerException();
    
    getFacesBean().addEntry(_COMPONENT_CHANGE_FILTERS_KEY, componentChangeFilter);
  }
  
  /**
   * Remove a component change filter to this component.
   * @param componentChangeFilter The ComponentChangeFilter instance to remove from this component
   * @see #addComponentChangeFilter(ComponentChangeFilter)
   */
  public final void removeComponentChangeFilter(ComponentChangeFilter componentChangeFilter)
  {
    if (componentChangeFilter == null)
      throw new NullPointerException();
    
    getFacesBean().removeEntry(_COMPONENT_CHANGE_FILTERS_KEY, componentChangeFilter);
  }
  
  /**
  * Returns all the ComponentChangeFilters that are registered with this component.
  *
  * @return An array of registered ComponentChangeFilters
  */
  public final ComponentChangeFilter[] getComponentChangeFilters()
  {
    Iterator<ComponentChangeFilter> filterIter = 
      (Iterator<ComponentChangeFilter>)getFacesBean().entries(_COMPONENT_CHANGE_FILTERS_KEY);
    
    ArrayList<ComponentChangeFilter> filterList = CollectionUtils.arrayList(filterIter);
    return filterList.toArray(new ComponentChangeFilter[filterList.size()]);
  }

  @Override
  protected Iterator<UIComponent> getRenderedFacetsAndChildren(
    FacesContext facesContext)
  {
    _cacheRenderer(facesContext);
    return super.getRenderedFacetsAndChildren(facesContext);
  }

  // ------------------------------------------------------------- Properties



  @Override
  public String getClientId(FacesContext context)
  {
    // NOTE - client ids cannot be cached because the generated
    // value has to be dynamically calculated in some cases (UIData)

    String clientId = getId();
    if (clientId == null)
    {
      clientId = (String) getProperty(_GENERATED_ID_KEY);
      if (clientId == null)
      {
        clientId = context.getViewRoot().createUniqueId();
        setProperty(_GENERATED_ID_KEY, clientId);
      }
    }

    // Search for an ancestor that is a naming container
    UIComponent containerComponent = getParent();
    String origId = (null != containerComponent) ? containerComponent.getId() : null;
    
    while (null != containerComponent)
    {
      if (containerComponent instanceof NamingContainer)
      {
        String contClientId;

        // Pass additional context information to naming containers which extend UIXComponent:
        if (containerComponent instanceof UIXComponent)
          contClientId = ((UIXComponent)containerComponent).getContainerClientId(context, this);
        else
          contClientId = containerComponent.getContainerClientId(context);

        StringBuilder bld = __getSharedStringBuilder();
        bld.append(contClientId).append(NamingContainer.SEPARATOR_CHAR).append(clientId);
        clientId = bld.toString();
        break;
      }

      containerComponent = containerComponent.getParent();
      if (origId != null && origId.equals(containerComponent != null ? containerComponent.getId():null)) 
        throw new IllegalStateException();
    }

    Renderer renderer = getRenderer(context);
    if (null != renderer)
      clientId = renderer.convertClientId(context, clientId);

    return clientId;
  }


  /**
   * Gets the identifier for the component.
   */
  @Override
  public String getId()
  {
    return (String) getProperty(ID_KEY);
  }


  /**
   * Sets the identifier for the component.  The identifier
   * must follow a subset of the syntax allowed in HTML:
   * <ul>
   * <li>Must not be a zero-length String.</li>
   * <li>First character must be an ASCII letter (A-Za-z) or an underscore ('_').</li>
   * <li>Subsequent characters must be an ASCII letter or digit (A-Za-z0-9), an underscore ('_'), or a dash ('-').
   * </ul>
   */
  @Override
  public void setId(String id)
  {
    // =-=AEW Currently, setId() assumes that resetting to
    // the same value *is not* short-circuited.
    _validateId(id);
    // If we're setting the ID to null, don't discard
    // the _GENERATED_ID
    if (id != null)
      setProperty(_GENERATED_ID_KEY, null);

    setProperty(ID_KEY, id);
  }



  @Override
  abstract public String getFamily();


  @Override
  public UIComponent getParent()
  {
    return _parent;
  }


  /**
   * <p>Set the parent <code>UIComponent</code> of this
   * <code>UIComponent</code>.</p>
   *
   * @param parent The new parent, or <code>null</code> for the root node
   *  of a component tree
   */
  @Override
  public void setParent(UIComponent parent)
  {
    _parent = parent;
  }


  @Override
  public boolean isRendered()
  {
    return getBooleanProperty(RENDERED_KEY, true);
  }


  @Override
  public void setRendered(boolean rendered)
  {
    setBooleanProperty(RENDERED_KEY, rendered);
  }

  public boolean isTransient()
  {
    return getBooleanProperty(TRANSIENT_KEY, false);
  }

  public void setTransient(boolean newTransient)
  {
    setBooleanProperty(TRANSIENT_KEY, newTransient);
  }

  @Override
  public String getRendererType()
  {
    // Don't create the FacesBean just to get the renderer type;
    // Generally, rendererType will be the first property
    // set, which will trigger the code below in setRendererType()
    // to run correctly.
    if (_facesBean == null)
      return null;

    return (String) getProperty(RENDERER_TYPE_KEY);
  }

  @Override
  public void setRendererType(String rendererType)
  {
    String oldRendererType = getRendererType();
    if (oldRendererType == null)
    {
      if (rendererType == null)
        return;
    }
    else if (oldRendererType.equals(rendererType))
    {
      return;
    }

    // prepare the faces bean
    _init(rendererType);
    setProperty(RENDERER_TYPE_KEY, rendererType);
  }


  @Override
  public boolean getRendersChildren()
  {
    Renderer renderer = getRenderer(getFacesContext());
    if (renderer == null)
      return false;

    return renderer.getRendersChildren();
  }




  // ------------------------------------------------ Tree Management Methods



  @Override
  public UIComponent findComponent(String id)
  {
    if (id == null)
      throw new NullPointerException();

    if ("".equals(id))
      throw new IllegalArgumentException();

    UIComponent from = this;
    // If it starts with the separator character, it's
    // an absolute path: start at the top
    if (id.charAt(0) == NamingContainer.SEPARATOR_CHAR)
    {
      id = id.substring(1);
      while (from.getParent() != null)
        from = from.getParent();
    }
    // If it's a NamingContainer, start right here
    else if (this instanceof NamingContainer)
    {
      ;
    }
    // Otherwise, go up to look for NamingContainer (or the top)
    else
    {
      while (from.getParent() != null)
      {
        from = from.getParent();
        if (from instanceof NamingContainer)
          break;
      }
    }

    // Evaluate each part of the expression
    String searchId;
    int separatorIndex = id.indexOf(NamingContainer.SEPARATOR_CHAR);
    if (separatorIndex < 0)
      searchId = id;
    else
      searchId = id.substring(0, separatorIndex);

    if (searchId.equals(from.getId()))
    {
      // Don't need to look inside if we're already there
      ;
    }
    else
    {
      from = _findInsideOf(from, searchId);
    }

    // Final ID:  break, and return whatever we've found
    if (separatorIndex < 0)
    {
      return from;
    }
    // Just an intermediate step.  Make sure we're at a NamingContainer,
    // and then ask it to find the rest of the expression.
    else
    {
      if (from == null)
        return null;

      if (!(from instanceof NamingContainer))
        throw new IllegalArgumentException();
      return from.findComponent(id.substring(separatorIndex + 1));
    }
  }



  /**
   * <p>Create (if necessary) and return a List of the children associated
   * with this component.</p>
   */
  @Override
  public List<UIComponent> getChildren()
  {
    if (_children == null)
      _children = new ChildArrayList(this);

    return _children;
  }

  @Override
  public int getChildCount()
  {
    if (_children == null)
      return 0;
    return getChildren().size();
  }


  /**
   * <p>Create (if necessary) and return a Map of the facets associated
   * with this component.</p>
   */
  @Override
  public Map<String, UIComponent> getFacets()
  {

    if (_facets == null)
      _facets = new FacetHashMap(this);

    return _facets;
  }


  @Override
  public UIComponent getFacet(String facetName)
  {
    if (facetName == null)
      throw new NullPointerException();
    if (_facets == null)
      return null;
    return getFacets().get(facetName);
  }


  /**
   * Returns an Iterator over the names of all facets.
   * Unlike getFacets().keySet().iterator(), this does
   * not require instantiating a Map if there are
   * no facets.  (Note that this is not part of the
   * UIComponent API.)
   */
  public Iterator<String> getFacetNames()
  {
    if (_facets == null)
      return _EMPTY_STRING_ITERATOR;
    return _facets.keySet().iterator();
  }

  @Override
  public Iterator<UIComponent> getFacetsAndChildren()
  {
    // =-=AEW Is this supposed to be an immutable Iterator?
    if (_facets == null)
    {
      if (_children == null)
        return _EMPTY_UICOMPONENT_ITERATOR;

      return _children.iterator();
    }
    else
    {
      if (_children == null)
        return _facets.values().iterator();
    }
    
    return new CompositeIterator<UIComponent>(_children.iterator(), _facets.values().iterator());
  }

  // ------------------------------------------- Event processing methods

  @Override
  public void broadcast(FacesEvent event)
    throws AbortProcessingException
  {
    if (event == null)
      throw new NullPointerException();

    if (_LOG.isFine())
      _LOG.fine("Broadcasting event " + event + " to " + this);

    UIComponent component = event.getComponent();
    if (component != null && satisfiesPartialTrigger(event))
    {
      RequestContext adfContext = RequestContext.getCurrentInstance();
      if (adfContext != null)
        adfContext.partialUpdateNotify(component);
    }

    Iterator<FacesListener> iter =
      (Iterator<FacesListener>)getFacesBean().entries(_LISTENERS_KEY);

    while (iter.hasNext())
    {
      FacesListener listener = iter.next();
      if (event.isAppropriateListener(listener))
      {
        event.processListener(listener);
      }
    }

    if (event instanceof AttributeChangeEvent)
    {
      broadcastToMethodExpression(event, getAttributeChangeListener());
    }
  }

  /**
   * Check if a faces event broadcast to this component should trigger the partial updates of the
   * target listeners of this component. By default, all events trigger a partial update of the listeners.
   *
   * @param event The event to check
   * @return true if the partial triggers should be updated by this event being broadcast
   */
  protected boolean satisfiesPartialTrigger(
    FacesEvent event)
  {
    return true;
  }

  // ------------------------------------------- Lifecycle Processing Methods


  @Override
  public void decode(FacesContext context)
  {
    if (context == null)
      throw new NullPointerException();

    // Find all the partialTriggers and save on the context
    Map<String, Object> attrs = getAttributes();
    Object triggers = attrs.get("partialTriggers");
    if (triggers instanceof String[])
    {
      RequestContext adfContext = RequestContext.getCurrentInstance();
      if (adfContext != null)
        adfContext.addPartialTriggerListeners(this, (String[]) triggers);
    }

    __rendererDecode(context);
  }

  @Override
  public void encodeBegin(FacesContext context) throws IOException
  {
    if (context == null)
      throw new NullPointerException();

    if (!isRendered())
      return;

    _cacheRenderer(context);
    Renderer renderer = getRenderer(context);
    // if there is a Renderer for this component
    if (renderer != null)
    {
      renderer.encodeBegin(context, this);
    }
  }

  @Override
  public void encodeChildren(FacesContext context) throws IOException
  {
    if (context == null)
      throw new NullPointerException();

    if (!isRendered())
      return;

    Renderer renderer = getRenderer(context);
    // if there is a Renderer for this component
    if (renderer != null)
    {
      renderer.encodeChildren(context, this);
    }
  }

  @Override
  public void encodeEnd(FacesContext context) throws IOException
  {
    if (context == null)
      throw new NullPointerException();

    if (isRendered())
    {
      Renderer renderer = getRenderer(context);
      // if there is a Renderer for this component
      if (renderer != null)
      {
        renderer.encodeEnd(context, this);
      }
    }
  }

  @Override
  public void queueEvent(FacesEvent event)
  {
    if (event == null)
      throw new NullPointerException();

    UIComponent parent = getParent();
    if (parent == null)
      throw new IllegalStateException();

    parent.queueEvent(event);
  }

  // ----------------------------------------------- Lifecycle Phase Handlers

  @Override
  public void processDecodes(FacesContext context)
  {
    if (context == null)
      throw new NullPointerException();

    if (!isRendered())
      return;

    // Process all facets and children of this component
    decodeChildren(context);

    // Process this component itself
    decode(context);

  }

  @Override
  public void processValidators(FacesContext context)
  {
    if (context == null)
      throw new NullPointerException();

    if (!isRendered())
      return;

    // Process all facets and children of this component
    validateChildren(context);
  }

  @Override
  public void processUpdates(FacesContext context)
  {
    if (context == null)
      throw new NullPointerException();

    if (!isRendered())
      return;

    // Process all facets and children of this component
    updateChildren(context);
  }

  @Override
  public Object processSaveState(FacesContext context)
  {
    if (context == null)
      throw new NullPointerException();

    if (_LOG.isFiner())
      _LOG.finer("processSaveState() on " + this);
    
    Object state = null;
    
    try
    {
      if (((_children == null) || _children.isEmpty()) &&
          ((_facets == null) || _facets.isEmpty()))
      {
        state = saveState(context);
      }
      else
      {
        TreeState treeState = new TreeState();
        treeState.saveState(context, this);
        if (treeState.isEmpty())
          state = null;
  
        state = treeState;
      }
    }
    catch (RuntimeException e)
    {
      _LOG.warning(_LOG.getMessage("COMPONENT_CHILDREN_SAVED_STATE_FAILED", this));
      
      throw e;
    }
    
    // if component state serialization checking is on, attempt to Serialize the
    // component state immediately in order to determine which component's state
    // failed state saving.  Note that since our parent will attempt this same
    // serialization, turning this on is expensive and should only be used once
    // a serialization error has been detected and we want to know which
    // component's state failed
    if (StateUtils.checkComponentStateSerialization(context))
    {
      try
      {
        new ObjectOutputStream(new ByteArrayOutputStream()).writeObject(state);  
      }
      catch (IOException e)
      {
        throw new RuntimeException(_LOG.getMessage("COMPONENT_SAVED_STATE_FAILED", this), e);
      }
    }

    return state;
  }

  // TODO  will have deep problems if UIComponent.saveState() ever
  //   returns a String.
  // TODO crashes and burns if there are fewer children or missing
  //  facets from when state was saved.
  @Override
  public void processRestoreState(FacesContext context, Object state)
  {
    if (context == null)
      throw new NullPointerException();

    if (_LOG.isFiner())
      _LOG.finer("processRestoreState() on " + this);

    // If we saved a "TreeState", use it to restore everything
    if (state instanceof TreeState)
    {
      ((TreeState) state).restoreState(context, this);
    }
    // Otherwise, we had no children or facets, and just use
    // the "state" object
    else
    {
      restoreState(context, state);
    }
  }

  @Override
  public void markInitialState()
  {
    // -= Simon Lessard =-
    // FIXME: Set to true, but never read
    //_initialStateMarked = true;
    getFacesBean().markInitialState();
  }

  public Object saveState(FacesContext context)
  {
    return getFacesBean().saveState(context);
  }

  public void restoreState(FacesContext context, Object stateObj)
  {
    getFacesBean().restoreState(context, stateObj);
  }


  @Override
  public String toString()
  {
    String className = getClass().getName();
    int periodIndex = className.lastIndexOf('.');
    if (periodIndex >= 0)
      className = className.substring(periodIndex + 1);

    return className + "[" + getFacesBean().toString() + ", id=" + getId() + "]";
  }

  /**
   * <p>Return the {@link FacesContext} instance for the current request.</p>
   */
  @Override
  protected FacesContext getFacesContext()
  {
    // If we ever have a way for a component to get notified
    // when it's finished being used for a given request,
    // we could cache this as an instance variable.
    return FacesContext.getCurrentInstance();
  }


  /**
   * Delegates to LifecycleRenderer, if present,
   * otherwise calls decodeChildrenImpl.
   *
   * @param context the current FacesContext
   */
  final protected void decodeChildren(FacesContext context)
  {
    LifecycleRenderer renderer = getLifecycleRenderer(context);
    // if there is a HierarchyRenderer for this component
    if (renderer != null)
    {
      if (renderer.decodeChildren(context, this))
        return;
    }

    decodeChildrenImpl(context);
  }

  /**
   * Calls processDecodes on all facets and children of this
   * component.
   * @param context the current FacesContext
   */
  protected void decodeChildrenImpl(FacesContext context)
  {
    Iterator<UIComponent> kids = getRenderedFacetsAndChildren(context);
    while (kids.hasNext())
    {
      UIComponent kid = kids.next();
      kid.processDecodes(context);
    }
  }


  /**
   * Delegates to LifecycleRenderer, if present,
   * otherwise calls validateChildrenImpl.
   *
   * @param context the current FacesContext
   */
  final protected void validateChildren(FacesContext context)
  {
    LifecycleRenderer renderer = getLifecycleRenderer(context);
    // if there is a ExtendedRenderer for this component
    if (renderer != null)
    {
      if (renderer.validateChildren(context, this))
        return;
    }

    validateChildrenImpl(context);
  }

  /**
   * Calls processValidators on all facets and children of this
   * component.
   * @param context the current FacesContext
   */
  protected void validateChildrenImpl(FacesContext context)
  {
    // Process all the facets and children of this component
    Iterator<UIComponent> kids = getRenderedFacetsAndChildren(context);
    while (kids.hasNext())
    {
      UIComponent kid = kids.next();
      kid.processValidators(context);
    }
  }


  /**
   * Delegates to LifecycleRenderer, if present,
   * otherwise calls upateChildrenImpl.
   *
   * @param context the current FacesContext
   */
  final protected void updateChildren(FacesContext context)
  {
    LifecycleRenderer renderer = getLifecycleRenderer(context);
    // if there is a ExtendedRenderer for this component
    if (renderer != null)
    {
      if (renderer.updateChildren(context, this))
        return;
    }

    updateChildrenImpl(context);
  }

  protected void updateChildrenImpl(FacesContext context)
  {
    // Process all the facets and children of this component
    Iterator<UIComponent> kids = getRenderedFacetsAndChildren(context);
    while (kids.hasNext())
    {
      UIComponent kid = kids.next();
      kid.processUpdates(context);
    }
  }

  @Override
  protected void addFacesListener(FacesListener listener)
  {
    if (listener == null)
      throw new NullPointerException();

    getFacesBean().addEntry(_LISTENERS_KEY, listener);
  }

  @Override
  protected void removeFacesListener(FacesListener listener)
  {
    if (listener == null)
      throw new NullPointerException();

    getFacesBean().removeEntry(_LISTENERS_KEY, listener);
  }

  @Override
  protected FacesListener[] getFacesListeners(Class clazz)
  {
    if (clazz == null)
      throw new NullPointerException();

    if (!FacesListener.class.isAssignableFrom(clazz))
      throw new IllegalArgumentException();

    return (FacesListener[])
       getFacesBean().getEntries(_LISTENERS_KEY, clazz);
  }

  /**
   * Checks if any of the ComponentChangeFilter instances that is attached to this component rejects the supplied
   * change for the supplied component.
   */
  private boolean _isAnyFilterRejectingChange(UIComponent uic, ComponentChange cc)
  {
    // assume we accept the change
    boolean rejectsChange = false;
    
    Iterator<ComponentChangeFilter> iter =
      (Iterator<ComponentChangeFilter>)getFacesBean().entries(_COMPONENT_CHANGE_FILTERS_KEY);

    while (iter.hasNext())
    {
      ComponentChangeFilter currentFilter = iter.next();
      if (currentFilter.accept(cc, uic) == ComponentChangeFilter.Result.REJECT)
      {
        // one of the filter rejected the change, look no further
        rejectsChange = true;
        break;
      }
    }
    
    return rejectsChange;
  }
  
  private UIXComponentBase _getNextUIXComponentBaseAnxcestor()
  {
    UIComponent parent = getParent();

    while (parent != null)
    {    
      if (parent instanceof UIXComponentBase)
      {
        return (UIXComponentBase)parent;
      }
      
      parent = parent.getParent();
    }
    
    return null;
  }

  /**
   * Called when adding a change to a Component, or the Component's subtree.
   * The default implementation delegates the call to the parent, if possible, otherwise
   * it adds the change to the ChangeManager directly.
   * Subclasses can override this method to among other things, filter or transform the changes.
   * @param component  The component that the change is for
   * @param change     The change to add for this component
   * @return The ComponentChange actually added, or
   * <code>null</code> if no change was added.
   * @see #addComponentChange(ComponentChange)
   * @see #addAttributeChange
   */
  protected ComponentChange addComponentChange(UIComponent component, ComponentChange change)
  {
    // check moved from addAttributeChange(), as this is more central
    if ((component == this) && (change instanceof AttributeComponentChange))
    {
      AttributeComponentChange aa             = (AttributeComponentChange)change;
      Object                   attributeValue = aa.getAttributeValue();
      
      if (attributeValue instanceof RowKeySet)
      {
        change = new RowKeySetAttributeChange(getClientId(getFacesContext()),
                                              aa.getAttributeName(),
                                              attributeValue);
      }
    }
    
    // add the change unless we have a change filter that is attached to this component wants to supress the change
    if (!_isAnyFilterRejectingChange(component, change))
    {
      UIXComponentBase nextUIXParent = _getNextUIXComponentBaseAnxcestor();
  
      if (nextUIXParent != null)
      {
        return nextUIXParent.addComponentChange(component, change);
      }
      else
      {
        RequestContext trinContext = RequestContext.getCurrentInstance();
        trinContext.getChangeManager().addComponentChange(getFacesContext(), component, change);
        return change;
      }
    }
    else
    {
      return null;
    }
  }

  /**
   * Convenience function for
   * <code>addComponentChange(new AttributeComponentChange(attributeName, attributeValue));</code>
   * This function is not <code>final</code> for backwards compatibility reasons, however,
   * existing subclassers whould override <code>addComponentChange</code> instead.
   * @param attributeName
   * @param attributeValue
   * @see #addComponentChange(UIComponent, ComponentChange)
   */
  protected void addAttributeChange(
    String attributeName,
    Object attributeValue)
  {
    addComponentChange(new AttributeComponentChange(attributeName, attributeValue));
  }

  void __rendererDecode(FacesContext context)
  {
    _cacheRenderer(context);
    Renderer renderer = getRenderer(context);
    // if there is a Renderer for this component
    if (renderer != null)
    {
      renderer.decode(context, this);
    }
  }

  private void _cacheRenderer(FacesContext context)
  {
    Renderer renderer = _getRendererImpl(context);
    _cachedRenderer = renderer;

    // cache the lifecycle renderer
    if (renderer instanceof LifecycleRenderer)
    {
      _cachedLifecycleRenderer = (LifecycleRenderer)renderer;
    }
    else
    {
      _cachedLifecycleRenderer = null;
    }
  }

  private Renderer _getRendererImpl(FacesContext context)
  {
    String rendererType = getRendererType();
    if (rendererType != null)
    {
      RenderKit renderKit = context.getRenderKit();
      Renderer renderer = renderKit.getRenderer(getFamily(), rendererType);
      if (renderer == null)
      {
        _LOG.warning("CANNOT_FIND_RENDERER", new Object[]{this, rendererType});
      }

      return renderer;
    }

    return null;
  }

  private LifecycleRenderer _getLifecycleRendererImpl(FacesContext context)
  {
    Renderer renderer = _getRendererImpl(context);
    if (renderer instanceof LifecycleRenderer)
    {
      return (LifecycleRenderer)renderer;
    }

    return null;
  }

  @Override
  protected Renderer getRenderer(FacesContext context)
  {
    Renderer renderer = _cachedRenderer;
    if (renderer != _UNDEFINED_RENDERER)
      return renderer;

    return _getRendererImpl(context);
  }

  protected LifecycleRenderer getLifecycleRenderer(FacesContext context)
  {
    LifecycleRenderer renderer = _cachedLifecycleRenderer;
    if (renderer != _UNDEFINED_LIFECYCLE_RENDERER)
      return renderer;

    return _getLifecycleRendererImpl(context);

  }

  protected void setProperty(PropertyKey key, Object value)
  {
    getFacesBean().setProperty(key, value);
  }

  protected Object getProperty(PropertyKey key)
  {
    return getFacesBean().getProperty(key);
  }

  protected void setBooleanProperty(PropertyKey key, boolean value)
  {
    getFacesBean().setProperty(key, value ? Boolean.TRUE : Boolean.FALSE);
  }

  protected boolean getBooleanProperty(PropertyKey key, boolean defaultValue)
  {
    Object o = getFacesBean().getProperty(key);
    if (defaultValue)
      return !Boolean.FALSE.equals(o);
    else
      return Boolean.TRUE.equals(o);
  }

  protected void setIntProperty(PropertyKey key, int value)
  {
    getFacesBean().setProperty(key, Integer.valueOf(value));
  }

  protected int getIntProperty(PropertyKey key, int defaultValue)
  {
    Number n = (Number) getFacesBean().getProperty(key);
    if (n == null)
      return defaultValue;

    return n.intValue();
  }


  /**
   * Return the number of facets.  This is more efficient than
   * calling getFacets().size();
   */
  @Override
  public int getFacetCount()
  {
    if (_facets == null)
      return 0;

    return _facets.size();
  }


  /**
   * Broadcast an event to a MethodBinding.
   * This can be used to support MethodBindings such as the "actionListener"
   * binding on ActionSource components:
   * &lt;tr:commandButton actionListener="#{mybean.myActionListener}">
   * @deprecated
   */
  protected final void broadcastToMethodBinding(
    FacesEvent event,
    MethodBinding method) throws AbortProcessingException
  {
    if (method != null)
    {
      try
      {
        FacesContext context = getFacesContext();
        method.invoke(context, new Object[] { event });
      }
      catch (EvaluationException ee)
      {
        // Checking for AbortProcessingExceptions, and unwrapping
        // it if the underlying exception is AbortProcessingExceptions.
        Throwable currentThrowable = ee.getCause();
        while (currentThrowable != null)
        {
          if (currentThrowable instanceof AbortProcessingException)
          {
            throw ((AbortProcessingException)currentThrowable);
          }
          currentThrowable = currentThrowable.getCause();
        }
        throw ee;
      }
    }
  }

  /**
   * Given a MethodBinding, create a MethodExpression that
   * adapts it.
   */
  static public MethodExpression adaptMethodBinding(MethodBinding binding)
  {
    return new MethodBindingMethodExpression(binding);
  }

  /**
   * Broadcast an event to a MethodExpression.
   * This can be used to support MethodBindings such as the "actionListener"
   * binding on ActionSource components:
   * &lt;tr:commandButton actionListener="#{mybean.myActionListener}">
   */
  protected final void broadcastToMethodExpression(
    FacesEvent event,
    MethodExpression method) throws AbortProcessingException
  {
    if (method != null)
    {
      try
      {
        FacesContext context = getFacesContext();
        method.invoke(context.getELContext(), new Object[] { event });
      }
      catch (ELException ee)
      {
        Throwable t = ee.getCause();
        // Unwrap AbortProcessingExceptions
        if (t instanceof AbortProcessingException)
          throw ((AbortProcessingException) t);
        throw ee;
      }
    }
  }

  /**
   * Convenience method to call <code>invokeOnComponent</code> on all of the
   * children of a component.  This is useful when a component sometimes optimizes
   * away calling <code>invokeOnComponent</code> on its children
   */
  protected final boolean invokeOnChildrenComponents(
    FacesContext context,
    String clientId,
    ContextCallback callback)
    throws FacesException
  {
    Iterator<UIComponent> children = getFacetsAndChildren();
    
    boolean found = false;
    
    while (children.hasNext() && !found)
    {
      found = children.next().invokeOnComponent(context, clientId, callback);
    }
    
    return found;
  }

  /**
   * <p>
   * Optimized implementation of <code>invokeOnComponent</code> for NamingContainers.
   * If the clientId isn't within the NamingContainer, invocation of the
   * NamingContainer's children is skipped.
   * </p>
   * <p>Subclasses implementing NamingContainer should override
   * <code>invokeOnComponent</code> and delegate to this method.</p>
   */
  protected final boolean invokeOnNamingContainerComponent(
    FacesContext context,
    String clientId,
    ContextCallback callback)
    throws FacesException
  {
    assert this instanceof NamingContainer : "Only use invokeOnNamingContainerComponent on NamingContainers";
    
    String thisClientId = getClientId(context);

    if (clientId.equals(thisClientId))
    {
      // this is the component we want, so invoke the callback
      callback.invokeContextCallback(context, this);
      return true;
    }
    else
    {
      // if this is a NamingContainer, only traverse into it if the clientId we are looking for
      // is inside of it
      if ((!clientId.startsWith(thisClientId) ||
          (clientId.charAt(thisClientId.length()) != NamingContainer.SEPARATOR_CHAR)))
      {
        return false;
      }

      boolean invokedComponent = false;
      
      // set up the context for visiting the children
      setupVisitingContext(context);
            
      try
      {
        // iterate through children. We inline this code instead of calling super in order
        // to avoid making an extra call to getClientId().
        invokedComponent = invokeOnChildrenComponents(context, clientId, callback);
      }
      finally
      {
        // teardown the context now that we have visited the children
        tearDownVisitingContext(context);
      }
      
      return invokedComponent;
    }
  }
  

  /**
   * Override to calls the hooks for setting up and tearing down the
   * context before the children are visited.
   * @see #setupVisitingContext
   * @see #tearDownVisitingContext
   */
  @Override
  public boolean invokeOnComponent(
    FacesContext context,
    String clientId,
    ContextCallback callback)
    throws FacesException
  {    
    String thisClientId = getClientId(context);

    if (clientId.equals(thisClientId))
    {
      callback.invokeContextCallback(context, this);
      return true;
    }
    else
    {
      boolean invokedComponent = false;
      
      // set up the context for visiting the children
      setupVisitingContext(context);
            
      try
      {
        // iterate through children. We inline this code instead of calling super in order
        // to avoid making an extra call to getClientId().
        invokedComponent = invokeOnChildrenComponents(context, clientId, callback);
      }
      finally
      {
        // teardown the context now that we have visited the children
        tearDownVisitingContext(context);
      }
      
      return invokedComponent;
    }
  }



  /**
   * <p>
   * This gets a single threadlocal shared stringbuilder instance, each time you call
   * __getSharedStringBuilder it sets the length of the stringBuilder instance to 0.
   * </p><p>
   * This allows you to use the same StringBuilder instance over and over.
   * You must call toString on the instance before calling __getSharedStringBuilder again.
   * </p>
   * Example that works
   * <pre><code>
   * StringBuilder sb1 = __getSharedStringBuilder();
   * sb1.append(a).append(b);
   * String c = sb1.toString();
   *
   * StringBuilder sb2 = __getSharedStringBuilder();
   * sb2.append(b).append(a);
   * String d = sb2.toString();
   * </code></pre>
   * <br><br>
   * Example that doesn't work, you must call toString on sb1 before
   * calling __getSharedStringBuilder again.
   * <pre><code>
   * StringBuilder sb1 = __getSharedStringBuilder();
   * StringBuilder sb2 = __getSharedStringBuilder();
   *
   * sb1.append(a).append(b);
   * String c = sb1.toString();
   *
   * sb2.append(b).append(a);
   * String d = sb2.toString();
   * </code></pre>
   *
   */
  static StringBuilder __getSharedStringBuilder()
  {
    StringBuilder sb = _STRING_BUILDER.get();

    if (sb == null)
    {
      sb = new StringBuilder();
      _STRING_BUILDER.set(sb);
    }

    // clear out the stringBuilder by setting the length to 0
    sb.setLength(0);

    return sb;
  }

  /**
   * render a component. this is called by renderers whose
   * getRendersChildren() return true.
   * @deprecated {@link UIComponent#encodeAll(FacesContext)} should be used instead of this method
   */
  @Deprecated
  void __encodeRecursive(FacesContext context, UIComponent component)
    throws IOException
  {
    component.encodeAll(context);
  }


  static private UIComponent _findInsideOf(
    UIComponent from,
    String id)
  {
    Iterator<UIComponent> kids = from.getFacetsAndChildren();
    while (kids.hasNext())
    {
      UIComponent kid = kids.next();
      if (id.equals(kid.getId()))
        return kid;

      if (!(kid instanceof NamingContainer))
      {
        UIComponent returned = _findInsideOf(kid, id);
        if (returned != null)
          return returned;
      }
    }

    return null;
  }

  /**
   * <p>Verify that the specified component id is safe to add to the tree.
   * </p>
   *
   * @param id The proposed component id to check for validity
   *
   * @exception IllegalArgumentException if <code>id</code>
   *  is <code>null</code> or contains invalid characters
   */
  private void _validateId(String id)
  {
    if (id == null)
      return;


    int n = id.length();
    if (0 == n ||
        NamingContainer.SEPARATOR_CHAR == id.charAt(0))
      _throwBadId(id);

    for (int i = 0; i < n; i++)
    {
      char c = id.charAt(i);
      if (i == 0)
      {
        if (!Character.isLetter(c) && (c != '_'))
          _throwBadId(id);
      }
      else
      {
        if (!(Character.isLetter(c) ||
              Character.isDigit(c) ||
              (c == '-') || (c == '_')))
        {
          _throwBadId(id);
        }
      }
    }
  }

  private void _throwBadId(String id)
  {
    throw new IllegalArgumentException(_LOG.getMessage(
      "ILLEGAL_ID", id));
  }

  private void _init(
    String rendererType)
  {
    FacesBean oldBean = _facesBean;
    _facesBean = createFacesBean(rendererType);
    if (oldBean != null)
      _facesBean.addAll(oldBean);

    _attributes = new ValueMap(_facesBean);
  }

  private FacesBean                _facesBean;
  private List<UIComponent>        _children;
  private Map<String, Object>      _attributes;
  private Map<String, UIComponent> _facets;
  private UIComponent              _parent;

  // Cached instance of the Renderer for this component.
  // The instance will be re-retrieved in encodeBegin()
  private transient Renderer _cachedRenderer = _UNDEFINED_RENDERER;
  private transient LifecycleRenderer _cachedLifecycleRenderer =
                                                _UNDEFINED_LIFECYCLE_RENDERER;

  // -= Simon Lessard =-
  // FIXME: _initialStateMarked is never read
  //        So commented out, is that ok? If so, this attribute should be deleted
  //private transient boolean _initialStateMarked;

  private static final Iterator<String> _EMPTY_STRING_ITERATOR =
    new EmptyIterator<String>();

  private static final Iterator<UIComponent> _EMPTY_UICOMPONENT_ITERATOR =
    new EmptyIterator<UIComponent>();


  static private final ThreadLocal<StringBuilder> _STRING_BUILDER =
                                                          ThreadLocalUtils.newRequestThreadLocal();

  static private FacesBean.Type _createType()
  {
    try
    {
      ClassLoader cl = _getClassLoader();
      URL url = cl.getResource("META-INF/faces-bean-type.properties");
      if (url != null)
      {
        Properties properties = new Properties();
        InputStream is = url.openStream();
        try
        {
          properties.load(is);
          String className = (String)
            properties.get(UIXComponentBase.class.getName());
          return (FacesBean.Type) cl.loadClass(className).newInstance();
        }
        finally
        {
          is.close();
        }
      }
    }
    catch (Exception e)
    {
      _LOG.severe("CANNOT_LOAD_TYPE_PROPERTIES", e);
    }

    // For testing purposes, return a valid Type
    return new FacesBean.Type();
  }

  static private ClassLoader _getClassLoader()
  {
    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    if (loader == null)
      loader = FacesBeanFactory.class.getClassLoader();
    return loader;
  }

  static private class RendererImpl extends Renderer
  {
  }

  static private class ExtendedRendererImpl extends ExtendedRenderer
  {
  }

  private static class EmptyIterator<T> implements Iterator<T>
  {
    public boolean hasNext()
    {
      return false;
    }

    public T next()
    {
      throw new NoSuchElementException();
    }

    public void remove()
    {
      throw new UnsupportedOperationException();
    }

  }

  static private final LifecycleRenderer _UNDEFINED_LIFECYCLE_RENDERER =
                                                new ExtendedRendererImpl();
  static private final Renderer _UNDEFINED_RENDERER = new RendererImpl();
}
