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
package org.apache.myfaces.trinidadinternal.taglib;

import java.io.Serializable;

import java.lang.reflect.Array;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import javax.el.ELContext;
import javax.el.PropertyNotWritableException;
import javax.el.ValueExpression;
import javax.el.VariableMapper;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.webapp.UIComponentClassicTagBase;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.JspTagException;
import javax.servlet.jsp.tagext.JspIdConsumer;

import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.webapp.TrinidadIterationTag;


//JSTL Core Library - <c:forEach> Tag
//===================================
//Syntax 1: Iterate over a collection of objects
//
//<c:forEach [var="varName "] items="collection"
//  [varStatus="varStatusName"]
//  [begin="begin"] [end="end"] [step=" step"]>
//  body content
//</c:forEach>
//
//Syntax 2: Iterate a fixed number of times
//
//<c:forEach [var="varName"]
//  [varStatus="varStatusName"]
//  begin=" begin" end="end" [step="step"]>
//  body content
//</c:forEach>

/**
 * Trinidad JSP for each tag that is based on the JSTL c:forEach tag
 * but provides additinal functionality.
 */
public class ForEachTag
  extends TrinidadIterationTag
  implements JspIdConsumer
{
  public ForEachTag()
  {
    _LOG.finest("ForEachTag instance created");
  }

  public void setItems(ValueExpression items)
  {
    if (items.isLiteralText())
      throw new IllegalArgumentException(_LOG.getMessage(
        "MUST_BE_SIMPLE_JSF_EL_EXPRESSION"));
    _items = items;
  }

  public void setBegin(ValueExpression begin)
  {
    _beginVE = begin;
  }

  public void setEnd(ValueExpression end)
  {
    _endVE = end;
  }

  public void setStep(ValueExpression step)
  {
    _stepVE = step;
  }

  public void setVar(String var)
  {
    _var = var;
  }

  public void setVarStatus(String varStatus)
  {
    _varStatus = varStatus;
  }

  @Override
  public void setJspId(String id)
  {
    _LOG.finest("setJspId called with ID {0}", id);
    FacesContext facesContext = FacesContext.getCurrentInstance();

    // Use an atomic integer to use for tracking how many times a for each loop has been
    // created for any JSP page during the current request. Unfortunately there is no hook to
    // tie the include to the page that is being included to make this page based.
    AtomicInteger includeCounter = (AtomicInteger)facesContext.getAttributes()
      .get(_INCLUDE_COUNTER_KEY);

    if (includeCounter == null)
    {
      // If the include counter is null, that means that this is the first for each tag processed
      // during this request.
      includeCounter = new AtomicInteger(0);
      facesContext.getAttributes().put(_INCLUDE_COUNTER_KEY, includeCounter);
    }

    Integer pageContextCounter = (Integer)pageContext.getAttribute(_INCLUDE_COUNTER_KEY);
    if (pageContextCounter == null)
    {
      // In this case, the page context has not been seen before. This means that this is the first
      // for each tag in this page (the actual jspx file, not necessarily the requested one).
      pageContextCounter = includeCounter.incrementAndGet();
      pageContext.setAttribute(_INCLUDE_COUNTER_KEY, pageContextCounter);
      _LOG.finest("Page context not seen before. Using counter value {0}", pageContextCounter);
    }
    else
    {
      _LOG.finest("Page context has already been seen. Using counter value {0}",
        includeCounter);
    }

    // If the view attributes are null, then this is the first time this method has been called
    // for this request.
    if (_viewAttributes == null)
    {
      String pcId = includeCounter.toString();

      // The iteration map key is a key that will allow us to get the map for this tag instance,
      // separated from other ForEachTags, that will map an iteration ID to the IterationMetaData
      // instances. EL will use this map to get to the IterationMetaData and the indirection will
      // allow the IterationMetaData to be updated without having to update the EL expressions.
      _iterationMapKey = new StringBuilder(_VIEW_ATTR_KEY_LENGTH + id.length() + pcId.length() + 1)
        .append(_VIEW_ATTR_KEY)
        .append(pcId)
        .append('.')
        .append(id)
        .toString();

      // store the map into the view attributes to put it in a location that the EL expressions
      // can access for not only the remainder of this request, but also the next request.
      UIViewRoot viewRoot = facesContext.getViewRoot();

      // We can cache the view attributes in the tag as a JSP tag marked with JspIdConsumer
      // is never reused.
      _viewAttributes = viewRoot.getAttributes();

      @SuppressWarnings("unchecked")
      Map<Integer, IterationMetaData> iterMap = (Map<Integer, IterationMetaData>)
        _viewAttributes.get(_iterationMapKey);
      if (iterMap == null)
      {
        _iterationMap = new HashMap<Integer, IterationMetaData>();
        _LOG.finest("Created a new iteration map for key {0}", _iterationMapKey);
        _viewAttributes.put(_iterationMapKey, _iterationMap);
      }
      else
      {
        // Clear the existing map so that the iteration IDs are cleared from the previous
        // request to avoid caching of the old data in the ForEachBaseValueExpression instances
        _iterationMap = iterMap;
        iterMap.clear();
      }
    }
  }

  @Override
  protected int doStartTagImpl()
    throws JspException
  {
    _LOG.finest("doStartTagImpl called");
    _validateAttributes();

    FacesContext facesContext = FacesContext.getCurrentInstance();
    int          length;

    _currentBegin = (_begin == null) ? 0 : _begin.intValue();
    _isFirst = true;

    if (null != _items)
    {
      // AdamWiner: for reasons I cannot yet explain, using the JSP's
      // ELContext is giving me big problems trying to grab Lists
      // from inside of managed beans.  Switching this one call
      // to the JSF ELContext seems to resolve that.  We certainly
      // have to use the JSPs ELResolver for calling through
      // to the VariableMapper
      Object items = _items.getValue(facesContext.getELContext());//pageContext.getELContext());

      //pu: If items is specified and resolves to null, it is treated as an
      //  empty collection, i.e., no iteration is performed.
      if (items == null)
      {
        _LOG.fine("Items expression {0} resolved to null.", _items);
        return SKIP_BODY;
      }

      // Build a wrapper around the items so that a common API can be used to interact with
      // the items regardless of the type.
      _itemsWrapper = _buildItemsWrapper(items);
      length = _itemsWrapper.getSize();

      if (length == 0)
      {
        _LOG.fine("Items found at {0} is empty.", _items);
        return SKIP_BODY;
      }

      //pu: If valid 'items' was specified, and so was 'begin', get out if size
      //  of collection were to be less than the begin. A mimic of c:forEach.
      if (length < _currentBegin)
      {
        _LOG.fine("Size of 'items' is less than 'begin'");
        return SKIP_BODY;
      }

      _currentEnd = (_end == null) ? length - 1 : _end.intValue();
      //pu: If 'end' were specified, but is beyond the size of collection, limit
      //  the iteration to where the collection ends. A mimic of c:forEach and
      //  fix for bug 4029853.
      if (length <= _currentEnd)
      {
        _currentEnd = length - 1;
      }
    }
    else
    {
      _currentEnd = (_end == null) ? 0 : _end.intValue();
    }

    _currentIndex = _currentBegin;
    _currentCount = 1;
    _currentStep = (_step == null) ? 1 : _step.intValue();

    //pu: Now check the valid relation between 'begin','end' and validity of 'step'
    _validateRangeAndStep();

    // If we can bail, do it now
    if (_currentEnd < _currentIndex)
    {
      return SKIP_BODY;
    }

    _isLast = _currentIndex == _currentEnd;

    // Save off the previous deferred variables
    VariableMapper vm = pageContext.getELContext().getVariableMapper();

    if (_var != null)
    {
      // Store off the current variable so that it may be restored after tag processing
      _previousDeferredVar = vm.resolveVariable(_var);
    }

    if (_LOG.isFiner())
    {
      _LOG.finer("Iterating from {0} to {1} by {2}",
        new Object[] { _currentIndex, _currentEnd, _currentStep });
    }

    _parentComponent = _getParentComponent();

    // Remember the non-Trinidad components before execution so that we may determine which
    // are created during this tags for each execution
    _previousIterationNonTrinidadChildren = _getNonTrinidadChildren();

    _updateVars(vm, true);

    return EVAL_BODY_INCLUDE;
  }

  @Override
  public int doAfterBody()
  {
    _LOG.finest("doAfterBody processing");

    // Process any non-Trinidad components that were added during the last body execution
    _processNonTrinidadComponents();

    _currentIndex += _currentStep;
    ++_currentCount;
    _isFirst = false;
    _isLast = _currentIndex == _currentEnd;

    VariableMapper vm = pageContext.getELContext().getVariableMapper();

    // If we're at the end, bail
    if (_currentEnd < _currentIndex)
    {
      // Restore EL state
      if (_var != null)
        vm.setVariable(_var, _previousDeferredVar);
      if (_varStatus != null)
        vm.setVariable(_varStatus, _previousDeferredVarStatus);

      _previousIterationNonTrinidadChildren = null;

      return SKIP_BODY;
    }

    // Otherwise, update the variables and go again
    _updateVars(vm, true);

    return EVAL_BODY_AGAIN;
  }

  /**
   * Release state.
   */
  @Override
  public void release()
  {
    super.release();
    _begin = null;
    _end = null;
    _step = null;
    _items = null;
    _var = null;
    _varStatus = null;
    _previousDeferredVar = null;
    _previousDeferredVarStatus = null;

    _LOG.finest("release called");
    _iterationId = null;
    _iterationData = null;
    _viewAttributes = null;
    _iterationMapKey = null;
    _iterationMap = null;
    _itemsWrapper = null;

    _previousIterationNonTrinidadChildren = null;

    _parentComponent = null;
}

  @Override
  public final void childComponentProcessed(
    UIComponent component)
  {
    // This code is called when a component is created or found, see which it is.
    // We are only interested in components that are directly under our parent.
    if (component.getParent() == _parentComponent)
    {
      Map<String, Object> compAttrs = component.getAttributes();
      Integer iterationId = (Integer)compAttrs.get(_ITERATION_ID_KEY);
      if (_LOG.isFinest())
      {
        _LOG.finest(
          "childComponentProcessed: {0} ({1}). Previous component iteration ID: {2}",
          new Object[] { component.getClass().getName(), component.getClientId(), iterationId });
      }

      if (iterationId == null)
      {
        // This is a new component, use the current iteration ID
        compAttrs.put(_ITERATION_ID_KEY, _iterationId);

        // Remember that the iteration ID was used
        _iterationIdRequiresIncrement = true;

        if (_LOG.isFinest())
        {
          _LOG.finest("New component processed.\n" +
            "  Iteration ID  : {0}\n" +
            "  Iteration data: {1}",
            new Object[] { _iterationId, _iterationData });
        }
      }
      else
      {
        if (_LOG.isFinest())
        {
          _LOG.finest("Component found with existing iteration ID.\n" +
            "  Iteration ID  : {0}\n" +
            "  Iteration data: {1}",
            new Object[] { iterationId, _iterationData });
        }

        // This component has been seen before, register the old iteration ID with the iteration
        // map so that the EL may look up the iteration data.
        _iterationMap.put(iterationId, _iterationData);
      }
    }
  }

  @Override
  public final void afterChildComponentProcessed(
    UIComponent component)
  {
    // This code is called when a component is created or found, see which it is.
    // We are only interested in components that are directly under our parent.
    if (component.getParent() == _parentComponent)
    {
      if (_LOG.isFinest())
      {
        _LOG.finest("afterChildComponentProcessed on component {0} ({1})",
          new Object[] { component.getClass().getName(), component.getClientId() });
      }
      // Store a unique iteration ID in each component. That way, if a component is ever moved
      // from one iteration to another between requests, but not all the components, no problems
      // will ensue. The use case is that ${} is used in the ID of one or more child components
      // of a for each loop to pin the component to the item in the collection rather than the
      // for each index.
      if (_iterationIdRequiresIncrement)
      {
        VariableMapper vm = pageContext.getELContext().getVariableMapper();
        _updateVars(vm, false);
        _iterationIdRequiresIncrement = false;
      }
    }
  }

  private Set<UIComponent> _getNonTrinidadChildren()
  {
    if (_parentComponent == null)
    {
      return Collections.emptySet();
    }

    Set<UIComponent> components = new HashSet<UIComponent>(_parentComponent.getChildCount());
    for (UIComponent child : _parentComponent.getChildren())
    {
      if (child instanceof UIXComponent)
      {
        continue;
      }

      components.add(child);
    }

    return components;
  }

  private UIComponent _getParentComponent()
  {
    UIComponentClassicTagBase tag = UIComponentClassicTagBase.getParentUIComponentClassicTagBase(
                                      pageContext);
    return tag == null ? null : tag.getComponentInstance();
  }

  /**
   * Get the key for the current item in the items. For non-key based collections, this is the
   * index. If there is no items attribute, this simply returns the current index as well.
   *
   * @return the key or index
   */
  private Serializable _getKey()
  {
    return (_itemsWrapper == null) ?
      _currentIndex :
      _asSerializable(_itemsWrapper.getKey(_currentIndex));
  }

  /**
   * Although in-efficient in how we process UIXComponent children, this function allows
   * non-Trinidad components to correctly map their value expressions to the iteration of the
   * for each loop better than the JSTL tag does in JSF. In order to do this, this function must
   * determine what non-UIXComponents belong to the current iteration of the loop.
   *
   * @see #afterChildComponentProcessed(UIComponent)
   * @see #childComponentProcessed(UIComponent)
   */
  private void _processNonTrinidadComponents()
  {
    Serializable key = null;

    // If _previousIterationNonTrinidadChildren is non-null then this is not the first
    // execution of this code in this request. We need to determine what components were added
    // during this iteration
    Set<UIComponent> childrenComponents = _getNonTrinidadChildren();
    if (childrenComponents.isEmpty())
    {
      _previousIterationNonTrinidadChildren.clear();
      return;
    }

    for (UIComponent child : childrenComponents)
    {
      Map<String, Object> attrs = child.getAttributes();
      if (_previousIterationNonTrinidadChildren.contains(child))
      {
        // This child component is either one that was created in a previous request, or
        // one that was not created by this for each loop, we need to determine which one.
        NonTrinidadIterationData data = (NonTrinidadIterationData)attrs.get(_iterationMapKey);

        // If the data is null, then this for each tag did not create the component and we
        // do not need to do anything.
        if (data == null)
        {
          continue;
        }

        // Get the key for the current item, if we have not already
        if (key == null)
        {
          key = _getKey();
        }

        // Since we clear the iteration map in the start tag processing, we need to re-map
        // the iteration ID from the component back to the current iteration data.
        // First, we need to ensure that this component belongs to the current iteration. If
        // it doesn't, we do not need to do anything as the component "belongs" to a different
        // iteration
        if (key.equals(data.getKey()))
        {
          // The keys are the same, update the map.
          _iterationMap.put(data.getIterationId(), _iterationData);
        }
      }
      else
      {
        // This is a component that was added to the parent while this for each loop was processing
        // the last iteration. We need to record the key and the iteration ID so that we can map
        // this component to its var status

        // Get the key for the current item, if we have not already
        if (key == null)
        {
          key = _getKey();
        }

        attrs.put(_iterationMapKey, new NonTrinidadIterationData(key, _iterationId));
      }
    }

    // Update the map so we know for the next iteration what components are being created
    _previousIterationNonTrinidadChildren = childrenComponents;
  }

  // Push new values into the VariableMapper and the pageContext
  private void _updateVars(
    VariableMapper vm,
    boolean        createNewIterationData)
  {
    Serializable key = null;

    // Generate a new iteration ID
    _updateIterationId();

    if (_var != null)
    {
      // Catch programmer error where _var has been set but _items has not
      if (_items != null)
      {
        // Determine if we need to use a key or an index based value expression
        // for the current items.
        ValueExpression expr;
        if (_itemsWrapper.isKeyBased())
        {
          // Use a key to get the value
          key = _getKey();
          expr = new KeyedValueExpression(_items, _getKey());
        }
        else
        {
          // Use indirection to get the index from the iteration data using the iteration ID
          // so that the expression is not hard-coded to one index. This allows the tag to
          // support components that are re-ordered (index changes during multiple requests)
          expr = new IndexedValueExpression(_iterationId, _iterationMapKey, _items);
        }

        vm.setVariable(_var, expr);
      }

      // Ditto (though, technically, one check for
      // _items is sufficient, because if _items evaluated
      // to null, we'd skip the whole loop)
      if (_itemsWrapper != null)
      {
        Object item = _itemsWrapper.getValue(_currentIndex);
        pageContext.setAttribute(_var, item);
      }
    }

    if (createNewIterationData || _iterationData == null)
    {
      if (key == null)
      {
        key = _getKey();
      }

      _iterationData = new IterationMetaData(key, _isFirst, _isLast,
                         _currentBegin, _currentCount, _currentIndex, _currentEnd);
    }

    // Store the iteration data into the view attributes to allow the EL expressions
    // gain access to it
    if (_LOG.isFinest())
    {
      _LOG.finest("Storing iteration data onto map.\n" +
        "  Iteration ID  : {0}\n" +
        "  Iteration data: {1}",
        new Object[] { _iterationId, _iterationData });
    }

    _iterationMap.put(_iterationId, _iterationData);

    if (_varStatus != null)
    {
      _previousDeferredVarStatus = vm.resolveVariable(_varStatus);

      if (_LOG.isFinest())
      {
        _LOG.finest("Storing iteration map key for varStatus." +
          "\n  Iteration ID: {0}" +
          "\n  Map key     : {1}",
          new Object[] { _iterationId, _iterationMapKey });
      }
      // Store a new var status value expression into the variable mapper
      vm.setVariable(_varStatus, new VarStatusValueExpression(_iterationId, _iterationMapKey));
    }

    if (_previousIterationNonTrinidadChildren == null)
    {
      // If _previousIterationNonTrinidadChildren is null, then this is the first execution
      // of this function for this tag in this request, store off the non-Trinidad children
      // components so that we can attempt to determine the ones that are added
      _previousIterationNonTrinidadChildren = _getNonTrinidadChildren();
    }
  }

  private Integer _evaluateInteger(
    FacesContext context,
    ValueExpression ve)
  {
    if (ve == null)
      return null;

    Object val = ve.getValue(context.getELContext());
    if (val instanceof Integer)
      return (Integer) val;
    else if (val instanceof Number)
      return Integer.valueOf(((Number) val).intValue());

    return null;
  }

  private void _validateAttributes() throws JspTagException
  {
    // Evaluate these three ValueExpressions into integers
    // For why we use FacesContext instead of PageContext, see
    // above (the evaluation of _items)
    FacesContext context = FacesContext.getCurrentInstance();
    _end = _evaluateInteger(context, _endVE);
    _begin = _evaluateInteger(context, _beginVE);
    _step = _evaluateInteger(context, _stepVE);

    if (null == _items)
    {
      if (null == _begin || null == _end)
      {
        throw new JspTagException(
          "'begin' and 'end' should be specified if 'items' is not specified");
      }
    }
    //pu: This is our own check - c:forEach behavior un-defined & unpredictable.
    if ((_var != null) &&
        _var.equals(_varStatus))
    {
      throw new JspTagException(
        "'var' and 'varStatus' should not have the same value");
    }
  }

  private void _validateRangeAndStep() throws JspTagException
  {
    if (_currentBegin < 0)
      throw new JspTagException("'begin' < 0");
    if (_currentStep < 1)
      throw new JspTagException("'step' < 1");
  }

  private void _updateIterationId()
  {
    Integer intObj = (Integer)_viewAttributes.get(_ITERATION_ID_KEY);

    if (intObj == null)
    {
      // By using MIN_VALUE, we can achive 4.2E9 requests for the current view (should
      // be way more than we need)
      _iterationId = new Integer(Integer.MIN_VALUE);
    }
    else
    {
      _iterationId = intObj + 1;
    }

    _LOG.finest("Iteration ID is now {0}", _iterationId);
    _viewAttributes.put(_ITERATION_ID_KEY, _iterationId);

    if (_iterationData != null)
    {
      _iterationMap.put(_iterationId, _iterationData);
    }
  }

  private Serializable _asSerializable(Object key)
  {
    if (key instanceof Serializable)
    {
      return (Serializable)key;
    }
    else
    {
      throw new IllegalStateException("The forEach tag only supports serializable keys for " +
        "maps and collection models");
    }
  }

  @SuppressWarnings("unchecked")
  private static ItemsWrapper _buildItemsWrapper(
    Object items)
  {
    if (items instanceof Array)
    {
      return new ArrayWrapper(items);
    }
    else if (items instanceof List)
    {
      return new ListWrapper((List<?>)items);
    }
    else if (items instanceof CollectionModel)
    {
      return new CollectionModelWrapper((CollectionModel)items);
    }
    else if (items instanceof Map)
    {
      return new MapWrapper((Map<?, ?>)items);
    }
    else
    {
      throw new IllegalArgumentException("Illegal items type: " + items.getClass());
    }
  }

  private static abstract class ForEachBaseValueExpression
    extends ValueExpression
    implements Serializable
  {
    protected ForEachBaseValueExpression(
      Integer iterationId,
      String  mapKey)
    {
      _iterationId = iterationId;
      _mapKey      = mapKey;
    }

    @Override
    public void setValue(
      ELContext context,
      Object    value)
    {
      throw new PropertyNotWritableException();
    }

    @Override
    public boolean isReadOnly(ELContext context)
    {
      return true;
    }

    @Override
    public Class getType(ELContext context)
    {
      return getExpectedType();
    }

    @Override
    public String getExpressionString()
    {
      return null;
    }

    @Override
    public boolean equals(Object obj)
    {
      return obj == this;
    }

    @Override
    public int hashCode()
    {
      return _iterationId.hashCode() | _mapKey.hashCode();
    }

    @Override
    public boolean isLiteralText()
    {
      return true;
    }

    protected IterationMetaData getIterationMetaData()
    {
      // A value expression should ever only be used for one view root,
      // so keep a transient reference to the view to increase performance.
      // Note that this map is cleared during setJspId so that it is save to use the same
      // map that was used after restore view and after the tags are processed in render response.
      if (_viewAttributes == null)
      {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        UIViewRoot view = facesContext.getViewRoot();
        _viewAttributes = view.getAttributes();
      }

      // Get the map from the view attributes created by the tag:
      @SuppressWarnings("unchecked")
      Map<Integer, IterationMetaData> map = (Map<Integer, IterationMetaData>)
        _viewAttributes.get(_mapKey);

      // The map will be null if, somehow, the component for a given for each loop execution
      // is still around, but the for each loop did not match the component during this request
      // (probably a temporary state until the unmatched component is removed).
      IterationMetaData metaData =  map == null ? null : map.get(_iterationId);

      if (metaData == null)
      {
        _LOG.finest("Unable to find iteration meta data for ID {0}", _iterationId);
      }

      return metaData;
    }

    private final Integer _iterationId;
    private final String  _mapKey;
    private transient Map<String, Object> _viewAttributes;

    @SuppressWarnings("compatibility:29745293788177755")
    private static final long serialVersionUID = 1L;
  }

  private static class KeyedValueExpression
    extends ValueExpression
  {
    private KeyedValueExpression(
      ValueExpression itemsExpression,
      Serializable    key)
    {
      _itemsExpression = itemsExpression;
      _key = key;
    }

    @Override
    public Object getValue(ELContext context)
    {
      Object items = _itemsExpression.getValue(context);

      if (items == null)
      {
        return null;
      }

      context.setPropertyResolved(false);
      return context.getELResolver().getValue(context, items, _key);
    }

    @Override
    public void setValue(ELContext context, Object value)
    {
      Object items = _itemsExpression.getValue(context);

      if (items != null)
      {
        context.setPropertyResolved(false);
        context.getELResolver().setValue(context, items, _key, value);
      }
    }

    @Override
    public boolean isReadOnly(ELContext context)
    {
      Object items = _itemsExpression.getValue(context);
      if (items == null)
      {
        return true;
      }

      return context.getELResolver().isReadOnly(context, items, _key);
    }

    @Override
    public Class<?> getType(ELContext context)
    {
      return null;
    }

    @Override
    public Class<?> getExpectedType()
    {
      return Object.class;
    }

    @Override
    public String getExpressionString()
    {
      return _itemsExpression.getExpressionString();
    }

    @Override
    public boolean isLiteralText()
    {
      return false;
    }

    @Override
    public int hashCode()
    {
      return _itemsExpression.hashCode();
    }

    @Override
    public boolean equals(Object obj)
    {
      return _itemsExpression.equals(obj);
    }

    private final ValueExpression _itemsExpression;
    private final Serializable    _key;

    @SuppressWarnings("compatibility:-8374272730669095059")
    private static final long serialVersionUID = 1L;
  }

  /**
   * Value expression that looks up the var value using an index.
   * This class is written in such a way that the index is dynamic, so that if a component is
   * used in different iterations of the for each loop across requests, the correct variable
   * is returned.
   */
  private static class IndexedValueExpression
    extends ForEachBaseValueExpression
    implements Serializable
  {
    private IndexedValueExpression(
      Integer         iterationId,
      String          mapKey,
      ValueExpression itemsExpression)
    {
      super(iterationId, mapKey);
      _itemsExpression = itemsExpression;
    }

    @Override
    public Object getValue(ELContext context)
    {
      // By using a layer of indirection, we can ensure that the correct value is returned for
      // users who pin their component ID to the varStatus so that the component is processed
      // during a different index across requests. We can use the index from the varStatus
      // to determine the correct index to use in the items collection.
      IterationMetaData data = getIterationMetaData();
      if (data == null)
      {
        return null;
      }

      Object items = _itemsExpression.getValue(context);
      if (items == null)
      {
        return null;
      }

      context.setPropertyResolved(false);
      return context.getELResolver().getValue(context, items, data.getIndex());
    }

    @Override
    public Class<?> getExpectedType()
    {
      return Object.class;
    }

    @Override
    public int hashCode()
    {
      return _itemsExpression.hashCode();
    }

    @Override
    public boolean equals(Object obj)
    {
      return _itemsExpression.equals(obj);
    }

    private final ValueExpression _itemsExpression;

    @SuppressWarnings("compatibility:1734834404228501647")
    private static final long serialVersionUID = 1L;
  }

  /**
   * Value Expression instance used to get an object containing the var status properties.
   */
  static private class VarStatusValueExpression
    extends ForEachBaseValueExpression
    implements Serializable
  {
    private VarStatusValueExpression(
      Integer iterationId,
      String  mapKey)
    {
      super(iterationId, mapKey);
    }

    @Override
    public Object getValue(ELContext context)
    {
      return super.getIterationMetaData();
    }

    @Override
    public Class getExpectedType()
    {
      return IterationMetaData.class;
    }

    @SuppressWarnings("compatibility:-3014844132563306923")
    private static final long serialVersionUID = 1L;
  }

  private abstract static class ItemsWrapper
  {
    public abstract Object getKey(int index);
    public abstract Object getValue(int index);
    public abstract int getSize();
    public abstract boolean isKeyBased();
  }

  private static class CollectionModelWrapper
    extends ItemsWrapper
  {
    private CollectionModelWrapper(
      CollectionModel collectionModel)
    {
      _collectionModel = collectionModel;
    }

    @Override
    public Object getKey(int index)
    {
      Object oldRowKey = _collectionModel.getRowKey();
      try
      {
        _collectionModel.setRowIndex(index);
        return _collectionModel.getRowKey();
      }
      finally
      {
        _collectionModel.setRowKey(oldRowKey);
      }
    }

    @Override
    public Object getValue(int index)
    {
      return _collectionModel.getRowData(index);
    }

    @Override
    public int getSize()
    {
      return _collectionModel.getRowCount();
    }

    @Override
    public boolean isKeyBased()
    {
      return true;
    }

    private CollectionModel _collectionModel;
  }

  private static class MapWrapper
    extends ItemsWrapper
  {
    private MapWrapper(
      Map<?, ?> map)
    {
      _map = map;
    }

    @Override
    public Object getKey(int index)
    {
      _moveToIndex(index);
      return _currentEntry.getKey();
    }

    @Override
    public Object getValue(int index)
    {
      _moveToIndex(index);
      return _currentEntry.getValue();
    }

    @Override
    public int getSize()
    {
      return _map.size();
    }

    @Override
    public boolean isKeyBased()
    {
      return true;
    }

    private void _moveToIndex(int index)
    {
      if (index == _currentIndex)
      {
        return;
      }

      if (_iter == null || index < _currentIndex)
      {
        // Need to re-create the iterator
        _iter = _map.entrySet().iterator();
        _currentIndex = -1;
      }

      for (int i = _currentIndex; i < index; ++i)
      {
        _currentEntry = _iter.next();
      }

      _currentIndex = index;
    }

    private final Map<?, ?>                     _map;
    private Iterator<? extends Map.Entry<?, ?>> _iter;
    private Map.Entry<?, ?>                     _currentEntry;
    private int                                 _currentIndex = -1;
  }

  private static class ListWrapper
    extends ItemsWrapper
  {
    private ListWrapper(
      List<?>  list)
    {
      _list = list;
    }

    @Override
    public Object getKey(int index)
    {
      return index;
    }

    @Override
    public Object getValue(int index)
    {
      return _list.get(index);
    }

    @Override
    public int getSize()
    {
      return _list.size();
    }

    @Override
    public boolean isKeyBased()
    {
      return false;
    }

    private final List<?> _list;
  }

  private static class ArrayWrapper
    extends ItemsWrapper
  {
    private ArrayWrapper(
      Object      array)
    {
      _array = array;
    }

    @Override
    public Object getKey(int index)
    {
      return index;
    }

    @Override
    public Object getValue(int index)
    {
      return Array.get(_array, index);
    }

    @Override
    public int getSize()
    {
      return Array.getLength(_array);
    }

    @Override
    public boolean isKeyBased()
    {
      return false;
    }

    private final Object _array;
  }

  /**
   * Data that is used for the children content of the tag. This contains
   * the var status information.
   */
  public static class IterationMetaData
    implements Serializable
  {
    private IterationMetaData(
      Serializable key,
      boolean      first,
      boolean      last,
      int          begin,
      int          count,
      int          index,
      int          end)
    {
      _key = key;
      _first = first;
      _last = last;
      _begin = begin;
      _count = count;
      _index = index;
      _end = end;
    }

    public final boolean isLast()
    {
      return _last;
    }

    public final boolean isFirst()
    {
      return _first;
    }

    public final int getIndex()
    {
      return _index;
    }

    public final int getCount()
    {
      return _count;
    }

    public final int getBegin()
    {
      return _begin;
    }

    public final int getEnd()
    {
      return _end;
    }

    public final Serializable getKey()
    {
      return _key;
    }

    @Override
    public String toString()
    {
      return String.format("IterationData[Key: %s, index: %d, first: %s, last: %s]",
               _key, _index, _first, _last);
    }

    @SuppressWarnings("compatibility:-1418334454154750553")
    private static final long serialVersionUID = 1L;

    private boolean _last;
    private boolean _first;
    private int _begin;
    private int _count;
    private int _index;
    private int _end;
    private Serializable _key;
  }

  private static class NonTrinidadIterationData
    implements Serializable
  {
    public NonTrinidadIterationData(
      Serializable key,
      Integer      iterationId)
    {
      _key = key;
      _iterationId = iterationId;
    }

    public Serializable getKey()
    {
      return _key;
    }

    public Integer getIterationId()
    {
      return _iterationId;
    }

    @SuppressWarnings("compatibility:-6078344977554689652")
    private static final long serialVersionUID = 1L;

    private final Serializable _key;
    private final Integer      _iterationId;
  }

  private int _currentBegin;
  private int _currentIndex;
  private int _currentEnd;
  private int _currentStep;
  private int _currentCount;
  private boolean _isFirst;
  private boolean _isLast;
  private boolean _iterationIdRequiresIncrement;

  private ValueExpression _items;
  private ValueExpression _beginVE;
  private ValueExpression _endVE;
  private ValueExpression _stepVE;

  private Integer _begin;
  private Integer _end;
  private Integer _step;

  private UIComponent _parentComponent;

  private Set<UIComponent> _previousIterationNonTrinidadChildren;

  private Integer _iterationId;
  private IterationMetaData _iterationData;
  private Map<String, Object> _viewAttributes;

  private String _iterationMapKey;
  private Map<Integer, IterationMetaData> _iterationMap;
  private ItemsWrapper _itemsWrapper;

  private String _var;
  private String _varStatus;

  // Saved values on the VariableMapper
  private ValueExpression _previousDeferredVar;
  private ValueExpression _previousDeferredVarStatus;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ForEachTag.class);

  private static final String _INCLUDE_COUNTER_KEY =
    ForEachTag.class.getName() + ".IC";

  private static final String _VIEW_ATTR_KEY =
    ForEachTag.class.getName() + ".VIEW.";
  private static final int _VIEW_ATTR_KEY_LENGTH = _VIEW_ATTR_KEY.length();
  private static final String _ITERATION_ID_KEY =
    ForEachTag.class.getName() + ".ITER";
}
