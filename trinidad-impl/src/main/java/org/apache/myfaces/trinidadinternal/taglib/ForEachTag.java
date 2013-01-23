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

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;

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

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.util.ComponentUtils;
import org.apache.myfaces.trinidad.webapp.TrinidadTagSupport;


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
 * <p><b>Internal implementation details</b></p>
 * <p>
 * The Trinidad for each tag overcomes limitations of the default forEach tag. This is due to
 * the standard for each tag using indexed value expressions stored in the variable mappers. This
 * usage causes issues if the collection that backs the for each is ever changed. In order to
 * address this, this Trinidad tag uses a level of indirection to store the information. An
 * iteration ID is generated that identifies a particular iteration of a for each tag, including
 * nested tags. This ID is stored in the value expressions and is used to look up the current
 * iteration status for that ID. The iteration ID is based on the scoped ID of the parent component,
 * the JSP ID of the forEach tag, the key of the item from the collection and any parent for
 * each iteration IDs.
 * </p>
 */
public class ForEachTag
  extends TrinidadTagSupport
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
    _jspId = id;
  }

  /**
   * Process the start tag. This is called at the beginning of the first iteration. It may be called
   * multiple times on this tag instance if the tag is nested in another iterating JSP tag. Each
   * time this method is called, the code treats as one execution. Each step of the for each loop
   * is referred to as an iteration.
   * @return the JSP ID to control body processing
   * @throws JspException if a JSF exception occurs
   */
  @Override
  public int doStartTag()
    throws JspException
  {
    _LOG.finest("doStartTag called");

    FacesContext facesContext = FacesContext.getCurrentInstance();

    // Get the values for end, start and begin.
    _parseTagAttributeExpressions(facesContext);
    _validateAttributes();

    int begin = (_begin == null) ? 0 : _begin.intValue();
    int end;

    if (null != _items)
    {
      // AdamWiner: for reasons I cannot yet explain, using the JSP's
      // ELContext is giving me big problems trying to grab Lists
      // from inside of managed beans.  Switching this one call
      // to the JSF ELContext seems to resolve that.  We certainly
      // have to use the JSPs ELResolver for calling through
      // to the VariableMapper
      Object items = _items.getValue(facesContext.getELContext());

      // If items is specified and resolves to null then we do not loop, regardless of if the
      // begin or end have been specified
      if (items == null)
      {
        _LOG.fine("Items expression {0} resolved to null.", _items);
        return SKIP_BODY;
      }

      // Build a wrapper around the items so that a common API can be used to interact with
      // the items regardless of the type.
      _itemsWrapper = _buildItemsWrapper(items);
      int length = _itemsWrapper.getSize();

      if (length == 0)
      {
        _LOG.fine("Items found at {0} is empty.", _items);
        return SKIP_BODY;
      }

      // If the begin attribute has been set, there must be at least "begin" number of items.
      if (length < begin)
      {
        _LOG.fine("Size of 'items' is less than 'begin'");
        return SKIP_BODY;
      }

      end = (_end == null) ? length - 1 : _end.intValue();

      // Ensure that the end is no more than the number of items available
      if (length <= end)
      {
        end = length - 1;
      }
    }
    else
    {
      end = (_end == null) ? 0 : _end.intValue();
    }

    if (end < begin)
    {
      return SKIP_BODY;
    }

    int step = (_step == null) ? 1 : _step;
    if (begin < 0)
    {
      throw new JspTagException("'begin' < 0");
    }

    if (step < 1)
    {
      throw new JspTagException("'step' < 1");
    }

    if (step != 1)
    {
      // If the step is not one the last item may not fall on the "end". Therefore adjust the
      // end to match the index of the last item to be processed
      int count = (int)Math.floor(((end - begin) / (double)step) + 1d);
      end = ((count - 1) * step) + begin;
    }

    // Setup the current status
    _currentIterationStatus = new IterationStatus(
      _getIterationKey(_itemsWrapper, begin),
      true,
      begin == end,
      begin,
      1,
      begin,
      end,
      step);

    // Save off the previous deferred variables
    VariableMapper vm = pageContext.getELContext().getVariableMapper();

    _backupContextVariables(vm);

    if (_varStatus != null)
    {
      // Setup the map to store the iteration status values for this execution of this for each tag
      _setupIterationStatusMap(facesContext);
    }

    // Create a set to track all iteration IDs viewed during the execution of this forEach tag
    // to be able todetect which keys from a previous request are no longer used.
    _assignIterationId(facesContext, true);

    // Add the "var" and "varStatus" into the EL context and variable mapper
    _updateVars(vm);

    if (_LOG.isFiner())
    {
      _LOG.finer("Initial iteration status: {0}", new Object[] { _currentIterationStatus });
    }

    return EVAL_BODY_INCLUDE;
  }

  @Override
  public int doAfterBody()
  {
    _LOG.finest("doAfterBody processing for iteration ID {0}", _iterationId);

    _currentIterationStatus = _currentIterationStatus.next(_itemsWrapper);

    _iterationIdUtil.pop();

    VariableMapper vm = pageContext.getELContext().getVariableMapper();

    if (_currentIterationStatus == null)
    {
      // We've finished iterating, we need to clean up by restore EL state and the variable mapper
      _restoreContextVariables(vm);

      _removeUnusedIterationIds();

      return SKIP_BODY;
    }
    else
    {
      // Otherwise, generate a new iteration ID or find the one from the previous request and then
      // update the variables and iterate again
      _assignIterationId(FacesContext.getCurrentInstance(), false);
      _updateVars(vm);

      return EVAL_BODY_AGAIN;
    }
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
    _iterationId = null;
    _iterationIdUtil = null;
    _previousVarExpression = null;
    _previousVarStatusExpression = null;
    _currentIterationStatus = null;
    _iterationStatusMap = null;
    _itemsWrapper = null;

    _LOG.finest("release called");
  }

  /**
   * Restore the variables backed up in the {@link #_backupContextVariables(VariableMapper)}
   * function.
   * @param vm the current variable mapper
   */
  private void _restoreContextVariables(VariableMapper vm)
  {
    if (_var != null)
    {
      vm.setVariable(_var, _previousVarExpression);
      pageContext.setAttribute(_var, _previousPageContextVarValue);
    }

    if (_varStatus != null)
    {
      vm.setVariable(_varStatus, _previousVarStatusExpression);
      pageContext.setAttribute(_varStatus, _previousPageContextVarStatusValue);
    }
  }

  /**
   * Saves the var and varStatus information from both the variable mapper as well as the
   * page context so that the values may be restored to their values after the for each tag
   * has finished iterating.
   * @param vm the current variable mapper
   */
  private void _backupContextVariables(VariableMapper vm)
  {
    if (_var != null)
    {
      // Store off the current values used by the var name so that it may be restored after
      // tag processing
      _previousVarExpression = vm.resolveVariable(_var);
      _previousPageContextVarValue = pageContext.getAttribute(_var);
    }

    if (_varStatus != null)
    {
      // Store off the current values for the varStatus name to be able to restore it after
      // processing this tag
      _previousVarStatusExpression = vm.resolveVariable(_varStatus);
      _previousPageContextVarStatusValue = pageContext.getAttribute(_varStatus);
    }
  }

  /**
   * Assigns an iteration ID to the current iteration of the for each tag. Called from both
   * doStartTag and doAfterBody when a new iteration is about to begin
   * @param facesContext the faces context
   * @param calledFromDoStartTag true if called from do start tag so that the tag can start tracking
   *        the processed iteration IDs to know which are no longer in use.
   */
  private void _assignIterationId(
    FacesContext facesContext,
    boolean      calledFromDoStartTag)
  {
    UIViewRoot viewRoot = facesContext.getViewRoot();
    if (_iterationIdUtil == null)
    {
      _iterationIdUtil = new IterationIdUtil(viewRoot.getViewMap(),
        facesContext.getExternalContext().getRequestMap());
    }

    if (calledFromDoStartTag)
    {
      _iterationIdUtil.beginTag();
    }

    Serializable key = _currentIterationStatus.getKey();
    String scopedId = _getParentComponentScopedId(facesContext);
    IterationState iterIdObj = new IterationState(scopedId, _jspId, key);
    _iterationId = _iterationIdUtil.push(iterIdObj);
    if (_LOG.isFinest())
    {
      _LOG.finest("Iteration ID object {0} associated with ID {1}",
        new Object[] { iterIdObj, _iterationId });
    }
  }

  /**
   * Sets up the variable mapper, exposing the var and the iteration status data to EL.
   * @param vm the variable mapper to modify
   */
  private void _updateVars(
    VariableMapper vm)
  {
    if (_var != null)
    {
      if (_items != null)
      {
        // Expose the var to the user. The key is used instead of the index so that changes to the
        // order of items in the collection will not cause the value expression to get out of sync
        // with the collection resulting in corruputed component state. This expression will be
        // used inside the variable mapper that is stored in each value expression that is created
        // by JSP when components in the body of the for each tag are created.
        KeyedValueExpression keyExpr = new KeyedValueExpression(_items,
          _currentIterationStatus.getKey());
        vm.setVariable(_var, keyExpr);
      }

      if (_itemsWrapper != null)
      {
        // Put the item onto the page context
        Object item = _itemsWrapper.getValue(_currentIterationStatus.getIndex());
        pageContext.setAttribute(_var, item);
      }
    }

    if (_varStatus != null)
    {
      // Store a new var status value expression into the variable mapper
      vm.setVariable(_varStatus, new VarStatusValueExpression(_iterationId));

      // We only need to store the current iteration status on a map if the varStatus variable is
      // being used. If the varStatus is not used, no value expressions would be stored that need
      // to be able to get the current information and it is okay to have the iteration status
      // be transient for this request.
      _iterationStatusMap.put(_iterationId, _currentIterationStatus);

      // Put the value onto the page context
      pageContext.setAttribute(_varStatus, _currentIterationStatus);
    }
  }

  /**
   * Get the integer value from a value expression. Leaves null values as null and converts Number
   * to Integer. Invalid values will result in a value of null.
   *
   * @param context
   * @param ve
   * @return
   */
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
      return Integer.valueOf(((Number)val).intValue());

    return null;
  }

  /**
   * Get the integer values of the begin, end and step value expressions, if set
   * @param facesContext
   */
  private void _parseTagAttributeExpressions(
    FacesContext facesContext)
  {
    _end = _evaluateInteger(facesContext, _endVE);
    _begin = _evaluateInteger(facesContext, _beginVE);
    _step = _evaluateInteger(facesContext, _stepVE);
  }

  private void _validateAttributes(
    ) throws JspTagException
  {
    // Ensure that the begin and and have been specified if the items attribute was not
    if (null == _items)
    {
      if (null == _begin || null == _end)
      {
        throw new JspTagException(
          "'begin' and 'end' should be specified if 'items' is not specified");
      }
    }

    // Ensure the user has not set the var and varStatus attributes to the save value
    if ((_var != null) && _var.equals(_varStatus))
    {
      throw new JspTagException(
        "'var' and 'varStatus' should not have the same value");
    }
  }

  /**
   * Called from doAfterBody once all iterations are done to remove the data tied to unused
   * iteration IDs
   */
  private void _removeUnusedIterationIds()
  {
    Set<Long> unusedIds = _iterationIdUtil.removeUnusedIds(_jspId,
      _getParentComponentScopedId(FacesContext.getCurrentInstance()));
    for (Long id : unusedIds)
    {
      _iterationStatusMap.remove(id);
    }
  }

  /**
   * Get the scoped ID of the parent component of this tag
   * @param facesContext the faces context
   * @return the scoped ID or an empty string if the tag is not under a component
   */
  private String _getParentComponentScopedId(
    FacesContext facesContext)
  {
    UIComponentClassicTagBase tag = UIComponentClassicTagBase.getParentUIComponentClassicTagBase(
      pageContext);
    UIComponent parentComponent = tag == null ? null : tag.getComponentInstance();

    return parentComponent == null ? "" :
      ComponentUtils.getLogicalScopedIdForComponent(parentComponent, facesContext.getViewRoot());
  }

  /**
   * Given the index, get the key to use as the identifier for the current iteration.
   * Returns the key for key-based collections otherwise returns the index as an object.
   *
   * @return the key or index
   */
  private static Serializable _getIterationKey(
    ItemsWrapper itemsWrapper,
    int         index)
  {
    if (itemsWrapper == null)
    {
      return index;
    }

    Object key = itemsWrapper.getKey(index);

    if (key instanceof Serializable)
    {
      return (Serializable)key;
    }
    else
    {
      throw new IllegalStateException("The forEach tag only supports serializable keys for " +
        "maps and collection models. Key does not implement serializable: " + key +
        " Class: " + (key == null ? "null" : key.getClass().getName()));
    }
  }

  /**
   * Create a wrapper around the items collection to provide a single API to be able to interact
   * with the data.
   * @param items The value from evaluating the EL on the items attribute
   * @return a wrapper class
   */
  @SuppressWarnings("unchecked")
  private static ItemsWrapper _buildItemsWrapper(
    Object items)
  {
    if (items.getClass().isArray())
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

  /**
   * Sets up the iteration status map. The iteration status map uses the parent component of the
   * tag, the JSP tag ID and the current iteration key to be able to identify the correct iteration
   * status. This takes into account nested for each loops.
   * @param facesContext the facesContext
   */
  private void _setupIterationStatusMap(
    FacesContext facesContext)
  {
    UIViewRoot viewRoot = facesContext.getViewRoot();
    Map<String, Object> viewMap = viewRoot.getViewMap();
    _iterationStatusMap = _getIterationStatusMap(viewMap, true);
  }

  /**
   * Retrieve the map that stores the iteration status for a given iteration ID. This map is
   * dynamically updated per request to ensure that the iteration status revealed in the var status
   * is up to date.
   * @param viewMap the view map
   * @param create the map will be created if it does not exist if this is true
   * @return the map
   */
  private static Map<Long, IterationStatus> _getIterationStatusMap(
    Map<String, Object> viewMap,
    boolean             create)
  {
    Map<Long, IterationStatus> map = (Map<Long, IterationStatus>)viewMap.get(_ITERATION_MAP_KEY);
    if (map == null)
    {
      if (create)
      {
        // The iteration status map is stored onto the view map so that we can still get the
        // iteration status values from EL during the next request, before the JSP tags are
        // processed once again.
        map = new HashMap<Long, IterationStatus>();
        viewMap.put(_ITERATION_MAP_KEY, map);
      }
      else
      {
        _LOG.warning("FOR_EACH_ITERATION_DATA_UNAVAILABLE");
        return Collections.emptyMap();
      }
    }

    return map;
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
      else if (items instanceof CollectionModel)
      {
        // We do not have an EL resolver for the CollectionModel to get values by row index
        // or by row key. Therefore, we must just go after the value manually.
        CollectionModel model = (CollectionModel)items;
        return model.getRowData(_key);
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
        if (items instanceof CollectionModel)
        {
          // There is no support for setting row data on the collection model
          throw new PropertyNotWritableException();
        }

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
      else if (items instanceof CollectionModel)
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
   * Value Expression instance used to get an object containing the var status information.
   */
  static private class VarStatusValueExpression
    extends ValueExpression
    implements Serializable
  {
    private VarStatusValueExpression(
      long iterationId)
    {
      _iterationId = iterationId;
    }

    @Override
    public Object getValue(ELContext context)
    {
      FacesContext facesContext = FacesContext.getCurrentInstance();
      assert facesContext != null :
        "Illegal attempt to evaluate for each EL outside of an active faces context";

      UIViewRoot viewRoot = facesContext.getViewRoot();
      assert viewRoot != null :
        "Illegal attempt to evaluate for each EL outside of an active view root";

      Map<Long, IterationStatus> map = _getIterationStatusMap(viewRoot.getViewMap(), false);
      IterationStatus status = map.get(_iterationId);
      if (status == null)
      {
        _LOG.warning("FOR_EACH_STATUS_UNAVAILABLE");
        return null;
      }

      return status;
    }

    @Override
    public Class getExpectedType()
    {
      return IterationStatus.class;
    }

    @Override
    public void setValue(ELContext context, Object value)
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public boolean isReadOnly(ELContext context)
    {
      return true;
    }

    @Override
    public Class<?> getType(ELContext context)
    {
      return IterationStatus.class;
    }

    @Override
    public String getExpressionString()
    {
      return null;
    }

    @Override
    public boolean equals(Object obj)
    {
      if (obj instanceof VarStatusValueExpression)
      {
        VarStatusValueExpression vsve = (VarStatusValueExpression)obj;
        return _iterationId == vsve._iterationId;
      }

      return false;
    }

    @Override
    public int hashCode()
    {
      return Long.valueOf(_iterationId).hashCode();
    }

    @Override
    public boolean isLiteralText()
    {
      return false;
    }

    @SuppressWarnings("compatibility:-2661519236237365267")
    private static final long serialVersionUID = 3L;

    private final long _iterationId;
  }

  /**
   * Class to provide a common API to all the collection types that are supported by the items
   * attribute to avoid having to branch code by the data type.
   */
  private abstract static class ItemsWrapper
  {
    /**
     * Get the key for the item at the given index
     * @param index the index
     * @return the key
     */
    public abstract Object getKey(int index);

    /**
     * Get the value for the item at the given index
     * @param index the index
     * @return the value from the collection
     */
    public abstract Object getValue(int index);

    /**
     * Get the number of items in the collection
     */
    public abstract int getSize();
  }

  /**
   * Wrapper for CollectionModel objects
   */
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

    private CollectionModel _collectionModel;
  }

  /**
   * Wrapper for Map objects
   */
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

  /**
   * Wrapper for List objects
   */
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

    private final List<?> _list;
  }

  /**
   * Wrapper for plain Java arrays
   */
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

    private final Object _array;
  }

  /**
   * Class that provides the information to identify the iteration of a particular for each tag
   */
  public final static class IterationState
    implements Serializable
  {
    public IterationState(
      String       parentComponentScopedId,
      String       jspId,
      Serializable key)
    {
      _scopedId = parentComponentScopedId;
      _jspId = jspId;
      _key = key;
    }

    @Override
    public boolean equals(Object object)
    {
      if (this == object)
      {
        return true;
      }
      if (!(object instanceof ForEachTag.IterationState))
      {
        return false;
      }
      final IterationState other = (IterationState) object;
      if (!(_scopedId == null ? other._scopedId == null : _scopedId.equals(other._scopedId)))
      {
        return false;
      }
      if (!(_jspId == null ? other._jspId == null : _jspId.equals(other._jspId)))
      {
        return false;
      }
      if (!(_key == null ? other._key == null : _key.equals(other._key)))
      {
        return false;
      }

      return true;
    }

    @Override
    public int hashCode()
    {
      final int PRIME = 37;
      int result = 1;
      result = PRIME * result + ((_scopedId == null) ? 0 : _scopedId.hashCode());
      result = PRIME * result + ((_jspId == null) ? 0 : _jspId.hashCode());
      result = PRIME * result + ((_key == null) ? 0 : _key.hashCode());
      return result;
    }

    /**
     * Get the scoped ID of the parent component of the for each tag. This is used in conjunction
     * with the JSP ID since there is nothing preventing the JSP container from using the same
     * JSP ID in different included files processed during the same request, it is only guaranteed
     * to be unique to the current page.
     * @return the scoped ID
     */
    public String getScopedId()
    {
      return _scopedId;
    }

    /**
     * The JSP ID of the for each tag
     * @return JSP ID
     */
    public String getJspId()
    {
      return _jspId;
    }

    /**
     * The key of the item from the items collection. If a indexed based collection, or if the
     * items attribute was not given the key is just the index.
     * @return the item's key
     */
    public Serializable getKey()
    {
      return _key;
    }

    @Override
    public String toString()
    {
      return String.format("IterationId[Scoped ID: %s, JSP ID: %s, Key: %s]",
               _scopedId, _jspId, _key);
    }

    @SuppressWarnings("compatibility:-2690507119119928732")
    private static final long serialVersionUID = 1L;

    private final String _scopedId;
    private final String _jspId;
    private final Serializable _key;
  }

  /**
   * Data that is used for the children content of the tag. This provides the information that is
   * exposed to the user via the varStatus attribute.
   */
  public static class IterationStatus
    implements Serializable
  {
    private IterationStatus(
      Serializable key,
      boolean      first,
      boolean      last,
      int          begin,
      int          count,
      int          index,
      int          end,
      int          step)
    {
      _key = key;
      _first = first;
      _last = last;
      _begin = begin;
      _count = count;
      _index = index;
      _end = end;
      _step = step;
    }

    /**
     * Retrieve if this represents the last iteration of the forEach tag
     * @return true if the last iteration
     */
    public final boolean isLast()
    {
      return _last;
    }

    /**
     * Retrieve if this is the first iteration of the tag
     * @return true if the first iteration
     */
    public final boolean isFirst()
    {
      return _first;
    }

    /**
     * Get the iteration index (0-based). For index based collections this represents the index
     * of the item in the collection.
     * @return the index of this iteration (0 based)
     */
    public final int getIndex()
    {
      return _index;
    }

    /**
     * Get the number associated with the iteration. The first iteration is one. This is different
     * from the index property if the begin attribute has been set to a non-zero value.
     * @return the count of this iteration
     */
    public final int getCount()
    {
      return _count;
    }

    /**
     * Get the index of the first item with which the tag begins iteration
     * @return the first index processed
     */
    public final int getBegin()
    {
      return _begin;
    }

    /**
     * Get the last index to be processed
     * @return the last index
     */
    public final int getEnd()
    {
      return _end;
    }

    /**
     * Get the key of the iteration. For index based collections, this is the index and for
     * key based collections, it is the key.
     * @return the key
     */
    public final Serializable getKey()
    {
      return _key;
    }

    @Override
    public String toString()
    {
      return String.format("IterationStatus[key: %s, index: %d, count: %d, " +
        "first: %s, last: %s, begin: %d, end: %d, step: %d]",
               _key, _index, _count, _first, _last, _begin, _end, _step);
    }

    /**
     * Generate the status information for the next iteration.
     * @param itemsWrapper The items wrapper, used to get the key for the current iteration
     * @return a status object for the next iteration or null if the for each tag is done iterating.
     */
    IterationStatus next(
      ItemsWrapper itemsWrapper)
    {
      int nextIndex = _index + _step;

      if (nextIndex > _end)
      {
        return null;
      }

      return new IterationStatus(
        _getIterationKey(itemsWrapper, nextIndex),
        false,
        nextIndex == _end,
        _begin,
        _count + 1,
        nextIndex,
        _end,
        _step);
    }

    @SuppressWarnings("compatibility:-8691554623604161106")
    private static final long serialVersionUID = 2L;

    private final boolean _last;
    private final boolean _first;
    private final int _begin;
    private final int _count;
    private final int _index;
    private final int _end;
    private final int _step;
    private final Serializable _key;
  }

  /**
   * Helper class to help keep track of the iteration IDs and storing the necessary values on the
   * view map to maintain the tag state between requests to be able to track changes.
   */
  private static class IterationIdUtil
  {
    private IterationIdUtil(
      Map<String, Object> viewMap,
      Map<String, Object> reqMap)
    {
      _viewMap = viewMap;
      _reqMap = reqMap;
    }

    /**
     * Called during the doAfterBody to signify the end of one iteration
     */
    IterationState pop()
    {
      return _getCurrentQueue(false).removeLast();
    }

    /**
     * Lets the class know that the doStartTag has been called on the for each tag. This method
     * begins tracking the processed iteration IDs so that IDs that are not used during the
     * current request can be removed from the view scope memory.
     */
    void beginTag()
    {
      Deque<IterationState> q = _getCurrentQueue(true);

      if (q.isEmpty())
      {
        // If the queue is empty, this is a top level (not nested) for each tag. Record the
        // iteration IDs for this tag and all nested tags to detect which ones are used during
        // the current request to be able to remove unused data.
        _processedIterationIds = new HashSet<Long>();
        _reqMap.put(_REQ_USED_IDS_KEY, _processedIterationIds);
      }
      else
      {
        // This is nested tag, use the set created by the root parent tag
        _processedIterationIds = (Set<Long>)_reqMap.get(_REQ_USED_IDS_KEY);
      }
    }

    /**
     * Called by doStartTag and doAfterBody to notify this class that a new iteration has begun.
     * @param iterationState the object with the information required to be able to identify the
     *        current iteration state of the calling for each tag
     * @return an iteration ID
     */
    Long push(
      IterationState iterationState)
    {
      Map<List<IterationState>, Long> map = _getIterationIdStackToIdMap(true);

      Deque<IterationState> q = _getCurrentQueue(true);
      q.offerLast(iterationState);
      List<IterationState> list = new ArrayList<IterationState>(q);

      // See if this is already has an ID from a previous request
      Long id = map.get(list);

      if (id == null)
      {
        // This is a new iteration, assign a new ID
        id = _getNextId().getAndIncrement();
        map.put(list, id);
      }

      _processedIterationIds.add(id);

      return id;
    }

    /**
     * Called from doAfterBody after all the iterations have been completed and the pop method
     * of this class has been called.
     *
     * @param currentJspId the JSP tag ID of the calling instance
     * @param parentComponentScopedId the scoped ID of the parent component of the tag
     * @return the IDs that should be removed
     */
    Set<Long> removeUnusedIds(
      String    currentJspId,
      String    parentComponentScopedId)
    {
      Deque<IterationState> q = _getCurrentQueue(false);

      if (!q.isEmpty())
      {
        // Only process top-level for each tags. This is because nested tags may be processed
        // multiple times during one request and we do not know if all the IDs have been used
        // until all have been complete
        return Collections.emptySet();
      }

      Set<Long> unusedIds = new HashSet<Long>();
      Map<List<IterationState>, Long> map = _getIterationIdStackToIdMap(false);

      for (Iterator<Map.Entry<List<IterationState>, Long>> iter = map.entrySet().iterator();
        iter.hasNext();)
      {
        Map.Entry<List<IterationState>, Long> entry = iter.next();

        List<IterationState> list = entry.getKey();
        IterationState baseId = list.get(0);

        // Only process the entries that are for this tag
        if (currentJspId.equals(baseId.getJspId()) &&
          parentComponentScopedId.equals(baseId.getScopedId()))
        {
          Long id = entry.getValue();
          if (_processedIterationIds.contains(id))
          {
            _LOG.finest("Iteration ID being retained: {0}", id);
          }
          else
          {
            unusedIds.add(id);
            if (_LOG.isFinest())
            {
              StringBuilder path = new StringBuilder();
              for (IterationState iterId : list)
              {
                path.append('\n').append("  ").append(iterId);
              }
              _LOG.finest("Iteration ID {0} being removed. Path to ID: {1}",
                new Object[] { id, path });
            }

            iter.remove();
          }
        }
      }

      return unusedIds;
    }

    /**
     * Retrieve the map that is used to tie the current iteration stack to the iteration IDs.
     * The keys of the map are the stack of iteration states to get to the current iteration.
     * For nested for each tags the list will be longer than one.
     *
     * @param create if the map does not exist yet setting this to true will cause the map to be
     *        created
     * @return the map
     */
    private Map<List<IterationState>, Long> _getIterationIdStackToIdMap(
      boolean create)
    {
      Map<List<IterationState>, Long> map = (Map<List<IterationState>, Long>)
        _viewMap.get(_VIEW_MAP_KEY);
      if (map == null && create)
      {
        map = new HashMap<List<IterationState>, Long>();
        _viewMap.put(_VIEW_MAP_KEY, map);
      }

      return map;
    }

    /**
     * By using an a atomic long instance, the value in the view map may be incremented instead
     * of having to keep putting new Long instances into the view map. It is only used as a
     * mutable long object and not for its concurrency properties.
     * @return the value that contains the next ID to be used as an iteration ID
     */
    private AtomicLong _getNextId()
    {
      if (_id == null)
      {
        _id = (AtomicLong)_viewMap.get(_VIEW_ID_KEY);
        if (_id == null)
        {
          _id = new AtomicLong();
          _viewMap.put(_VIEW_ID_KEY, _id);
        }
      }

      return _id;
    }

    /**
     * Gets the current queue of iteration state objects. This will be larger than one for nested
     * for each tags.
     * @param create if true the queue will be created if it does not exist
     * @return the queue
     */
    private Deque<IterationState> _getCurrentQueue(
      boolean create)
    {
      if (_queue == null)
      {
        _queue = (Deque)_viewMap.get(_CURRENT_STACK_KEY);
        if (_queue == null && create)
        {
          _queue = new ArrayDeque<IterationState>();
          _viewMap.put(_CURRENT_STACK_KEY, _queue);
        }
      }

      return _queue;
    }

    private AtomicLong _id;
    private Deque _queue;
    private Set<Long> _processedIterationIds;
    private final Map<String, Object> _viewMap;
    private final Map<String, Object> _reqMap;
    private final static String _CURRENT_STACK_KEY = IterationIdUtil.class.getName() + ".Q";
    private final static String _VIEW_MAP_KEY = IterationIdUtil.class.getName() + ".MAP";
    private final static String _VIEW_ID_KEY = IterationIdUtil.class.getName() + ".NEXT_ID";
    private final static String _REQ_USED_IDS_KEY = IterationIdUtil.class.getName() + ".IDS";
  }

  private String _jspId;

  private ValueExpression _items;
  private ValueExpression _beginVE;
  private ValueExpression _endVE;
  private ValueExpression _stepVE;

  private Integer _begin;
  private Integer _end;
  private Integer _step;

  private Long _iterationId;
  private IterationStatus _currentIterationStatus;
  private IterationIdUtil _iterationIdUtil;
  private Map<Long, IterationStatus> _iterationStatusMap;
  private ItemsWrapper _itemsWrapper;

  private String _var;
  private String _varStatus;

  // Saved values
  private Object _previousPageContextVarValue;
  private Object _previousPageContextVarStatusValue;
  private ValueExpression _previousVarExpression;
  private ValueExpression _previousVarStatusExpression;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ForEachTag.class);

  private static final String _ITERATION_MAP_KEY =
    ForEachTag.class.getName() + ".ITER";
}
