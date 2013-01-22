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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
 * address this, this Trinidad tag uses a level of indirection to store the information. The tag
 * uses something called and execution ID that represents each invocation of a for each tags'
 * doStartTag. This execution ID is used between requests to store a map of the keys of the
 * collection to tie to the iteration status data (exposed via varStatus).
 * </p>
 * <p>
 * This indirection is used my the value expressions. Instead of saving off the current var status
 * data of an item directly into the variable mapper, the execution ID and the key of the item is
 * stored into the value expression. This allows the for each tag to keep the iteration status
 * up to date for each value expression even when changes are made to the collection.
 * </p>
 *
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
      _setupExecutionMap(facesContext);
    }

    // Create a set to track all keys viewed during the execution of this forEach tag to be able to
    // detect which keys from a previous request are no longer used.
    _processedKeysDuringIteration = new HashSet<Serializable>();
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
    _LOG.finest("doAfterBody processing for execution ID {0}", _executionId);

    _currentIterationStatus = _currentIterationStatus.next(_itemsWrapper);

    VariableMapper vm = pageContext.getELContext().getVariableMapper();

    if (_currentIterationStatus == null)
    {
      // We've finished iterating, we need to clean up by restore EL state and the variable mapper
      _restoreContextVariables(vm);

      if (_varStatus != null)
      {
        // Due to the fact that we are retaining the map keys in the view attributes, check to see
        // if any keys were not used during this execution and remove them so that we are not
        // retaining more objects than necessary in the view state
        for (Iterator<Serializable> iter = _iterationKeyToIterationStatusMap.keySet().iterator();
          iter.hasNext();)
        {
          Serializable key = iter.next();
          if (!_processedKeysDuringIteration.contains(key))
          {
            _LOG.finest("Removing unused key: {0}", key);
            iter.remove();
          }
        }
      }

      _processedKeysDuringIteration = null;

      return SKIP_BODY;
    }
    else
    {
      // Otherwise, update the variables and iterate again
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
    _executionId = null;
    _previousVarExpression = null;
    _previousVarStatusExpression = null;
    _currentIterationStatus = null;
    _iterationKeyToIterationStatusMap = null;
    _itemsWrapper = null;
    _processedKeysDuringIteration = null;

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
   * Sets up the variable mapper, exposing the var and the iteration status data to EL.
   * @param vm the variable mapper to modify
   */
  private void _updateVars(
    VariableMapper vm)
  {
    Serializable key = _currentIterationStatus.getKey();

    if (_var != null)
    {
      if (_items != null)
      {
        // Expose the var to the user. The key is used instead of the index so that changes to the
        // order of items in the collection will not cause the value expression to get out of sync
        // with the collection resulting in corruputed component state. This expression will be
        // used inside the variable mapper that is stored in each value expression that is created
        // by JSP when components in the body of the for each tag are created.
        KeyedValueExpression keyExpr = new KeyedValueExpression(_items, key);
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
      vm.setVariable(_varStatus, new VarStatusValueExpression(key, _executionId));

      // We only need to store the current iteration status on a map if the varStatus variable is
      // being used. If the varStatus is not used, no value expressions would be stored that need
      // to be able to get the current information and it is okay to have the iteration status
      // be transient for this request.
      _iterationKeyToIterationStatusMap.put(key, _currentIterationStatus);

      // Record that this key was used during this execution
      _processedKeysDuringIteration.add(key);

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
   * Generate an ID (key) that can be used to identify the for each tag for each time the tag is
   * executed (each time doStartTag is called, not each time the tag iterates).
   * @param facesContext The faces context
   * @return an ID to be used as a key
   */
  private String _generateExecutionId(
    FacesContext facesContext)
  {
    // Due to the fact that the number of includes that use the forEach loop may change over time,
    // (consider regions that navigate that may or may not have for each tags), we need a way to
    // store iteration status information between requests and be able to match the tags between
    // requests.
    // As such, this code uses the parent scoped ID plus the JSP ID assigned to the tag by the
    // container to identify the forEach tag. Since a forEach tag may be executed more than once
    // (use case of a nested forEach tag), we must also use a counter to ensure that the
    // execution is unique for each time the for each tag's doStartTag is called.
    // Since we also keep data based on this keep in the view scope, we also need to track which
    // execution IDs have been generated during each request so that we can delete any unused
    // data. This must be done using a phaseListener

    // Create an ID that represents the execution of this forEach tag (tied to each invocation
    // of the doStartTag). This ID is used as a key to save a map to store the iteration status
    // data between requests to be able to keep the varStatus value expressions up to date between
    // requests taking into account that the collection may be changed between requests.

    // TODO: be able to clean up unused execution IDs in the current request
    UIComponentClassicTagBase tag = UIComponentClassicTagBase.getParentUIComponentClassicTagBase(
                                      pageContext);
    UIComponent parentComponent = tag == null ? null : tag.getComponentInstance();

    String scopedId = parentComponent == null ? "" : ComponentUtils.getLogicalScopedIdForComponent(
                                                       parentComponent, facesContext.getViewRoot());

    String id = new StringBuilder(_VIEW_ATTR_KEY_LENGTH + _jspId.length() +
                                         scopedId.length() + 1)
      .append(_VIEW_ATTR_KEY)
      .append(scopedId)
      .append('.')
      .append(_jspId)
      .toString();

    Map<Object, Object> fcAttrs = facesContext.getAttributes();
    @SuppressWarnings("unchecked")
    Set<String> usedExecutionIds = (Set<String>)fcAttrs.get(_EXECUTION_ID_MAP_KEY);

    if (usedExecutionIds == null)
    {
      usedExecutionIds = new HashSet<String>();
      fcAttrs.put(_EXECUTION_ID_MAP_KEY, usedExecutionIds);
    }
    else
    {
      String origId = id;
      for (int i = 1; usedExecutionIds.contains(id); ++i)
      {
        id = origId + Integer.toString(i);
      }
    }

    usedExecutionIds.add(id);

    _LOG.finest("Execution ID: {0}", id);

    // Save the execution ID just for logging and debugging reasons
    _executionId = id;

    return id;
  }

  /**
   * Sets up the execution map. The execution map is the map used to store the map keys to their
   * iteration status values for each execution of the tag (one map for each invocation of
   * doStartTag). This method is called by doStartTag.
   * @param facesContext
   */
  private void _setupExecutionMap(
    FacesContext facesContext)
  {
    String executionId = _generateExecutionId(facesContext);

    // We need to store the information into something that will persist between requests. This
    // is so that we can keep a handle on each key that is used during each request and be able
    // to update the iteration status data so that the varStatus reports correct information
    // in each request even if the collection has been changed.
    Map<String, Object> viewAttributes = facesContext.getViewRoot().getAttributes();

    // Use a local variable instead of assigning directly to _iterationKeyToIterationStatusMap
    // To avoid compiler errors on the generic cast.
    @SuppressWarnings("unchecked")
    Map<Serializable, IterationStatus> iterationKeyToIterationStatusMap =
      (Map<Serializable, IterationStatus>)viewAttributes.get(executionId);

    if (iterationKeyToIterationStatusMap == null)
    {
      _iterationKeyToIterationStatusMap = new HashMap<Serializable, IterationStatus>();
      _LOG.finest("Created a new iteration status map for execution ID {0}", executionId);
      viewAttributes.put(executionId, _iterationKeyToIterationStatusMap);
    }
    else
    {
      _iterationKeyToIterationStatusMap = iterationKeyToIterationStatusMap;
    }
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
      Serializable iterationKey,
      String       executionId)
    {
      _iterationKey = iterationKey;
      _exectionId = executionId;
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

      Map<String, Object> viewAttributes = viewRoot.getAttributes();
      Map<Serializable, IterationStatus> metaDataMap = (Map<Serializable, IterationStatus>)
        viewAttributes.get(_exectionId);

      if (metaDataMap == null)
      {
        _LOG.warning("FOR_EACH_META_DATA_UNAVAILABLE");
        return null;
      }

      IterationStatus metaData = metaDataMap.get(_iterationKey);
      if (metaData == null)
      {
        _LOG.warning("FOR_EACH_META_DATA_KEY_UNAVAILABLE");
        return null;
      }

      return metaData;
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
        return _iterationKey.equals(vsve._iterationKey) && _exectionId.equals(vsve._exectionId);
      }

      return false;
    }

    @Override
    public int hashCode()
    {
      int hc = _iterationKey.hashCode();
      // Use 31 as a prime number, a technique used in the JRE classes:
      hc = 31 * hc + _exectionId.hashCode();
      return hc;
    }

    @Override
    public boolean isLiteralText()
    {
      return false;
    }

    @SuppressWarnings("compatibility:6103993756296276343")
    private static final long serialVersionUID = 2L;

    private final String _exectionId;
    private final Serializable _iterationKey;
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

  private String _jspId;

  private ValueExpression _items;
  private ValueExpression _beginVE;
  private ValueExpression _endVE;
  private ValueExpression _stepVE;

  private Integer _begin;
  private Integer _end;
  private Integer _step;

  private String _executionId;
  private IterationStatus _currentIterationStatus;
  private Map<Serializable, IterationStatus> _iterationKeyToIterationStatusMap;
  private Set<Serializable> _processedKeysDuringIteration;
  private ItemsWrapper _itemsWrapper;

  private String _var;
  private String _varStatus;

  // Saved values
  private Object _previousPageContextVarValue;
  private Object _previousPageContextVarStatusValue;
  private ValueExpression _previousVarExpression;
  private ValueExpression _previousVarStatusExpression;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ForEachTag.class);

  private static final String _INCLUDE_COUNTER_KEY =
    ForEachTag.class.getName() + ".IC";

  private static final String _VIEW_ATTR_KEY =
    ForEachTag.class.getName() + ".VIEW.";
  private static final int _VIEW_ATTR_KEY_LENGTH = _VIEW_ATTR_KEY.length();
  private static final String _EXECUTION_ID_MAP_KEY =
    ForEachTag.class.getName() + ".EXEC_ID";
}
