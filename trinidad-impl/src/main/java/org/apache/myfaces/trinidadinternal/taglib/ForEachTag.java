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
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
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
import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;
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
    _LOG.finest("doStartTag called for tag with ID {0}", _jspId);

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
      if (_iterationUtils == null)
      {
        _iterationUtils = new IterationUtils(
          facesContext, _getParentComponentScopedId(facesContext), _jspId);
      }

      // Add, if necessary, the cleanup phase listener to remove unused iteration data
      CleanupPhaseListener.installListener(facesContext);

      _iterationUtils.beginIteration(_currentIterationStatus);
    }

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
    if (_LOG.isFinest())
    {
      if (_varStatus == null)
      {
        _LOG.finest("doAfterBody processing for tag {0}", _jspId);
      }
      else
      {
        _LOG.finest("doAfterBody processing for iteration ID {0}",
          _iterationUtils.getCurrentIterationId());
      }
    }

    if (_iterationUtils != null)
    {
      _iterationUtils.endIteration();
    }

    _currentIterationStatus = _currentIterationStatus.next(_itemsWrapper);

    VariableMapper vm = pageContext.getELContext().getVariableMapper();

    if (_currentIterationStatus == null)
    {
      // We've finished iterating, we need to clean up by restore EL state and the variable mapper
      _restoreContextVariables(vm);

      return SKIP_BODY;
    }
    else
    {
      // Otherwise, notify the iteration utils and and then update the variables and iterate again
      if (_iterationUtils != null)
      {
        _iterationUtils.beginIteration(_currentIterationStatus);
      }

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
    _iterationUtils = null;
    _previousVarExpression = null;
    _previousVarStatusExpression = null;
    _currentIterationStatus = null;
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
      vm.setVariable(_varStatus, new VarStatusValueExpression(
        _iterationUtils.getCurrentIterationId()));

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
      IterationId iterationId)
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

      IterationStatus status = null;
      Map<IterationId, IterationStatus> map =
        IterationUtils.getIterationStatusMap(viewRoot.getViewMap(), false);

      if (map != null)
      {
        status = map.get(_iterationId);
      }

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
      return _iterationId.hashCode();
    }

    @Override
    public boolean isLiteralText()
    {
      return false;
    }

    @SuppressWarnings("compatibility:525689422534872700")
    private static final long serialVersionUID = 4L;

    private final IterationId _iterationId;
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
      return String.format("IterationState[Scoped ID: %s, JSP ID: %s, Key: %s]",
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
   * Class to remove any unused iteration data so that stale data is not kept on the view scope.
   * Since view phase listeners are run after the build view when the JSP tags are run, we can
   * safely check in a phase listener what iteration IDs were used during the current request and
   * therefore be able to remove any data that is no longer needed.
   */
  public static class CleanupPhaseListener
    implements PhaseListener
  {
    static void installListener(
      FacesContext facesContext)
    {
      UIViewRoot viewRoot = facesContext.getViewRoot();

      // See if the phase listener has already been added (only add it once)
      Map<String, Object> viewMap = viewRoot.getViewMap(true);
      if (!viewMap.containsKey(_PL_KEY))
      {
        PhaseListener newPhaseListener = new CleanupPhaseListener();
        viewRoot.addPhaseListener(newPhaseListener);

        viewMap.put(_PL_KEY, Boolean.TRUE);

        _LOG.finer("Cleanup phase listener has been installed");
      }
      else
      {
        _LOG.finest("Cleanup phase listener has already been installed on the current view");
      }
    }

    @Override
    public void afterPhase(PhaseEvent event) { ; }

    @Override
    public void beforePhase(PhaseEvent event)
    {
      _LOG.finest("Running the iteration status cleanup code");
      FacesContext facesContext = event.getFacesContext();
      UIViewRoot viewRoot = facesContext.getViewRoot();
      Map<String, Object> viewMap = viewRoot.getViewMap(false);
      if (viewMap != null)
      {
        Set<IterationId> usedIterationIds = IterationUtils.getUsedIterationIds(
          facesContext.getExternalContext().getRequestMap(), false);
        Map<IterationId, IterationStatus> iterStatusMap = IterationUtils.getIterationStatusMap(
          viewMap, false);

        if (iterStatusMap != null)
        {
          if (_LOG.isFinest())
          {
            if (usedIterationIds == null)
            {
              _LOG.finest("No used iteration IDs, all iteration data will be removed");
            }
            else
            {
              HashSet<IterationId> removeIds = new HashSet<IterationId>(iterStatusMap.keySet());

              removeIds.removeAll(usedIterationIds);
              if (removeIds.size() > 0)
              {
                StringBuilder sb = new StringBuilder();
                for (IterationId id : removeIds)
                {
                  if (sb.length() > 0)
                  {
                    sb.append(", ");
                  }
                  sb.append(id);
                }
                _LOG.finest("Removing {0} iteration IDs. IDs: {1}",
                  new Object[] { removeIds.size(), sb });
              }
              else
              {
                _LOG.finest("No iteration IDs to remove");
              }
            }
          }

          int size = iterStatusMap.size();
          if (usedIterationIds == null)
          {
            iterStatusMap.clear();
            _LOG.finer("All {0} iteration IDs have been removed", size);
          }
          else if (iterStatusMap.keySet().retainAll(usedIterationIds))
          {
            if (_LOG.isFiner())
            {
              _LOG.finer("Iteration IDs have been removed. Previous count: {0}, current count: {1}",
                new Object[] { size, iterStatusMap.size() });
            }
          }
          else
          {
            _LOG.finer("No iteration IDs were removed this request");
          }
        }
      }
    }

    @Override
    public PhaseId getPhaseId()
    {
      return PhaseId.RENDER_RESPONSE;
    }

    @SuppressWarnings("compatibility:2061027649143065494")
    private static final long serialVersionUID = 1L;

    private final static String _PL_KEY = CleanupPhaseListener.class.getName();
  }

  /**
   * Class to encapsulate the logic of the handling the iteration data for the for each tag.
   * This class maintains the iteration status map used by the variable expressions use by
   * the varStatus attribute, the iteration IDs to identify each iteration of a for each tag
   * and what iteration IDs are used in each request so that we do not keep unused information
   * in the view map.
   */
  private static class IterationUtils
  {
    /**
     * Retrieve or create the iteration status map from the view map
     * @param viewMap the view map
     * @param create create the map if it does not exist
     * @return the iteration status map
     */
    static Map<IterationId, IterationStatus> getIterationStatusMap(
      Map<String, Object> viewMap,
      boolean             create)
    {
      Map<IterationId, IterationStatus> iterationStatusMap = (Map<IterationId, IterationStatus>)
        viewMap.get(_ITERATION_MAP_KEY);
      if (iterationStatusMap == null)
      {
        iterationStatusMap = new HashMap<IterationId, IterationStatus>();
        viewMap.put(_ITERATION_MAP_KEY, iterationStatusMap);
      }

      return iterationStatusMap;
    }

    static Set<IterationId> getUsedIterationIds(
      Map<String, Object> reqMap,
      boolean             create)
    {
      Set<IterationId> usedIterationIds = (Set<IterationId>)reqMap.get(_USED_ITER_IDS_KEY);
      if (usedIterationIds == null && create)
      {
        usedIterationIds = new HashSet<IterationId>();
        reqMap.put(_USED_ITER_IDS_KEY, usedIterationIds);
      }

      return usedIterationIds;
    }

    IterationUtils(
      FacesContext facesContext,
      String       parentComponentScopedId,
      String       jspId)
    {
      Map<String, Object> viewMap = facesContext.getViewRoot().getViewMap();
      _iterationStatusMap = getIterationStatusMap(viewMap, true);

      Map<String, Object> reqMap = facesContext.getExternalContext().getRequestMap();
      Deque<IterationState> queue = (Deque<IterationState>)reqMap.get(
        _CURRENT_ITERATION_STATE_QUEUE_KEY);
      if (queue == null)
      {
        queue = new ArrayDeque<IterationState>(5);
        reqMap.put(_CURRENT_ITERATION_STATE_QUEUE_KEY, queue);
      }
      _iterationStateQueue = queue;

      _usedIterationIds = getUsedIterationIds(reqMap, true);
      _jspId = jspId;
      _parentComponentScopedId = parentComponentScopedId;
    }

    IterationId getCurrentIterationId()
    {
      return _currentId;
    }

    /**
     * Called form doStartTag and doAfterBody when a new iteration is about to begin
     * @param iterationStatus the status information for the current iteration
     */
    void beginIteration(
      IterationStatus iterationStatus)
    {
      IterationState state = new IterationState(_parentComponentScopedId, _jspId,
        iterationStatus.getKey());
      _iterationStateQueue.offerLast(state);
      _currentId = new IterationId(_iterationStateQueue);
      _iterationStatusMap.put(_currentId, iterationStatus);
      _usedIterationIds.add(_currentId);
      _LOG.finest("Iteration begun with ID {0}", _currentId);
    }

    /**
     * Called from doAfterBody after processing an iteration
     */
    void endIteration()
    {
      _LOG.finest("Iteration ended with ID {0}", _currentId);
      _currentId = null;
      _iterationStateQueue.removeLast();
    }

    private final Map<IterationId, IterationStatus> _iterationStatusMap;
    private final Deque<IterationState> _iterationStateQueue;
    private final Set<IterationId> _usedIterationIds;
    private final String _jspId;
    private final String _parentComponentScopedId;
    private IterationId _currentId;

    private static final String _ITERATION_MAP_KEY =
      IterationUtils.class.getName() + ".ITER";
    private final static String _CURRENT_ITERATION_STATE_QUEUE_KEY =
      IterationUtils.class.getName() + ".ISQ";
    private final static String _USED_ITER_IDS_KEY = IterationUtils.class.getName() + ".ITER_IDS";
  }

  /**
   * Class that is the ID for a given iteration. Uniquely identifies the for each iteration
   * by its key. Also supports nested for each tags so that each iteration of a nested tag gets
   * its own unique key as well.
   */
  private static class IterationId
    implements Serializable
  {
    IterationId(
      Collection<IterationState> iterationStateStack)
    {
      _iterationStates = Collections.unmodifiableList(
        new ArrayList<IterationState>(iterationStateStack));
    }

    @Override
    public boolean equals(Object object)
    {
      if (this == object)
      {
        return true;
      }
      if (!(object instanceof IterationId))
      {
        return false;
      }

      final IterationId other = (IterationId) object;
      if (this.hashCode() != object.hashCode())
      {
        return false;
      }

      return _iterationStates.equals(other._iterationStates);
    }

    @Override
    public int hashCode()
    {
      // Since the list is immutable, cache the hash code to improve performance
      if (_cachedHashCode == null)
      {
        _cachedHashCode = _iterationStates.hashCode();
      }

      return _cachedHashCode;
    }

    @Override
    public String toString()
    {
      StringBuilder sb = new StringBuilder("IterationId[");
      boolean first = true;
      for (IterationState state : _iterationStates)
      {
        if (first)
        {
          first = false;
        }
        else
        {
          sb.append(" -> ");
        }
        sb.append(state);
      }

      return sb.append(']').toString();
    }

    @SuppressWarnings("compatibility:7690084616223991985")
    private static final long serialVersionUID = 1L;
    private transient Integer _cachedHashCode;
    private final List<IterationState> _iterationStates;
  }

  private String _jspId;

  private ValueExpression _items;
  private ValueExpression _beginVE;
  private ValueExpression _endVE;
  private ValueExpression _stepVE;

  private Integer _begin;
  private Integer _end;
  private Integer _step;

  private IterationUtils _iterationUtils;
  private IterationStatus _currentIterationStatus;
  private ItemsWrapper _itemsWrapper;

  private String _var;
  private String _varStatus;

  // Saved values
  private Object _previousPageContextVarValue;
  private Object _previousPageContextVarStatusValue;
  private ValueExpression _previousVarExpression;
  private ValueExpression _previousVarStatusExpression;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(ForEachTag.class);
}
