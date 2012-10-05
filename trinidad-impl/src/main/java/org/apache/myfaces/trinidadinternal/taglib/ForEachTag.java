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

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.CollectionModel;
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

  @Override
  public int doStartTag()
    throws JspException
  {
    _LOG.finest("doStartTag called");
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

    // Do not process the meta-data functionality unless the user needs the varStatus variable
    if (_varStatus != null)
    {
      _configureMetaDataMap();
    }

    // Create a set to track all keys viewed during this iteration to be able to detect
    // which keys from a previous request are no longer used.
    _processedKeys = new HashSet<Serializable>();
    _updateVars(vm);

    return EVAL_BODY_INCLUDE;
  }

  @Override
  public int doAfterBody()
  {
    _LOG.finest("doAfterBody processing with iteration meta data map key of {0}", _iterationMapKey);

    _currentIndex += _currentStep;
    ++_currentCount;
    _isFirst = false;
    _isLast = _currentIndex == _currentEnd;

    // Clear any cached variables
    _key = null;
    _metaData = null;

    VariableMapper vm = pageContext.getELContext().getVariableMapper();

    // If we're at the end, bail
    if (_currentEnd < _currentIndex)
    {
      // Restore EL state
      if (_var != null)
      {
        vm.setVariable(_var, _previousDeferredVar);
      }
      if (_varStatus != null)
      {
        vm.setVariable(_varStatus, _previousDeferredVarStatus);
      }

      // Due to the fact that we are retaining the map keys in the view attributes, check to see
      // if any keys were not used during this execution and remove them

      for (Iterator<Serializable> iter = _metaDataMap.keySet().iterator(); iter.hasNext(); )
      {
        Serializable key = iter.next();
        if (!_processedKeys.contains(key))
        {
          _LOG.finest("Removing unused key: {0}", key);
          iter.remove();
        }
      }

      _processedKeys = null;

      return SKIP_BODY;
    }

    // Otherwise, update the variables and go again
    _updateVars(vm);

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

    _metaDataMap = null;
    _metaData = null;
    _viewAttributes = null;
    _iterationMapKey = null;
    _itemsWrapper = null;
    _key = null;
    _parentComponent = null;

    _LOG.finest("release called");
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
    if (_key != null)
    {
      return _key;
    }

    _key = (_itemsWrapper == null) ?
      _currentIndex :
      _asSerializable(_itemsWrapper.getKey(_currentIndex));

    return _key;
  }

  // Push new values into the VariableMapper and the pageContext
  private void _updateVars(
    VariableMapper vm)
  {
    Serializable key = null;

    if (_var != null)
    {
      // Catch programmer error where _var has been set but _items has not
      if (_items != null)
      {
        key = _getKey();
        vm.setVariable(_var, new KeyedValueExpression(_items, key));
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

    if (_varStatus != null)
    {
      _previousDeferredVarStatus = vm.resolveVariable(_varStatus);

      if (key == null)
      {
        key = _getKey();
      }

      if (_LOG.isFinest())
      {
        _LOG.finest("Storing iteration map key for varStatus." +
          "\n  Key              : {0}" +
          "\n  Meta data map key: {1}",
          new Object[] { key, _iterationMapKey });
      }
      // Store a new var status value expression into the variable mapper
      vm.setVariable(_varStatus, new VarStatusValueExpression(key, _iterationMapKey));

      // Only set up the meta data if the varStatus attribute is used
      MetaData metaData = new MetaData(key,
        _isFirst, _isLast, _currentBegin, _currentCount, _currentIndex, _currentEnd);

      _metaDataMap.put(key, metaData);

      // Record that this key was used during this request
      _processedKeys.add(key);
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

  private Serializable _asSerializable(Object key)
  {
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

  private void _configureMetaDataMap()
  {
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
      _iterationMapKey = new StringBuilder(_VIEW_ATTR_KEY_LENGTH + _jspId.length() +
                                           pcId.length() + 1)
        .append(_VIEW_ATTR_KEY)
        .append(pcId)
        .append('.')
        .append(_jspId)
        .toString();

      // store the map into the view attributes to put it in a location that the EL expressions
      // can access for not only the remainder of this request, but also the next request.
      UIViewRoot viewRoot = facesContext.getViewRoot();

      // We can cache the view attributes in the tag as a JSP tag marked with JspIdConsumer
      // is never reused.
      _viewAttributes = viewRoot.getAttributes();

      @SuppressWarnings("unchecked")
      Map<Serializable, MetaData> metaDataMap = (Map<Serializable, MetaData>)
        _viewAttributes.get(_iterationMapKey);
      if (metaDataMap == null)
      {
        _metaDataMap = new HashMap<Serializable, MetaData>();
        _LOG.finest("Created a new meta data map for key {0}", _iterationMapKey);
        _viewAttributes.put(_iterationMapKey, _metaDataMap);
      }
      else
      {
        _metaDataMap = metaDataMap;
      }
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
   * Value Expression instance used to get an object containing the var status properties.
   */
  static private class VarStatusValueExpression
    extends ValueExpression
    implements Serializable
  {
    private VarStatusValueExpression(
      Serializable itemsKey,
      String       metaDataMapKey)
    {
      _key = itemsKey;
      _metaDataMapKey = metaDataMapKey;
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

      // We can cache the view attributes in the tag as a JSP tag marked with JspIdConsumer
      // is never reused.
      Map<String, Object> viewAttributes = viewRoot.getAttributes();

      Map<Serializable, MetaData> metaDataMap = (Map<Serializable, MetaData>)
        viewAttributes.get(_metaDataMapKey);

      if (metaDataMap == null)
      {
        _LOG.warning("FOR_EACH_META_DATA_UNAVAILABLE");
        return null;
      }

      MetaData metaData = metaDataMap.get(_key);
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
      return MetaData.class;
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
      return MetaData.class;
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
        return _key.equals(vsve._key) && _metaDataMapKey.equals(vsve._metaDataMapKey);
      }

      return false;
    }

    @Override
    public int hashCode()
    {
      int hc = _key.hashCode();
      // Use 31 as a prime number, a technique used in the JRE classes:
      hc = 31 * hc + _metaDataMapKey.hashCode();
      return hc;
    }

    @Override
    public boolean isLiteralText()
    {
      return false;
    }

    @SuppressWarnings("compatibility:7866012729338284490")
    private static final long serialVersionUID = 1L;

    private final String _metaDataMapKey;
    private final Serializable    _key;
  }

  private abstract static class ItemsWrapper
  {
    public abstract Object getKey(int index);
    public abstract Object getValue(int index);
    public abstract int getSize();
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

    private final Object _array;
  }

  /**
   * Data that is used for the children content of the tag. This contains
   * the var status information.
   */
  public static class MetaData
    implements Serializable
  {
    private MetaData(
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
      return String.format("MetaData[Key: %s, index: %d, first: %s, last: %s]",
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

  private String _jspId;

  private int _currentBegin;
  private int _currentIndex;
  private int _currentEnd;
  private int _currentStep;
  private int _currentCount;
  private boolean _isFirst;
  private boolean _isLast;

  private ValueExpression _items;
  private ValueExpression _beginVE;
  private ValueExpression _endVE;
  private ValueExpression _stepVE;

  private Integer _begin;
  private Integer _end;
  private Integer _step;

  private UIComponent _parentComponent;

  private Serializable _key;
  private MetaData _metaData;
  private Map<String, Object> _viewAttributes;

  private String _iterationMapKey;
  private Map<Serializable, MetaData> _metaDataMap;
  private Set<Serializable> _processedKeys;
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
  private static final String _META_DATA_MAP_KEY =
    ForEachTag.class.getName() + ".ITER";
}
