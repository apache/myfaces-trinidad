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

import java.util.List;

import javax.el.MethodExpression;

import javax.faces.context.FacesContext;

import javax.faces.event.AbortProcessingException;
import javax.faces.event.FacesEvent;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.bean.PropertyKey;
import org.apache.myfaces.trinidad.event.RowDisclosureEvent;
import org.apache.myfaces.trinidad.event.RowDisclosureListener;
import org.apache.myfaces.trinidad.event.SelectionEvent;
import org.apache.myfaces.trinidad.event.SelectionListener;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.CollectionModel;
import org.apache.myfaces.trinidad.model.RowKeySet;
import org.apache.myfaces.trinidad.model.RowKeySetImpl;
import org.apache.myfaces.trinidad.model.RowKeySetTreeImpl;
import org.apache.myfaces.trinidad.model.TreeModel;

 /**
  * Base class for ListView component.
  * 
  * This class iprovides stamping of  items vertically. In addition it provides grouping functionality through  
  * a treeModel and provides the wrapper around the treeModel for tree  related operations similar to the UIXTree. 
  * "groupDisclosedRowKeys"  provides grouping functionality along with disclosure support. 
  */
public abstract class UIXListViewTemplate extends UIXIterator
{
  /**/ public abstract RowKeySet getSelectedRowKeys();
  /**/ public abstract  void setSelectedRowKeys(RowKeySet selectedRowKeys);
  /**/ public abstract RowKeySet getGroupDisclosedRowKeys();
  /**/ public abstract void setGroupDisclosedRowKeys(RowKeySet groupDisclosedRowKeys);
  /**/ public abstract MethodExpression getSelectionListener();
  /**/ public abstract public MethodExpression getGroupDisclosureListener();

  /**/  static public final PropertyKey GROUP_DISCLOSED_ROW_KEYS_KEY = null;
  /**/  static public final PropertyKey SELECTED_ROW_KEYS_KEY = null;
  /**/  static public final FacesBean.Type TYPE = new FacesBean.Type(org.apache.myfaces.trinidad.component.UIXIterator.TYPE);

  // These are "fake" properties that allow the listView to get the disclosed row keys and the
  // selected row key without triggering the call to getCollectionModel from the
  // RowKeyFacesBeanWrapper class. See the stamp state saving code for its usage.
  static private final PropertyKey _GROUP_DISCLOSED_ROW_KEYS_WITHOUT_MODEL_KEY =
    TYPE.registerKey("groupDisclosedRowKeysWithoutModel", RowKeySet.class);
  static private final PropertyKey _SELECTED_ROW_KEYS_WITHOUT_MODEL_KEY =
     TYPE.registerKey("selectedRowKeysWithoutModel", RowKeySet.class);

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    UIXListView.class);

  /**
   * Sets the phaseID of UI events depending on the "immediate" property.
   */
  @Override
  public void queueEvent(FacesEvent event)
  {
    TableUtils.__handleQueueEvent(this, event);
    super.queueEvent(event);
  }

  /**
   * {@inheritDoc}
   *
   * Overridden to handle broadcast of the selection and the row disclosure events
   */

  @Override
  public void broadcast(FacesEvent event)
    throws AbortProcessingException
  {
    if (event instanceof SelectionEvent)
    {
      //vg: Implicitly record a Change for 'selectionState' attribute
      addAttributeChange("selectedRowKeys", getSelectedRowKeys());
      broadcastToMethodExpression(event, getSelectionListener());
    }
    else if (event instanceof RowDisclosureEvent)
    {
      RowDisclosureEvent dEvent = (RowDisclosureEvent) event;
      RowKeySet          state = getGroupDisclosedRowKeys();

      state.removeAll(dEvent.getRemovedSet());
      state.addAll(dEvent.getAddedSet());
      //vg: Implicitly record a Change for 'groupDisclosedRowKeys' attribute
      addAttributeChange("groupDisclosedRowKeys", state);
      broadcastToMethodExpression(event, getGroupDisclosureListener());
    }
    super.broadcast(event);
  }


  @Override
  protected void postCreateCollectionModel(CollectionModel model)
  {
    RowKeySet selectedRowKeys = getSelectedRowKeys();

    if (selectedRowKeys == null)
    {
      selectedRowKeys = (model instanceof TreeModel) ? new RowKeySetTreeImpl() : new RowKeySetImpl();
      setSelectedRowKeys(selectedRowKeys);
    }

    RowKeySet disclosedRowKeys = getGroupDisclosedRowKeys();

    if (disclosedRowKeys == null)
    {
      disclosedRowKeys = (model instanceof TreeModel) ? new RowKeySetTreeImpl() : new RowKeySetImpl();
      setGroupDisclosedRowKeys(disclosedRowKeys);
    }

    selectedRowKeys.setCollectionModel(model);
    disclosedRowKeys.setCollectionModel(model);
  }

  /**
   * Gets the internal state of this component.
   */
  @Override
  Object __getMyStampState()
  {
    Object[] state = new Object[3];
    state[0] = super.__getMyStampState();

    // Use "hidden" property keys to allow the row key sets to be retrieved without the
    // RowKeyFacesBeanWrapper trying to resolve the collection model to be set into the row key
    // set. This is needed to stop the unnecessary lookup of the collection model when it is not
    // needed during stamp state saving of the table.
    RowKeySet selectedRowKeys = (RowKeySet)getProperty(_SELECTED_ROW_KEYS_WITHOUT_MODEL_KEY);
    RowKeySet disclosedRowKeys = (RowKeySet)getProperty(_GROUP_DISCLOSED_ROW_KEYS_WITHOUT_MODEL_KEY);

    state[1] = selectedRowKeys;
    state[2] = disclosedRowKeys;

    return state;
  }

  /**
   * Sets the internal state of this component.
   * @param stampState the internal state is obtained from this object.
   */
  @Override
  @SuppressWarnings("unchecked")
  void __setMyStampState(Object stampState)
  {
    Object[] state = (Object[]) stampState;
    super.__setMyStampState(state[0]);
    setSelectedRowKeys((RowKeySet) state[1]);
    setGroupDisclosedRowKeys((RowKeySet) state[2]);
  }

  @Override
  void __resetMyStampState()
  {
    super.__resetMyStampState();
    setSelectedRowKeys(null);
    setGroupDisclosedRowKeys(null);
  }

  @Override
  protected FacesBean createFacesBean(String rendererType)
  {
    return new RowKeyFacesBeanWrapper(super.createFacesBean(rendererType));
  }

  private class RowKeyFacesBeanWrapper
    extends FacesBeanWrapper
  {
    private boolean _retrievingDisclosedRows = false;
    private boolean _retrievingSelectedRows = false;

    RowKeyFacesBeanWrapper(FacesBean bean)
    {
      super(bean);
    }

    @Override
    public Object getProperty(PropertyKey key)
    {
      if (key == _GROUP_DISCLOSED_ROW_KEYS_WITHOUT_MODEL_KEY)
      {
        // This case is only true if the table is trying to serialize the disclosed row keys to
        // the stamp state of a parent UIXCollection. This work-around prevents EL evaluation to
        // get the collection model during stamp state saving. This should be permissible as the
        // state saving code does not need the collection model to be set in the row key set in
        // order to save its state.
        return super.getProperty(GROUP_DISCLOSED_ROW_KEYS_KEY);
      }
      else if (key == _SELECTED_ROW_KEYS_WITHOUT_MODEL_KEY)
      {
        // This case is only true if the table is trying to serialize the selected row keys to
        // the stamp state of a parent UIXCollection. This work-around prevents EL evaluation to
        // get the collection model during stamp state saving. This should be permissible as the
        // state saving code does not need the collection model to be set in the row key set in
        // order to save its state.
        return super.getProperty(SELECTED_ROW_KEYS_KEY);
      }

      Object value = super.getProperty(key);
      if (key == GROUP_DISCLOSED_ROW_KEYS_KEY)
      {
        if (!_retrievingDisclosedRows && value instanceof RowKeySet)
        {
          // Ensure that when we are retrieving and setting the collection model, this property
          // is not asked for which would create an infinite loop
          _retrievingDisclosedRows = true;

          try
          {
            RowKeySet rowKeys = (RowKeySet) value;
            // row key sets need the most recent collection model, but there is no one common entry
            // point to set this on the set besides when code asks for the value from the bean
            rowKeys.setCollectionModel(getCollectionModel());
          }
          finally
          {
            _retrievingDisclosedRows = false;
          }
        }
      }
      else if (key == SELECTED_ROW_KEYS_KEY)
      {
        if (!_retrievingSelectedRows && value instanceof RowKeySet)
        {
          // Ensure that when we are retrieving and setting the collection model, this property
          // is not asked for which would create an infinite loop
          _retrievingSelectedRows = true;

          try
          {
            RowKeySet rowKeys = (RowKeySet) value;
            // row key sets need the most recent collection model, but there is no one common entry
            // point to set this on the set besides when code asks for the value from the bean
            rowKeys.setCollectionModel(getCollectionModel());
          }
          finally
          {
            _retrievingSelectedRows = false;
          }
        }
      }
      return value;
    }

    @Override
    public Object saveState(FacesContext context)
    {
      RowKeySet rowKeys = (RowKeySet)super.getProperty(GROUP_DISCLOSED_ROW_KEYS_KEY);
      if (rowKeys != null)
      {
        // make sure the set does not pin the model in memory
        rowKeys.setCollectionModel(null);
      }
      rowKeys = (RowKeySet)super.getProperty(SELECTED_ROW_KEYS_KEY);
      if (rowKeys != null)
      {
        // make sure the set does not pin the model in memory
        rowKeys.setCollectionModel(null);
      }
      return super.saveState(context);
    }
  }

  ////////////////////////////////////////////////////////////
  // Tree related methods
  ///////////////////////////////////////////////////////////

  /**
  * Treats the current element as a parent element and steps into the children.
  * A new path is constructed by appending the null value to the old path.
  * The rowData becomes null.
  * It is legal to call this method only if {@link #isContainer}
  * returns true.
  * @see TreeModel#enterContainer
  */
  public final void enterContainer()
  {
    preRowDataChange();
    _getTreeModel().enterContainer();
    postRowDataChange();
  }


  /**
  * Changes the rowData to be the parent rowData.
  * A new path is constructed by removing the last rowKey from the old path.
  * The element that is identified by the new path is made current.
  * @see TreeModel#exitContainer
  */
  public final void exitContainer()
  {
    preRowDataChange();
    _getTreeModel().exitContainer();
    postRowDataChange();
  }

  /**
   * Checks to see if the current element is a container of other elements.
   * @see TreeModel#isContainer
   * @return true if the current element contains other elements.
   */
  public final boolean isContainer()
  {
    return _getTreeModel().isContainer();
  }

  /**
   * Checks to see if the container is empty.
   * @see TreeModel#isContainerEmpty
   * @return true if the current container element has no children.
   */
  public boolean isContainerEmpty()
  {
    return _getTreeModel().isContainerEmpty();
  }

  /**
   * Gets the depth of the current row in this tree hierarchy
   * @see TreeModel#getDepth()
   * @return zero for any root rows.
   */
  public int getDepth()
  {
    return _getTreeModel().getDepth();
  }

  /**
   * Gets the depth of the current row in this tree hierarchy
   * @see TreeModel#getDepth(Object)
   * @return zero for any root rows.
   */
  public int getDepth(Object rowKey)
  {
    return _getTreeModel().getDepth(rowKey);
  }

  /**
   * Gets the rowKey of the current row's container.
   * @see TreeModel#getContainerRowKey
   */
  public Object getContainerRowKey()
  {
    return _getTreeModel().getContainerRowKey();
  }

  /**
   * Gets the rowKey of the given row's container.
   * @see TreeModel#getContainerRowKey(Object)
   */
  public Object getContainerRowKey(Object childKey)
  {
    return _getTreeModel().getContainerRowKey(childKey);
  }

  /**
   * Gets the all the rowKeys of the ancestors of the given child row.
   * @see TreeModel#getAllAncestorContainerRowKeys(Object)
   */
  public List<Object> getAllAncestorContainerRowKeys(Object childRowKey)
  {
    return _getTreeModel().getAllAncestorContainerRowKeys(childRowKey);
  }

  /**
   * Gets the TreeModel that this tree is displaying.
   */
  private TreeModel _getTreeModel()
  {
    CollectionModel model = getCollectionModel();

    if(!(model instanceof TreeModel))
    {
      throw new IllegalStateException(_LOG.getMessage("LISTVIEW_MODEL_NOT_TREEMODEL"));
    }
    return (TreeModel)model;
  }
}