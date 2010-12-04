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
package org.apache.myfaces.trinidadinternal.el;

import java.beans.FeatureDescriptor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.el.ELContext;
import javax.el.ELResolver;

import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.model.CollectionModel;


/**
 * ELResolver implementation for Trinidad.  Serves up:
 * <ul>
 *   <li>requestContext: the RequestContext instance</li>
 *   <li>pageFlowScope: the page flow scope Map</li>
 *   <li>processScope: a backwards compatibility alias for page flow scope</li>
 * </ul>
 */
public class TrinidadELResolver
  extends ELResolver
{
  /**
   * EL variable name for page-flow scope
   */
  static public final String PAGE_FLOW_SCOPE_VARIABLE_NAME = "pageFlowScope";

  public TrinidadELResolver()
  {
  }

  public Object getValue(
    ELContext elContext,
    Object    base,
    Object    property)
  {
    if (base == null)
    {
      if (RequestContext.VARIABLE_NAME.equals(property))
      {
        elContext.setPropertyResolved(true);
        return RequestContext.getCurrentInstance();
      }
      // Support both "pageFlowScope" and "processScope"
      // as EL variables to give developers a time to migrate
      else if (PAGE_FLOW_SCOPE_VARIABLE_NAME.equals(property) ||
             "processScope".equals(property))
      {
        elContext.setPropertyResolved(true);
        return RequestContext.getCurrentInstance().getPageFlowScope();
      }
    }
    else if (base != null && base instanceof CollectionModel)
    {
      elContext.setPropertyResolved(true);
      CollectionModel model = (CollectionModel)base;
      return (property instanceof Integer) ?
        model.getRowData(((Integer)property).intValue()) :
        model.getRowData(property);
    }

    return null;
  }

  public Class<?> getType(
    ELContext elContext,
    Object    base,
    Object    property)
  {
    if (base == null)
    {
      if (RequestContext.VARIABLE_NAME.equals(property))
      {
        elContext.setPropertyResolved(true);
        return RequestContext.class;
      }
      // Support both "pageFlowScope" and "processScope"
      // as EL variables to give developers a time to migrate
      else if (PAGE_FLOW_SCOPE_VARIABLE_NAME.equals(property) ||
             "processScope".equals(property))
      {
        elContext.setPropertyResolved(true);
        return Map.class;
      }
    }
    else if (base != null && base instanceof CollectionModel)
    {
      elContext.setPropertyResolved(true);
      return Object.class;
    }

    return null;
  }

  public void setValue(ELContext elContext, Object base, Object property,
                       Object value)
  {
    if (PAGE_FLOW_SCOPE_VARIABLE_NAME.equals(base) ||
        "processScope".equals(base))
    {
      Map m = RequestContext.getCurrentInstance().getPageFlowScope();
      m.put(property, value);
      elContext.setPropertyResolved(true);
    }
  }

  public boolean isReadOnly(
    ELContext elContext,
    Object    base,
    Object    property)
  {
    if (base != null && base instanceof CollectionModel)
    {
      return true;
    }

    return false;
  }

  public Iterator<FeatureDescriptor> getFeatureDescriptors(ELContext elContext,
                                                           Object base)
  {
    if (base == null)
    {
      if (_featureDescriptorList == null)
        _featureDescriptorList = _createFeatureDescriptorList();
      return _featureDescriptorList.iterator();
    }
    else if (base != null && base instanceof CollectionModel)
    {
      CollectionModel model = (CollectionModel)base;
      int rowCount = model.getRowCount();

      List<FeatureDescriptor> list = rowCount == -1 ?
        new ArrayList<FeatureDescriptor>() :
        new ArrayList<FeatureDescriptor>(rowCount);

      Object origRowKey = model.getRowKey();
      try
      {
        for (int rowIndex = 0; model.isRowAvailable(rowIndex); ++rowIndex)
        {
          model.setRowIndex(rowIndex);

          String name = Integer.toString(rowIndex);
          FeatureDescriptor descriptor = new FeatureDescriptor();

          descriptor.setName(name);
          descriptor.setDisplayName(name);
          descriptor.setShortDescription("");
          descriptor.setExpert(false);
          descriptor.setHidden(false);
          descriptor.setPreferred(true);
          descriptor.setValue("type", Integer.class);
          descriptor.setValue("resolvableAtDesignTime", Boolean.TRUE);

          list.add(descriptor);
        }
      }
      finally
      {
        model.setRowKey(origRowKey);
      }

      return list.iterator();
    }

    return null;
  }

  public Class<?> getCommonPropertyType(
    ELContext elContext,
    Object    base)
  {
    if (base == null)
    {
      return String.class;
    }
    else if (base != null && base instanceof CollectionModel)
    {
      return Object.class;
    }

    return null;
  }

  /**
   * Creates the list of FeatureDescriptors.
   */
  private List<FeatureDescriptor> _createFeatureDescriptorList()
  {
    ArrayList<FeatureDescriptor> list = new ArrayList<FeatureDescriptor>();
    // FeatureDescriptor for "requestContext"
    FeatureDescriptor requestContext = new FeatureDescriptor();
    requestContext.setName(RequestContext.VARIABLE_NAME);
    // TODO translate this
    requestContext.setShortDescription("Trinidad RequestContext");
    requestContext.setValue(ELResolver.RESOLVABLE_AT_DESIGN_TIME, false);
    requestContext.setValue(ELResolver.TYPE, RequestContext.class);
    list.add(requestContext);

    // FeatureDescriptor for "pageFlowScope"
    FeatureDescriptor pageFlowScope = new FeatureDescriptor();
    pageFlowScope.setName(PAGE_FLOW_SCOPE_VARIABLE_NAME);
    // TODO translate this
    pageFlowScope.setShortDescription("Trinidad Page Flow Scope");
    pageFlowScope.setValue(ELResolver.RESOLVABLE_AT_DESIGN_TIME, false);
    pageFlowScope.setValue(ELResolver.TYPE, Map.class);
    list.add(pageFlowScope);

    // FeatureDescriptor for "processScope"
    FeatureDescriptor processScope = new FeatureDescriptor();
    processScope.setName(PAGE_FLOW_SCOPE_VARIABLE_NAME);
    // TODO translate this
    pageFlowScope.setShortDescription(
      "Backwards compatibility alias for Trinidad Page Flow Scope");
    processScope.setExpert(true);
    processScope.setValue(ELResolver.RESOLVABLE_AT_DESIGN_TIME, false);
    processScope.setValue(ELResolver.TYPE, Map.class);
    list.add(pageFlowScope);

    return Collections.unmodifiableList(list);
  }

  private List<FeatureDescriptor> _featureDescriptorList;
}
