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
package org.apache.myfaces.trinidad.change;

import javax.faces.component.UIComponent;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Change specialization for change in attributes.
 * While applying this Change, the specified attribute state is  restored.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-api/src/main/java/oracle/adf/view/faces/change/AttributeComponentChange.java#0 $) $Date: 10-nov-2005.19:09:56 $
 */
public class AttributeComponentChange extends ComponentChange
{
  /**
   * Constructs an AttributeChange with the given attributeName and 
   *  attributeValue.
   * @param attributeName The name of the attribute for which the value needs
   *         to be restored.
   * @param attributeValue The value of the attribute that needs to be restored.
   *         This value should be of type java.io.Serializable in order to be 
   *         persisted.
   * @throws IllegalArgumentException if specified attributeName were to be null.
   */
  public AttributeComponentChange(
    String attributeName,
    Object attributeValue
    )
  {
    if ((attributeName == null) || (attributeName.length() == 0))
      throw new IllegalArgumentException(_LOG.getMessage(
        "CANNOT_CONSTRUCT_ATTRIBUTECHANGE_WITH_NULL_NAME"));
    _attributeName = attributeName;
    _attributeValue = attributeValue;
  }
  
  /**
   * Returns the name of the attribute that represents this Change.
   */
  public String getAttributeName()
  {
    return _attributeName;
  }

  /**
   * Returns the value of the attribute corresponding to this AttributeChange.
   */
  public Object getAttributeValue()
  {
    return _attributeValue;
  }
  
  /**
   * {@inheritDoc}
   */
  @SuppressWarnings("unchecked")
  @Override
  public void changeComponent(UIComponent uiComponent)
  {
    uiComponent.getAttributes().put(_attributeName, _attributeValue);
  }

  private final String _attributeName;
  
  //=-=pu: Should we disallow non-serializable values during construction itself
  //      and throw IllegalArgumentException ?
  // Current known cases:
  //  The focusPath attribute of a tree, which is considered as a persistible
  //    Change takes a value that is a java.util.List. (-ve)
  //  treeState of a tree is a org.apache.myfaces.trinidad.model.PathSet that
  //    implements serializable. (+ve)
  //  selectionState of a table is org.apache.myfaces.trinidad.model.RowKeySet that
  //    implements externalizable. (+ve)
  private final Object _attributeValue;
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    AttributeComponentChange.class);
  private static final long serialVersionUID = 1L;
}
