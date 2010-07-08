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
package org.apache.myfaces.trinidadinternal.menu;

import java.util.List;
import java.util.Map;

import javax.el.ELContext;
import javax.el.ExpressionFactory;
import javax.el.MethodExpression;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.trinidad.util.ContainerUtils;

/**
 * This class is a thread safe version of ItemNode class.
 * It replicates most of the code in ItemNode but makes
 * sure it does not modify state of the object.
 * 
 * Therefore multiple request threads can access the
 * properties of the objects of this class,in a thread safe
 * manner.
 * 
 * Please note that setters should not be called on objects
 * of this class.Objects of this class are fully initialized
 * on construction.
 *
 */
public class ImmutableItemNode extends org.apache.myfaces.trinidad.menu.ImmutableItemNode
{
  /**
    * Constructs an ItemNode
    */
  public ImmutableItemNode(ItemNode node)
  {
    super(node);
  }
}
