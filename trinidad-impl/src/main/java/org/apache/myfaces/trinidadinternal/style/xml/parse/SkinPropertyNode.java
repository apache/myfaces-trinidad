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
package org.apache.myfaces.trinidadinternal.style.xml.parse;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
/**
 * A SkinProperyNode is a node that
 * contains the selector, the -tr- property, and the value
 * e.g., af|breadCrumbs, -tr-show-last-item, true
 * This parses the XSS file.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/SkinPropertyNode.java#0 $) $Date: 10-nov-2005.18:50:45 $
 */
public class SkinPropertyNode
{
  /**
   * Creates a SkinPropertyNode
   *
   * @param key The key of the skin property, like af|breadCrumbs-tr-show-last-item
   * @param value The value of the skin property
   */
  public SkinPropertyNode(
    Object key,
    Object value
    )
  {

    assert(key != null);
    assert(value != null);
  
    _key = key;
    _value = value;
  }


  /**
   * Returns the value of the skin property that is defined
   * by this SkinPropertyNode.
   */
  public Object getValue()
  {
    return _value;
  }
  

  /**
   * Returns the key of the SkinPropertyNode
   */
  public Object getKey()
  {
    return _key;
  }

  private Object      _value;
  // The key is the SkinProperty key, which is usually the selector + name.
  private Object      _key;

}
