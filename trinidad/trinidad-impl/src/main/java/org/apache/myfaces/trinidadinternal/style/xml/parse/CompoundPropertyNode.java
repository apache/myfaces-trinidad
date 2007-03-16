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

import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;

/**
 * PropertyNode subclass for compound properties.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/style/xml/parse/CompoundPropertyNode.java#0 $) $Date: 10-nov-2005.18:58:04 $
 */
public class CompoundPropertyNode
{
  /**
   * Creates a PropertyNode with the specified name and value
   */
  public CompoundPropertyNode(
    String   name,
    Object[] values
    )
  {
    _name = name;

    if (values != null)
    {
      _values = new Object[values.length];
      System.arraycopy(values, 0,
                       _values, 0,
                       _values.length);
    }
  }

  /**
   * Returns the name of the CompoundPropertyNode
   */
  public String getName()
  {
    return _name;
  }

  /**
   * Returns an Iterator of values - which can be either Strings or
   * IncludePropertyNodes.
   */
  public Iterator<Object> getValues()
  {
    if (_values!=null)
    {
      return (Arrays.asList(_values)).iterator();  
    }
    else
    {
      return (Collections.emptyList()).iterator();
    }
    
  }

  private String   _name;
  private Object[] _values; // These are either Strings or IncludePropertyNodes
}
