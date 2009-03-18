package org.apache.myfaces.trinidad.style;

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

import java.util.Iterator;

/**
 * A Style object defines a set of visual (or aural) style properties.
 * The Style interface exposes one method for retrieving properties:
 * getProperty().  getProperty() takes a String property name and 
 * returns a String property value. You can get an Iterator of PropertyNames
 * by calling getPropretyNames(). 
 *
 */
public interface Style
{

  /**
   * Returns the names of the properties defined by this style.
   */
  // -= Simon Lessard =-
  // FIXME: This should be changed to <String> once the issues 
  //        with ArrayMap are fixed. ATM (2006-08-04) ArrayMap 
  //        have huge problem working with anything but Object???
  // TODO???
  public Iterator<Object> getPropertyNames();

  /**
   * Returns the value of the property with the specified name.
   *
   * @param name The property name for the property to return
   */
  public String getProperty(String name);


  /**
   * Converts the style to a String suitable for use as an inline style
   * attribute value.
   */
  public String toInlineString();
}

