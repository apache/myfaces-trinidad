/*
 * Copyright  2000-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.adfinternal.share.expl;

/**
 * NSFunctionMapper is the abstraction for mapping namespaced names to
 * functions.
 *  <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/expl/NSFunctionMapper.java#0 $) $Date: 10-nov-2005.19:00:14 $
 * @author The Oracle ADF Faces Team
 */
public abstract class NSFunctionMapper
{
  /**
   * Returns the function for this namespace and name.
   * This should return a public static Method.
   */
  public abstract Function resolveFunction(
    String namespaceURI,
    String localName);
}
