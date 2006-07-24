/*
 * Copyright  2005,2006 The Apache Software Foundation.
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

package org.apache.myfaces.adf.change;

/**
 * Strategy Object for converting converting ComponentChanges to
 * DocumentChanges.
 * 
 * @author The Oracle ADF Faces Team
 */
public abstract class DocumentChangeFactory
{
  /**
  * Converts a ComponentChange to a DocumentChange, returning
  * <code>null</code> if no such conversion is possible.
   * @param compChange ComponentChange to convert.
   * @return equivalent DocumetnChange or <code>null</code> if no
   * conversion is possible.
   */
   public abstract DocumentChange convert(ComponentChange compChange);
}
