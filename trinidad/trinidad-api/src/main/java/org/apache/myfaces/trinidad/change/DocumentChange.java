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

package org.apache.myfaces.trinidad.change;

import org.w3c.dom.Node;

/**
 * Document-aware Changes inrfaces.  Changes that can apply their changes
 * to the underlying JSP DOM document, implement this interface to avoid requiring
 * a converter class be registered with the ChangeMananger
 * 
 * @author The Oracle ADF Faces Team
 */
public interface DocumentChange
{
  /**
   * Apply this change to the specied root DOM Node of the component
   * @param Root DOM node of the component to apply the change to
   */
  public void changeDocument(Node componentNode);
  
  /** 
   * Returns true if adding the DocumentChange should force the JSP Document
   * to reload
   */
  public boolean getForcesDocumentReload();
}
