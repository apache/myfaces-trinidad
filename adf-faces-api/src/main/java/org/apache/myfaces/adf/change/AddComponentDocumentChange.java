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

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Node;

/**
 * Base class for specialized DocumentChanges that when applied will add a component
 * instance to the component tree.
 * @author The Oracle ADF Faces Team
 */
abstract public class AddComponentDocumentChange implements DocumentChange
{
  protected AddComponentDocumentChange(
    DocumentFragment fragment)
  {
    if (fragment == null)
      throw new IllegalArgumentException("DocumentFragment required");
      
    _fragment = fragment;
  }
  
  
  /** 
   * Returns true if adding the DocumentChange should force the JSP Document
   * to reload
   */
  public boolean getForcesDocumentReload()
  {
    // adding components should force the document to reload
    return true;
  }

  /**
   * Returns the component that is to be added either as a child or a facet 
   *  while applying this Change. Returns null if the component cannot be 
   *  successfully re-constructed.
   */
  protected final DocumentFragment getComponentFragment()
  {
    return _fragment;
  }
  
  /**
   * Given the target Node, return the DocumentFragment, imported into the
   * target Document
   * @param targetNode 
   * @return 
   */
  protected final DocumentFragment getImportedComponentFragment(Node targetNode)
  {
    Document targetDocument = targetNode.getOwnerDocument();
    
    // return a deep import
    return (DocumentFragment)targetDocument.importNode(_fragment, true);
  }
  
  private final DocumentFragment _fragment;
}
