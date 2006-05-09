/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.adfbuild.plugin.xrts;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

/**
 * The <code>OrderedDictionary</code> class is an implementation of the
 * Sun's <code>Hashtable</code> that guarantees order of keys.
 *
 * @version $Name:  $ ($Revision: 1.2 $) $Date: 2001/04/09 23:20:09 $
 * @author Craig R. Cummings
 */
final class OrderedDictionary extends Hashtable
{
  OrderedDictionary()
  {
    super();
    _keys = new Vector();
  }

  public synchronized Enumeration keys()
  {
    return _keys.elements();
  }

  public synchronized Object put(Object key, Object value)
  {
    Object oldValue = super.put(key, value);
    _keys.addElement(key);

    return oldValue;
  }

  private Vector _keys;
}
