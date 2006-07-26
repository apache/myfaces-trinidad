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
package org.apache.myfaces.adfinternal.ui.laf.base;

/**
 * The key used to look up an icon. Passed to a LafIconProvider to
 * retrieve the proper icon for a given look and feel. Each laf has
 * its own array of icons, but the indices into these arrays
 * are the same across all lafs. For example even though the
 * error icon is different in 2 lafs, its array index is the
 * same for those 2 lafs. The index is retrieved from the IconKey.
 *
 * @see org.apache.myfaces.adfinternal.ui.laf.base.LafIconProvider
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/IconKey.java#0 $) $Date: 10-nov-2005.18:53:01 $
 * @author The Oracle ADF Faces Team
 */
public final class IconKey
{
 /*
  *Each IconKey is given a unique keyIndex,
  * which can be used to return the correct icon for the current look and feel.
  */
  public IconKey(
    )
  {
    synchronized (IconKey.class)
    {
      _keyIndex = _sKeyCount++;
    }    
  }

  /**
   * Returns the total number of keys created
   */
  public static int getKeyCount()
  {
    return _sKeyCount;
  }
  
  /**
   * Returns the 0-based index of this IconKey
   */
  public int getKeyIndex()
  {
    return _keyIndex;
  }

  public boolean equals(Object o)
  {
    return this == o;
  }

  public int hashCode()
  {
    return _keyIndex;
  }

  private int _keyIndex;
  private static int _sKeyCount =0;

}
