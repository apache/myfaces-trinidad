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
package org.apache.myfaces.trinidadinternal.share.nls;

/**
 * The ImmutableDecimalFormatContext class contains all number
 * format parameters, and cannot be changed once initialised.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/nls/ImmutableDecimalFormatContext.java#0 $) $Date: 10-nov-2005.19:00:02 $
 * @author The Oracle ADF Faces Team
 */
public final class ImmutableDecimalFormatContext extends DecimalFormatContext
{
  /**
   * Constructs a new ImmutableDecimalFormatContext,
   * cloning the instance passed in.
   */
  public ImmutableDecimalFormatContext(
    DecimalFormatContext dfc)
  {
    _hashCode = dfc.hashCode();
    _dfc = (DecimalFormatContext) dfc.clone();
  }


  /**
   * Returns the character used to separate number groupings.
   */
  public char getGroupingSeparator()
  {
    return _dfc.getGroupingSeparator();
  }

  /**
   * Returns the character used as a decimal separator.
   */
  public char getDecimalSeparator()
  {
    return _dfc.getDecimalSeparator();
  }

  public boolean equals(Object obj)
  {
    return super.equals(obj);
  }

  public int hashCode()
  {
    return _hashCode;
  }

  private DecimalFormatContext _dfc;
  private int _hashCode;
}

