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
package org.apache.myfaces.trinidadinternal.skin;

import org.apache.myfaces.trinidad.skin.SkinVersion;

/**
 * Implementation of the SkinVersion public API.
 * If needed later, we can implement Comparable so we can sort SkinVersions.
 */
final public class SkinVersionImpl extends SkinVersion
{
  /**
   * Constructor that takes a version name. A version 'name' is required.
   * @param name the name of the version, like "v1".
   * same skin family
   */
  public SkinVersionImpl(String name)
  {
    this(name, false);
  }
  
  /**
   * Constructor that takes a name and a defaultVersion. A version 'name' is required.
   * @param name the name of the version, like "v1".
   * @param defaultVersion true if this skin is the default version for all skins with the
   * same skin family
   */
  public SkinVersionImpl(
    String  name,
    boolean defaultVersion)
  {
    if(name == null) 
      throw new IllegalArgumentException("SkinVersionImpl requires a non-null name parameter");
    
    _default = defaultVersion;
    _name = name;
    
    
  }
  
  @Override
  public boolean equals(Object o) 
  {
    if (o == this)
      return true;
    if (!(o instanceof SkinVersionImpl))
    {
      return false;
    }
    SkinVersionImpl test = (SkinVersionImpl)o;
    return (test._default == _default) &&
      (_name.equals(test._name));
  }
  
  @Override
  public int hashCode()
  {
    int hash = 17;
    hash = 37*hash + _name.hashCode();
    hash = 37*hash + ((_default) ? 1231 : 1237 );

    return hash; 
  } 
  
  public boolean isDefault()
  {
    return _default;
  }
  
  public String getName()
  {
    return _name;
  }

  @Override
  public String toString()
  {
    StringBuffer buffer = new StringBuffer("Version[");
    buffer.append(getName());

    boolean isDefault = isDefault();

    if (isDefault)
    {
      buffer.append(',');
      buffer.append("default");
    }
    return buffer.toString();
  }
  
  private final boolean _default;
  private final String _name;
}
