/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidadinternal.skin.pregen.variant;

import org.apache.myfaces.trinidad.context.Version;
import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;

/**
 * A convenience duple class that holds onto a TrinidadAgent.Application and
 * Version value.
 * 
 * Only note of interest: the AppicationAndVersion.version is guaranteed to
 * be concrete (ie. no wildcards).
 */
public final class ApplicationAndVersion implements Comparable<ApplicationAndVersion>
{
  public final TrinidadAgent.Application application;
  public final Version version;

  /**
   * An instance that can be used as a placeholder for cases
   * where the application and version are not known.
   */
  public static final ApplicationAndVersion UNKNOWN = 
    new ApplicationAndVersion(
      TrinidadAgent.Application.UNKNOWN,
      new Version("0"));
  
  /**
   * Creates an AppplicationAndVersion instance for the
   * specified application and version.
   * 
   * If the Version contains wildcards, this will be replaced by a new,
   * matching, concrete (wildcard-free) Version instance.
   * 
   * @param application a non-null TrinidadAgent.Application instance.
   * @param version a non-null Version instance.
   */
  public ApplicationAndVersion(
    TrinidadAgent.Application application,
    Version version
    )
  {
    assert(application != null);
    assert(version != null);

    this.application = application;
    this.version = version.toConcreteVersion();
  }
  
  @Override
  public String toString()
  {
    return this.application + "v" + this.version;
  }

  @Override
  public int compareTo(ApplicationAndVersion appAndVersion)
  {
    int appResult = this.application.compareTo(appAndVersion.application);
    
    if (appResult != 0)
    {
      return appResult;
    }
    
    return this.version.compareTo(appAndVersion.version);
  }
  
  @Override
  public boolean equals(Object o)
  {
    if (this == o)
    {
      return true;
    }
    
    if (!(o instanceof ApplicationAndVersion))
    {
      return false;
    }
    
    ApplicationAndVersion appAndVersion = (ApplicationAndVersion)o;
    
    return (this.application.equals(appAndVersion.application) &&
            this.version.equals(appAndVersion.version));
  }
  
  @Override
  public int hashCode()
  {
    int result = 17;
    
    result = 31 * result + this.application.hashCode();
    result = 31 * result + this.version.hashCode();
    
    return result;
  }
}
