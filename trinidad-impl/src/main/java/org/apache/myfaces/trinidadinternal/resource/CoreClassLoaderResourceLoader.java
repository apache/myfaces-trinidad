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
package org.apache.myfaces.trinidadinternal.resource;

import java.io.IOException;
import java.net.URL;

import org.apache.myfaces.trinidad.resource.ClassLoaderResourceLoader;
import org.apache.myfaces.trinidad.resource.ResourceLoader;


/**
 * A resource loader implementation which loads resources
 * using the context class loader with a rootPackage of META-INF. This will
 * find the resources within the META-INF directory.
 */
public class CoreClassLoaderResourceLoader extends ClassLoaderResourceLoader
{
  /**
   * Creates a new CoreClassLoaderResourceLoader
   */
  public CoreClassLoaderResourceLoader(ResourceLoader parent)
  {
   super("META-INF", parent);
  }


    
  /**
   * Override to pull out the version from the path.
   */
   @Override
  protected URL findResource(
    String path) throws IOException
  {
    String version = CoreRenderKitResourceLoader.__getVersion();
    int index = path.indexOf(version);
    if (index >= 0)
    {
      path = (path.substring(0, index) +
              path.substring(index + version.length()));
    }

    return super.findResource(path);
  }
}
