/*
 * Copyright  2004-2006 The Apache Software Foundation.
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

package org.apache.myfaces.trinidad.resource;

import java.io.IOException;

import java.net.URL;


/**
 * A resource loader implementation which loads resources
 * using the context class loader.
 *
 * @author The Oracle ADF Faces Team
 */
public class ClassLoaderResourceLoader extends ResourceLoader
{
  /**
   * Constructs a new root ClassLoaderResourceLoader.
   */
  public ClassLoaderResourceLoader()
  {
    this((String)null);
  }

  /**
   * Constructs a new ClassLoaderResourceLoader with specified parent.
   *
   * @param parent  the parent resource loader
   */
  public ClassLoaderResourceLoader(
    ResourceLoader parent)
  {
    this(null, parent);
  }

  /**
   * Constructs a new root ClassLoaderResourceLoader with specified top
   * level resource package.
   *
   * @param rootPackage  the top level package used to interpret resource paths
   */
  public ClassLoaderResourceLoader(
    String rootPackage)
  {
    _resourcePrefix = _getResourcePrefix(rootPackage);
  }

  /**
   * Constructs a new root ClassLoaderResourceLoader with specified top
   * level resource package and parent resource loader.
   *
   * @param rootPackage  the top level package used to interpret resource paths
   * @param parent  the parent resource loader
   */
  public ClassLoaderResourceLoader(
    String rootPackage,
    ResourceLoader parent)
  {
    super(parent);
    _resourcePrefix = _getResourcePrefix(rootPackage);
  }

  protected URL findResource(
    String path) throws IOException
  {
    if (_resourcePrefix != null)
    {
      if (path.charAt(0) == '/')
        path = _resourcePrefix + path;
      else
        path = _resourcePrefix + "/" + path;
    }
    else
    {
      // String off leading slash, since this can
      // trip up ClassLoader.getResource()
      if (path.charAt(0) == '/')
        path = path.substring(1);
    }

    return getClassLoader().getResource(path);
  }

  /**
   * Returns the ClassLoader to use when looking up resources under the top
   * level package.  By default, this is the context class loader.
   *
   * @return the ClassLoader used to lookup resources
   */
  protected ClassLoader getClassLoader()
  {
    return Thread.currentThread().getContextClassLoader();
  }

  /**
   * Converts root package into a resource prefix.  For example, converts
   * the package "org.example" into resource prefix "org/example/".
   *
   * @param rootPackage  the root package
   *
   * @return the resource prefix
   */
  static private String _getResourcePrefix(
    String rootPackage)
  {
    if (rootPackage == null ||
        rootPackage.length() == 0)
    {
      return null;
    }

    return rootPackage.replace('.', '/');
  }

  private final String _resourcePrefix;
}
