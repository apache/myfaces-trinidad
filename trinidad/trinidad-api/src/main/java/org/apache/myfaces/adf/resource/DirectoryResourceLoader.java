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

package org.apache.myfaces.adf.resource;

import java.io.File;
import java.io.IOException;

import java.net.URL;

/**
 * A resource loader implementation which loads resources
 * from a directory.  The returned resource URL will be null
 * for file resources that do not exist, or for relative paths 
 * that attempt to access paths outside the root directory.
 *
 * @author The Oracle ADF Faces Team
 */
public class DirectoryResourceLoader extends ResourceLoader
{
  /**
   * Constructs a new DirectoryResourceLoader.
   *
   * @param directory  the root directory
   */
  public DirectoryResourceLoader(
    File directory)
  {
    if (directory == null)
      throw new NullPointerException();

    if (!directory.isDirectory())
      throw new IllegalArgumentException();

    _directory = directory;
  }

  /**
   * Constructs a new DirectoryResourceLoader.
   *
   * @param directory  the root directory
   * @param parent     the parent resource loader
   */
  public DirectoryResourceLoader(
    File           directory,
    ResourceLoader parent)
  {
    super(parent);

    if (directory == null)
      throw new NullPointerException();

    if (!directory.isDirectory())
      throw new IllegalArgumentException();

    _directory = directory;
  }

  protected URL findResource(
    String path) throws IOException
  {
    if (path.charAt(0) == '/')
      path = path.substring(1);

    // construct the relative file under the "root" directory
    File file = new File(_directory, path).getCanonicalFile();

    // "root" directory path should always be less than the file path
    boolean isContained = (_directory.compareTo(file) <= 0);

    // return null if relative paths were used, 
    // or if the file does not exist,
    // otherwise return an URL to the file resource
    return (isContained && file.exists()) ? file.toURL() : null;
  }

  private final File _directory;
}
