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
package org.apache.myfaces.trinidad.webapp;

import java.io.File;
import java.io.IOException;

import java.net.URL;

import javax.faces.context.FacesContext;

import javax.servlet.ServletContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.resource.DirectoryResourceLoader;

/**
 * This is a package-private version of DirectoryResourceLoader that is used
 * with temp directories. When a resource fails to load, we check the current
 * temp directory against the one we've cached and if they are different, we
 * throw an illegal state exception to indicate the temp directory location
 * was illegally changed.
 */
class TempDirectoryResourceLoader extends DirectoryResourceLoader
{
  /**
   * Constructs a new TempDirectoryResourceLoader.
   *
   * @param context  the ServletContext object
   */
  public TempDirectoryResourceLoader(
    ServletContext context)
  {
    super((File)context.getAttribute("javax.servlet.context.tempdir"));
  }

  @Override
  protected URL findResource(
    String path) throws IOException
  {
    URL resourceURL = super.findResource(path);
    
    // On failure to find the resource, we want to log an error if the saved temp directory
    // is different than the current temp directory.
    if (resourceURL == null)
    {
      // Getting a fresh ServletContext object as we don't trust caching the one passed in to
      // the constructor.
      FacesContext fContext = FacesContext.getCurrentInstance();

      // this is OK because we only create instances of this class from the ResourceServlet 
      ServletContext sContext = (ServletContext) fContext.getExternalContext().getContext();
      File tempdir = (File) sContext.getAttribute("javax.servlet.context.tempdir");

      // Get the current tempdir path
      String newTempdDirPath = tempdir.getCanonicalPath();

      // Retrieve the old (cached) tempdir path from DirectoryResourceLoader.
      String oldTempDirPath = getDirectoryPath();
      
      //  If they aren't equal, throw exception as this indicates a serious problem.
      if (!newTempdDirPath.equals(oldTempDirPath))
      {
        throw new IllegalStateException(_LOG.getMessage(
          "TEMPDIR_CHANGED", new Object[] { newTempdDirPath, oldTempDirPath }));
      }
    }
    
    return resourceURL;
  }

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    TempDirectoryResourceLoader.class);
}
