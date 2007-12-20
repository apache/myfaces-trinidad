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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import java.net.MalformedURLException;
import java.net.URL;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import org.apache.myfaces.trinidadinternal.share.io.DefaultNameResolver;
import org.apache.myfaces.trinidadinternal.share.io.FileInputStreamProvider;
import org.apache.myfaces.trinidadinternal.share.io.InputStreamProvider;
import org.apache.myfaces.trinidadinternal.share.io.NameResolver;
import org.apache.myfaces.trinidadinternal.share.io.URLInputStreamProvider;

import org.apache.myfaces.trinidadinternal.style.StyleContext;


/**
 * Package-private utility class used by StyleSheetEntry to
 * locate style sheet source files.  We look for style sheets
 * in both the local and the shared "styles" directory.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/skin/StyleSheetNameResolver.java#0 $) $Date: 10-nov-2005.18:59:02 $
 */
class StyleSheetNameResolver implements NameResolver
{
  /**
   * Creates a NameResolver which can locate style sheets
   * for the specified StyleContext
   */
  public static NameResolver createResolver(StyleContext context)
  {
    File localStylesDir = _getStylesDir(context);

    // Make sure we have some styles directory
    if ((localStylesDir == null))
    {
      _LOG.warning(_STYLES_DIR_ERROR);
      return null;
    }

    return new StyleSheetNameResolver(localStylesDir);
  }

  /**
   * Creates a StyleSheetNameResolver which looks in the specified
   * styles directories.  Note that the constructor is private since
   * StyleSheetEntry always calls createResolver().
   * @param localStylesDirectory The location of the local styles directory
   */
  private StyleSheetNameResolver(
    File localStylesDirectory
    )
  {
    // We should always have some directory
    assert ((localStylesDirectory != null));

    _localStylesDir = localStylesDirectory;

  }

  /**
   * Implementation of NameResolver.getProvider().
   * Given the name of the file, create an InputStreamProvider. I
   */
  public InputStreamProvider getProvider(String name) throws IOException
  {
    File file = _resolveLocalFile(name);
    if (file != null)
      return new FileInputStreamProvider(file);
      
    // Gets an URL for the specified name. 
    // Try a few different means to get the file as an url and then create the appropriate
    // InputStreamProvider from that URL.
    URL url = _resolveNonStaticURL(name);
    if (url != null)
      return new URLInputStreamProvider(url);
    else
    {
      // see if it is an URL that can be loaded by the ClassLoader. 
      // We create a StaticURLInputStreamProvider from the url because we consider the
      // url static because it can't be changed without restarting the server, so we don't
      // need to check if the source has changed.
      url = _resolveClassLoaderURL(name);
      if (url != null)
        return new StaticURLInputStreamProvider(url);
    }
      


    // If we couldn't locate the file, throw an IOException
    throw new FileNotFoundException(_getFileNotFoundMessage(name));
  }

  /**
   * Implementation of NameResolver.getResolver()
   */
  public NameResolver getResolver(String name)
  {
    URL url = null;
    File file = _resolveLocalFile(name);
    if (file == null)
    {
      // Gets an URL for the specified name. 
      // Try a few different means to get the file as an url: 
      // new URL, ExternalContext's getResource, ClassLoaderUtils getResource
      
      url = _resolveNonStaticURL(name);
      if (url == null)
        url =_resolveClassLoaderURL(name);
    }

    // Just use a DefaultNameResolver to resolve relative files
    return new DefaultNameResolver(file, url);
  }

  // Gets a File for the specified name, or returns null if no file exists
  // Try the local styles directory.
  private File _resolveLocalFile(String name)
  {
    // Try the local styles directory
    File file = new File(_localStylesDir, name);
    if (file.exists())
      return file;

    return null;
  }

  // Gets an URL for the specified name using ClassLoaderUtils.getResource
  private URL _resolveClassLoaderURL(String name)
  {
    if (name == null)
      return null;
    return ClassLoaderUtils.getResource(name);
    
  }
  
  // Gets an URL for the non static urls -- that is, urls that could change after the 
  // server has started.
  private URL _resolveNonStaticURL(String name)
  {
    if (name == null)
      return null;
    FacesContext fContext = FacesContext.getCurrentInstance();
    if (fContext != null)
    {
      try
      {
        if (name.startsWith("http:") ||
            name.startsWith("https:") ||
            name.startsWith("file:") ||
            name.startsWith("ftp:") ||
            name.startsWith("jar:"))
        {
          URL url = new URL(name);
          if (url != null)
            return url;
        }
        else
        {
          String rootName = _getRootName(name);
          // Return a URL for the application resource mapped to the specified path, 
          // if it exists; otherwise, return null.
          URL url = fContext.getExternalContext().getResource(rootName);
          if (url != null)
            return url;
        }
      }
      catch (MalformedURLException e)
      {
        // Eat the MalformedURLException - maybe the name isn't an URL
        ;
      }
    }
    return null;
  }

  // Construct error message for the specified file name
  private String _getFileNotFoundMessage(String name)
  {
    StringBuffer buffer = new StringBuffer();
    buffer.append("Unable to locate style sheet \"");
    buffer.append(name);
    buffer.append("\" in ");

    if (_localStylesDir != null)
    {
      buffer.append("local styles directory (");
      buffer.append(_localStylesDir.getPath());
      buffer.append("), ");
    }

    buffer.append("or on the class path.\n");
    buffer.append("Please be sure that this style sheet is installed.");

    return buffer.toString();
  }

  // Returns the File corresponding to the styles directory - either
  // the local directory or the shared directory - depending on the
  // shared value
  private static File _getStylesDir(
    StyleContext context)
  {
    String contextPath = context.getGeneratedFilesPath();

    // We only need to look up the shared styles path if the shared
    // context path is non-null.  If the shared context path is null,
    // we don't have a shared styles directory (and calling
    // Configuration.getPath() may throw a DirectoryUnavailableException).
    if (contextPath == null)
      return null;

    String stylesPath = contextPath + "/adf/styles";

    // Convert the path to a File
    File stylesDir = new File(stylesPath);

    // Make sure the directory actually exists
    if (stylesDir.exists())
       return stylesDir;

    return null;
  }

  // Returns a name which can be resolved relative to the
  // ServletContext root.
  private static String _getRootName(String name)
  {
    // Tack on a starting "/" if the name doesn't already have one -
    // seems to be required by ServletContext.getRealPath() and
    // ServletContext.getResource() - at least on OC4J.
    return (name.startsWith("/")) ? name : ("/" + name);
  }



  // A subclass of URLInputStreamProvider which never checks for
  // modifications
  private static class StaticURLInputStreamProvider
    extends URLInputStreamProvider
  {
    public StaticURLInputStreamProvider(URL url)
    {
      super(url);
    }

    @Override
    public boolean hasSourceChanged()
    {
      return false;
    }
  }


  private File _localStylesDir;

  // Error messages
  private static final String _STYLES_DIR_ERROR =
    "Could not locate the Trinidad styles directory."
    + "Please be sure that the Trinidad installable resources are installed.";

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(StyleSheetNameResolver.class);
}

