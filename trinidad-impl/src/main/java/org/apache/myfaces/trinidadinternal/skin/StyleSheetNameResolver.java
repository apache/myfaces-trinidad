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
import javax.servlet.ServletContext;

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
 * @author The Oracle ADF Faces Team
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

    // Load the ServletContext so we can try getRealPath();  but
    // we also use ExternalContext to get URLs
    // =-=AEW This is a holdover from ancient days;  it should
    // be sufficient to use URL-based access
    ServletContext servletContext = _getServletContext();

    return new StyleSheetNameResolver(localStylesDir,
                                      servletContext);
  }

  /**
   * Creates a StyleSheetNameResolver which looks in the specified
   * styles directories.  Note that the constructor is private since
   * StyleSheetEntry always calls createResolver().
   * @param localStylesDirectory The location of the local styles directory
   * @param sharedStylesDirectory The location of the shared styles directory
   */
  private StyleSheetNameResolver(
    File localStylesDirectory,
    ServletContext servletContext
    )
  {
    // We should always have some directory
    assert ((localStylesDirectory != null));

    _localStylesDir = localStylesDirectory;

    _servletContext = servletContext;
  }

  /**
   * Implementation of NameResolver.getProvider().
   */
  public InputStreamProvider getProvider(String name) throws IOException
  {
    File file = _resolveFile(name);
    if (file != null)
      return new FileInputStreamProvider(file);

    URL url = _resolveURL(name);
    if (url != null)
      return new StaticURLInputStreamProvider(url);

    // If we couldn't locate the file, throw an IOException
    throw new FileNotFoundException(_getFileNotFoundMessage(name));
  }

  /**
   * Implementation of NameResolver.getResolver()
   */
  public NameResolver getResolver(String name)
  {
    URL url = null;
    File file = _resolveFile(name);
    if (file == null)
      url = _resolveURL(name);

    // Just use a DefaultNameResolver to resolve relative files
    return new DefaultNameResolver(file, url);
  }

  // Gets a File for the specified name, or returns null if no file exists
  private File _resolveFile(String name)
  {
    // First try to local styles directory
    File file = _createFile(_localStylesDir, name);
    if (file != null)
      return file;


    // Finally, try relative to the context root
    if (_servletContext != null)
    {
      // Use ServletContext.getRealPath() to locate the file.
      String rootName = _getRootName(name);
      String realPath = _servletContext.getRealPath(rootName);

      if (realPath != null)
      {
        File realFile = new File(realPath);
        if (realFile.exists())
          return realFile;
      }
    }

    return null;
  }

  // Creates the File for the specified base directory/file name,
  // assuming the file exists.  Otherwise, returns null;
  private File _createFile(File baseDir, String name)
  {
    File file = new File(baseDir, name);
    if (file.exists())
      return file;

    return null;
  }

  // Gets an URL for the specified name
  private URL _resolveURL(String name)
  {
    FacesContext fContext = FacesContext.getCurrentInstance();
    if (fContext != null)
    {
      try
      {
        String rootName = _getRootName(name);
        URL url = fContext.getExternalContext().getResource(rootName);
        if (url != null)
          return url;
      }
      catch (MalformedURLException e)
      {
        // Eat the MalformedURLException - maybe the name isn't an URL
        ;
      }
    }


    return ClassLoaderUtils.getResource(name);
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


    if (_servletContext != null)
    {
      buffer.append("or in context root (");
      buffer.append(_servletContext.getRealPath("/"));
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

  // Private utility method for retrieving a ServletContext from
  // a StyleContext
  private static ServletContext _getServletContext()
  {
    FacesContext fContext = FacesContext.getCurrentInstance();
    Object app = fContext.getExternalContext().getContext();
    if (app instanceof ServletContext)
      return (ServletContext) app;

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

  // We use the ServletContext to search for context-relative
  // style sheets.
  private ServletContext _servletContext;

  // Error messages
  private static final String _STYLES_DIR_ERROR =
    "Could not locate the Trinidad styles directory."
    + "Please be sure that the Trinidad installable resources are installed.";

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(StyleSheetNameResolver.class);
}

