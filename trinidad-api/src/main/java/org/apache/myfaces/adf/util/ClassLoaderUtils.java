/*
 * Copyright  2001-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adf.util;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.IOException;

import java.io.InputStreamReader;

import java.net.URL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

import org.apache.myfaces.adf.logging.ADFLogger;

/**
 * Utility methods for accessing classes and resources using an appropriate
 * class loader.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/util/ClassLoaderUtils.java#0 $) $Date: 10-nov-2005.18:49:08 $
 * @author The Oracle ADF Faces Team
 */
public class ClassLoaderUtils
{
  /**
   * Loads the class with the specified name.  For Java 2 callers, the
   * current thread's context class loader is preferred, falling back on the
   * system class loader of the caller when the current thread's context is not
   * set, or the caller is pre Java 2.
   *
   * @param     name  the name of the class
   * @return    the resulting <code>Class</code> object
   * @exception ClassNotFoundException if the class was not found
   */
  static public Class loadClass(
    String name) throws ClassNotFoundException
  {
    return loadClass(name, null);
  }

  /**
   * Locates the resource with the specified name.  For Java 2 callers, the
   * current thread's context class loader is preferred, falling back on the
   * system class loader of the caller when the current thread's context is not
   * set, or the caller is pre Java 2.
   *
   * @param     name  the name of the resource
   * @return    the resulting <code>URL</code> object
   */
  static public URL getResource(
    String name)
  {
    return getResource(name, null);
  }

  /**
   * Locates the stream resource with the specified name.  For Java 2 callers,
   * the current thread's context class loader is preferred, falling back on
   * the system class loader of the caller when the current thread's context is
   * not set, or the caller is pre Java 2.
   *
   * @param     name  the name of the resource
   * @return    the resulting <code>InputStream</code> object
   */
  static public InputStream getResourceAsStream(
    String name)
  {
    return getResourceAsStream(name, null);
  }

  /**
   * Loads the class with the specified name.  For Java 2 callers, the
   * current thread's context class loader is preferred, falling back on the
   * class loader of the caller when the current thread's context is not set,
   * or the caller is pre Java 2.  If the callerClassLoader is null, then
   * fall back on the system class loader.
   *
   * @param     name  the name of the class
   * @param     callerClassLoader  the calling class loader context
   * @return    the resulting <code>Class</code> object
   * @exception ClassNotFoundException if the class was not found
   */
  static public Class loadClass(
    String      name,
    ClassLoader callerClassLoader) throws ClassNotFoundException
  {
    Class clazz = null;

    try
    {
      ClassLoader loader = getContextClassLoader();

      if (loader != null)
        clazz = loader.loadClass(name);
    }
    catch (ClassNotFoundException e)
    {
      // treat as though loader not set
      ;
    }

    if (clazz == null)
    {
      if (callerClassLoader != null)
        clazz = callerClassLoader.loadClass(name);
      else
        clazz = Class.forName(name);
    }

    return clazz;
  }

  /**
   * Locates the resource with the specified name.  For Java 2 callers, the
   * current thread's context class loader is preferred, falling back on the
   * class loader of the caller when the current thread's context is not set,
   * or the caller is pre Java 2.  If the callerClassLoader is null, then
   * fall back on the system class loader.
   *
   * @param     name  the name of the resource
   * @param     callerClassLoader  the calling class loader context
   * @return    the resulting <code>URL</code> object
   */
  static public URL getResource(
    String      name,
    ClassLoader callerClassLoader)
  {
    URL url = null;

    ClassLoader loader = getContextClassLoader();

    if (loader != null)
      url = loader.getResource(name);

    if (url == null)
    {
      if (callerClassLoader != null)
        url = callerClassLoader.getResource(name);
      else
        url = ClassLoader.getSystemResource(name);
    }

    return url;
  }

  /**
   * Locates the resource stream with the specified name.  For Java 2 callers,
   * the current thread's context class loader is preferred, falling back on
   * the class loader of the caller when the current thread's context is not
   * set, or the caller is pre Java 2.  If the callerClassLoader is null, then
   * fall back on the system class loader.
   *
   * @param     name  the name of the resource
   * @param     callerClassLoader  the calling class loader context
   * @return    the resulting <code>InputStream</code> object
   */
  static public InputStream getResourceAsStream(
    String      name,
    ClassLoader callerClassLoader)
  {
    InputStream stream = null;

    ClassLoader loader = getContextClassLoader();

    if (loader != null)
      stream = loader.getResourceAsStream(name);

    if (stream == null)
    {
      if (callerClassLoader != null)
        stream = callerClassLoader.getResourceAsStream(name);
      else
        stream = ClassLoader.getSystemResourceAsStream(name);
    }

    return stream;
  }

  /**
   * Dynamically accesses the current context class loader.
   * Returns null if there is no per-thread context class loader.
   */
  static public ClassLoader getContextClassLoader()
  {
    return Thread.currentThread().getContextClassLoader();
  }


  /**
   * Instantiate a service from a file in /META-INF/services.
   * <P>
   * The following is an excerpt from the JAR File specification:
   * A service provider identifies itself by placing a provider-configuration file 
   * in the resource directory META-INF/services. 
   * The file's name should consist of the fully-qualified name of the abstract service class. 
   * The file should contain a newline-separated list of unique concrete provider-class names. 
   * Space and tab characters, as well as blank lines, are ignored. The comment character is '#' (0x23); 
   * on each line all characters following the first comment character are ignored. 
   * The file must be encoded in UTF-8. 
   * 
   * @param service the classname of the abstract service class.
   * eg: javax.servlet.Filter
   */
  static public <T> List<T> getServices(String service)
  {
    String serviceUri ="META-INF/services/" + service;
    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    try
    {
      Enumeration urls = loader.getResources(serviceUri);
      if (urls.hasMoreElements())
      {
        List<T> services = new ArrayList<T>(1);
        do
        {
          URL url = (URL) urls.nextElement();
          _LOG.finest("Processing:{0}", url);
          try
          {
            BufferedReader in = new BufferedReader(new InputStreamReader(url.openStream()));
            while(true)
            {
              String line = in.readLine();
              if (line == null)
                break;
  
              T instance = (T) _parseLine(loader, line);
              if (instance != null)
                services.add(instance);
            }
            in.close();
          }
          catch (Exception e)
          {
            _LOG.warning("Error parsing:"+url, e);
          }
        } 
        while(urls.hasMoreElements());
        
        if (services.size() == 1)
          return Collections.singletonList(services.get(0));
        
        return Collections.unmodifiableList(services);
      }
    }
    catch (IOException e)
    {
      _LOG.severe("error loading resource:"+serviceUri, e);
    }

    return Collections.emptyList();
  }

  private static Object _parseLine(ClassLoader loader, String line)
    throws ClassNotFoundException, InstantiationException, 
           IllegalAccessException
  {
    // Eliminate any comments
    int hashIndex = line.indexOf('#');
    if (hashIndex >= 0)
      line = line.substring(0, hashIndex);

    // and any whitespace
    line = line.trim();
    if (line.length() > 0)
    {
      Class clazz = loader.loadClass(line);
      return clazz.newInstance();
    }
    return null;
  }

  // Utility class only, no instances
  private ClassLoaderUtils()
  {
  }


  private static final ADFLogger _LOG =
    ADFLogger.createADFLogger(ClassLoaderUtils.class);
}
