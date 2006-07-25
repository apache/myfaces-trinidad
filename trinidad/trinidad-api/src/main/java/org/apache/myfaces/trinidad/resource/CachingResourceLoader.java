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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import java.io.InputStream;
import java.net.URL;

import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.Map;
import java.util.HashMap;



/**
 * Base class for resource loaders.  Resource loaders can lookup resources
 * as URLs from arbitrary locations, including JAR files.
 *
 * @author The Oracle ADF Faces Team
 */
public class CachingResourceLoader extends ResourceLoader
{
  /**
   * Constructs a new CachingResourceLoader.
   *
   * @param parent  the parent resource loader
   */
  public CachingResourceLoader(
    ResourceLoader parent)
  {
    super(parent);

    _cache = new HashMap();
  }

  /**
   * Returns the cached resource url if previously requested.  Otherwise,
   * fully reads the resource contents stores in the cache.
   *
   * @param path  the resource path
   *
   * @return the cached resource url
   *
   * @throws java.io.IOException  if an I/O error occurs
   */
  protected URL findResource(
    String path
    ) throws IOException
  {
    URL url = (URL)_cache.get(path);

    if (url == null)
    {
      url = getParent().getResource(path);

      if (url != null)
      {
        url = new URL("cache", null, -1, path, new URLStreamHandlerImpl(url));
        _cache.put(path, url);
      }
    }

    return url;
  }

  private final Map _cache;

  /**
   * URLStreamHandler to cache URL contents and URLConnection headers.
   */
  static private class URLStreamHandlerImpl extends URLStreamHandler
  {
    public URLStreamHandlerImpl(
      URL delegate)
    {
      _delegate = delegate;
    }

    protected URLConnection openConnection(
      URL url
      ) throws IOException
    {
      return new URLConnectionImpl(url, _delegate.openConnection(), this);
    }

    protected InputStream getInputStream(
      URLConnection conn) throws IOException
    {
      long lastModified = conn.getLastModified();

      if (_contents == null || _contentsModified < lastModified)
      {
        InputStream in = conn.getInputStream();
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        try
        {
          byte[] buffer = new byte[2048];
          int length;
          while ((length = (in.read(buffer))) >= 0)
          {
            out.write(buffer, 0, length);
          }
        }
        finally
        {
          in.close();
        }
        _contents = out.toByteArray();
        _contentsModified = conn.getLastModified();
      }

      return new ByteArrayInputStream(_contents);
    }

    private final URL    _delegate;
    private       byte[] _contents;
    private       long   _contentsModified;
  }

  /**
   * URLConnection to cache URL contents and header fields.
   */
  static private class URLConnectionImpl extends URLConnection
  {
    /**
     * Creates a new URLConnectionImpl.
     *
     * @param url      the cached url
     * @param handler  the caching stream handler
     */
    public URLConnectionImpl(
      URL                  url,
      URLConnection        conn,
      URLStreamHandlerImpl handler)
    {
      super(url);
      _conn = conn;
      _handler = handler;
    }

    public void connect() throws IOException
    {
      // cache: no-op
    }

    public String getContentType()
    {
      return _conn.getContentType();
    }

    public int getContentLength()
    {
      return _conn.getContentLength();
    }

    public long getLastModified()
    {
      return _conn.getLastModified();
    }

    public String getHeaderField(
      String name)
    {
      return _conn.getHeaderField(name);
    }

    public InputStream getInputStream() throws IOException
    {
      return _handler.getInputStream(_conn);
    }

    private final URLConnection        _conn;
    private final URLStreamHandlerImpl _handler;
  }
}
