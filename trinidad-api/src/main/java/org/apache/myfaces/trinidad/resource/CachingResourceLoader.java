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
package org.apache.myfaces.trinidad.resource;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import java.util.concurrent.atomic.AtomicReference;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.Args;
import org.apache.myfaces.trinidad.util.URLUtils;

/**
 * Base class for resource loaders.  Resource loaders can lookup resources
 * as URLs from arbitrary locations, including JAR files.
 *
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

    _cache = new ConcurrentHashMap<String, URL>();
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
  @Override
  protected URL findResource(
    String path
    ) throws IOException
  {
    URL url = _cache.get(path);

    if (url == null)
    {
      url = getParent().getResource(path);

      if (url != null)
      {
        url = new URL("cache", null, -1, path, new CachingURLStreamHandler(url));
        _cache.putIfAbsent(path, url);
      }
    }

    return url;
  }

  private final ConcurrentMap<String, URL> _cache;

  @Override
  public boolean isCachable()
  {
    return false;
  }

  /**
   * URLStreamHandler to cache URL contents and URLConnection headers.
   * 
   * The implementation is thread-safe.
   */
  static private final class CachingURLStreamHandler extends URLStreamHandler
  {
    public CachingURLStreamHandler(
      URL delegate)
    {
      _delegate = delegate;
      _contents = new AtomicReference<CachedContents>();
    }

    /**
     * Compares a new content length against the length of the contents that
     * we have cached.  If these don't match, we need to dump the cache and reload
     * our contents.
     */
    public void validateContentLength(int newContentLength)
    {
      CachedContents contents = _contents.get();
      
      if ((contents != null) && !contents.validateContentLength(newContentLength))
      {
        // The new content length does not match the size of our cached
        // contents.  Clear out the cached contents and start over.
        _contents.compareAndSet(contents, null);
        _logResourceSizeChanged(newContentLength, contents);

      }
    }
    
    private void _logResourceSizeChanged(int newContentLength, CachedContents contents)
    {
      if (_LOG.isFine())
      {
        _LOG.fine("RESOURCE_SIZE_CHANGED",
                  new Object[]
                  {
                    newContentLength,
                    contents
                  });
      }      
    }

    @Override
    protected URLConnection openConnection(
      URL url
      ) throws IOException
    {
      return new URLConnectionImpl(url, _delegate.openConnection(), this);
    }

    protected InputStream getInputStream(URLConnection conn) throws IOException
    {
      CachedContents contents = _contents.get();
      
      if (contents == null || _isStale(contents, _delegate))
      {
        contents = _updateContents(conn);
        assert(contents != null);
      }

      return contents.toInputStream();
    }
    
    // Tests whether the CachedContents is stale based on the url's current lastModified time.
    private boolean _isStale(CachedContents contents, URL url) throws IOException
    {
      Args.notNull(contents, "contents");
      Args.notNull(url, "url");

      long lastModified = URLUtils.getLastModified(_delegate);
      return contents.isStale(lastModified);
    }

    private CachedContents _updateContents(URLConnection conn) throws IOException
    {
      CachedContents newContents = _createContents(conn);
      assert(newContents != null);

      // We're not doing a compareAndSet here because _contents may have
      // changed - eg. _contents may have been nulled out or set to a new
      // value by another request.  We're okay with replacing the current value
      // with our newly created instance.
      _contents.set(newContents);
      
      return newContents;
    }
    
    private CachedContents _createContents(URLConnection conn) throws IOException
    {
      // Note that the order of the following operations is intentional.
      // We get the last modified time before reading in the data in order
      // to protect against the possibility that the data is being modified
      // while read.  In this case, we want the earliest last modified time
      // to increase the chance that we will detect that our cached data
      // is stale on subsequent requests.
      long lastModified = URLUtils.getLastModified(conn);
      byte[] data = _readBytes(conn);
      int contentLength = conn.getContentLength();

      return new CachedContents(this._delegate, data, lastModified, contentLength);
    }

    @SuppressWarnings("oracle.jdeveloper.java.nested-assignment")
    private byte[] _readBytes(URLConnection conn) throws IOException
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
      
      return out.toByteArray();
    }

    private final URL    _delegate;
    private final AtomicReference<CachedContents> _contents;
  }
  
  // An immutable class that holds the data and metadadta for a single cached resource.
  // Note that we do not override equals() or hashCode() since we do not (yet) need
  // to hash or check for equality, but keep this in mind if we ever need to expand the
  // usage of this class.
  static private final class CachedContents
  {
    public CachedContents(
      URL resourceURL,
      byte[] data,
      long lastModified,
      int contentLength
      )
    {
      Args.notNull(data, "data");
      Args.notNull(resourceURL, "resourceURL");
      _ensureValidSize(resourceURL, data, contentLength);

      this._url = resourceURL;
      this._data = data;
      _lastModified = lastModified;
      _contentLength = contentLength;
    }

    public InputStream toInputStream()
    {
      return new ByteArrayInputStream(_data);       
    }

    /**
     * Tests whether this CacheContents instance contains stale data.
     * 
     * @return true if the specified lastModified time is new than
     *   the lastModified time that was recorded when this CachedContents
     *   instance was created.
     */
    public boolean isStale(long lastModified)
    {
      return (lastModified > _lastModified);
    }

    /**
     * Tests whether the specified content length is consistent with size of the
     * data held by this CachedContents.
     *
     * @return true if the newContentLength matches the current data size, false otherwise.
     */
    public boolean validateContentLength(int newContentLength)
    {
      return _isValidSize(_data, newContentLength);
    }

    /**
     * The string representation of this internal class is unspecified.
     * There is no reason anyone should need to parse this string representation,
     * but if that ever becomes an issue, listen to Joshua Bloch and add accessors
     * instead.  (See Item 10 in Effective Java, 2nd ed.)
     */
    @Override
    public String toString()
    {
      String urlString = _url.toString();
      String sizeString = Integer.toString(_data.length);
      int builderLength = urlString.length() + sizeString.length() + 13;
      
      StringBuilder builder = new StringBuilder(builderLength);
      builder.append("[url=");
      builder.append(urlString);
      builder.append(", size=");
      builder.append(sizeString);
      builder.append("]");
      
      return builder.toString();
    }

    private void _ensureValidSize(
      URL resourceURL,
      byte[] data,
      int contentLength
      ) throws IllegalStateException
    {
      assert(data != null);
      
      if (!_isValidSize(data, contentLength))
      {
        String messageKey = "INVALID_RESOURCE_SIZE";
        String message = _LOG.getMessage(messageKey,
                                         new Object[]
                                         {
                                           resourceURL.toString(),
                                           data.length,
                                           contentLength
                                         });
        
        _LOG.severe(message);
        
        // The message contains potentially sensitive data (eg. file system paths).
        // In order to make sure that this doesn't escape to the client, we don't
        // include the message in the exception.  The message key should be sufficient.
        throw new IllegalStateException(messageKey);
      }
    }
    
    private boolean _isValidSize(byte[] data, int contentLength)
    {
      assert(data != null);
      return ((contentLength < 0) || (contentLength == data.length));      
    }

    private final URL _url;
    private final byte[] _data;
    private final long _lastModified;
    private final int _contentLength;
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
      CachingURLStreamHandler handler)
    {
      super(url);
      _conn = conn;
      _handler = handler;
    }

    @Override
    public void connect() throws IOException
    {
      // cache: no-op
    }

    @Override
    public String getContentType()
    {
      return _conn.getContentType();
    }

    @Override
    public int getContentLength()
    {
      int contentLength = _conn.getContentLength();
      _handler.validateContentLength(contentLength);

      return contentLength;
    }

    @Override
    public long getLastModified()
    {
      try
      {
        return URLUtils.getLastModified(_conn);
      }
      catch (IOException exception)
      {
        return -1;
      }
    }

    @Override
    public String getHeaderField(
      String name)
    {
      return _conn.getHeaderField(name);
    }

    @Override
    public InputStream getInputStream() throws IOException
    {
      return _handler.getInputStream(_conn);
    }

    private final URLConnection        _conn;
    private final CachingURLStreamHandler _handler;
  }

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(CachingResourceLoader.class);
}
