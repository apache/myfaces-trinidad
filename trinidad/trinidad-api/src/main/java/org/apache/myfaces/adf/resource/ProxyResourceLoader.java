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

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import java.net.URLConnection;
import java.net.URLStreamHandler;
import java.util.Map;
import java.io.OutputStream;
import java.security.Permission;

/**
 * A resource loader implementation that proxies another
 * resource loader, controlling the URLConnection.
 *
 * @author The Oracle ADF Faces Team
 */
public class ProxyResourceLoader extends ResourceLoader
{
  /**
   * Constructs a new ProxyResourceLoader with specified parent.
   * 
   * @param parent  the parent resource loader
   */
  public ProxyResourceLoader(
    ResourceLoader parent)
  {
    super(parent);
  }
  
  public URL getResource(
    String path) throws IOException
  {
    URL url = super.getResource(path);
    return (url != null) 
              ? new URL("proxy", null, -1, url.toExternalForm(),  
                        new ProxyURLStreamHandler(url))
              : null;
  }

  /**
   * Helper class used to manage decorated URL input stream.
   */
  private class ProxyURLStreamHandler extends URLStreamHandler
  {
    public ProxyURLStreamHandler(
      URL proxied)
    {
      _proxied = proxied;
    }
    
    protected URLConnection openConnection(
      URL url
      ) throws IOException
    {
      return new ProxyURLConnection(url, _proxied);
    }
    
    private final URL _proxied;
  }

  /**
   * Helper class used to manage aggregated URL input stream.
   */
  protected class ProxyURLConnection extends URLConnection
  {
    public ProxyURLConnection(
      URL url,
      URL proxied
      ) throws IOException
    {
      super(url);
      
      _delegate = proxied.openConnection();
    }

    public void addRequestProperty(String key, String value)
    {
      getURLConnection().addRequestProperty(key, value);
    }

    public void connect() throws IOException
    {
      getURLConnection().connect();
    }

    public boolean getAllowUserInteraction()
    {
      return getURLConnection().getAllowUserInteraction();
    }

    public Object getContent() throws IOException
    {
      return getURLConnection().getContent();
    }

    public Object getContent(Class[] classes) throws IOException
    {
      return getURLConnection().getContent(classes);
    }

    public String getContentEncoding()
    {
      return getURLConnection().getContentEncoding();
    }

    public int getContentLength()
    {
      return getURLConnection().getContentLength();
    }

    public String getContentType()
    {
      return ProxyResourceLoader.this.getContentType(getURLConnection());
    }

    public long getDate()
    {
      return getURLConnection().getDate();
    }

    public boolean getDefaultUseCaches()
    {
      return getURLConnection().getDefaultUseCaches();
    }

    public boolean getDoInput()
    {
      return getURLConnection().getDoInput();
    }

    public boolean getDoOutput()
    {
      return getURLConnection().getDoOutput();
    }

    public long getExpiration()
    {
      return getURLConnection().getExpiration();
    }

    public String getHeaderField(int n)
    {
      return getURLConnection().getHeaderField(n);
    }

    public String getHeaderField(String name)
    {
      return getURLConnection().getHeaderField(name);
    }

    public long getHeaderFieldDate(String name, long Default)
    {
      return getURLConnection().getHeaderFieldDate(name, Default);
    }

    public int getHeaderFieldInt(String name, int Default)
    {
      return getURLConnection().getHeaderFieldInt(name, Default);
    }

    public String getHeaderFieldKey(int n)
    {
      return getURLConnection().getHeaderFieldKey(n);
    }

    public Map getHeaderFields()
    {
      return getURLConnection().getHeaderFields();
    }

    public long getIfModifiedSince()
    {
      return getURLConnection().getIfModifiedSince();
    }

    public InputStream getInputStream() throws IOException
    {
      return getURLConnection().getInputStream();
    }

    public long getLastModified()
    {
      return getURLConnection().getLastModified();
    }

    public OutputStream getOutputStream() throws IOException
    {
      return getURLConnection().getOutputStream();
    }

    public Permission getPermission() throws IOException
    {
      return getURLConnection().getPermission();
    }

    public Map getRequestProperties()
    {
      return getURLConnection().getRequestProperties();
    }

    public String getRequestProperty(String key)
    {
      return getURLConnection().getRequestProperty(key);
    }

    public boolean getUseCaches()
    {
      return getURLConnection().getUseCaches();
    }

    public void setAllowUserInteraction(boolean allowuserinteraction)
    {
      getURLConnection().setAllowUserInteraction(allowuserinteraction);
    }

    public void setDefaultUseCaches(boolean defaultusecaches)
    {
      getURLConnection().setDefaultUseCaches(defaultusecaches);
    }

    public void setDoInput(boolean doinput)
    {
      getURLConnection().setDoInput(doinput);
    }

    public void setDoOutput(boolean dooutput)
    {
      getURLConnection().setDoOutput(dooutput);
    }

    public void setIfModifiedSince(long ifmodifiedsince)
    {
      getURLConnection().setIfModifiedSince(ifmodifiedsince);
    }

    public void setRequestProperty(String key, String value)
    {
      getURLConnection().setRequestProperty(key, value);
    }

    public void setUseCaches(boolean usecaches)
    {
      getURLConnection().setUseCaches(usecaches);
    }
    
    protected URLConnection getURLConnection()
    {
      return _delegate;
    }
    
    private final URLConnection _delegate;
  }
}
