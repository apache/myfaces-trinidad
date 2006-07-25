/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.trinidad.resource;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.URL;
import java.io.IOException;
import java.net.URLConnection;
import java.net.URLStreamHandler;

public abstract class StringContentResourceLoader extends DynamicResourceLoader 
{
  public StringContentResourceLoader(String path)
  {
    super(path);
  }
  
  public StringContentResourceLoader(String path, ResourceLoader parent)
  {
    super(path, parent);
  }

  protected URL getURL(String path) throws IOException
  {
    return new URL("dynamic", null, -1, path, new StringContentURLStreamHandler(getString(path), getContentType(path)));
  }
  
  protected abstract String getString(String path) throws IOException;
  
  protected String getContentType(String path)
  {
    return "text";
  }
  
  /**
   * This is a stream handler that can be used to construct a URL whose connection will return content
   * specified in a String.
   * 
   * @author The Oracle ADF Faces Team
   */
  private class StringContentURLStreamHandler extends URLStreamHandler
  { 
    /**
     * Created a StringContentURLStreamHandler which will provide the specified content on the
     * connection.
     * 
     * @param content the content of the connection
     * @param contentType the content type of this connection (i.e. "text/javascript")
     */
    public StringContentURLStreamHandler(String content, String contentType)
    {
      _buff = content.getBytes();
      _contentType = contentType;
    }
  
    /**
     * Returns a URLConnection containing the specified content.
     * 
     * @param u the URL for this connection
     * @return a connection containing the string of data
     * @throws IOException when something bad happens
     */
    protected URLConnection openConnection(URL u) throws IOException
    {
      return new StringContentURLConnection(u, _buff, _contentType); 
    }
  
    private byte[] _buff;
    private String _contentType;
  }
  
  private class StringContentURLConnection extends URLConnection 
  {
    public StringContentURLConnection(URL url, byte[] buff, String contentType)
    {
      super(url);
      connected = false;
      _buff = buff;
      _contentType = contentType;
    }
  
    public void connect() throws IOException
    {
      connected = true;
    }
  
    public String getContentEncoding()
    {
      //No content Encoding in Strings (like gzip or deflate)
      return null;
    }
  
    public int getContentLength()
    {
      return _buff.length;
    }
  
    public String getContentType()
    {
      return _contentType;
    }
  
    public String getHeaderField(String name)
    {
      if("content-encoding".equals(name))
      {
        return getContentEncoding();
      }
      else if ("content-length".equals(name))
      {
        return String.valueOf(getContentLength());
      }
      else if ("content-type".equals(name))
      {
        return getContentType();
      }
      
      return null;
    }
  
    public InputStream getInputStream() throws IOException
    {
      return new ByteArrayInputStream(_buff);
    }
    
    private byte[] _buff;
    private String _contentType;
  }
}
