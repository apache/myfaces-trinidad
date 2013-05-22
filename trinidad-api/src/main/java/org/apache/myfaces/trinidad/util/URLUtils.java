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
package org.apache.myfaces.trinidad.util;

import java.io.File;
import java.io.IOException;

import java.io.UnsupportedEncodingException;

import java.net.JarURLConnection;
import java.net.URL;
import java.net.URLConnection;

import java.net.URLDecoder;

import java.net.URLEncoder;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletResponse;

public final class URLUtils
{
  private URLUtils()
  {
  }

  public static long getLastModified(URL url) throws IOException
  {
    if ("file".equals(url.getProtocol()))
    {
      String externalForm = url.toExternalForm();
      // Remove the "file:"
      File file = new File(externalForm.substring(5));

      return file.lastModified();
    }
    else
    {
      return getLastModified(url.openConnection());
    }
  }

  public static long getLastModified(URLConnection connection) throws IOException
  {
    long modified;
    if (connection instanceof JarURLConnection)
    {
      // The following hack is required to work-around a JDK bug.
      // getLastModified() on a JAR entry URL delegates to the actual JAR file
      // rather than the JAR entry.
      // This opens internally, and does not close, an input stream to the JAR
      // file.
      // In turn, you cannot close it by yourself, because it's internal.
      // The work-around is to get the modification date of the JAR file
      // manually,
      // and then close that connection again.

      URL jarFileUrl = ((JarURLConnection) connection).getJarFileURL();
      URLConnection jarFileConnection = jarFileUrl.openConnection();

      try
      {
        modified = jarFileConnection.getLastModified();
      }
      finally
      {
        try
        {
          jarFileConnection.getInputStream().close();
        }
        catch (Exception exception)
        {
          // Ignored
        }
      }
    }
    else
    {
      modified = connection.getLastModified();
    }

    return modified;
  }
  
  /**
   * Encodes a URL (with or without an existing query string) such that the value in the params map are added to them.
   * A valid character encoding must be provided to ensure the parameters are encoded properly.
   * 
   * @param url the base URL
   * @param params the map of parameters to add, or <code>null</code>
   * @param characterResponseEncoding the character response encoding
   * @return the properly encoded url
   * 
   * @throws UnsupportedOperationException if the encoding is not supported.
   */
  public static String encodeURL(String url, Map<String, List<String>> params, String characterResponseEncoding)
  {
    String fragment = null;
    String queryString = null;
    Map<String, List<String>> paramMap = null;

    //extract any URL fragment
    int index = url.indexOf(_URL_FRAGMENT_SEPERATOR);
    if (index != -1)
    {
      fragment = url.substring(index+1);
      url = url.substring(0,index);
    }

    //extract the current query string and add the params to the paramMap
    index = url.indexOf(_URL_QUERY_SEPERATOR);
    if (index != -1)
    {
      queryString = url.substring(index + 1);
      url = url.substring(0, index);
      String[] nameValuePairs = queryString.split(_URL_PARAM_SEPERATOR);
      for (int i = 0; i < nameValuePairs.length; i++)
      {
        String[] currentPair = nameValuePairs[i].split(_URL_NAME_VALUE_PAIR_SEPERATOR);

        ArrayList<String> value = new ArrayList<String>(1);
        try
        {
          value.add(currentPair.length > 1
                    ? URLDecoder.decode(currentPair[1], characterResponseEncoding)
                    : "");
        }
        catch (UnsupportedEncodingException e)
        {
          //shouldn't ever get here
          throw new UnsupportedOperationException("Encoding type=" + characterResponseEncoding
                                                          + " not supported", e);
        }
        if (paramMap == null)
        {
          paramMap = new HashMap<String, List<String>>();
        }
        paramMap.put(currentPair[0], value);
      }
    }

    //add/update with new params on the paramMap
    if (params != null && params.size() > 0)
    {
      for (Map.Entry<String, List<String>> pair : params.entrySet())
      {
        if (pair.getKey() != null && pair.getKey().trim().length() != 0)
        {
          if (paramMap == null)
          {
            paramMap = new HashMap<String, List<String>>();
          }
          paramMap.put(pair.getKey(), pair.getValue());
        }
      }
    }

    // start building the new URL
    StringBuilder newUrl = new StringBuilder(url);

    //now add the updated param list onto the url
    if (paramMap != null && paramMap.size()>0)
    {
      boolean isFirstPair = true;
      for (Map.Entry<String, List<String>> pair : paramMap.entrySet())
      {
        for (String value : pair.getValue())
        {
          if (!isFirstPair)
          {
            newUrl.append(_URL_PARAM_SEPERATOR);
          }
          else
          {
            newUrl.append(_URL_QUERY_SEPERATOR);
            isFirstPair = false;
          }

          newUrl.append(pair.getKey());
          newUrl.append(_URL_NAME_VALUE_PAIR_SEPERATOR);
          try
          {
            newUrl.append(URLEncoder.encode(value,characterResponseEncoding));
          }
          catch (UnsupportedEncodingException e)
          {
            //shouldn't ever get here
            throw new UnsupportedOperationException("Encoding type=" + characterResponseEncoding
                                                  + " not supported", e);
          }
        }
      }    
    }
    
    //add the fragment back on (if any)
    if (fragment != null)
    {
      newUrl.append(_URL_FRAGMENT_SEPERATOR + fragment);
    }
    
    return newUrl.toString();
  }
  
  private static final String _URL_PARAM_SEPERATOR="&";
  private static final String _URL_QUERY_SEPERATOR="?";
  private static final String _URL_FRAGMENT_SEPERATOR="#";
  private static final String _URL_NAME_VALUE_PAIR_SEPERATOR="=";
}