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
package org.apache.myfaces.trinidadinternal.resource;

import java.io.IOException;

import java.net.URL;
import java.net.URLConnection;

import org.apache.myfaces.trinidad.resource.AggregatingResourceLoader;
import org.apache.myfaces.trinidad.resource.ClassLoaderResourceLoader;

public class CoreFormatScriptsResourceLoader extends AggregatingResourceLoader
{
  public CoreFormatScriptsResourceLoader(
    boolean debug)
  {
    super("",
          debug ? _DEBUG_LIBRARIES : _LIBRARIES,
          new ClassLoaderResourceLoader());
    
    setSeparator("\n");
  }
  
  /**
   * Since CoreRenderKitResourceLoader already does the matching, this method is overridden
   * to just call getURL()
   */
  @Override
  protected URL findResource(String path) throws IOException
  {
    return getURL(path);
  }
  
  @Override
  protected String getContentType(
    URLConnection conn)
  {
    return "text/javascript";
  }
  
  static private final String[] _LIBRARIES =
  {
    "META-INF/adf/jsLibs/CharSets.js",
    "META-INF/adf/jsLibs/TrCollections.js",
    "META-INF/adf/jsLibs/NumberFormat.js",
    "META-INF/adf/jsLibs/NumberConverter.js",
    "META-INF/adf/jsLibs/CoreFormat.js"
  };
  
  static private final String[] _DEBUG_LIBRARIES =
  {
    "META-INF/adf/jsLibsDebug/CharSets.js",
    "META-INF/adf/jsLibsDebug/TrCollections.js",
    "META-INF/adf/jsLibsDebug/NumberFormat.js",
    "META-INF/adf/jsLibsDebug/NumberConverter.js",
    "META-INF/adf/jsLibsDebug/CoreFormat.js"
  };
}
