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

import java.net.URL;

import java.util.Collections;
import java.util.Iterator;

import javax.faces.context.ExternalContext;

/**
 * Non-Trinidad skin resource loader implementations should extend this class and override
 * findResources to specify where from where to load skin resources.
 * We will find all overridden classes by calling
 * <pre>
 * List&lt;SkinResourceLoader> urlProviders = ClassLoaderUtils.getServices(
 *                                 "org.apache.myfaces.trinidad.resource.SkinResourceLoader");
 * </pre>
 */
public class SkinResourceLoader
{

  /**
   * Returns an iterator of URL objects representing all the resources with the given name.
   * @param context The ExternalContext
   * @param filename The filename of the resource to find, e.g., "trinidad-skins.xml"
   * @return An iterator of URL objects for the resources.
   *  Returns an empty iterator if no resources were found.
   */
  public Iterator<URL> findResources(ExternalContext context, String filename)
  {
      return Collections.<URL>emptyList().iterator();
  }
}