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

import java.io.IOException;

import org.apache.myfaces.trinidad.share.io.NameResolver;
import org.apache.myfaces.trinidadinternal.share.io.ClassResourceNameResolver;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContextImpl;
import org.apache.myfaces.trinidadinternal.style.xml.parse.StyleSheetDocument;

/**
 * Utilities for testing skinning
 */
public final class SkinTestUtils
{
  /**
   * Parses a skin css document.
   * 
   * @param resourceName name of the document resource, which is located via a call
   *   to ClassLoader.getResource().
   * @return the StyleSheetDocument for the skin.
   * @throws IllegalStateException if an IOException occurs during parsing.
   */
  public static StyleSheetDocument parseSkin(String resourceName)
    throws IllegalStateException
  {
    ParseContext parseContext = new ParseContextImpl();
    NameResolver nameResolver = new ClassResourceNameResolver(null);
    StyleSheetEntry entry = null;

    try
    {
      entry = SkinStyleSheetParserUtils.parseCSSSource(parseContext,
                                                       nameResolver,
                                                       resourceName,
                                                       StyleSheetEntry.class);
    }
    catch (IOException e)
    {
      throw new IllegalStateException(e);
    }

    return entry.getDocument();    
  }

  private SkinTestUtils()
  {
  }
}
