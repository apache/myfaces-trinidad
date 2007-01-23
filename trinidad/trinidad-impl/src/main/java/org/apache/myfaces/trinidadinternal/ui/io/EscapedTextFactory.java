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
package org.apache.myfaces.trinidadinternal.ui.io;

/**
 * Text that is pre-escaped for output to a particular MimeType.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/io/EscapedTextFactory.java#0 $) $Date: 10-nov-2005.18:56:23 $
 * @author The Oracle ADF Faces Team
 */
public final class EscapedTextFactory
{
  /** Mime Type for HTML */
  public static final String HTML_MIME_TYPE = "text/html";
  
  static final String JAVASCRIPT_MIME_TYPE = "text/x-javascript";
  static final String CSS_MIME_TYPE = "text/css";
  static final String XML_MIME_TYPE = "text/xml";

  public static EscapedText createEscapedText(
    String mimeType,
    String baseText
    )
  {
    if (HTML_MIME_TYPE == mimeType)
    {
      String escapedText = HTMLEscapes.escapeText(baseText);
      
      return new EscapedText(mimeType,
                             baseText,
                             escapedText,
                             escapedText);
    }
    else
    {
      // =-= bts If we don't know about the type, should
      //         we throw an exception, and if so, which one
      return new EscapedText(mimeType,
                             baseText,
                             baseText,
                             baseText);
    }
  }  
}
