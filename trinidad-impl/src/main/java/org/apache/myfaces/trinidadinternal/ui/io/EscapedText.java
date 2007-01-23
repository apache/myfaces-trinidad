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
 * Text that is pre-escaped for output in the specified MimeType.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/io/EscapedText.java#0 $) $Date: 10-nov-2005.18:56:22 $
 * @author The Oracle ADF Faces Team
 */
public final class EscapedText
{
  /**
   * Create a MimeText object with the original text and escaped text
   * @param mimeType the interned mime type
   * @param baseText The unescaped text
   * @param contentText The baseText escaped for use as content
   * @param attributeText The baseText escaped for use as an attribute
   */
  EscapedText(
    String mimeType,
    String baseText,
    String contentText,
    String attributeText
    )
  {
    // no null's allowed, as this would give different behavior than
    // having a null String
    if ((baseText == null) ||
        (contentText == null) ||
        (attributeText == null) ||
        (mimeType == null))
    {
      throw new IllegalArgumentException();
    }

    assert (mimeType == mimeType.intern());


    _baseText      = baseText;
    _contentText   = contentText;
    _attributeText = attributeText;
    _mimeType      = mimeType;
  }


  public String getMimeType()
  {
    return _mimeType;
  }

  /**
   * Returns the escaped form of the text for use as element content
   */
  public String getContentText()
  {
    return _contentText;
  }


  /**
   * Returns the escaped form of the text for use as an element attribute
   */
  public String getAttributeText()
  {
    return _attributeText;
  }

  /**
   * Returns the unescaped form of the String
   */
  @Override
  public String toString()
  {
    return _baseText;
  }

  @Override
  public boolean equals(
    Object other
    )
  {
    if (this == other)
      return true;

    if (other == null)
      return false;

    if (other.getClass() == EscapedText.class)
    {
      EscapedText otherText = (EscapedText)other;

      // ok because we don't allow _baseText to be null
      return (otherText._mimeType == _mimeType) &&
             _baseText.equals(otherText._baseText);
    }
    else
    {
      return false;
    }
  }

  @Override
  public int hashCode()
  {
    return _baseText.hashCode();
  }

  private String _mimeType;
  private String _baseText;
  private String _contentText;
  private String _attributeText;
}
