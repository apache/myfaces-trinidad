/*
 * Copyright  2000-2006 The Apache Software Foundation.
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
package org.apache.myfaces.trinidadinternal.ui;

/**
 * Node implementation for text.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/TextNode.java#0 $) $Date: 10-nov-2005.18:50:22 $
 * @author The Oracle ADF Faces Team
 */
public class TextNode extends AbstractTextNode
{
  /**
   * Creates a TextNode that will display a string.
   */
  public TextNode(
    String text
    )
  {
    _text = text;
  }


  /**
   * Creates a TextNode that will display the contents
   * of a character array.
   */
  public TextNode(
    char[] text
    )
  {
    this(((text != null) ? new String(text) : null));    
  }

  
  /**
   * Returns the text for the current rendering context.
   * For this class, just returns the value passed to the constructor.
   */
  protected Object getTextObject(
    UIXRenderingContext context
    )
  {
    return _text;
  }

  private String _text;
}
