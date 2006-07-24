/*
 * Copyright  1997-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.image.painter;

/**
 * A subinterface of Painter adding the ability to get the insets of the
 * painter.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/painter/BorderPainter.java#0 $) $Date: 10-nov-2005.19:04:53 $
 * @author The Oracle ADF Faces Team
 */
public interface BorderPainter extends Painter
{
  /**
   * Returns the amount of space the border will
   * require on each side.
   * <p>
   * @param context the context for painting
   */
  public ImmInsets getInsets(PaintContext context);


  /**
   * Returns the amount of space by which fills
   * should be inset.
   * <p>
   * @param context the context for painting
   */
  public ImmInsets getFillInsets(PaintContext context);
}
