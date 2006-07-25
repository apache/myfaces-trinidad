/*
 * Copyright 2006 The Apache Software Foundation.
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

package org.apache.myfaces.adfinternal.image.encode;

import java.awt.image.RGBImageFilter;
import java.awt.Image;

/**
 * A filter which reduces the number of colors in an image according to an Octree reduction scheme.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/encode/OctreeFilter.java#0 $) $Date: 10-nov-2005.19:05:18 $
 * @author The Oracle ADF Faces Team
 * @since 0.1.4
 * @see OctreeQuantizer 
*/
class OctreeFilter extends RGBImageFilter
{

  /** 
   * Constructor requires the original image, in order to construct the tree appropriate for this
   * conversion.
   * @param im The original image
   */
  public OctreeFilter(Image im)
  {
    canFilterIndexColorModel = true;
    _tree = new OctreeQuantizer(im);
  
  }

  /**
   * Implementation of abstract method from RGBImageFilter
   */
  public int filterRGB(int x, int y, int rgb)
  {
    return _tree.mapColor(rgb);
  }

  private OctreeQuantizer _tree;

}
