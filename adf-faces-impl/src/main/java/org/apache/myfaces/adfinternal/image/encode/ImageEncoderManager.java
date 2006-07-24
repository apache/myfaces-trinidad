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

package org.apache.myfaces.adfinternal.image.encode;


import org.apache.myfaces.adf.util.ArrayMap;

import org.apache.myfaces.adfinternal.image.PropertyInstantiator;

/**
 * ImageEncoderManager maintains a registry of image encodings.
 * Currently, clients must use the getDefaultImageEncoderManager()
 * to obtain a reference to the single, shared ImageEncoderManager.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/image/encode/ImageEncoderManager.java#0 $) $Date: 10-nov-2005.19:05:18 $
 * @author The Oracle ADF Faces Team
 */
public class ImageEncoderManager
{
  /**
   * Mime type for GIF
   */
  public static final String GIF_TYPE = "image/gif";

  /**
   * Mime type for PNG
   */
  public static final String PNG_TYPE = "image/png";

  // Private for now
  private ImageEncoderManager() {}

  /**
   * Returns the default ImageEncoderManager
   */
  static public ImageEncoderManager getDefaultImageEncoderManager()
  {
    return _sDefaultInstance;
  }

  /**
   * Returns the ImageEncoder for the specified encoding.
   * @param type The mime type for the encoding, eg. "image/gif".
   */
  public ImageEncoder getImageEncoder(String type)
  {
    synchronized (this)
    {
      Object encoder = _encoders.get(type);
      
      if (encoder instanceof PropertyInstantiator)
      {
        encoder = ((PropertyInstantiator)encoder).instantiate();
        
        assert (encoder instanceof ImageEncoder);
        
        _encoders.put(type, encoder);
      }

      return (ImageEncoder)encoder;
    }
  }

  /**
   * Returns the file extension for the specified type.
   * @param type The mime type for the encoding, eg. "image/gif".
   */
  public String getImageExtension(String type)
  {
    return (String)_extensions.get(type);
  }

  /**
   * Registers an encoding.
   * @param type The mime type of the encoding
   * @param extension The file extension for the encoding
   * @param encoder The ImageEncoder instance to use for this encoding
   */
  public void registerEncoding(
    String type,
    String extension,
    ImageEncoder encoder
    )
  {
    synchronized (this)
    {
      _encoders.put(type, encoder);
      _extensions.put(type, extension);
    }
  }

  /**
   * Registers an encoding.
   * The ImageEncoder is instantiated using the specified class name when
   * it is first requested.
   * @param type The mime type of the encoding
   * @param extension The file extension for the encoding
   * @param encoderClassName The name of a class which implements
   *   the ImageEncoder interface
   */
  public void registerEncoding(
    String type,
    String extension,
    String encoderClassName
    )
  {
    synchronized (this)
    {
      _encoders.put(type, new PropertyInstantiator(encoderClassName));
      _extensions.put(type, extension);
    }
  }

  /**
   * Unregisters an encoding
   * @param type The mime type of the encoding to unregister
   */
  public void unregisterEncoding(String type)
  {
    synchronized (this)
    {
      _encoders.remove(type);
      _extensions.remove(type);
    }
  }

  // Registers default encodings
  static private void _registerDefaultEncodings(ImageEncoderManager manager)
  {
    // Register the default GIF encoder
    manager.registerEncoding(
      GIF_TYPE,
      _GIF_EXTENSION,
      "org.apache.myfaces.adfinternal.image.encode.DefaultGIFEncoder"
      );

    manager.registerEncoding(
      PNG_TYPE,
      _PNG_EXTENSION,
      "org.apache.myfaces.adfinternal.image.encode.DefaultPNGEncoder"
      );
  }

  // Maps mime type to ImageEncoder objects
  private ArrayMap _encoders = new ArrayMap();

  // Maps mime type to extension
  private ArrayMap _extensions = new ArrayMap();

  static private ImageEncoderManager _sDefaultInstance = 
    new ImageEncoderManager();

  static
  {
    _registerDefaultEncodings(_sDefaultInstance);
  }

  static private final String _GIF_EXTENSION = ".gif";
  static private final String _PNG_EXTENSION = ".png";
}
