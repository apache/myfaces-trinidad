/*
 * Copyright  2001-2006 The Apache Software Foundation.
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

package org.apache.myfaces.adfinternal.share.config;

/**
 * This class is used to control the level of accessibility support in an 
 * application. Currently three modes of accessibility are supported:
 * <dl><li>{@link #DEFAULT_MODE}</li> At this level, code that is strictly
 * accessible must be produced.
 * <li>{@link #INACCESSIBLE_MODE}</li> At this level, code may be optimized
 * to strip out accessibility-specific constructs.
 * <li>{@link #SCREEN_READER_MODE}</li> At this level, content specific for
 * screen readers may be rendered.
 * <P>
 * The current mode is obtained by calling 
 * {@link Configuration#getProperty(Object key)} and using
 * {@link Configuration#ACCESSIBILITY_MODE} as the key.
 * @see Configuration
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/share/config/AccessibilityMode.java#0 $) $Date: 10-nov-2005.19:00:17 $
 * @author The Oracle ADF Faces Team
 */
public final class AccessibilityMode
{

  public String toString()
  {
    return _text;
  }

  /**
   * Return the name of the accessibility mode.
   */
  public String getName()
  {
    return _text;
  }
  
  /**
   * Return the accessibility mode with a given name, or null
   * if no accessibility modes correspond.
   */
  static public AccessibilityMode getAccessibilityMode(String name)
  {
    if (name != null)
    {
      for (int i = 0; i < _MODES.length; i++)
      {
        if (_MODES[i].getName().equals(name))
          return _MODES[i];
      }
    }

    return null;
  }

  private AccessibilityMode(String text)
  {
    _text = text;
  }

  private final String _text;

  //
  // static code follows
  //

  /**
   * This value indicates an accessible level of accessibility support.
   */
  public static final AccessibilityMode DEFAULT_MODE = 
    new AccessibilityMode("default");

  /**
   * This value indicates that accessibility is not required. All HTML
   * pertaining to accessibility may be stripped from the output.
   * @see #isInaccessibleMode(Configuration)
   */
  public static final AccessibilityMode INACCESSIBLE_MODE =
    new AccessibilityMode("inaccessible");

  /**
   * This value indicates that the content must be catered specifically
   * for screen readers (eg: JAWS). This mode will also generate accessible
   * ccontent.
   * @see #isScreenReaderMode(Configuration)
   */
  public static final AccessibilityMode SCREEN_READER_MODE =
    new AccessibilityMode("screenReader");


  /**
   * @param config the Configuration object to examine
   * @return true if the accessiblity mode set on the Configuration object
   * is at the inaccessible level.
   * @see #INACCESSIBLE_MODE
   * @see Configuration#ACCESSIBILITY_MODE
   */
  public static boolean isInaccessibleMode(Configuration config)
  {
    Object mode = _getMode(config);
    return (INACCESSIBLE_MODE == mode);
  }

  /**
   * @param config the Configuration object to examine
   * @return true if the accessiblity mode set on the Configuration object
   * is at the screen reader level.
   * @see #SCREEN_READER_MODE
   * @see Configuration#ACCESSIBILITY_MODE
   */
  public static boolean isScreenReaderMode(Configuration config)
  {
    Object mode = _getMode(config);
    return (SCREEN_READER_MODE == mode);
  }

  private static Object _getMode(Configuration config)
  {
    return config.getProperty(Configuration.ACCESSIBILITY_MODE);
  }

  static private final AccessibilityMode[] _MODES = 
    new AccessibilityMode[]{
      DEFAULT_MODE,
      INACCESSIBLE_MODE,
      SCREEN_READER_MODE,
    };
}
