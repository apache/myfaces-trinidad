/*
 * Copyright  2004-2006 The Apache Software Foundation.
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

package org.apache.myfaces.trinidadinternal.resource;

import java.net.URLConnection;
import org.apache.myfaces.trinidad.resource.AggregatingResourceLoader;
import org.apache.myfaces.trinidad.resource.ClassLoaderResourceLoader;

/**
 * A resource loader implementation which serves up the rich
 * client framework JavaScript library.
 *
 * @author The Oracle ADF Faces Team
 */
public class CoreCommonScriptsResourceLoader extends AggregatingResourceLoader
{
  public CoreCommonScriptsResourceLoader(
    String  path,
    boolean debug)
  {
    super(path,
          debug ? _DEBUG_LIBRARIES : _LIBRARIES,
          new ClassLoaderResourceLoader());

    // force a newline between libraries to avoid the syntax error when
    // the last line of one library contains a line comment "//"
    // and the first line of the next library starts with a
    // block comment "/*"
    setSeparator(_NEWLINE_SEPARATOR);
  }

  @Override
  protected String getContentType(
    URLConnection conn)
  {
    return "text/javascript";
  }

  // List of all libraries
  static private final String[] _LIBRARIES =
  {
    "META-INF/adf/jsLibs/CharSets.js",
    "META-INF/adf/jsLibs/CoreFormat.js",
    "META-INF/adf/jsLibs/DateField.js",
    "META-INF/adf/jsLibs/DateFieldFormat.js",
    "META-INF/adf/jsLibs/DateFormat.js",
    "META-INF/adf/jsLibs/Locale.js",
    "META-INF/adf/jsLibs/Core.js",
    "META-INF/adf/jsLibs/Window.js",
    "META-INF/adf/jsLibs/PPR.js",
    "META-INF/adf/jsLibs/TableProxy.js",
    "META-INF/adf/jsLibs/Poll.js",
    "META-INF/adf/jsLibs/ColorField.js",
    "META-INF/adf/jsLibs/ColorFieldFormat.js",
    "META-INF/adf/jsLibs/ColorFormat.js",
    "META-INF/adf/jsLibs/Shuttle.js",
  };

  // List of all libraries
  static private final String[] _DEBUG_LIBRARIES =
  {
    "META-INF/adf/jsDebug/CharSets.js",
    "META-INF/adf/jsDebug/CoreFormat.js",
    "META-INF/adf/jsDebug/DateField.js",
    "META-INF/adf/jsDebug/DateFieldFormat.js",
    "META-INF/adf/jsDebug/DateFormat.js",
    "META-INF/adf/jsDebug/Locale.js",
    "META-INF/adf/jsDebug/Core.js",
    "META-INF/adf/jsDebug/Window.js",
    "META-INF/adf/jsDebug/PPR.js",
    "META-INF/adf/jsDebug/TableProxy.js",
    "META-INF/adf/jsDebug/Poll.js",
    "META-INF/adf/jsDebug/ColorField.js",
    "META-INF/adf/jsDebug/ColorFieldFormat.js",
    "META-INF/adf/jsDebug/ColorFormat.js",
    "META-INF/adf/jsDebug/Shuttle.js",
  };

  static private final String _NEWLINE_SEPARATOR = "\n";
}
