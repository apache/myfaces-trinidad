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

package org.apache.myfaces.adf.resource;

import java.io.IOException;
import java.net.URL;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A resource loader implementation which loads resources
 * by pattern matching the requested resource path to a
 * registered resource loader.
 *
 * @author The Oracle ADF Faces Team
 */
public class RegexResourceLoader extends ResourceLoader
{
  /**
   * Creates a new RegexResourceLoader.
   */
  public RegexResourceLoader()
  {
    _loaders = new ArrayList();
  }
  
  protected URL findResource(
    String path) throws IOException
  {
    Iterator iter = _loaders.iterator();
    while (iter.hasNext())
    {
      Pattern pattern = (Pattern) iter.next();
      assert iter.hasNext();
      ResourceLoader loader = (ResourceLoader) iter.next();

      Matcher matcher = pattern.matcher(path);
      if (matcher.matches())
      {
        return loader.getResource(matcher.group(1));
      }
    }
    
    return null;
  }
  
  /**
   * Registers a resource loader by regular expression.
   * 
   * @param regex  the regular expression to match
   * @param loader  the resource loader to use for matching paths
   */
  protected void register(
    String         regex,
    ResourceLoader loader)
  {
    Pattern pattern = Pattern.compile(regex);
    _checkPathRegex(regex);
    _loaders.add(pattern);
    _loaders.add(loader);
  }
  
  /**
   * Deregisters a resource loader by regular expression.
   * 
   * @param regex  the regular expression to remove
   */
  protected void deregister(
    String regex)
  {
    Pattern pattern = Pattern.compile(regex);

    for (int i = 0; i < _loaders.size(); i += 2)
    {
      if (pattern.equals(_loaders.get(i)))
      {
        _loaders.remove(i);
        _loaders.remove(i);
        return;
      }
    }
  }

  /**
   * Verify that the regular expression will match only paths with a 
   * leading slash.
   * 
   * @param regex  the regular expression to verify
   */
  private void _checkPathRegex(
    String regex)
  {
    if (!regex.startsWith("/")  &&
        !regex.startsWith("(/"))
    {
      throw new IllegalArgumentException("Resource path regular expression \"" +
                                         regex + 
                                         "\" does not have leading slash");
    }
  }

  private final List _loaders;
}
