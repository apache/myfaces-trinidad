/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.trinidadinternal.resource;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;

import org.apache.myfaces.trinidad.resource.StringContentResourceLoader;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.XhtmlUtils;

public class TrTranslationsResourceLoader extends StringContentResourceLoader
{
  /**
   * Constructs a dynamic resouce loader for this path which serves up translations
   * 
   * @param path the path of this dynamic resource loader
   */
  public TrTranslationsResourceLoader(String path)
  {
    super(path);
    setMap("TrMessageFactory._TRANSLATIONS");
    setBundle("org.apache.myfaces.trinidad.resource.MessageBundle");
  }
  
  protected void setMap(String mapName)
  {
    this.mapName = mapName;
  }
  
  protected void setBundle(String bundleName)
  {
    this.bundleName = bundleName;
  }
  
  @Override
  protected String getString(String path) throws IOException
  {
    // its always better to initialize the StringBuffer with size instead of leaving it unset
    StringBuffer bundleMap = new StringBuffer(50000);
    String content   = "";
    
    String locale = CoreRenderKitResourceLoader.getLocale();
    
    bundleMap.append(mapName)
             .append(" = ")
             .append("\n{\n");
    
    content += _processBundle(bundleName, locale);
    
    //Remove the last 2 characters( ie, newLine & ',')
    bundleMap.append(content.substring(0, content.length()-2))
             .append("\n};");
    
    return bundleMap.toString();
  }
  
  @Override
  protected String getContentType(String path)
  {
    return _CONTENT_TYPE;
  }
  
  @Override
  protected URL findResource(
    String path) throws IOException
  {
    return getURL(path);
  }
  
  protected String _processBundle(
    String  bundleName,
    String  localeName
    ) throws IOException
  {
    StringBuffer transMap = new StringBuffer(50000);
    Object obj = invokeGetContents(bundleName, localeName);
    
    if( obj != null)
    {
      Object entry[] = (Object [])obj;
      
      for(int i=0; i< entry.length; i++)
      {
        transMap.append("'" + ((Object [])entry[i])[0] + "'")
                .append(":")
                .append("'" + XhtmlUtils.escapeJS(((Object [])entry[i])[1].toString(), true) + "',")
                .append("\n");
      }
    }
    
    return transMap.toString();
  }
  
  protected Object invokeGetContents(
    String  bundleName,
    String  localeName
    ) throws IOException
  {
    String className  = bundleName + "_" + localeName; 
    Object obj = null;
    
    try
    {
      Class<?> clazz;
      try
      {
        clazz = ClassLoaderUtils.loadClass(className);
      }
      catch (ClassNotFoundException e)    
      { 
        // If there is no bundle specific to locale, use Default locale
        clazz = ClassLoaderUtils.loadClass(bundleName);    
      }
      
      //Invoke getContents() method which gives us the Translations
      Method x = clazz.getMethod("getContents", (Class[]) null);  
      obj = x.invoke(clazz.newInstance(), (Object[]) null);
    
    } catch (NoSuchMethodException e)     { ;
    } catch (InvocationTargetException e) { ;
    } catch (IllegalAccessException e)    { ;
    } catch (InstantiationException e)    { ;
    } catch (ClassNotFoundException e)    { ;
    }
    
    return obj;  
  }
  
  protected String mapName;
  protected String bundleName;
  
  private static final String _CONTENT_TYPE = "text/javascript";
}
