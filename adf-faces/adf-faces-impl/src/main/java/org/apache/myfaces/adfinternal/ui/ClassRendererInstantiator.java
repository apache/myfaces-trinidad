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
package org.apache.myfaces.adfinternal.ui;

import org.apache.myfaces.adf.logging.ADFLogger;

import org.apache.myfaces.adf.util.ClassLoaderUtils;
import java.lang.reflect.UndeclaredThrowableException;

/**
 * Implements deferred loading of Renderer implementations.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/ClassRendererInstantiator.java#0 $) $Date: 10-nov-2005.18:50:12 $
 * @author The Oracle ADF Faces Team
 */
class ClassRendererInstantiator implements RendererInstantiator
{
  public ClassRendererInstantiator(String className)
  {
    if (className == null)
      throw new IllegalArgumentException();

    _className = className;
  }


  public Renderer instantiate()
  {
    try
    {
      Class classInstance = ClassLoaderUtils.loadClass(_className);
      return (Renderer) classInstance.newInstance();
    }
    catch (ClassNotFoundException cnfe)
    {
      _showInstantiationError(cnfe);
      throw new UndeclaredThrowableException( cnfe,
         "Instantiation of UIX Components Renderer failed, class " +
         _className + " not found.");
    }
    catch (IllegalAccessException iae)
    {
      _showInstantiationError(iae);
    }
    catch (InstantiationException ie)
    {
      _showInstantiationError(ie);
    }

    return null;
  }

  private void _showInstantiationError(
    Throwable e
    )
  {
    _LOG.severe("Instantiation of Renderer " +
                _className                   +
                " failed.",
                e);
  }

  private String _className;
  static private final ADFLogger _LOG =
    ADFLogger.createADFLogger(ClassRendererInstantiator.class);
}
