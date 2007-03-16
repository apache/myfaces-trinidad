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
package org.apache.myfaces.trinidadinternal.ui.laf.xml.parse;

import org.xml.sax.Attributes;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;


import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

import org.apache.myfaces.trinidadinternal.share.xml.LeafNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidad.skin.Icon;
import org.apache.myfaces.trinidadinternal.ui.laf.xml.XMLConstants;

/**
 * NodeParser for instance icons.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/InstanceIconParser.java#0 $) $Date: 10-nov-2005.18:50:39 $
 * @deprecated This class comes from the old Java 1.2 UIX codebase and should not be used anymore.
 */
@Deprecated
public class InstanceIconParser extends LeafNodeParser implements XMLConstants
{
  @Override
  protected Object getNodeValue(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs)
  {
    return _createInstanceIcon(context, attrs);
  }

  // =-=ags The following code is copied almost verbatim from
  //        org.apache.myfaces.trinidadinternal.uix22.servlet.xml.parse.EventHandlerFactory!
  //        This common code should live somewhere in share.

  private Class<?> _getClass(
    ParseContext context,
    Attributes   attrs)
  {
    String className = getRequiredAttribute(context, attrs, "class");
    if (className != null)
    {
      try
      {
        return ClassLoaderUtils.loadClass(className);
      }
      catch (ClassNotFoundException cnfe)
      {
        if (_LOG.isWarning())
          _LOG.warning("Could not find class " + className);
      }
      catch (Error error)
      {
        if (_LOG.isWarning())
          _LOG.warning("Could not load class " + className + ": " + error);
      }
    }

    return null;
  }

  private Icon _createInstanceIcon(
    ParseContext context,
    Attributes   attrs)
  {
    Class<?> handlerClass = _getClass(context, attrs);
    if (handlerClass != null)
    {
      String methodName = attrs.getValue("method");

      if(methodName == null)
        methodName = _DEFAULT_INSTANCE_METHOD_NAME;

      try
      {
        try
        {
          Method instanceMethod = handlerClass.getMethod(methodName);
          if (!Icon.class.isAssignableFrom(
                  instanceMethod.getReturnType()))
          {
            if (_LOG.isWarning())
              _LOG.warning("Method " + methodName + " does not return an Icon");
          }
          else
          {
            return (Icon) instanceMethod.invoke(null);
          }
        }
        catch (NoSuchMethodException nsme)
        {
          if ((methodName == _DEFAULT_INSTANCE_METHOD_NAME) &&
              (Icon.class.isAssignableFrom(handlerClass)))
            return (Icon) handlerClass.newInstance();

          if (_LOG.isWarning())
            _LOG.warning("Could not find method " + methodName + " in " +
                         handlerClass.getName());
        }
      }
      catch (InstantiationException ie)
      {
        _LOG.warning(ie);
      }
      catch (IllegalAccessException iacce)
      {
        if (_LOG.isWarning())
          _LOG.warning("Could not find access " + methodName + " in " +
                       handlerClass.getName());
      }
      catch (IllegalArgumentException iarge)
      {
        _LOG.warning(iarge);
      }
      catch (InvocationTargetException ite)
      {
        Throwable t = ite.getTargetException();
        if (t instanceof RuntimeException)
          throw ((RuntimeException) t);
        _LOG.warning(ite);
      }
    }

    return null;
  }

  static private final String _DEFAULT_INSTANCE_METHOD_NAME =
    "sharedInstance";
  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(InstanceIconParser.class);
}

