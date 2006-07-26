/*
 * Copyright  2003-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.ui.expl;

import org.apache.myfaces.adfinternal.share.expl.ExplException;

import org.apache.myfaces.adfinternal.ui.RenderingContext;

/**
 * VariableResolver that supports UIX objects.  This VariableResolver
 * supports one named variable: uix, which provides access to UIX objects, 
 * like registered data objects and root attributes (in templates).
 * <p>
 * UIVariableResolver also supports implicit objects for any data providers
 * that have been registered in the default (empty-string) namespace.
 * For example, if a DataObject is available via
 * <pre>
 *   RenderingContext.getDataObject("", "foo")
 * </pre>
 * ... then that data object can be accessed in an expression like
 * <code>text="${foo.bar}"
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/expl/UIVariableResolver.java#0 $) $Date: 10-nov-2005.18:56:29 $
 * @author The Oracle ADF Faces Team
 */
public class UIVariableResolver
{

  public UIVariableResolver()
  {
    _uiObjects = createUIImplicitObject();
  }

  public Object resolveVariable(String pName)
    throws ExplException
  {
    //ystem.out.println("pName:"+pName+" pContxt:"+pContext);
    if (UI_IMPLICIT_OBJECT_NAME.equals(pName))
    {
      return _uiObjects;
    }

    return null;
  }

  protected UIImplicitObject createUIImplicitObject()
  {
    return new UIImplicitObject(this);
  }

  /**
   * gets the current RenderingContext
   */
  protected final RenderingContext getRenderingContext()
  {
    return _renderingContext;
  }

  /**
   * @see UIImplicitObject#adapt
   */
  protected final Object adapt(Object value)
  {
    return _uiObjects.adapt(value);
  }

  final void __setRenderingContext(RenderingContext rc)
  {
    _renderingContext = rc;
  }


  private RenderingContext _renderingContext = null;
  private final UIImplicitObject _uiObjects;

  /**
   * the name that UIImplicitObject is registered under. The value is 'uix'
   */
  public static final String UI_IMPLICIT_OBJECT_NAME = "uix";
}
