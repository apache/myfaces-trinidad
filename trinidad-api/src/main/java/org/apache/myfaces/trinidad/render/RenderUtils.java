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
package org.apache.myfaces.trinidad.render;

import java.io.IOException;
import java.util.List;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.component.UIForm;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.UIXForm;

/**
 * Generic utilities for rendering.
 *
 */
public class RenderUtils
{
  private RenderUtils()
  {
  }


  /**
   * Encodes a component and all of its children.
   */
  @SuppressWarnings("unchecked")
  static public void encodeRecursive(FacesContext context,
                                     UIComponent component)
    throws IOException
  {
    if (component.isRendered())
    {
      component.encodeBegin(context);
      if (component.getRendersChildren())
      {
        component.encodeChildren(context);
      }
      else
      {
        if (component.getChildCount() > 0)
        {
          for(UIComponent child : (List<UIComponent>)component.getChildren())
          {
            encodeRecursive(context, child);
          }
        }
      }

      component.encodeEnd(context);
    }
  }

  /**
   *  Retrieves id of the form the component is contained in.
   *
   * @param context the faces context object
   * @param component UIComponent whose container form id is to be retuirned
   *
   * @return String id of the form if one exists in component's hierarchy,
   *                otherwise null
   */
  public static String getFormId(
    FacesContext context,
    UIComponent  component)
  {
    UIComponent form = null;
    while (component != null)
    {
      if ((component instanceof UIForm) ||
          (component instanceof UIXForm))
      {
        form = component;
        break;
      }

      component = component.getParent();
    }

    if (form == null)
      return null;

    return form.getClientId(context);
  }

  /**
   * Returns a relative ID for use at rendering time, e.g. "for"
   * attributes on components.  It does not assume that the target
   * component can be located.  A relative ID starting with
   * NamingContainer.SEPARATOR_CHAR (that is, ':') will be
   * treated as absolute (after dropping that character).
   */
  public static String getRelativeId(
    FacesContext context,
    UIComponent  from,
    String       relativeId)
  {
    if ((relativeId == null) || (relativeId.length() == 0))
      return null;

    if (relativeId.charAt(0) == NamingContainer.SEPARATOR_CHAR)
      return relativeId.substring(1);

    UIComponent parentNC = _getParentNamingContainer(from.getParent());
    if (parentNC == null)
      return relativeId;

    return (parentNC.getClientId(context) +
            NamingContainer.SEPARATOR_CHAR + relativeId);
  }

  private static UIComponent _getParentNamingContainer(UIComponent from)
  {
    while (from != null)
    {
      if (from instanceof NamingContainer)
        return from;
      from = from.getParent();
    }

    return null;
  }
}
