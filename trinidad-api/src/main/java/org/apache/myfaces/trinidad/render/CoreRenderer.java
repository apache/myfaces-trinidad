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

import java.util.Iterator;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.UIXComponent;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.RequestContext;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * Basic implementation of the core rendering functionality
 * across render kits.
 */
public class CoreRenderer extends Renderer
{
  // TODO Move elsewhere?
  static public final char CHAR_UNDEFINED = (char) -1;
  static public final int NO_CHILD_INDEX = -1;

  protected CoreRenderer()
  {
  }


  //
  // COERCION HELPERS
  //

  /**
   * Coerces an object into a String.
   */
  static public String toString(Object o)
  {
    if (o == null)
      return null;
    return o.toString();
  }

  /**
   * Coerces an object into a resource URI, calling the view-handler.
   */
  static public String toResourceUri(FacesContext fc, Object o)
  {
    if (o == null)
      return null;

    String uri = o.toString();

    // Treat two slashes as server-relative
    if (uri.startsWith("//"))
    {
      return uri.substring(1);
    }
    else
    {
      return fc.getApplication().getViewHandler().getResourceURL(fc, uri);
    }
  }

  /**
   * Coerces an object into an action URI, calling the view-handler.
   */
  static public String toActionUri(FacesContext fc, Object o)
  {
    if (o == null)
      return null;

    String uri = o.toString();

    // Treat two slashes as server-relative
    if (uri.startsWith("//"))
    {
      return uri.substring(1);
    }
    else
    {
      return fc.getApplication().getViewHandler().getActionURL(fc, uri);
    }
  }


  /**
   * Coerces an object into a resource URI, calling the view-handler.
   * @deprecated use toResourceUri
   */
  static public String toUri(Object o)
  {
    return toResourceUri(FacesContext.getCurrentInstance(),o);
  }


  /**
   * Returns the integer value of an object;  this does
   * not support null (which must be substituted with a default
   * before calling).
   */
  static public int toInt(Object o)
  {
    return ((Number) o).intValue();
  }



  /**
   * Returns the integer value of an object;  this does
   * not support null (which must be substituted with a default
   * before calling).
   */
  static public long toLong(Object o)
  {
    return ((Number) o).longValue();
  }

  /**
   * Returns the character value of an object, XhtmlConstants.CHAR_UNDEFINED
   * if there is none.
   */
  static public char toChar(Object o)
  {
    if (o == null)
      return CHAR_UNDEFINED;

    char c;
    if (o instanceof Character)
    {
      c = ((Character) o).charValue();
    }
    else
    {
      // If it's not a Character object, then let's turn it into
      // a CharSequence and grab the first character
      CharSequence cs;
      if (o instanceof CharSequence)
      {
        cs = (CharSequence) o;
      }
      else
      {
        cs = o.toString();
      }

      if (cs.length() == 0)
        c = CHAR_UNDEFINED;
      else
        c = cs.charAt(0);
    }

    // Handle the occasional odd bit of code that likes
    // returning null, and treat it identically to UNDEFINED.
    if (c == '\u0000')
      c = CHAR_UNDEFINED;

    return c;
  }


  @Override
  public final void encodeBegin(FacesContext context,
                          UIComponent component) throws IOException
  {
    if (!getRendersChildren())
    {
      RenderingContext rc = RenderingContext.getCurrentInstance();
      if (rc == null)
        throw new IllegalStateException(_LOG.getMessage(
          "NO_RENDERINGCONTEXT"));

      FacesBean bean = getFacesBean(component);

      beforeEncode(context, rc, component, bean);
      encodeBegin(context, rc, component, bean);
    }
  }

  @Override
  public final void encodeChildren(FacesContext context, UIComponent component)
  {
    // encodeChildren() is fairly useless - it's simpler to just
    // put the output in encodeEnd(), or use the encodeAll() hook
  }

  @Override
  public final void encodeEnd(FacesContext context,
                        UIComponent component) throws IOException
  {
    RenderingContext rc = RenderingContext.getCurrentInstance();
    if (rc == null)
      throw new IllegalStateException(_LOG.getMessage(
        "NO_RENDERINGCONTEXT"));

    FacesBean bean = getFacesBean(component);
    if (getRendersChildren())
    {
      beforeEncode(context, rc, component, bean);
      encodeAll(context, rc, component, bean);
    }
    else
    {
      encodeEnd(context, rc, component, bean);
    }
    afterEncode(context, rc, component, bean);
  }

  /**
   * Hook for rendering the start of a component;  only
   * called if getRendersChildren() is <em>false</em>.
   */
  protected void encodeBegin(
    FacesContext        context,
    RenderingContext rc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    if (getRendersChildren())
      throw new IllegalStateException();
  }

  /**
   * Hook for rendering the end of a component;  only
   * called if getRendersChildren() is <em>false</em>.
   */
  protected void encodeEnd(
    FacesContext        context,
    RenderingContext rc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    if (getRendersChildren())
      throw new IllegalStateException();
  }


  /**
   * Hook for rendering all of a component;  only
   * called if getRendersChildren() is <em>true</em>.
   */
  protected void encodeAll(
    FacesContext        context,
    RenderingContext rc,
    UIComponent         component,
    FacesBean           bean) throws IOException
  {
    if (!getRendersChildren())
      throw new IllegalStateException();
  }

  /**
   * Hook for encoding a child;  this assumes that isRendered()
   * has already been called. (RenderUtils.encodeRecursive()
   * can be used if you don't need that check.)
   * =-=AEW Ugh.
   */
  @SuppressWarnings("unchecked")
  protected void encodeChild(
    FacesContext context,
    UIComponent  child) throws IOException
  {
    assert(child.isRendered());
    child.encodeBegin(context);
    if (child.getRendersChildren())
    {
      child.encodeChildren(context);
    }
    else
    {
      if (child.getChildCount() > 0)
      {
        for(UIComponent subChild : (List<UIComponent>)child.getChildren())
        {
          RenderUtils.encodeRecursive(context, subChild);
        }
      }
    }

    child.encodeEnd(context);
  }


  @SuppressWarnings("unchecked")
  protected void encodeAllChildren(
    FacesContext context,
    UIComponent  component) throws IOException
  {
    int childCount = component.getChildCount();
    if (childCount == 0)
      return;

    for(UIComponent child : (List<UIComponent>)component.getChildren())
    {
      if (child.isRendered())
      {
        encodeChild(context, child);
      }
    }
  }

  protected void delegateRenderer(
    FacesContext        context,
    RenderingContext rc,
    UIComponent         component,
    FacesBean           bean,
    CoreRenderer        renderer) throws IOException
  {
    if (renderer.getRendersChildren())
    {
      renderer.encodeAll(context, rc, component, bean);
    }
    else
    {
      throw new IllegalStateException();
    }
  }

  protected void delegateRendererBegin(
    FacesContext        context,
    RenderingContext rc,
    UIComponent         component,
    FacesBean           bean,
    CoreRenderer        renderer) throws IOException
  {
    if (renderer.getRendersChildren())
    {
      throw new IllegalStateException();
    }
    else
    {
      renderer.encodeBegin(context, rc, component, bean);
    }
  }

  protected void delegateRendererEnd(
    FacesContext        context,
    RenderingContext rc,
    UIComponent         component,
    FacesBean           bean,
    CoreRenderer        renderer) throws IOException
  {
    if (renderer.getRendersChildren())
    {
      throw new IllegalStateException();
    }
    else
    {
      renderer.encodeEnd(context, rc, component, bean);
    }
  }

  /**
   * Renders the client ID as an "id".
   */
  protected void renderId(
    FacesContext context,
    UIComponent  component) throws IOException
  {
    if (shouldRenderId(context, component))
    {
      String clientId = getClientId(context, component);
      context.getResponseWriter().writeAttribute("id", clientId, "id");
    }
  }

  /**
   * Returns the client ID that should be used for rendering (if
   * {@link #shouldRenderId} returns true).
   */
  protected String getClientId(
    FacesContext context,
    UIComponent  component)
  {
    return component.getClientId(context);
  }

  /**
   * Returns true if the component should render an ID.  Components
   * that deliver events should always return "true".
   */
  // TODO Is this a bottleneck?  If so, optimize!
  protected boolean shouldRenderId(
    FacesContext context,
    UIComponent  component)
  {
    String id = component.getId();

    // Otherwise, if ID isn't set, don't bother
    if (id == null)
      return false;

    // ... or if the ID was generated, don't bother
    if (id.startsWith(UIViewRoot.UNIQUE_ID_PREFIX))
      return false;

    return true;
  }

  protected boolean skipDecode(FacesContext context)
  {
    return false;
  }

  protected FacesBean getFacesBean(UIComponent component)
  {
    return ((UIXComponent) component).getFacesBean();
  }

  static protected final Object getRenderingProperty(
    RenderingContext rc,
    Object              key)
  {
    return rc.getProperties().get(key);
  }

  static protected final Object setRenderingProperty(
    RenderingContext rc,
    Object              key,
    Object              value)
  {
    return rc.getProperties().put(key, value);
  }

  /**
   * Gets a facet, verifying that the facet should be rendered.
   */
  static public UIComponent getFacet(
    UIComponent component,
    String      name)
  {
    UIComponent facet = component.getFacet(name);
    if ((facet == null) || !facet.isRendered())
      return null;

    return facet;
  }

  /**
   * Returns true if the component has children and at least
   * one has rendered=="true".
   */
  @SuppressWarnings("unchecked")
  static public boolean hasRenderedChildren(UIComponent component)
  {
    int count = component.getChildCount();
    if (count == 0)
      return false;

    for(UIComponent child : (List<UIComponent>)component.getChildren())
    {
      if (child.isRendered())
      {
        return true;
      }
    }

    return false;
  }

  /**
   * Returns the total number of children with rendered=="true".
   */
  @SuppressWarnings("unchecked")
  static public int getRenderedChildCount(UIComponent component)
  {
    int count = component.getChildCount();
    if (count == 0)
      return 0;

    int total = 0;
    for(UIComponent child : (List<UIComponent>)component.getChildren())
    {
      if (child.isRendered())
      {
        total++;
      }
    }

    return total;
  }


 /**
   * @param afterChildIndex The children coming after this index, will
   * be considered.
   * @return the index of the next child that must be rendered, or
   * {@link #NO_CHILD_INDEX} if there is none.
   */
  public static int getNextRenderedChildIndex(
    List<UIComponent> components,
    int  afterChildIndex
    )
  {
    int childIndex = afterChildIndex + 1;
    Iterator<UIComponent> iter = components.listIterator(childIndex);
    for(; iter.hasNext(); childIndex++)
    {
      if(iter.next().isRendered())
      {
        return childIndex;
      }
    }

    return NO_CHILD_INDEX;
  }


  //
  // AGENT CAPABILITY CONVENIENCE METHODS
  //

  static public boolean isDesktop(RenderingContext rc)
  {
    return (Agent.TYPE_DESKTOP.equals(rc.getAgent().getType()));
  }

  static public boolean isPDA(RenderingContext rc)
  {
    return (Agent.TYPE_PDA.equals(rc.getAgent().getType()));
  }

  static public boolean isIE(RenderingContext rc)
  {
    return (Agent.AGENT_IE.equals(rc.getAgent().getAgentName()));
  }

  static public boolean isKonqueror(RenderingContext rc)
  {
    return (Agent.AGENT_KONQUEROR.equals(rc.getAgent().getAgentName()));
  }

  static public boolean isGecko(RenderingContext rc)
  {
    return (Agent.AGENT_GECKO.equals(rc.getAgent().getAgentName()));
  }

  static public boolean isWebKit(RenderingContext rc)
  {
    return (Agent.AGENT_WEBKIT.equals(rc.getAgent().getAgentName()));
  }

  static public boolean isIPhone(RenderingContext rc)
  {
    return (Agent.PLATFORM_IPHONE.equals(rc.getAgent().getPlatformName()));
  }

  static public boolean isGenericPDA(RenderingContext rc)
  {
    return (Agent.PLATFORM_GENERICPDA.equals(rc.getAgent().getPlatformName()));
  }

  static public boolean isInaccessibleMode(RenderingContext rc)
  {
    return (rc.getAccessibilityMode() ==
            RequestContext.Accessibility.INACCESSIBLE);
  }

  static public boolean isScreenReaderMode(RenderingContext rc)
  {
    return (rc.getAccessibilityMode() ==
            RequestContext.Accessibility.SCREEN_READER);
  }

  //
  // Encoding hook methods for sub-classes
  //

  /**
   * Hook method that gets invoked before the component is encoded
   *
   * @see #encodeBegin(FacesContext, RederingContext, UIComponent, FacesBean)
   * @see #encodeAll(FacesContext, RederingContext, UIComponent, FacesBean)
   */
  protected void beforeEncode(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {}

  /**
   * Hook method that gets invoked after the component is encoded
   *
   * @see #encodeEnd(FacesContext, RederingContext, UIComponent, FacesBean)
   * @see #encodeAll(FacesContext, RederingContext, UIComponent, FacesBean)
   */
  protected void afterEncode(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean)
  {}

  //
  // Rendering convenience methods.
  //

  protected void renderEncodedActionURI(
   FacesContext context,
   String       name,
   Object       value) throws IOException
  {
    if (value != null)
    {
      value = context.getExternalContext().encodeActionURL(value.toString());
      context.getResponseWriter().writeURIAttribute(name, value, null);
    }
  }

  protected void renderEncodedResourceURI(
   FacesContext context,
   String       name,
   Object       value) throws IOException
  {
    if (value != null)
    {
      value = context.getExternalContext().encodeResourceURL(value.toString());
      context.getResponseWriter().writeURIAttribute(name, value, null);
    }
  }



  /**
   * Render a generic CSS styleClass (not one derived from an attribute).
   * The styleclass will be passed through the RenderingContext
   * getStyleClass() API.
   * @param context  the FacesContext
   * @param styleClass the style class
   */
  static public void renderStyleClass(
    FacesContext        context,
    RenderingContext rc,
    String              styleClass) throws IOException
  {
    if (styleClass != null)
    {
      styleClass = rc.getStyleClass(styleClass);
      context.getResponseWriter().writeAttribute("class", styleClass, null);
    }
  }

  /**
   * Render an array of CSS styleClasses as space-separated values.
   * @param context  the FacesContext
   * @param styleClasses the style classes
   */
  static public void renderStyleClasses(
    FacesContext        context,
    RenderingContext    rc,
    String[]            styleClasses) throws IOException
  {
    int length = styleClasses.length;
    if (length == 0)
      return;

    String value;
    // Optimize one-element arrays
    if (length == 1)
    {
      value = rc.getStyleClass(styleClasses[0]);
    }
    // Otherwise, build up the array of mutated style classes.
    else
    {
      // Assume that styleclass compression is active in terms of sizing
      // this buffer - this is not true for portlets, but this isn't a
      // huge optimizations, and the relatively smaller content delivered
      // to portlets makes this still less important
      StringBuilder builder =
        new StringBuilder((_COMPRESSED_LENGTH + 1) * length);
      for (int i = 0; i < length; i++)
      {
        if (styleClasses[i] != null)
        {
          String styleClass = rc.getStyleClass(styleClasses[i]);
          if (styleClass != null)
          {
            if (builder.length() != 0)
              builder.append(' ');
            builder.append(styleClass);
          }
        }
      }

      if (builder.length() == 0)
        value = null;
      else
        value = builder.toString();
    }

    context.getResponseWriter().writeAttribute("class", value, null);
  }


  // Heuristic guess of the maximum length of a typical compressed style
  private static final int _COMPRESSED_LENGTH = 4;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(
    CoreRenderer.class);
}
