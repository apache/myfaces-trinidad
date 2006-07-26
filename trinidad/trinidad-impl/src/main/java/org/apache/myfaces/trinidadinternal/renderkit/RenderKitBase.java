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
package org.apache.myfaces.trinidadinternal.renderkit;

import java.util.Map;

import java.util.concurrent.ConcurrentHashMap;

import javax.faces.render.Renderer;
import javax.faces.render.RenderKit;


import org.apache.myfaces.trinidad.logging.ADFLogger;


/**
 * Base implementation of RenderKit.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/RenderKitBase.java#0 $) $Date: 10-nov-2005.19:00:26 $
 * @author The Oracle ADF Faces Team
 */
abstract public class RenderKitBase extends RenderKit
{
  public RenderKitBase()
  {
    _renderers = new ConcurrentHashMap<String, Map<String, Object>>
                         (64, 0.75f, 1);
  }



  /**
   * <p>Add a new {@link Renderer} instance, associated with the
   * specified <code>rendererType</code>, to the set of
   * {@link Renderer}s registered with this {@link RenderKit}.
   *
   * @param rendererType Renderer type of the new {@link Renderer}
   * @param renderer The new {@link Renderer} instance
   *
   * @exception IllegalArgumentException if a {@link Renderer} with the
   *  specified <code>rendererType</code> has already been registered
   * @exception NullPointerException if <code>rendererType</code> or
   *  <code>renderer</code> is null
   */
  public void addRenderer(
     String family,
     String rendererType,
     Renderer renderer)
  {
    /* =-=AEW No: it is legal to override a renderer.
    if ( _get(family, rendererType) != null )
      throw new IllegalArgumentException("Duplicate renderer type \"" +
                                         rendererType +
                                         "\" for family \"" +
                                         family + "\"");
    */
    _put(family, rendererType, renderer);
  }




  /**
   * Extension allowing for on-demand class loading.
   */
  public void addRenderer(String family,
                          String rendererType,
                          String rendererClassName)
  {
    /* =-=AEW No: it is legal to override a renderer.
    if ( _get(family, rendererType) != null )
      throw new IllegalArgumentException("Duplicate renderer type \"" +
                                         rendererType +
                                         "\" for family \"" +
                                         family + "\"");
    */

    _put(family, rendererType,
         new ClassRendererInstantiator(rendererClassName));
  }





  /**
   * <p>Create (if necessary) and return a {@link Renderer} instance
   * with the specified renderer type.  Subsequent calls to this method
   * with the same <code>rendererType</code>, from the same web application,
   * must return the same instance.</p>
   *
   * @param rendererType Renderer type to be returned
   *
   * @exception IllegalArgumentException if the requested renderer type
   *  is not supported by this {@link RenderKit}
   * @exception NullPointerException if <code>rendererType</code>
   *  is <code>null</code>
   */
  public Renderer getRenderer(String family, String rendererType)
  {
    Renderer renderer = findRenderer(family, rendererType);
    if (renderer == null)
    {
      if (_LOG.isWarning())
        _LOG.warning("Renderer ''{0}'' not found for component family ''{1}''",
                     new String[]{rendererType, family});
    }

    return renderer;
  }


  /**
   * Finds a renderer - and returns null if one cannot be found.
   * getRenderer() will log a warning when one cannot be found.
   */
  protected Renderer findRenderer(String family, String rendererType)
  {
    Object o = _get(family, rendererType);
    Renderer renderer = null;

    if (o == null)
    {
      if (_aggregated != null)
      {
        // Use findRenderer() to avoid "not found" warning messages
        if (_aggregated instanceof RenderKitBase)
          renderer = ((RenderKitBase) _aggregated).findRenderer(
                                                      family, rendererType);
        else
          renderer = _aggregated.getRenderer(family, rendererType);
      }
    }
    else if (o instanceof Renderer)
    {
      renderer = (Renderer) o;
    }
    else if (o instanceof RendererInstantiator)
    {
      renderer = ((RendererInstantiator) o).instantiate();
      _put(family, rendererType, renderer);
    }

    return renderer;
  }

  protected void attachAggregatedRenderKit(RenderKit aggregated)
  {
    _aggregated = aggregated;
  }

  /**
   * @todo use ArrayMap for "sub-maps", since they'll almost
   * always be small?
   */
  synchronized private void _put(
    String family,
    String rendererType,
    Object o)
  {
    Map<String, Object> subMap = _renderers.get(family);
    if (subMap == null)
    {
      subMap = new ConcurrentHashMap<String, Object>(8, 0.75f, 1);
      _renderers.put(family, subMap);
    }

    subMap.put(rendererType, o);
  }


  private Object _get(
    String family,
    String rendererType)
  {
    Map<String, Object> subMap = _renderers.get(family);
    if (subMap == null)
      return null;

    return subMap.get(rendererType);
  }

  private RenderKit _aggregated;
  private Map<String, Map<String, Object>>  _renderers;

  static private final ADFLogger _LOG = ADFLogger.createADFLogger(RenderKitBase.class);
}
