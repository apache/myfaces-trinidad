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
package org.apache.myfaces.adfinternal.renderkit;

import java.io.OutputStream;
import java.io.Writer;

import javax.faces.FactoryFinder;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseStream;
import javax.faces.context.ResponseWriter;
import javax.faces.render.RenderKit;
import javax.faces.render.RenderKitFactory;
import javax.faces.render.Renderer;
import javax.faces.render.ResponseStateManager;

import org.apache.myfaces.adf.util.Service;

abstract public class RenderKitDecorator extends RenderKitBase
                                         implements Service.Provider
{
  public <T> T getService(Class<T> serviceClass)
  {
    return Service.getService(getRenderKit(), serviceClass);
  }

  public ResponseWriter createResponseWriter(
    Writer writer,
    String contentTypeList,
    String encoding)
  {
    RenderKit renderKit = getRenderKit();
    ResponseWriter out =
      renderKit.createResponseWriter(writer, contentTypeList, encoding);

    return createDecoratedResponseWriter(out);
  }

  public ResponseStream createResponseStream(
    OutputStream out)
  {
    RenderKit renderKit = getRenderKit();
    return renderKit.createResponseStream(out);
  }

  public ResponseStateManager getResponseStateManager()
  {
    RenderKit renderKit = getRenderKit();
    return renderKit.getResponseStateManager();
  }

  protected ResponseWriter createDecoratedResponseWriter(
    ResponseWriter delegate)
  {
    return delegate;
  }

  protected Renderer findRenderer(
    String componentFamily,
    String rendererType)
  {
    Renderer renderer = super.findRenderer(componentFamily, rendererType);

    // We did not find a renderer in our own render kit, so check
    // the decorated RenderKit
    if (renderer == null)
    {
      RenderKit renderKit = getRenderKit();
            
      // Use findRenderer() to avoid "not found" warning messages
      if (renderKit instanceof RenderKitBase)
        renderer = ((RenderKitBase) renderKit).findRenderer(
                                        componentFamily, rendererType);
      else
        renderer = renderKit.getRenderer(componentFamily, rendererType);
        
      // copy-on-read
      if (renderer != null)
        addRenderer(componentFamily, rendererType, renderer);
    }

    return renderer;
  }

  protected RenderKit getRenderKit()
  {
    FacesContext context = FacesContext.getCurrentInstance();
    RenderKitFactory factory = (RenderKitFactory)
      FactoryFinder.getFactory(FactoryFinder.RENDER_KIT_FACTORY);

    RenderKit renderKit = factory.getRenderKit(context,
					       getDecoratedRenderKitId());
    assert (renderKit != null);
    return renderKit;
  }

  abstract protected String getDecoratedRenderKitId();
}
