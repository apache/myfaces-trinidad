/*
 * Copyright  2005,2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.renderkit;


import java.util.Iterator;

import javax.faces.context.FacesContext;
import javax.faces.render.RenderKit;
import javax.faces.render.RenderKitFactory;

import org.apache.myfaces.adfinternal.renderkit.core.CoreRenderKit;

/**
 * RenderKitFactory that implements CoreRenderKit switching.
 * 
 * @author The Oracle ADF Faces Team
 */
public class CoreRenderKitFactory extends RenderKitFactory
{
  public CoreRenderKitFactory(RenderKitFactory factory)
  {
    _factory = factory;
  }


  public void addRenderKit(String renderKitId, RenderKit renderKit)
  {
    _factory.addRenderKit(renderKitId, renderKit);
  }


  public RenderKit getRenderKit(FacesContext context, String renderKitId)
  {
    if (CoreRenderKit.getId().equals(renderKitId))
    {
      renderKitId = CoreRenderKit.chooseRenderKit(context);
    }

    return _factory.getRenderKit(context, renderKitId);
  }

  public Iterator getRenderKitIds()
  {
    return _factory.getRenderKitIds();
  }

  private RenderKitFactory _factory;
}
