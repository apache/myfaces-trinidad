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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.faces.context.FacesContext;
import javax.faces.render.RenderKit;
import javax.faces.render.RenderKitFactory;

public class MRenderKitFactory extends RenderKitFactory
{
  public MRenderKitFactory()
  {
  }

  public void addRenderKit(String renderKitId,
                           RenderKit renderKit)
  {
    _kits.put(renderKitId, renderKit);
  }

  public RenderKit getRenderKit(FacesContext context, 
                                String renderKitId)
  {
    return (RenderKit) _kits.get(renderKitId);
  }
  
  public Iterator getRenderKitIds()
  {
    return _kits.keySet().iterator();
  }

  private Map _kits = new HashMap();
}
