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

import org.xml.sax.SAXException;

import javax.faces.FactoryFinder;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.render.RenderKit;
import javax.faces.render.RenderKitFactory;

import java.io.IOException;

import org.apache.myfaces.adf.context.Agent;

import org.apache.myfaces.adfinternal.agent.AgentFactoryImpl;

import org.apache.myfaces.adfinternal.renderkit.core.CoreRenderKit;

import org.apache.myfaces.adfinternal.skin.SkinFactory;
import org.apache.myfaces.adfinternal.skin.SkinFactoryImpl;
import org.apache.myfaces.adfinternal.skin.SkinUtils;

public class RenderKitBootstrap
{
  static public UIViewRoot createUIViewRoot(MFacesContext context)
  {
    UIViewRoot root = new UIViewRoot();
    root.setRenderKitId("org.apache.myfaces.adf.core");
    root.setViewId("/test-view-id.jspx");
    root.setLocale(context.getLocale());
    return root;
  }


  public FacesConfigInfo getFacesConfigInfo()
  {
    return _facesConfigInfo;
  }

  static public Agent getGeckoAgent()
  {
    return _geckoAgent;
  }

  static public Agent getIEAgent()
  {
    return _ieAgent;
  }

  static public Agent getSafariAgent()
  {
    return _safariAgent;
  }

  static public Agent getPocketPCAgent()
  {
    return _pocketPCAgent;
  }

  static public RenderKit getRenderKit(FacesContext context)
  {
    RenderKitFactory factory = (RenderKitFactory)
      FactoryFinder.getFactory(FactoryFinder.RENDER_KIT_FACTORY);

    String renderKitId = null;
    if (context.getViewRoot() != null)
      renderKitId = context.getViewRoot().getRenderKitId();
    if (renderKitId == null)
      renderKitId = "org.apache.myfaces.adf.core";

    RenderKit renderKit = factory.getRenderKit(context,renderKitId);
    if (renderKit == null)
      throw new IllegalStateException("Could not create renderKit " + renderKitId);

    return renderKit;
  }

  static public void clearFactories()
  {
    FactoryFinder.releaseFactories();
  }

  static public void setFactories(FacesConfigInfo info)
  {
    FactoryFinder.releaseFactories();
    FactoryFinder.setFactory(FactoryFinder.RENDER_KIT_FACTORY,
                             MRenderKitFactory.class.getName());
    FactoryFinder.setFactory(FactoryFinder.RENDER_KIT_FACTORY,
                             CoreRenderKitFactory.class.getName());
    RenderKitFactory rkFactory = (RenderKitFactory)
      FactoryFinder.getFactory(FactoryFinder.RENDER_KIT_FACTORY);
    rkFactory.addRenderKit(RenderKitFactory.HTML_BASIC_RENDER_KIT,
                           new BasicHtmlRenderKit());
    if (info != null)
    {
      for (String rkId : info.getRenderKits().keySet())
      {
        rkFactory.addRenderKit(rkId, info.getRenderKits().get(rkId));
      }
    }

    // Set up the SkinFactory
    if (SkinFactory.getFactory() == null)
    {
      SkinFactory.setFactory(new SkinFactoryImpl());

      // register the base skins
      // =-=AEW Because we don't have a "ServletContext", we can't
      // find any custom skins
      SkinUtils.registerBaseSkins();
    }
  }


  public void init() throws IOException, SAXException
  {
    // Set up the Basic HTML RenderKit and core factories
    setFactories(null);
    _facesConfigInfo = new FacesConfigInfo();
    _facesConfigInfo.load("META-INF/faces-config.xml");
    clearFactories();


    _facesConfigInfo.registerComponents(MApplication.sharedInstance());
    _facesConfigInfo.registerConverters(MApplication.sharedInstance());
  }

  static private void _createAgents()
  {
    AgentFactoryImpl factory = new AgentFactoryImpl();
    _geckoAgent = factory.createAgent("Mozilla/5.0 (Windows; U; Windows NT 5.0; en-US; rv:1.7.5) Gecko/20050207 Firefox/1.0.1",
                        null);
    _ieAgent = factory.createAgent("Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)",
                                   null);

    // Give a Safari 2.0 agent
    _safariAgent = factory.createAgent("Mozilla/5.0 (Macintosh; U; PPC Mac OS X; en) AppleWebKit/412 (KHTML, like Gecko) Safari/412",
                                   null);

    // I think this is PocketPC 2003
    _pocketPCAgent = factory.createAgent("Mozilla/4.0 (compatible; MSIE 4.01; Windows CE; MSN Companion 2.0; 800x600; Compaq)",
                                   null);
  }

  private FacesConfigInfo _facesConfigInfo;
  private CoreRenderKit   _coreRenderKit;

  static private Agent           _geckoAgent;
  static private Agent           _ieAgent;
  static private Agent           _safariAgent;
  static private Agent           _pocketPCAgent;

  static
  {
    _createAgents();
  }
}

