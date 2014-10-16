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
package org.apache.myfaces.trinidadinternal.skin.provider;

import junit.framework.TestCase;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.render.ExtendedRenderKitService;
import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidad.skin.SkinFactory;
import org.apache.myfaces.trinidad.skin.SkinMetadata;
import org.apache.myfaces.trinidad.skin.SkinProvider;
import org.apache.myfaces.trinidad.skin.SkinVersion;
import org.apache.myfaces.trinidad.util.Service;
import org.apache.myfaces.trinidadinternal.io.XhtmlResponseWriter;
import org.apache.myfaces.trinidadinternal.renderkit.FacesConfigInfo;
import org.apache.myfaces.trinidadinternal.renderkit.MApplication;
import org.apache.myfaces.trinidadinternal.renderkit.MFacesContext;
import org.apache.myfaces.trinidadinternal.renderkit.MRequestContext;
import org.apache.myfaces.trinidadinternal.renderkit.NullWriter;
import org.apache.myfaces.trinidadinternal.renderkit.RenderKitBootstrap;
import org.apache.myfaces.trinidadinternal.renderkit.RenderUtils;
import org.apache.myfaces.trinidadinternal.renderkit.TestResponseWriter;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.TrinidadRenderingConstants;
import org.apache.myfaces.trinidadinternal.skin.SkinFactoryImpl;
import org.apache.myfaces.trinidadinternal.skin.custom.CustomSkinProvider1;
import org.apache.myfaces.trinidadinternal.skin.custom.CustomSkinProvider2;

import org.apache.myfaces.trinidad.component.core.CoreDocument;
import org.apache.myfaces.trinidad.component.core.CoreForm;

import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentMap;

public class SkinProviderTest extends TestCase
{

  public SkinProviderTest(String testName)
  {
    super(testName);
  }

  @Override
  protected void setUp() throws IOException
  {
    MFacesContext ctx = new MFacesContext(MApplication.sharedInstance(), true);
    ExternalContext externalContext = ctx.getExternalContext();

    Map<String, Object> applicationMap = externalContext.getApplicationMap();
    applicationMap.put(TrinidadSkinProvider.TRINDIAD_SKIN_PROVIDER_KEY,
                       new TrinidadSkinProvider());
    applicationMap.put(ExternalSkinProvider.EXTERNAL_SKIN_PROVIDER_KEY,
                       new ExternalSkinProvider());
    applicationMap.put(SkinProvider.SKIN_PROVIDER_INSTANCE_KEY,
                       new SkinProviderRegistry());
    _facesContext = ctx;
    _externalContext = externalContext;
    _provider = SkinProvider.getCurrentInstance(_externalContext);

    if (SkinFactory.getFactory() == null)
      SkinFactory.setFactory(new SkinFactoryImpl());
    RenderKitBootstrap.setFactories(_facesConfigInfo);
  }

  @Override
  protected void tearDown() throws IOException
  {
    MFacesContext.clearContext();
    _facesContext = null;
    _externalContext = null;
    _provider = null;
    RenderKitBootstrap.clearFactories();
  }

  public void testTrinidadBaseSkinsUsingId()
  {
    _testUsingId(TrinidadRenderingConstants.SIMPLE_DESKTOP_ID);
    _testUsingId(TrinidadRenderingConstants.SIMPLE_PORTLET_ID);
    _testUsingId(TrinidadRenderingConstants.SIMPLE_PDA_ID);
    _testUsingId(TrinidadRenderingConstants.MINIMAL_DESKTOP_ID);
    _testUsingId(TrinidadRenderingConstants.MINIMAL_PORTLET_ID);
    _testUsingId(TrinidadRenderingConstants.MINIMAL_PDA_ID);
    _testUsingId(TrinidadRenderingConstants.CASABLANCA_DESKTOP_ID);
    _testUsingId(TrinidadRenderingConstants.CASABLANCA_PORTLET_ID);
    _testUsingId(TrinidadRenderingConstants.CASABLANCA_PDA_ID);
  }

  public void testTrinidadBaseSkinsUsingFamily()
  {
    _testUsingFamily(TrinidadRenderingConstants.SIMPLE_SKIN_FAMILY,
                     TrinidadRenderingConstants.SIMPLE_DESKTOP_ID);
    _testUsingFamily(TrinidadRenderingConstants.MINIMAL_SKIN_FAMILY,
                     TrinidadRenderingConstants.MINIMAL_DESKTOP_ID);
    _testUsingFamily(TrinidadRenderingConstants.CASABLANCA_SKIN_FAMILY,
                     TrinidadRenderingConstants.CASABLANCA_DESKTOP_ID);
  }

  public void testTrinidadBaseSkinsUsingFamilyAndRenderKit()
  {
    _testUsingFamilyAndRenderkit(TrinidadRenderingConstants.SIMPLE_SKIN_FAMILY,
                                 SkinMetadata.RenderKitId.DESKTOP.toString(),
                                 TrinidadRenderingConstants.SIMPLE_DESKTOP_ID);
    _testUsingFamilyAndRenderkit(TrinidadRenderingConstants.SIMPLE_SKIN_FAMILY,
                                 SkinMetadata.RenderKitId.PDA.toString(),
                                 TrinidadRenderingConstants.SIMPLE_PDA_ID);
    _testUsingFamilyAndRenderkit(TrinidadRenderingConstants.SIMPLE_SKIN_FAMILY,
                                 SkinMetadata.RenderKitId.PORTLET.toString(),
                                 TrinidadRenderingConstants.SIMPLE_PORTLET_ID);
    _testUsingFamilyAndRenderkit(TrinidadRenderingConstants.MINIMAL_SKIN_FAMILY,
                                 SkinMetadata.RenderKitId.DESKTOP.toString(),
                                 TrinidadRenderingConstants.MINIMAL_DESKTOP_ID);
    _testUsingFamilyAndRenderkit(TrinidadRenderingConstants.MINIMAL_SKIN_FAMILY,
                                 SkinMetadata.RenderKitId.PDA.toString(),
                                 TrinidadRenderingConstants.MINIMAL_PDA_ID);
    _testUsingFamilyAndRenderkit(TrinidadRenderingConstants.MINIMAL_SKIN_FAMILY,
                                 SkinMetadata.RenderKitId.PORTLET.toString(),
                                 TrinidadRenderingConstants.MINIMAL_PORTLET_ID);
    _testUsingFamilyAndRenderkit(TrinidadRenderingConstants.CASABLANCA_SKIN_FAMILY,
                                 SkinMetadata.RenderKitId.DESKTOP.toString(),
                                 TrinidadRenderingConstants.CASABLANCA_DESKTOP_ID);
    _testUsingFamilyAndRenderkit(TrinidadRenderingConstants.CASABLANCA_SKIN_FAMILY,
                                 SkinMetadata.RenderKitId.PDA.toString(),
                                 TrinidadRenderingConstants.CASABLANCA_PDA_ID);
    _testUsingFamilyAndRenderkit(TrinidadRenderingConstants.CASABLANCA_SKIN_FAMILY,
                                 SkinMetadata.RenderKitId.PORTLET.toString(),
                                 TrinidadRenderingConstants.CASABLANCA_PORTLET_ID);
  }

  public void testNonExistingSkin()
  {
    SkinMetadata skinMetadata = new SkinMetadata.Builder().id("non-existing-skin").build();
    Skin skin = _provider.getSkin(_externalContext, skinMetadata);
    assertTrue(TrinidadRenderingConstants.SIMPLE_DESKTOP_ID.equals(skin.getId()));

    skinMetadata = new SkinMetadata.Builder().family("non-existing-family").build();
    skin = _provider.getSkin(_externalContext, skinMetadata);
    assertTrue(TrinidadRenderingConstants.SIMPLE_DESKTOP_ID.equals(skin.getId()));

    skinMetadata = new SkinMetadata.Builder().id("non-existing-skin")
                                             .renderKitId(SkinMetadata.RenderKitId.PDA)
                                             .build();
    skin = _provider.getSkin(_externalContext, skinMetadata);
    assertTrue(TrinidadRenderingConstants.SIMPLE_PDA_ID.equals(skin.getId()));

    skinMetadata = new SkinMetadata.Builder().family("non-existing-family")
                                             .renderKitId(SkinMetadata.RenderKitId.PORTLET)
                                             .build();
    skin = _provider.getSkin(_externalContext, skinMetadata);
    assertTrue(TrinidadRenderingConstants.SIMPLE_PORTLET_ID.equals(skin.getId()));
  }

  public void testCircularSkin()
  {
    try
    {
      _testUsingId("circular1.desktop");
      fail("Failed to detect circular dependency in trinidad-skins.xml");
    }
    catch (Exception e)
    {
      assertEquals("Unexpected type of exception caught.", "IllegalStateException", e.getClass().getSimpleName());
    }
  }

  public void testXmlConfiguredSkins()
  {
    _testUsingId("empty.desktop");
    _testUsingId("suede.desktop");
    _testUsingId("beach.desktop");

    _testUsingFamily("test", "empty.desktop");
    _testUsingFamilyAndVersion("test", new SkinVersion(null, true), "empty.desktop");
    _testUsingFamilyAndVersion("test", new SkinVersion("v1"), "empty.desktop");
    _testUsingFamilyAndVersion("test", new SkinVersion("v2"), "suede.desktop");
    _testUsingFamilyAndVersion("test", new SkinVersion("v3"), "beach.desktop");

    SkinMetadata skinMetadata = new SkinMetadata.Builder().id("beach.desktop").build();
    Skin beach = _provider.getSkin(_externalContext, skinMetadata);
    assertEquals("Unexpected number of skin additions in beach.desktop.", 1, beach.getSkinAdditions().size());
  }

  public void testSkinMetadataFromProvider()
  {
    Collection<SkinProvider> providers = ((SkinProviderRegistry) _provider).getProviders();

    assertEquals("Unexpected number of providers.", 5, providers.size());


    for (SkinProvider provider : providers)
    {
      if (provider instanceof TrinidadBaseSkinProvider)
        assertEquals("Unexpected number of skins in " + provider, 9, provider.getSkinMetadata(_externalContext).size());
      else if (provider instanceof TrinidadSkinProvider)
        assertEquals("Unexpected number of skins in " + provider, 5, provider.getSkinMetadata(_externalContext).size());
      else if (provider instanceof ExternalSkinProvider)
        assertEquals("Unexpected number of skins in " + provider, 0, provider.getSkinMetadata(_externalContext).size());
      else if (provider instanceof CustomSkinProvider1)
        assertEquals("Unexpected number of skins in " + provider, 3, provider.getSkinMetadata(_externalContext).size());
      else if (provider instanceof CustomSkinProvider2)
        assertEquals("Unexpected number of skins in " + provider, 2, provider.getSkinMetadata(_externalContext).size());
    }

    Collection<SkinMetadata> allSkins = _provider.getSkinMetadata(_externalContext);
    assertEquals("Unexpected number of total skins.", 19, allSkins.size());
  }

  public void testCustomSkinProvider()
  {
    Skin greenSkin = _testUsingId("green.desktop");
    Skin blueSkin = _testUsingId("blue.desktop");
    Skin cyanSkin = _testUsingId("cyan.desktop");
    Skin purpleSkin = _testUsingId("purple.desktop");
    Skin violetSkin = _testUsingId("violet.desktop");
    assertEquals("Unexpected base skin for green skin.", "simple.desktop", greenSkin.getBaseSkin().getId());
    assertEquals("Unexpected base skin for blue skin.", "purple.desktop", blueSkin.getBaseSkin().getId());
    assertEquals("Unexpected base skin for cyan skin.", "beach.desktop", cyanSkin.getBaseSkin().getId());
    assertEquals("Unexpected base skin for cyan skin.", "minimal.desktop", purpleSkin.getBaseSkin().getId());
    assertEquals("Unexpected base skin for cyan skin.", "purple.desktop", violetSkin.getBaseSkin().getId());
    assertEquals("Unexpected purple base skin in violet skin.", purpleSkin, violetSkin.getBaseSkin());
  }

  public void testSkinCache()
  {

    _cleanTempDir();

    // access skin from external provider
    _initRequestContextAndGenerateCss("green");
    _verifyNumberOfSkinInCache(0);
    _verifyNumberOfCssFiles(1);
    _releaseRequestContext();

    // access skin from external provider
    _initRequestContextAndGenerateCss("blue");
    _verifyNumberOfSkinInCache(0);
    _verifyNumberOfCssFiles(2);
    _releaseRequestContext();

    // access internal skin defined in trinidad-skins.xml
    _initRequestContextAndGenerateCss("suede");
    _verifyNumberOfSkinInCache(1);
    _verifyNumberOfCssFiles(3);
    _releaseRequestContext();

    // access skin from external provider
    _initRequestContextAndGenerateCss("cyan");
    _verifyNumberOfSkinInCache(1);
    _verifyNumberOfCssFiles(4);
    _releaseRequestContext();

    // access internal static skin
    _initRequestContextAndGenerateCss("casablanca");
    _verifyNumberOfSkinInCache(2);
    _verifyNumberOfCssFiles(5);
    _releaseRequestContext();

    // access skin from external provider
    _initRequestContextAndGenerateCss("purple");
    _verifyNumberOfSkinInCache(2);
    _verifyNumberOfCssFiles(6);
    _releaseRequestContext();

    // access skin from external provider
    _initRequestContextAndGenerateCss("violet");
    _verifyNumberOfSkinInCache(2);
    _verifyNumberOfCssFiles(7);
    _releaseRequestContext();
  }

  private void _verifyNumberOfCssFiles(int skinNumber)
  {
    assertEquals("Skin file not generated as expected.", skinNumber, _getTempDir().list().length);
  }

  private void _verifyNumberOfSkinInCache(int skinNumber)
  {
    assertEquals("StyleProvider cache size does not match.", skinNumber, _getStyleProviderCacheSize());
  }

  private int _getStyleProviderCacheSize()
  {
    ConcurrentMap<String, Object> appMap = RequestContext.getCurrentInstance().getApplicationScopedConcurrentMap();
    Object cache = appMap.get(_SKIN_PROVIDERS_CACHE_KEY);
    if (cache == null)
      fail("Skin provider cache cannot be null.");
    if (!(cache instanceof Map))
      fail("Skin provider cache should be a map.");
    return ((Map) appMap.get(_SKIN_PROVIDERS_CACHE_KEY)).keySet().size();
  }

  private File _getTempDir()
  {
    Map<String, Object> applicationMap = _externalContext.getApplicationMap();
    String path = ((File) applicationMap.get("javax.servlet.context.tempdir")).getAbsolutePath() + "/adf/styles/cache/";
    File tempDir = new File(path);

    if (!tempDir.exists())
      tempDir.mkdirs();

    return tempDir;
  }

  private void _cleanTempDir()
  {
    File tempDir = _getTempDir();
    System.out.println("Cleaning temp directory: " + tempDir.getAbsolutePath());

    if (tempDir.exists())
      for (File file : tempDir.listFiles())
        file.delete();
  }

  private Skin _testUsingId(String skinId)
  {
    SkinMetadata skinMetadata = new SkinMetadata.Builder().id(skinId).build();
    Skin skin = _provider.getSkin(_externalContext, skinMetadata);
    assertEquals("Unexpected skin id.", skinId, skin.getId());
    return skin;
  }

  private Skin _testUsingFamily(String family,
                                String expectedSkinId)
  {
    SkinMetadata skinMetadata = new SkinMetadata.Builder().family(family).build();
    Skin skin = _provider.getSkin(_externalContext, skinMetadata);
    assertEquals("Unexpected skin id.", expectedSkinId, skin.getId());
    assertEquals("Unexpected skin renderKitId.", SkinMetadata.RenderKitId.DESKTOP.toString(), skin.getRenderKitId());
    return skin;
  }

  private Skin _testUsingFamilyAndRenderkit(String family,
                                            String renderkitId,
                                            String expectedSkinId)
  {
    SkinMetadata skinMetadata = new SkinMetadata.Builder()
        .family(family)
        .renderKitId(SkinMetadata.RenderKitId.fromId(renderkitId))
        .build();
    Skin skin = _provider.getSkin(_externalContext, skinMetadata);
    assertEquals("Unexpected skin id.", expectedSkinId, skin.getId());
    assertEquals("Unexpected skin renderKitId.", renderkitId, skin.getRenderKitId());
    return skin;
  }

  private Skin _testUsingFamilyAndVersion(String family,
                                          SkinVersion version,
                                          String expectedSkinId)
  {
    SkinMetadata skinMetadata = new SkinMetadata.Builder()
        .family(family)
        .version(version)
        .build();
    Skin skin = _provider.getSkin(_externalContext, skinMetadata);
    assertEquals("Unexpected skin id.", expectedSkinId, skin.getId());
    assertEquals("Unexpected skin renderKitId.", SkinMetadata.RenderKitId.DESKTOP.toString(), skin.getRenderKitId());
    return skin;
  }

  /**
   * This method sets skinFamily into request context and executes
   * encode of a simple page. This results in generation of the css
   * for the skin family specified.
   * This code is similar to what we do in RenderKitTestCase.BaseTest
   * @see org.apache.myfaces.trinidadinternal.renderkit.RenderKitTestCase.BaseTest
   * @param skinFamily
   */
  private void _initRequestContextAndGenerateCss(String skinFamily)
  {
    try
    {
      _requestContext = new MRequestContext();
      _requestContext.setSkinFamily(skinFamily);
      _requestContext.setRightToLeft(false);
      _requestContext.setAgent(RenderKitBootstrap.getGeckoAgent());

      UIViewRoot root = RenderKitBootstrap.createUIViewRoot(_facesContext);
      root.setRenderKitId("org.apache.myfaces.trinidad.core");
      _facesContext.setViewRoot(root);
      _facesContext.setResponseWriter(new TestResponseWriter(new NullWriter(),
                                                             XhtmlResponseWriter.XHTML_CONTENT_TYPE,
                                                             "UTF-8",
                                                             this,
                                                             null));
      CoreDocument doc = new CoreDocument();
      doc.setId("docId");
      root.getChildren().add(doc);
      CoreForm form = new CoreForm();
      form.setId("formId");
      doc.getChildren().add(form);

      ExtendedRenderKitService service = Service.getService(_facesContext.getRenderKit(),
                                                            ExtendedRenderKitService.class);
      if (service != null)
        service.encodeBegin(_facesContext);

      RenderUtils.encodeRecursive(_facesContext, root);
    }
    catch (Exception e)
    {
      System.out.println("Exception in _initRequestContextAndGenerateCss");
      e.printStackTrace();
    }
  }

  private void _releaseRequestContext()
  {
    _requestContext.release();
    _requestContext = null;
  }

  private        MRequestContext      _requestContext;
  private        MFacesContext        _facesContext;
  private        SkinProvider         _provider;
  private        ExternalContext      _externalContext;

  private static FacesConfigInfo      _facesConfigInfo;

  private static final String _SKIN_PROVIDERS_CACHE_KEY = "org.apache.myfaces.trinidadinternal.skin.SKIN_PROVIDERS_KEY";

  static
  {
    try
    {
      RenderKitBootstrap bootstrap = new RenderKitBootstrap();
      bootstrap.init();
      _facesConfigInfo = bootstrap.getFacesConfigInfo();
    }
    catch (Exception e)
    {
    }
  }
}
