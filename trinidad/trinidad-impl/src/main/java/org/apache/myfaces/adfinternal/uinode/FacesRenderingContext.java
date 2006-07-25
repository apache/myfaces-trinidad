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
package org.apache.myfaces.adfinternal.uinode;

import java.io.IOException;

import java.util.Map;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.adf.logging.ADFLogger;
import org.apache.myfaces.adf.context.AdfFacesContext;

import org.apache.myfaces.adfinternal.renderkit.core.ppr.PartialPageContext;

import org.apache.myfaces.adfinternal.agent.AdfFacesAgent;
import org.apache.myfaces.adfinternal.share.config.ContextBasedConfiguration;
import org.apache.myfaces.adfinternal.share.nls.LocaleContext;
import org.apache.myfaces.adfinternal.share.xml.XMLUtils;
import org.apache.myfaces.adfinternal.ui.RenderingContext;
import org.apache.myfaces.adfinternal.ui.RootRenderingContext;
import org.apache.myfaces.adfinternal.ui.UIConstants;
import org.apache.myfaces.adfinternal.skin.Skin;
import org.apache.myfaces.adfinternal.style.StyleContext;

import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;

/**
 * RenderingContext implementation that supports JSF.
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/uinode/FacesRenderingContext.java#0 $) $Date: 10-nov-2005.18:49:15 $
 * @author The Oracle ADF Faces Team
 */
public class FacesRenderingContext extends RootRenderingContext
{
  /**
   * Gets the current RenderingContext.
   * @todo Rename to getCurrentInstance()
   * @todo Why are passing a UIComponent?  In some places,
   *   we're passing null for the component!
   */
  static public RenderingContext getRenderingContext(
    FacesContext fContext,
    UIComponent  component) throws IOException
  {
    return getRenderingContext(fContext, component, true);
  }

  /**
   * Gets the current RenderingContext.
   * @todo Rename to getCurrentInstance()
   * @todo Why are passing a UIComponent?  In some places,
   *   we're passing null for the component!
   */
  static public RenderingContext getRenderingContext(
    FacesContext fContext,
    UIComponent  component,
    boolean      createIfNull) throws IOException
  {
    return UINodeRendererBase.getRenderingContext(fContext,
                                                  component,
                                                  createIfNull);
  }


  static public FacesRenderingContext createRenderingContext(
    FacesContext fContext) throws IOException
  {
    if (UINodeRendererBase.__getRenderingContext(fContext) != null)
      throw new IllegalStateException("RenderingContext has already been " +
                                      "created!");

    FacesRenderingContext rContext = new FacesRenderingContext(fContext);

    UINodeRendererBase.__setRenderingContext(fContext, rContext);

    return rContext;
  }


  /**
   * Gets the current PartialPageContext.
   */
  static public PartialPageContext getPartialPageContext(FacesContext fContext)
  {
    return (PartialPageContext)
      AdfRenderingContext.getCurrentInstance().getPartialPageContext();
  }

  /**
   * Creates a FacesRenderingContext.
   * @param fContext the context
   */
  private FacesRenderingContext(FacesContext fContext)
  {
    super();

    _adfRenderingContext = AdfRenderingContext.getCurrentInstance();

    init(fContext);

    AdfFacesContext context = AdfFacesContext.getCurrentInstance();
    String outputMode = context.getOutputMode();
    if (outputMode != null)
      setFacet(outputMode);

    _initializeConfiguration(fContext, context);

    _initializePPR(fContext);
  }

  public PartialPageContext getPartialPageContext()
  {
    return _adfRenderingContext.getPartialPageContext();
  }

  public LocaleContext getLocaleContext()
  {
    return _adfRenderingContext.getLocaleContext();
  }


  /**
   * Get an interface that can be used for style lookups and generation.
   */
  public StyleContext getStyleContext()
  {
    return _adfRenderingContext.getStyleContext();
  }

  /**
   * Returns the Skin to use for this render.
   */
  public Skin getSkin()
  {
    return _adfRenderingContext.getSkin();
  }

  public AdfFacesAgent getAgent()
  {
    return _adfRenderingContext.getAgent();
  }

  protected Object getRenderingProperty(Object key)
  {
    return _adfRenderingContext.getProperties().get(key);
  }

  protected void setRenderingProperty(Object key, Object value)
  {
    _adfRenderingContext.getProperties().put(key, value);
  }
  /**
   * Store a Map that maps a skin's resource keys from one key to another.
   * For example, if the renderer uses a new HideShowBean, it will need
   * to map the HideShowBean's keys to its keys. It can store the map
   * here, so that context.getTranslatedValue(key) can use this map to get
   * the correct translated value key.
   * @param mapping
   */
  public void setSkinResourceKeyMap(Map mapping)
  {
    _adfRenderingContext.setSkinResourceKeyMap(mapping);
  }


  /**
   * Get the _skinResourceKeyMap Map.
   * @param mapping
   */
  public Map getSkinResourceKeyMap()
  {
    return _adfRenderingContext.getSkinResourceKeyMap();
  }


  private void _initializeConfiguration(FacesContext fContext,
                                        AdfFacesContext context)
  {
    setConfiguration(new ContextBasedConfiguration(fContext, context));
  }

  //
  // Initialize PPR, if needed
  //
  private void _initializePPR(
    FacesContext    fContext)
  {
    PartialPageContext pprContext =
      _adfRenderingContext.getPartialPageContext();
    if (pprContext != null)
    {
      // For compatibility with our current renderers, look for
      // the PARTIAL_TARGETS parameter, and add any that are found
      Map parameters = fContext.getExternalContext().
                            getRequestParameterMap();
      String param = (String) parameters.get(
                                   UIConstants.PARTIAL_TARGETS_PARAM);
      if ((null != param) && !"".equals(param))
      {
        _LOG.finer("Adding partial targets from parameter: {0}", param);
        // Parse the parameter value to a String[]
        String[] partialTargets = XMLUtils.parseNameTokens(param);
        for (int i = 0; i < partialTargets.length; i++)
          pprContext.addPartialTarget(partialTargets[i]);
      }
    }
  }

  private AdfRenderingContext _adfRenderingContext;

  private static final ADFLogger _LOG =
    ADFLogger.createADFLogger(FacesRenderingContext.class);

}
