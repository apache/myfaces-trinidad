/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidadinternal.renderkit;

import java.io.IOException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;

import junit.framework.Test;

import org.apache.myfaces.trinidad.component.core.CoreDocument;
import org.apache.myfaces.trinidad.component.core.CoreForm;
import org.apache.myfaces.trinidad.component.html.HtmlHtml;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.render.RenderUtils;

import org.apache.myfaces.trinidadinternal.renderkit.RenderKitTestCase.RendererTest;
import org.apache.myfaces.trinidadinternal.renderkit.RenderKitTestCase.SuiteDefinition;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;

import org.xml.sax.SAXException;


public class CoreRenderKitTest extends RenderKitTestCase
{
  public static Test suite() throws Throwable
  {
    CoreRenderKitTest suite =
      new CoreRenderKitTest(CoreRenderKitTest.class.getName());
    return suite;
  }

  /**
   * To limit the set of allowed SuiteDefinition variants that the tests are run against,
   * pass the parameter:<code>definitions=<i><comma-delimited list of defintions><i></code>, such
   * as <code>definitions=minimal,minimalScrRdr</code>
   * To limit the set test scripts run, pass the parameter
   * <code>tests=<i><comma-delimited list of test script names without .xml extension><i></code>,
   * such as <code>tests=inputText,media</code>
   * @param args
   * @throws Throwable
   */
  public static void main(String[] args) throws Throwable
  {    
    _parseAllowedDefinitions(args);
    _parseAllowedTests(args);
    
    junit.textui.TestRunner.run(suite());
  }

  public CoreRenderKitTest(String testName) throws IOException, SAXException
  {
    super(testName);
  }

  @Override
  protected List<SuiteDefinition> getSuiteDefinitions()
  {
    if (_definitions == null)
    {
      _definitions = _filterDefinitions();
    }
    
    return _definitions;
  }

  @Override
  protected String getRenderKitId()
  {
    return "org.apache.myfaces.trinidad.core";
  }

  @SuppressWarnings("unchecked")
  @Override
  protected UIComponent populateDefaultComponentTree(
    UIViewRoot  root,
    TestScript  script)
  {
    String componentType =
           script.getDefinition().getComponentInfo().componentType;

    if ("org.apache.myfaces.trinidad.HtmlHtml".equals(componentType))
    {
      return root;
    }

    if (_HTML_COMPONENTS.contains(componentType))
    {
      HtmlHtml html = new HtmlHtml();
      html.setId("htmlId");
      root.getChildren().add(html);
      return html;
    }
    else
    {
      CoreDocument doc = new CoreDocument();
      doc.setId("docId");
      root.getChildren().add(doc);
      CoreForm form = new CoreForm();
      form.setId("formId");
      if (script.getDefinition().isUsesUpload())
        form.setUsesUpload(true);
      doc.getChildren().add(form);
      return form;
    }
  }

  /**
   * Override to filter which component tests to add
   * @param name
   * @param definition
   * @param lenient
   * @throws IOException
   * @throws SAXException
   */
  @Override
  protected void addRendererTest(
    String name,
    SuiteDefinition definition,
    boolean lenient) throws IOException, SAXException
  {
    if (_ALLOWED_TEST_NAMES.isEmpty() || _ALLOWED_TEST_NAMES.contains(name))
    {
      super.addRendererTest(name, definition, lenient);
    }
    else
    {
      _LOG.info("Suppress running RenderKitTest:" + name + " " + definition);
    }
  }
  
  private static List<String> _parseListParameter(String[] args, String parameterPrefix)
  {    
    for (String arg : args)
    {
      if (arg.startsWith(parameterPrefix))
      {
        String definitionsString = arg.substring(parameterPrefix.length());
        return Arrays.asList(definitionsString.split(","));
      }
    }
    
    return Collections.emptyList();
  }
   
  private static void _parseAllowedDefinitions(String[] args)
  {
    _ALLOWED_DEFINITIONS_CATEGORIES.addAll(_parseListParameter(args, "definitions="));
  }

  private static void _parseAllowedTests(String[] args)
  {
    _ALLOWED_TEST_NAMES.addAll(_parseListParameter(args, "tests="));
  }

  private static SuiteDefinition _getSuitedDefinitionByCategory(
    Iterable<SuiteDefinition> suitedDefinitions, String category)
  {
    for (SuiteDefinition def : suitedDefinitions)
    {
      if (category.equals(def.getCategory()))
      {
        return def;
      }
    }
    
    return null;
  }
  
  private static List<SuiteDefinition> _filterDefinitions()
  {    
    if (_ALLOWED_DEFINITIONS_CATEGORIES.isEmpty())
    {
      return _DEFINITIONS;
    }
    else
    {
      ArrayList<SuiteDefinition> definitions = new ArrayList<SuiteDefinition>();
      
      for (String category : _ALLOWED_DEFINITIONS_CATEGORIES)
      {
        SuiteDefinition allowedDefinition = _getSuitedDefinitionByCategory(_DEFINITIONS, category);
        
        if (allowedDefinition != null)
        {
          definitions.add(allowedDefinition);
        }
        else
        {
          _LOG.warning("Unabled to find test category named:" + category);
        }
      }

      definitions.trimToSize();
      return Collections.unmodifiableList(definitions);
    }
  }


  private static final Set<String> _ALLOWED_TEST_NAMES = new HashSet<String>();
  private static final Set<String> _ALLOWED_DEFINITIONS_CATEGORIES = new HashSet<String>();

  private static final List<SuiteDefinition> _DEFINITIONS;
  private static final Set<String> _HTML_COMPONENTS;

  private static final Logger _LOG = Logger.getLogger(CoreRenderKitTest.class.getName());
  
  private List<SuiteDefinition> _definitions;

  static
  {
    // Force the CoreRenderKit logger level to SEVERE, to bypass the
    // warnings about not finding the Basic HTML RenderKit.
    Logger logger = Logger.getLogger(CoreRenderKit.class.getName());
    logger.setLevel(Level.SEVERE);
    logger.setUseParentHandlers(false);
    
    // Force the RenderUtils logger level to SEVERE to bypass the
    // warnings in getRelativeId method when the component
    // with the relativeId could not be found which is the case in our
    // render kit rendering tests.
    Logger loggerTwo = Logger.getLogger(RenderUtils.class.getName());
    loggerTwo.setLevel(Level.SEVERE);
    loggerTwo.setUseParentHandlers(false);

    // order is apparently important
    ArrayList<SuiteDefinition> definitions = new ArrayList<SuiteDefinition>();
    
    definitions.add(new SuiteDefinition("minimal",
                                        "minimal",
                                        null,
                                        RenderKitBootstrap.getGeckoAgent(),
                                        false));
    definitions.add(new SuiteDefinition("minimalIE",
                                        "minimal",
                                        null,
                                        RenderKitBootstrap.getIEAgent(),
                                        false));
    definitions.add(new SuiteDefinition("minimalIERtl",
                                        "minimal",
                                        null,
                                        RenderKitBootstrap.getIEAgent(),
                                        true));
    definitions.add(new SuiteDefinition("minimalPPC",
                                        "minimal",
                                        null,
                                        RenderKitBootstrap.getPocketPCAgent(),
                                        false));
    definitions.add(new SuiteDefinition("minimalSaf",
                                        "minimal",
                                        null,
                                        RenderKitBootstrap.getSafariAgent(),
                                        false));
    definitions.add(new SuiteDefinition("minimalScrRdr",
                                        "minimal",
                                        RequestContext.Accessibility.SCREEN_READER,
                                        RenderKitBootstrap.getGeckoAgent(),
                                        false));
    definitions.add(new SuiteDefinition("minimalInacc",
                                        "minimal",
                                        RequestContext.Accessibility.INACCESSIBLE,
                                        RenderKitBootstrap.getGeckoAgent(),
                                        false));

    definitions.trimToSize();
    _DEFINITIONS = Collections.unmodifiableList(definitions);
   
    String[] htmlComponents = {"org.apache.myfaces.trinidad.HtmlBody",
                               "org.apache.myfaces.trinidad.HtmlFrame",
                               "org.apache.myfaces.trinidad.HtmlFrameBorderLayout",
                               "org.apache.myfaces.trinidad.HtmlHead",
                               "org.apache.myfaces.trinidad.CoreStyleSheet"};
    
    _HTML_COMPONENTS = Collections.unmodifiableSet(new HashSet<String>(Arrays.asList(htmlComponents)));
  }
}
