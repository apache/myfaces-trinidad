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
package org.apache.myfaces.adfbuild.plugin.faces;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.apache.myfaces.adfbuild.plugin.faces.util.XIncludeFilter;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

/**
 * @version $Id$
 * @requiresDependencyResolution compile
 * @goal generate-faces-config
 * @phase generate-resources
 */
public class GenerateFacesConfigMojo extends AbstractFacesMojo
{
  /**
   * Execute the Mojo.
   */
  public void execute() throws MojoExecutionException
  {
    try
    {
      // always add resources directory to project resource root
      addResourceRoot(project, targetDirectory.getCanonicalPath());

      File targetFile = new File(targetDirectory, targetPath);

      URL[] index = readIndex(project);

      if (!force && !isModifiedSince(index, targetFile.lastModified()))
      {
        getLog().info("Nothing to generate - " + targetPath + " is up to date");
      }
      else
      {
        List configURLs = getMasterConfigs(project);

        File configFile = new File(configDirectory, configPath);
        if (configFile.exists())
        {
          configURLs = new LinkedList(configURLs);
          // base config goes first
          configURLs.add(0, configFile.toURL());
        }

        if (!configURLs.isEmpty())
        {
          URL baseURL = GenerateFacesConfigMojo.class.getResource("resources/faces-config.xml");
          DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
          DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
          Document document = docBuilder.parse(baseURL.openStream());
          Element root = document.getDocumentElement();

          // embed each <xi:include> element
          for (Iterator i = configURLs.iterator(); i.hasNext(); )
          {
            URL url = (URL)i.next();
            Element include = document.createElementNS("http://www.w3.org/2001/XInclude",
                                                       "xi:include");
            include.setAttribute("href", url.toExternalForm());
            include.setAttribute("xpointer", "/faces-config/*");
            root.insertBefore(include, root.getFirstChild());
          }

          // Note: use a direct DOM->SAX reader
          //       rather than transforming to string
          TransformerFactory transFactory = TransformerFactory.newInstance();
          Transformer identity = transFactory.newTransformer();
          ByteArrayOutputStream out = new ByteArrayOutputStream();
          identity.transform(new DOMSource(document), new StreamResult(out));

          InputStream mergedStream = new ByteArrayInputStream(out.toByteArray());
          // expand all the xi:include elements
          SAXParserFactory saxFactory = SAXParserFactory.newInstance();
          saxFactory.setNamespaceAware(true);
          saxFactory.setValidating(false);
          SAXParser saxParser = saxFactory.newSAXParser();
          XMLReader mergedReader = saxParser.getXMLReader();
          mergedReader = new XIncludeFilter(mergedReader, baseURL);
          mergedReader.setEntityResolver(new EntityResolver()
            {
              public InputSource resolveEntity(
                String publicId,
                String systemId)
              {
                return new InputSource(new ByteArrayInputStream(new byte[0]));
              }
            });
          InputSource mergedInput = new InputSource(mergedStream);
          Source mergedSource = new SAXSource(mergedReader, mergedInput);

          // Transform the combined faces-config.xml file to resolve
          // component-supertype, inject the DTD and ensure that
          // metadata is represented compatibly with JSF 1.1 syntax.
          targetFile.delete();
          targetFile.getParentFile().mkdirs();
          Result mergedResult = new StreamResult(new FileOutputStream(targetFile));

          URL xslURL = getClass().getResource("resources/transform.xsl");
          InputStream xsl = xslURL.openStream();
          StreamSource xslSource = new StreamSource(xsl);
          Transformer transformer = transFactory.newTransformer(xslSource);
          transformer.setParameter("packageContains", packageContains);
          transformer.setParameter("typePrefix", typePrefix);
          transformer.transform(mergedSource, mergedResult);

          targetFile.setReadOnly();

          getLog().info("Generated " + targetPath);
        }
      }
    }
    catch (SAXException e)
    {
      throw new MojoExecutionException("Error during generation", e);
    }
    catch (TransformerException e)
    {
      throw new MojoExecutionException("Error during generation", e);
    }
    catch (ParserConfigurationException e)
    {
      throw new MojoExecutionException("Error during generation", e);
    }
    catch (IOException e)
    {
      throw new MojoExecutionException("Error during generation", e);
    }
  }

  /**
   * @parameter expression="${project}"
   * @readonly
   */
  private MavenProject project;

  /**
   * @parameter expression="src/main/conf"
   * @required
   */
  private File configDirectory;

  /**
   * @parameter
   */
  private String configPath = "META-INF/faces-config-base.xml";

  /**
   * @parameter
   */
  private String packageContains = "";

  /**
   * @parameter expression="${project.build.directory}/maven-faces-plugin/main/resources"
   * @required
   */
  private File targetDirectory;

  /**
   * @parameter
   */
  private String targetPath = "META-INF/faces-config.xml";

  /**
   * @parameter
   */
  private boolean force;

  /**
   * @parameter
   * @required
   */
  private String typePrefix;
}
