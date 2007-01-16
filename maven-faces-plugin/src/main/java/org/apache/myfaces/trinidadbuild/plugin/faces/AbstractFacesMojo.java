/*
*            Licensed to the Apache Software Foundation (ASF) under one
*            or more contributor license agreements.  See the NOTICE file
*            distributed with this work for additional information
*            regarding copyright ownership.  The ASF licenses this file
*            to you under the Apache License, Version 2.0 (the
*            "License"); you may not use this file except in compliance
*            with the License.  You may obtain a copy of the License at
* 
*              http://www.apache.org/licenses/LICENSE-2.0
* 
*            Unless required by applicable law or agreed to in writing,
*            software distributed under the License is distributed on an
*            "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
*            KIND, either express or implied.  See the License for the
*            specific language governing permissions and limitations
*            under the License.
*/
package org.apache.myfaces.trinidadbuild.plugin.faces;

import java.io.File;
import java.io.IOException;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;

import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParserFactory;


import org.apache.myfaces.trinidadbuild.plugin.faces.io.PrettyWriter;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.AttributeBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.ComponentBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.ConverterBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.FacesConfigBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.FacesConfigParser;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.PropertyBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.ValidatorBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.AttributeFilter;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.ComponentFilter;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.ConverterFilter;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.Filter;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.FilteredIterator;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.PropertyFilter;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.Util;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.ValidatorFilter;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.XIncludeFilter;
import org.apache.commons.digester.AbstractObjectCreationFactory;
import org.apache.commons.digester.Digester;

import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.model.Resource;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;

import org.codehaus.plexus.util.FileUtils;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

abstract public class AbstractFacesMojo extends AbstractMojo
{
  protected List getCompileDependencyResources(
    MavenProject project,
    String       resourcePath) throws MojoExecutionException
  {
    try
    {
      ClassLoader cl = createCompileClassLoader(project);
      Enumeration e = cl.getResources(resourcePath);
      List urls = new ArrayList();
      while (e.hasMoreElements())
      {
		URL url = (URL)e.nextElement();
        urls.add(url);
      }
      return Collections.unmodifiableList(urls);
    }
    catch (IOException e)
    {
      throw new MojoExecutionException("Unable to get resources for path " +
                                       "\"" + resourcePath + "\"", e);
    }

  }

  protected void addResourceRoot(
    MavenProject project,
    String       resourceRoot)
  {
    List resources = project.getBuild().getResources();
    Resource resource = new Resource();
    resource.setDirectory(resourceRoot);
    resources.add(resource);
  }

  /**
   * @deprecated
   */
  protected URL[] readIndex(
    MavenProject project,
    String       resourcePath) throws MojoExecutionException
  {
    return readIndex(project);
  }

  protected List getMasterConfigs(
    MavenProject project) throws MojoExecutionException
  {
    String resourcePath = "META-INF/maven-faces-plugin/faces-config.xml";
    return getCompileDependencyResources(project, resourcePath);
  }

  protected URL[] readIndex(
    MavenProject project) throws MojoExecutionException
  {
    try
    {
      // 1. read master faces-config.xml resources
      List masters = getMasterConfigs(project);
      if (masters.isEmpty())
      {
        getLog().warn("Master faces-config.xml not found");
        return new URL[0];
      }
      else
      {
        List entries = new LinkedList();

        SAXParserFactory spf = SAXParserFactory.newInstance();
        spf.setNamespaceAware(true);
        // requires JAXP 1.3, in JavaSE 5.0
        // spf.setXIncludeAware(false);

        for (Iterator i=masters.iterator(); i.hasNext();)
        {
          URL url = (URL)i.next();
          Digester digester = new Digester(spf.newSAXParser());
          digester.setNamespaceAware(true);

          // XInclude
          digester.setRuleNamespaceURI(XIncludeFilter.XINCLUDE_NAMESPACE);
          digester.addCallMethod("faces-config/include", "add", 1);
          digester.addFactoryCreate("faces-config/include",
                                    URLCreationFactory.class);
          digester.addCallParam("faces-config/include", 0, 0);

          digester.push(url);
          digester.push(entries);
          digester.parse(url.openStream());
        }

        return (URL[])entries.toArray(new URL[0]);
      }
    }
    catch (ParserConfigurationException e)
    {
      throw new MojoExecutionException("Failed to parse master config", e);
    }
    catch (SAXException e)
    {
      throw new MojoExecutionException("Failed to parse master config", e);
    }
    catch (IOException e)
    {
      throw new MojoExecutionException("Failed to parse master config", e);
    }
  }

  protected boolean isModifiedSince(
    URL[] urls,
    long  lastModified) throws IOException
  {
    for (int i=0; i < urls.length; i++)
    {
      URLConnection conn = urls[i].openConnection();
      if (conn.getLastModified() > lastModified)
        return true;
    }

    return false;
  }

  protected String convertStringToLiteral(String value)
  {
    return convertStringToLiteral("String", value);
  }

  protected String convertStringToLiteral(String className, String value)
  {
    if (value == null)
    {
      return null;
    }
    else if ("String".equals(className))
    {
      return "\"" + value.replaceAll("\'", "\\'") + "\"";
    }
    else
    {
      return value;
    }
  }

  static public class URLCreationFactory extends AbstractObjectCreationFactory
  {
    public Object createObject(
      Attributes attributes) throws MalformedURLException
    {
      String href = attributes.getValue("href");
      if (href == null)
        throw new IllegalStateException("Missing href attribute");

      URL master = (URL)digester.getRoot();
      return new URL(master, href);
    }
  }

  protected void processIndex(
    MavenProject project,
    String       resourcePath) throws MojoExecutionException
  {
    _facesConfig = new FacesConfigBean();

    URL[] index = readIndex(project, resourcePath);
    for (int i=0; i < index.length; i++)
    {
      processIndexEntry(index[i]);
    }
  }

  protected void processIndexEntry(
    URL entry) throws MojoExecutionException
  {
    URL old = _facesConfig.setCurrentResource(entry);
    try
    {
      new FacesConfigParser().merge(_facesConfig, entry);
    }
    finally
    {
      _facesConfig.setCurrentResource(old);
    }
  }

  protected FacesConfigBean getFacesConfig()
  {
    return _facesConfig;
  }

  protected void writePreamble(
    PrettyWriter out)
  {
    out.write(_AUTO_GENERATE_WARNING);
    out.write(_COPYRIGHT);
  }

  protected void copyFile(
    File   sourceDirectory,
    String sourcePath,
    File   targetDirectory) throws MojoExecutionException
  {
    try
    {
      File sourceFile = new File(sourceDirectory, sourcePath);
      File targetFile = new File(targetDirectory, sourcePath);

      if (sourceFile.exists() &&
          sourceFile.lastModified() > targetFile.lastModified())
      {
        if (getLog().isDebugEnabled())
          getLog().debug("Copying file \"" + sourcePath + "\"");

        FileUtils.copyFile(sourceFile, targetFile);
      }
    }
    catch (IOException e)
    {
      throw new MojoExecutionException("Error copying file \"" +
                                       sourcePath + "\"", e);
    }
  }

  protected void writeImports(
    PrettyWriter out,
    String       packageName,
    Set          imports)
  {
    Iterator iterator = imports.iterator();
    iterator = new FilteredIterator(iterator,
                                    new PackageImportsFilter(packageName));
    while (iterator.hasNext())
    {
      String className = (String)iterator.next();
      out.println("import " + className + ";");
    }

    out.println();
  }

  protected String convertMultilineComment(
    String commentBody)
  {
    return commentBody.replaceAll("\n", "\n * ");
  }

  private ClassLoader createCompileClassLoader(
    MavenProject project) throws MojoExecutionException
  {
    Thread current = Thread.currentThread();
    ClassLoader cl = current.getContextClassLoader();

    try
    {
      List classpathElements = project.getCompileClasspathElements();
      if (!classpathElements.isEmpty())
      {
        String[] entries = (String[])classpathElements.toArray(new String[0]);
        URL[] urls = new URL[entries.length];
        for (int i=0; i < urls.length; i++)
        {
          urls[i] = new File(entries[i]).toURL();
        }
        cl = new URLClassLoader(urls, cl);
      }
    }
    catch (DependencyResolutionRequiredException e)
    {
      throw new MojoExecutionException("Error calculating scope classpath", e);
    }
    catch (MalformedURLException e)
    {
      throw new MojoExecutionException("Error calculating scope classpath", e);
    }

    return cl;
  }

  static protected class SkipFilter extends ComponentFilter
  {
    protected boolean accept(
      ComponentBean component)
    {
      String componentType = component.getComponentType();

      // always skip API and base class generation
      return (!componentType.startsWith("javax") &&
              !componentType.endsWith("Base"));
    }
  }

  static final protected class ComponentTypeFilter extends ComponentFilter
  {
    public ComponentTypeFilter(
      String typePrefix)
    {
      _typePrefix = typePrefix;
    }

    protected boolean accept(
      ComponentBean component)
    {
      String componentType = component.getComponentType();
      return (componentType.startsWith(_typePrefix));
    }
     private final String _typePrefix;
  }

  static final protected class ComponentClassFilter extends ComponentFilter
  {
    public ComponentClassFilter(
      String packageContains)
    {
      _packageContains = packageContains;
    }

    protected boolean accept(
      ComponentBean component)
    {
      String componentClass = component.getComponentClass();
      String packageName = Util.getPackageFromFullClass(componentClass);

      // accept only classes that match the package
      return (packageName.contains(_packageContains));
    }

    private final String _packageContains;
  }

  static final protected class ComponentTagClassFilter extends ComponentFilter
  {
    public ComponentTagClassFilter(
      String packageContains)
    {
      _packageContains = packageContains;
    }

    protected boolean accept(
      ComponentBean component)
    {
      String tagClass = component.getTagClass();
      String packageName = Util.getPackageFromFullClass(tagClass);

      // accept only classes in this project's (sub)package
      return (packageName.contains(_packageContains));
    }

    private final String _packageContains;
  }

  static final protected class ConverterTagClassFilter extends ConverterFilter
  {
    public ConverterTagClassFilter(
      String packageContains)
    {
      _packageContains = packageContains;
    }

    protected boolean accept(
      ConverterBean converter)
    {
      String tagClass = converter.getTagClass();
      String packageName = Util.getPackageFromFullClass(tagClass);

      // accept only classes that contain the desired string
      return (packageName.contains(_packageContains));
    }

    private final String _packageContains;
  }

  static final protected class ValidatorTagClassFilter extends ValidatorFilter
  {
    public ValidatorTagClassFilter(
      String packageContains)
    {
      _packageContains = packageContains;
    }

    protected boolean accept(
      ValidatorBean validator)
    {
      String tagClass = validator.getTagClass();
      String packageName = Util.getPackageFromFullClass(tagClass);

      // accept only classes that contain the desired string
      return (packageName.contains(_packageContains));
    }

    private final String _packageContains;
  }

  static final protected class TagAttributeFilter extends PropertyFilter
  {
    protected boolean accept(
      PropertyBean property)
    {
      return (!property.isTagAttributeExcluded());
    }
  }

  static final protected class ComponentTagFilter extends ComponentFilter
  {
    protected boolean accept(
      ComponentBean component)
    {
      return (component.getTagClass() != null);
    }
  }

  static final protected class ConverterTagFilter extends ConverterFilter
  {
    protected boolean accept(
      ConverterBean converter)
    {
      return (converter.getTagClass() != null);
    }
  }

  static final protected class ValidatorTagFilter extends ValidatorFilter
  {
    protected boolean accept(
      ValidatorBean validator)
    {
      return (validator.getTagClass() != null);
    }
  }

  static final protected class ComponentTagLibraryFilter extends ComponentFilter
  {
    public ComponentTagLibraryFilter(
      String namespaceURI)
    {
      _namespaceURI = namespaceURI;
    }

    protected boolean accept(
      ComponentBean component)
    {
      QName tagName = component.getTagName();
      String tagClass = component.getTagClass();

      // accept if tagClass is present
      // and if tagName is in the desired namespaceURI
      return (tagClass != null && tagName != null &&
              _namespaceURI.equals(tagName.getNamespaceURI()));
    }

    private final String _namespaceURI;
  }

  static final protected class ValidatorTagLibraryFilter extends ValidatorFilter
  {
    public ValidatorTagLibraryFilter(
      String namespaceURI)
    {
      _namespaceURI = namespaceURI;
    }

    protected boolean accept(
      ValidatorBean validator)
    {
      QName tagName = validator.getTagName();
      String tagClass = validator.getTagClass();

      // accept if tagClass is present
      // and if tagName is in the desired namespaceURI
      return (tagClass != null && tagName != null &&
              _namespaceURI.equals(tagName.getNamespaceURI()));
    }

    private final String _namespaceURI;
  }

  static final protected class ConverterTagLibraryFilter extends ConverterFilter
  {
    public ConverterTagLibraryFilter(
      String namespaceURI)
    {
      _namespaceURI = namespaceURI;
    }

    protected boolean accept(
      ConverterBean converter)
    {
      QName tagName = converter.getTagName();
      String tagClass = converter.getTagClass();

      // accept if tagClass is present
      // and if tagName is in the desired namespaceURI
      return (tagClass != null && tagName != null &&
              _namespaceURI.equals(tagName.getNamespaceURI()));
    }

    private final String _namespaceURI;
  }

  static protected class VirtualAttributeFilter extends AttributeFilter
  {
    protected boolean accept(
      AttributeBean attribute)
    {
      return !attribute.isVirtual();
    }
  }

  static private class PackageImportsFilter implements Filter
  {
    public PackageImportsFilter(
      String packageName)
    {
      _packageName = packageName;
    }

    public boolean accept(
      Object object)
    {
      String className = (String)object;
      String packageName = Util.getPackageFromFullClass(className);
      return (!packageName.equals(_packageName) &&
              !packageName.equals("java.lang"));
    }

    private final String _packageName;
  }

  private FacesConfigBean _facesConfig;

  static final private String _AUTO_GENERATE_WARNING =
"// WARNING: This file was automatically generated. Do not edit it directly,\n"+
"//          or you will lose your changes.\n\n";

  static private final String _COPYRIGHT =
"/*\n"                                                                         +
"** (TBD - insert proper license in generated code).\n"             +
"*/\n";
}
