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
package org.apache.myfaces.trinidadbuild.plugin.faces;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;

import java.lang.reflect.Modifier;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.myfaces.trinidadbuild.plugin.faces.io.PrettyWriter;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.ComponentBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.ConverterBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.FacesConfigBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.MethodSignatureBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.PropertyBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.ValidatorBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.ComponentFilter;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.ConverterFilter;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.FilteredIterator;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.Util;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.ValidatorFilter;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.XIncludeFilter;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;

import org.codehaus.plexus.util.FileUtils;

import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

/**
 * @version $Id$
 * @requiresDependencyResolution compile
 * @goal generate-jsp-taglibs
 * @phase generate-sources
 */
public class GenerateJspTaglibsMojo extends AbstractFacesMojo
{
  /**
   * Execute the Mojo.
   */
  public void execute() throws MojoExecutionException
  {
    try
    {
      processIndex(project, resourcePath);
      _generateTagHandlers();
      _generateTagLibraryDescriptors();
    }
    catch (IOException e)
    {
      throw new MojoExecutionException("Error generating components", e);
    }
  }

  /**
   * Generates tag library descriptors for parsed component metadata.
   */
  private void _generateTagLibraryDescriptors() throws MojoExecutionException
  {
    try
    {
      // always add resources directory to project resource root
      addResourceRoot(project, generatedResourcesDirectory.getCanonicalPath());

      // taglibs map syntax requires distinct shortNames,
      // which is a Good Thing!
      for (Iterator i = taglibs.entrySet().iterator(); i.hasNext(); )
      {
        Map.Entry entry = (Map.Entry)i.next();
        String shortName = (String)entry.getKey();
        String namespaceURI = (String)entry.getValue();

        FacesConfigBean facesConfig = getFacesConfig();
        Iterator components = facesConfig.components();
        components = new FilteredIterator(components, new SkipFilter());
        components = new FilteredIterator(components, new ComponentTagLibraryFilter(namespaceURI));

        Iterator validators = facesConfig.validators();
        validators = new FilteredIterator(validators, new ValidatorTagLibraryFilter(namespaceURI));

        Iterator converters = facesConfig.converters();
        converters = new FilteredIterator(converters, new ConverterTagLibraryFilter(namespaceURI));

        String targetPath = "META-INF/" + shortName + ".tld";
        File targetFile = new File(generatedResourcesDirectory, targetPath);

        String configPath = "META-INF/" + shortName + "-base.tld";
        File configFile = new File(configSourceDirectory, configPath);

        targetFile.delete();

        boolean hasGeneratedTags = (components.hasNext() ||
                                    converters.hasNext() ||
                                    validators.hasNext());

        if (hasGeneratedTags && configFile.exists())
        {
          ByteArrayOutputStream out = new ByteArrayOutputStream();
          XMLOutputFactory outputFactory = XMLOutputFactory.newInstance();
          XMLStreamWriter stream = outputFactory.createXMLStreamWriter(out);

          _writeStartTagLibrary(stream, _XINCLUDE_JSP_TAG_LIBRARY_DTD);
          // base goes first
          stream.writeStartElement("xi", "include",
                                   XIncludeFilter.XINCLUDE_NAMESPACE);
          stream.writeNamespace("xi", XIncludeFilter.XINCLUDE_NAMESPACE);
          stream.writeAttribute("href", configFile.toURL().toExternalForm());
          stream.writeAttribute("xpointer", "/taglib/*");
          stream.writeEndElement();
          while (components.hasNext())
          {
            ComponentBean component = (ComponentBean)components.next();
            _writeTag(stream, component);
          }
          while (converters.hasNext())
          {
            ConverterBean converter = (ConverterBean)converters.next();
            _writeTag(stream, converter);
          }
          while (validators.hasNext())
          {
            ValidatorBean validator = (ValidatorBean)validators.next();
            _writeTag(stream, validator);
          }
          _writeEndTagLibrary(stream);
          stream.close();

          InputStream mergedStream = new ByteArrayInputStream(out.toByteArray());

          // expand all the xi:include elements
          SAXParserFactory saxFactory = SAXParserFactory.newInstance();
          saxFactory.setNamespaceAware(true);
          saxFactory.setValidating(false);
          SAXParser saxParser = saxFactory.newSAXParser();
          XMLReader mergedReader = saxParser.getXMLReader();
          mergedReader = new XIncludeFilter(mergedReader, configFile.toURL());
          // even with validating=false, DTD is still downloaded so that
          // any entities contained in the document can be expanded.
          // the following disables that behavior, also saving the time
          // spent to parse the DTD
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

          targetFile.delete();
          targetFile.getParentFile().mkdirs();
          Result mergedResult = new StreamResult(new FileOutputStream(targetFile));

          TransformerFactory transFactory = TransformerFactory.newInstance();
          Transformer identity = transFactory.newTransformer();
          identity.setOutputProperty(OutputKeys.DOCTYPE_PUBLIC,
                                     _JSP_TAG_LIBRARY_DOCTYPE_PUBLIC);
          identity.setOutputProperty(OutputKeys.DOCTYPE_SYSTEM,
                                     _JSP_TAG_LIBRARY_DOCTYPE_SYSTEM);
          identity.transform(mergedSource, mergedResult);

          targetFile.setReadOnly();
        }
        else if (hasGeneratedTags)
        {
          targetFile.getParentFile().mkdirs();
          OutputStream out = new FileOutputStream(targetFile);
          XMLOutputFactory outputFactory = XMLOutputFactory.newInstance();
          XMLStreamWriter stream = outputFactory.createXMLStreamWriter(out);

          _writeStartTagLibrary(stream, "1.2", shortName, namespaceURI);
          while (components.hasNext())
          {
            ComponentBean component = (ComponentBean)components.next();
            _writeTag(stream, component);
          }
          while (converters.hasNext())
          {
            ConverterBean converter = (ConverterBean)converters.next();
            _writeTag(stream, converter);
          }
          while (validators.hasNext())
          {
            ValidatorBean validator = (ValidatorBean)validators.next();
            _writeTag(stream, validator);
          }
          _writeEndTagLibrary(stream);
          stream.close();
        }
        else if (configFile.exists())
        {
          // copy if newer
          if (configFile.lastModified() > targetFile.lastModified())
          {
            targetFile.delete();
            targetFile.getParentFile().mkdirs();
            FileUtils.copyFile(configFile, targetFile);
            targetFile.setReadOnly();
          }
        }
      }
    }
    catch (SAXException e)
    {
      throw new MojoExecutionException("Error generating tag library", e);
    }
    catch (ParserConfigurationException e)
    {
      throw new MojoExecutionException("Error generating tag library", e);
    }
    catch (TransformerException e)
    {
      throw new MojoExecutionException("Error generating tag library", e);
    }
    catch (XMLStreamException e)
    {
      throw new MojoExecutionException("Error generating tag library", e);
    }
    catch (IOException e)
    {
      throw new MojoExecutionException("Error generating tag libraries", e);
    }
  }

  private void _writeStartTagLibrary(
    XMLStreamWriter stream,
    String          dtd) throws XMLStreamException
  {
    stream.writeStartDocument("1.0");
    stream.writeCharacters("\n");
    stream.writeDTD(dtd);
    stream.writeCharacters("\n");
    stream.writeStartElement("taglib");
    stream.writeCharacters("\n  ");
  }

  private void _writeStartTagLibrary(
    XMLStreamWriter stream,
    String          version,
    String          shortName,
    String          namespaceURI) throws XMLStreamException
  {
    _writeStartTagLibrary(stream, _JSP_TAG_LIBRARY_DTD);
    stream.writeStartElement("tlib-version");
    stream.writeCharacters(project.getVersion());
    stream.writeEndElement();

    stream.writeCharacters("\n  ");

    stream.writeStartElement("jsp-version");
    stream.writeCharacters(version);
    stream.writeEndElement();
    stream.writeCharacters("\n  ");
    stream.writeStartElement("short-name");
    stream.writeCharacters(shortName);
    stream.writeEndElement();
    stream.writeCharacters("\n  ");
    stream.writeStartElement("uri");
    stream.writeCharacters(namespaceURI);
    stream.writeEndElement();
  }

  private void _writeEndTagLibrary(
    XMLStreamWriter stream) throws XMLStreamException
  {

    stream.writeCharacters("\n");
    stream.writeEndElement();
    stream.writeEndDocument();
  }

  /**
   * Generates tag library descriptor for parsed component metadata.
   */
  private void _writeTag(
    XMLStreamWriter stream,
    ComponentBean   component) throws XMLStreamException
  {
    stream.writeCharacters("\n  ");
    stream.writeStartElement("tag");
    stream.writeCharacters("\n    ");
    stream.writeStartElement("name");
    stream.writeCharacters(component.getTagName().getLocalPart());
    stream.writeEndElement();
    stream.writeCharacters("\n    ");
    stream.writeStartElement("tag-class");
    stream.writeCharacters(component.getTagClass());
    stream.writeEndElement();
    if (component.getDescription() != null)
    {
      stream.writeCharacters("\n    ");
      stream.writeStartElement("description");
      stream.writeCData(component.getDescription());
      stream.writeEndElement();
    }

    Iterator properties = component.properties(true);
    properties = new FilteredIterator(properties, new TagAttributeFilter());
    while (properties.hasNext())
    {
      PropertyBean property = (PropertyBean)properties.next();
      _writeTagAttribute(stream,
                         property.getPropertyName(),
                         property.getDescription(),
                         property.getUnsupportedAgents());
    }

    stream.writeCharacters("\n  ");
    stream.writeEndElement();
  }

  /**
   * Generates tag library descriptor for parsed converter metadata.
   */
  private void _writeTag(
    XMLStreamWriter stream,
    ConverterBean   converter) throws XMLStreamException
  {
    stream.writeCharacters("\n  ");
    stream.writeStartElement("tag");
    stream.writeCharacters("\n    ");
    stream.writeStartElement("name");
    stream.writeCharacters(converter.getTagName().getLocalPart());
    stream.writeEndElement();
    stream.writeCharacters("\n    ");
    stream.writeStartElement("tag-class");
    stream.writeCharacters(converter.getTagClass());
    stream.writeEndElement();
    if (converter.getDescription() != null)
    {
      stream.writeCharacters("\n    ");
      stream.writeStartElement("description");
      stream.writeCData(converter.getDescription());
      stream.writeEndElement();
    }

    // converters need an id attribute
    _writeTagAttribute(stream, "id", "the identifier for the component", null);

    Iterator properties = converter.properties();
    properties = new FilteredIterator(properties, new TagAttributeFilter());
    while (properties.hasNext())
    {
      PropertyBean property = (PropertyBean)properties.next();
      _writeTagAttribute(stream,
                         property.getPropertyName(),
                         property.getDescription(),
                         property.getUnsupportedAgents());
    }

    stream.writeCharacters("\n  ");
    stream.writeEndElement();
  }

  private void _writeTagAttribute(
    XMLStreamWriter stream,
    String          propertyName,
    String          description,
    String[]        unsupportedAgents) throws XMLStreamException
  {
    stream.writeCharacters("\n    ");
    stream.writeStartElement("attribute");

    stream.writeCharacters("\n      ");
    stream.writeStartElement("name");
    stream.writeCharacters(propertyName);
    stream.writeEndElement();

    stream.writeCharacters("\n      ");
    stream.writeStartElement("rtexprvalue");
    stream.writeCharacters("false");
    stream.writeEndElement();

    if (description != null ||
        unsupportedAgents.length > 0)
    {
      stream.writeCharacters("\n      ");
      stream.writeStartElement("description");

      if (unsupportedAgents != null &&
          unsupportedAgents.length > 0)
      {
        if (description == null)
          description = "";

        description += "\n\n    This attribute is not supported on the following agent types:\n";

        for (int i=0; i < unsupportedAgents.length; i++)
        {
          description += " " + unsupportedAgents[i];
          description += (i < unsupportedAgents.length - 1) ? "," : ".";
        }
      }

      stream.writeCData(description);
      stream.writeEndElement();
    }

    stream.writeCharacters("\n    ");
    stream.writeEndElement();
  }

  /**
   * Generates tag library descriptor for parsed validator metadata.
   */
  private void _writeTag(
    XMLStreamWriter stream,
    ValidatorBean   validator) throws XMLStreamException
  {
    stream.writeCharacters("\n  ");
    stream.writeStartElement("tag");
    stream.writeCharacters("\n    ");
    stream.writeStartElement("name");
    stream.writeCharacters(validator.getTagName().getLocalPart());
    stream.writeEndElement();
    stream.writeCharacters("\n    ");
    stream.writeStartElement("tag-class");
    stream.writeCharacters(validator.getTagClass());
    stream.writeEndElement();
    if (validator.getDescription() != null)
    {
      stream.writeCharacters("\n    ");
      stream.writeStartElement("description");
      stream.writeCData(validator.getDescription());
      stream.writeEndElement();
    }

    // validators need an id attribute
    _writeTagAttribute(stream, "id", "the identifier for the component", null);

    Iterator properties = validator.properties();
    properties = new FilteredIterator(properties, new TagAttributeFilter());
    while (properties.hasNext())
    {
      PropertyBean property = (PropertyBean)properties.next();
      _writeTagAttribute(stream,
                         property.getPropertyName(),
                         property.getDescription(),
                         property.getUnsupportedAgents());
    }

    stream.writeCharacters("\n  ");
    stream.writeEndElement();
  }

  /**
   * Generates tag handlers for parsed component metadata.
   */
  private void _generateTagHandlers() throws IOException
  {
    // Make sure generated source directory
    // is added to compilation source path
    project.addCompileSourceRoot(generatedSourceDirectory.getCanonicalPath());

    FacesConfigBean facesConfig = getFacesConfig();
    if (!facesConfig.hasComponents())
    {
      getLog().info("Nothing to generate - no components found");
    }
    else
    {
      Iterator components = facesConfig.components();
      components = new FilteredIterator(components, new SkipFilter());
      components = new FilteredIterator(components, new ComponentTagFilter());
      components = new FilteredIterator(components, new ComponentTagClassFilter(packageContains));

      Iterator validators = facesConfig.validators();
      validators = new FilteredIterator(validators, new ValidatorTagFilter());
      validators = new FilteredIterator(validators, new ValidatorTagClassFilter(packageContains));

      Iterator converters = facesConfig.converters();
      converters = new FilteredIterator(converters, new ConverterTagFilter());
      converters = new FilteredIterator(converters, new ConverterTagClassFilter(packageContains));

      // incremental unless forced
      if (!force)
      {
        components = new FilteredIterator(components, new IfComponentModifiedFilter());
        converters = new FilteredIterator(converters, new IfConverterModifiedFilter());
        validators = new FilteredIterator(validators, new IfValidatorModifiedFilter());
      }

      if (!components.hasNext() && !converters.hasNext())
      {
        getLog().info("Nothing to generate - all JSP tags are up to date");
      }
      else
      {
        ComponentTagGenerator componentGen = new ComponentTagGenerator();
        ConverterTagGenerator converterGen = new ConverterTagGenerator();
        ValidatorTagGenerator validatorGen = new ValidatorTagGenerator();
        int count = 0;
        while (components.hasNext())
        {
          componentGen.generateTagHandler((ComponentBean)components.next());
          count++;
        }
        while (converters.hasNext())
        {
          converterGen.generateTagHandler((ConverterBean)converters.next());
          count++;
        }
        while (validators.hasNext())
        {
          validatorGen.generateTagHandler((ValidatorBean)validators.next());
          count++;
        }
        getLog().info("Generated " + count + " JSP tag(s)");
      }
    }
  }

  class ConverterTagGenerator
  {
    public void generateTagHandler(
      ConverterBean converter)
    {
      String fullClassName = converter.getTagClass();

      try
      {
        getLog().debug("Generating " + fullClassName);

        String sourcePath = Util.convertClassToSourcePath(fullClassName, ".java");
        File targetFile = new File(generatedSourceDirectory, sourcePath);

        targetFile.getParentFile().mkdirs();
        StringWriter sw = new StringWriter();
        PrettyWriter out = new PrettyWriter(sw);

        String className = Util.getClassFromFullClass(fullClassName);
        String packageName = Util.getPackageFromFullClass(fullClassName);

        // header/copyright
        writePreamble(out);

        // package
        out.println("package " + packageName + ";");

        out.println();
        _writeImports(out, converter);

        out.println("/**");
        // TODO: remove this blank line.
        out.println();
        out.println(" * Auto-generated tag class.");
        out.println(" */");

        out.println("public class " + className +
                          " extends ConverterTag");
        out.println("{");
        out.indent();

        _writeConstructor(out, converter);

        _writePropertyMethods(out, converter);
        _writeDoStartTag(out, converter);
        _writeCreateConverter(out, converter);
        _writeSetProperties(out, converter);
        _writeRelease(out, converter);

        out.unindent();
        out.println("}");
        out.close();

        // delay write in case of error
        // timestamp should not be updated when an error occurs
        // delete target file first, because it is readonly
        targetFile.delete();
        FileWriter fw = new FileWriter(targetFile);
        StringBuffer buf = sw.getBuffer();
        fw.write(buf.toString());
        fw.close();
        targetFile.setReadOnly();
      }
      catch (Throwable e)
      {
        getLog().error("Error generating " + fullClassName, e);
      }
    }

    private void _writeImports(
      PrettyWriter   out,
      ConverterBean  converter)
    {
      Set imports = new TreeSet();

      imports.add("javax.faces.webapp.ConverterTag");
      imports.add("javax.servlet.jsp.JspException");
      imports.add(converter.getConverterClass());

      Iterator properties = converter.properties();
      properties = new FilteredIterator(properties, new TagAttributeFilter());
      if (properties.hasNext())
      {
        imports.add("javax.faces.convert.Converter");
        imports.add("javax.faces.el.ValueBinding");
        imports.add("org.apache.myfaces.trinidadinternal.taglib.util.TagUtils");
      }

      while (properties.hasNext())
      {
        PropertyBean property = (PropertyBean)properties.next();

        String propertyClass = property.getPropertyClass();
        if (propertyClass != null)
          imports.add(propertyClass);

        if ("java.lang.String[]".equals(propertyClass))
        {
          imports.add("java.text.ParseException");
        }
      }

      // do not import implicit!
      imports.removeAll(Util.PRIMITIVE_TYPES);

      String tagClass = converter.getTagClass();
      String packageName = Util.getPackageFromFullClass(tagClass);
      writeImports(out, packageName, imports);
    }

    private void _writeConstructor(
      PrettyWriter  out,
      ConverterBean converter) throws IOException
    {
      String fullClassName = converter.getTagClass();
      String className = Util.getClassFromFullClass(fullClassName);
      out.println();
      out.println("/**");
      // TODO: restore this correctly phrased comment (tense vs. command)
      //out.println(" * Constructs an instance of " + className + ".");
      out.println(" * Construct an instance of the " + className + ".");
      out.println(" */");
      out.println("public " + className + "()");
      out.println("{");
      out.println("}");
    }

    private void _writePropertyMethods(
      PrettyWriter  out,
      ConverterBean converter) throws IOException
    {
      Iterator properties = converter.properties();
      properties = new FilteredIterator(properties, new TagAttributeFilter());
      while (properties.hasNext())
      {
        PropertyBean property = (PropertyBean)properties.next();
        out.println();
        _writePropertyMember(out, property);
        _writePropertySet(out, property);
      }
    }

    private void _writePropertyMember(
     PrettyWriter  out,
     PropertyBean  property) throws IOException
    {
      String propName = property.getPropertyName();
      String propVar = "_" + Util.getVariableFromName(propName);
      out.println("private String " + propVar + ";");
    }

    private void _writePropertySet(
     PrettyWriter  out,
     PropertyBean  property) throws IOException
    {
      String propName = property.getPropertyName();
      String propVar = Util.getVariableFromName(propName);
      String setMethod = Util.getPrefixedPropertyName("set", propName);

      // TODO: restore coding standards, and make final
      out.println("public void " + setMethod + "(String " + propVar + ")");
      out.println("{");
      out.indent();
      out.println("_" + propVar + " = " + propVar + ";");
      out.unindent();
      out.println("}");
    }

    private void _writeDoStartTag(
      PrettyWriter  out,
      ConverterBean converter) throws IOException
    {
      String converterFullClass = converter.getConverterClass();
      String converterClass = Util.getClassFromFullClass(converterFullClass);

      out.println();
      // TODO: restore coding standards, and make final
      out.println("public int doStartTag() throws JspException");
      out.println("{");
      out.indent();
      out.println("super.setConverterId(" + converterClass + ".CONVERTER_ID);");
      out.println("return super.doStartTag();");
      out.unindent();
      out.println("}");
    }

    private void _writeCreateConverter(
      PrettyWriter  out,
      ConverterBean converter) throws IOException
    {
      Iterator properties = converter.properties();
      properties = new FilteredIterator(properties, new TagAttributeFilter());
      if (properties.hasNext())
      {
        String converterFullClass = converter.getConverterClass();
        String converterClass = Util.getClassFromFullClass(converterFullClass);

        out.println();
        // TODO: restore coding standards, and make final
        out.println("protected Converter createConverter() throws JspException");
        out.println("{");
        out.indent();
        out.println(converterClass + " converter = " +
                    "(" + converterClass + ")super.createConverter();");
        out.println("_setProperties(converter);");
        out.println("return converter;");
        out.unindent();
        out.println("}");
      }
    }

    private void _writeSetProperties(
      PrettyWriter  out,
      ConverterBean converter) throws IOException
    {
      Iterator properties = converter.properties();
      properties = new FilteredIterator(properties, new TagAttributeFilter());
      if (properties.hasNext())
      {
        String converterFullClass = converter.getConverterClass();
        String converterClass = Util.getClassFromFullClass(converterFullClass);
        out.println();
        out.println("private void _setProperties(");
        out.indent();
        out.println(converterClass + " converter) throws JspException");
        out.unindent();
        out.println("{");
        out.indent();
        while (properties.hasNext())
        {
          PropertyBean property = (PropertyBean)properties.next();
          _writeSetProperty(out, property);
        }
        out.unindent();
        out.println("}");
      }
    }

    private void _writeSetProperty(
      PrettyWriter out,
      PropertyBean property)
    {
      String propName = property.getPropertyName();
      String propFullClass = property.getPropertyClass();
      String propClass = Util.getClassFromFullClass(propFullClass);
      String propVar = "_" + Util.getVariableFromName(propName);
      out.println("if (" + propVar + " != null)");
      out.println("{");
      out.indent();
      out.println("if (TagUtils.isValueReference(" + propVar + "))");
      out.println("{");
      out.indent();
      out.println("ValueBinding vb = TagUtils.getValueBinding(" + propVar + ");");
      out.println("converter.setValueBinding(\"" + propName + "\", vb);");
      out.unindent();
      out.println("}");
      String propType = _resolveType(propFullClass);
      if (propType != null)
      {
        out.println("else");
        out.println("{");
        out.indent();
        if ("StringArray".equals(propType))
        {
          out.println("try");
          out.println("{");
        }
        out.println(propClass + " value = TagUtils.get" + propType + "(" + propVar + ");");
        String setMethod = Util.getPrefixedPropertyName("set", propName);
        out.println("converter." + setMethod + "(value);");
        if ("StringArray".equals(propType))
        {
          out.println("}");
          out.println("catch (ParseException pe)");
          out.println("{");
          out.indent();
          out.println("throw new JspException(");
          out.println("  pe.getMessage() + \": \" + \"Position \" + pe.getErrorOffset());");
          out.unindent();
          out.println("}");
        }
        out.unindent();
        out.println("}");
      }
      out.unindent();
      out.println("}");
    }

    private void _writeRelease(
      PrettyWriter  out,
      ConverterBean converter) throws IOException
    {
      Iterator properties = converter.properties();
      properties = new FilteredIterator(properties, new TagAttributeFilter());
      if (properties.hasNext())
      {
        out.println();
        out.println("public void release()");
        out.println("{");
        out.indent();
        out.println("super.release();");
        while (properties.hasNext())
        {
          PropertyBean property = (PropertyBean)properties.next();
          String propName = property.getPropertyName();
          String propVar = "_" + Util.getVariableFromName(propName);
          out.println(propVar + " = null;");
        }
        out.unindent();
        out.println("}");
      }
    }
  }

  class ValidatorTagGenerator
  {
    public void generateTagHandler(
      ValidatorBean validator)
    {
      String fullClassName = validator.getTagClass();

      try
      {
        getLog().debug("Generating " + fullClassName);
        String sourcePath = Util.convertClassToSourcePath(fullClassName, ".java");
        File targetFile = new File(generatedSourceDirectory, sourcePath);

        targetFile.getParentFile().mkdirs();
        StringWriter sw = new StringWriter();
        PrettyWriter out = new PrettyWriter(sw);

        String className = Util.getClassFromFullClass(fullClassName);
        String packageName = Util.getPackageFromFullClass(fullClassName);

        // header/copyright
        writePreamble(out);

        // package
        out.println("package " + packageName + ";");

        out.println();
        _writeImports(out, validator);

        out.println("/**");
        // TODO: remove this blank line.
        out.println();
        out.println(" * Auto-generated tag class.");
        out.println(" */");

        out.println("public class " + className +
                          " extends ValidatorTag");
        out.println("{");
        out.indent();

        _writeConstructor(out, validator);

        _writePropertyMethods(out, validator);
        _writeDoStartTag(out, validator);
        _writeCreateValidator(out, validator);
        _writeSetProperties(out, validator);
        _writeRelease(out, validator);

        out.unindent();
        out.println("}");
        out.close();

        // delay write in case of error
        // timestamp should not be updated when an error occurs
        // delete target file first, because it is readonly
        targetFile.delete();
        FileWriter fw = new FileWriter(targetFile);
        StringBuffer buf = sw.getBuffer();
        fw.write(buf.toString());
        fw.close();
        targetFile.setReadOnly();
      }
      catch (Throwable e)
      {
        getLog().error("Error generating " + fullClassName, e);
      }
    }

    private void _writeImports(
      PrettyWriter   out,
      ValidatorBean  validator)
    {
      Set imports = new TreeSet();

      imports.add("javax.faces.webapp.ValidatorTag");
      imports.add("javax.servlet.jsp.JspException");
      imports.add(validator.getValidatorClass());

      Iterator properties = validator.properties();
      properties = new FilteredIterator(properties, new TagAttributeFilter());
      if (properties.hasNext())
      {
        imports.add("javax.faces.validator.Validator");
        imports.add("javax.faces.el.ValueBinding");
        imports.add("org.apache.myfaces.trinidadinternal.taglib.util.TagUtils");
      }

      while (properties.hasNext())
      {
        PropertyBean property = (PropertyBean)properties.next();

        String propertyClass = property.getPropertyClass();
        if (propertyClass != null)
          imports.add(propertyClass);

        if ("java.lang.String[]".equals(propertyClass))
        {
          imports.add("java.text.ParseException");
        }
      }

      // do not import implicit!
      imports.removeAll(Util.PRIMITIVE_TYPES);

      String tagClass = validator.getTagClass();
      String packageName = Util.getPackageFromFullClass(tagClass);
      writeImports(out, packageName, imports);
    }

    private void _writeConstructor(
      PrettyWriter  out,
      ValidatorBean validator) throws IOException
    {
      String fullClassName = validator.getTagClass();
      String className = Util.getClassFromFullClass(fullClassName);
      out.println();
      out.println("/**");
      // TODO: restore this correctly phrased comment (tense vs. command)
      //out.println(" * Constructs an instance of " + className + ".");
      out.println(" * Construct an instance of the " + className + ".");
      out.println(" */");
      out.println("public " + className + "()");
      out.println("{");
      out.println("}");
    }

    private void _writePropertyMethods(
      PrettyWriter  out,
      ValidatorBean validator) throws IOException
    {
      Iterator properties = validator.properties();
      properties = new FilteredIterator(properties, new TagAttributeFilter());
      while (properties.hasNext())
      {
        PropertyBean property = (PropertyBean)properties.next();
        out.println();
        _writePropertyMember(out, property);
        _writePropertySet(out, property);
      }
    }

    private void _writePropertyMember(
     PrettyWriter  out,
     PropertyBean  property) throws IOException
    {
      String propName = property.getPropertyName();
      String propVar = "_" + Util.getVariableFromName(propName);
      out.println("private String " + propVar + ";");
    }

    private void _writePropertySet(
     PrettyWriter  out,
     PropertyBean  property) throws IOException
    {
      String propName = property.getPropertyName();
      String propVar = Util.getVariableFromName(propName);
      String setMethod = Util.getPrefixedPropertyName("set", propName);

      // TODO: restore coding standards, and make final
      out.println("public void " + setMethod + "(String " + propVar + ")");
      out.println("{");
      out.indent();
      out.println("_" + propVar + " = " + propVar + ";");
      out.unindent();
      out.println("}");
    }

    private void _writeDoStartTag(
      PrettyWriter  out,
      ValidatorBean validator) throws IOException
    {
      String validatorFullClass = validator.getValidatorClass();
      String validatorClass = Util.getClassFromFullClass(validatorFullClass);

      out.println();
      // TODO: restore coding standards, and make final
      out.println("public int doStartTag() throws JspException");
      out.println("{");
      out.indent();
      out.println("super.setValidatorId(" + validatorClass + ".VALIDATOR_ID);");
      out.println("return super.doStartTag();");
      out.unindent();
      out.println("}");
    }

    private void _writeCreateValidator(
      PrettyWriter  out,
      ValidatorBean validator) throws IOException
    {
      Iterator properties = validator.properties();
      properties = new FilteredIterator(properties, new TagAttributeFilter());
      if (properties.hasNext())
      {
        String validatorFullClass = validator.getValidatorClass();
        String validatorClass = Util.getClassFromFullClass(validatorFullClass);

        out.println();
        // TODO: restore coding standards, and make final
        out.println("protected Validator createValidator() throws JspException");
        out.println("{");
        out.indent();
        out.println(validatorClass + " validator = " +
                    "(" + validatorClass + ")super.createValidator();");
        out.println("_setProperties(validator);");
        out.println("return validator;");
        out.unindent();
        out.println("}");
      }
    }

    private void _writeSetProperties(
      PrettyWriter  out,
      ValidatorBean validator) throws IOException
    {
      Iterator properties = validator.properties();
      properties = new FilteredIterator(properties, new TagAttributeFilter());
      if (properties.hasNext())
      {
        String validatorFullClass = validator.getValidatorClass();
        String validatorClass = Util.getClassFromFullClass(validatorFullClass);
        out.println();
        out.println("private void _setProperties(");
        out.indent();
        out.println(validatorClass + " validator) throws JspException");
        out.unindent();
        out.println("{");
        out.indent();
        while (properties.hasNext())
        {
          PropertyBean property = (PropertyBean)properties.next();
          _writeSetProperty(out, property);
        }
        out.unindent();
        out.println("}");
      }
    }

    private void _writeSetProperty(
      PrettyWriter out,
      PropertyBean property)
    {
      String propName = property.getPropertyName();
      String propFullClass = property.getPropertyClass();
      String propClass = Util.getClassFromFullClass(propFullClass);
      String propVar = "_" + Util.getVariableFromName(propName);
      out.println("if (" + propVar + " != null)");
      out.println("{");
      out.indent();
      out.println("if (TagUtils.isValueReference(" + propVar + "))");
      out.println("{");
      out.indent();
      out.println("ValueBinding vb = TagUtils.getValueBinding(" + propVar + ");");
      out.println("validator.setValueBinding(\"" + propName + "\", vb);");
      out.unindent();
      out.println("}");
      String propType = _resolveType(propFullClass);
      if (propType != null)
      {
        out.println("else");
        out.println("{");
        out.indent();
        if ("StringArray".equals(propType))
        {
          out.println("try");
          out.println("{");
        }
        out.println(propClass + " value = TagUtils.get" + propType + "(" + propVar + ");");
        String setMethod = Util.getPrefixedPropertyName("set", propName);
        out.println("validator." + setMethod + "(value);");
        if ("StringArray".equals(propType))
        {
          out.println("}");
          out.println("catch (ParseException pe)");
          out.println("{");
          out.indent();
          out.println("throw new JspException(");
          out.println("  pe.getMessage() + \": \" + \"Position \" + pe.getErrorOffset());");
          out.unindent();
          out.println("}");
        }
        out.unindent();
        out.println("}");
      }
      out.unindent();
      out.println("}");
    }

    private void _writeRelease(
      PrettyWriter  out,
      ValidatorBean validator) throws IOException
    {
      Iterator properties = validator.properties();
      properties = new FilteredIterator(properties, new TagAttributeFilter());
      if (properties.hasNext())
      {
        out.println();
        out.println("public void release()");
        out.println("{");
        out.indent();
        out.println("super.release();");
        while (properties.hasNext())
        {
          PropertyBean property = (PropertyBean)properties.next();
          String propName = property.getPropertyName();
          String propVar = "_" + Util.getVariableFromName(propName);
          out.println(propVar + " = null;");
        }
        out.unindent();
        out.println("}");
      }
    }
  }

  class ComponentTagGenerator
  {
    public void generateTagHandler(
      ComponentBean component)
    {
      String fullClassName = component.getTagClass();

      try
      {
        getLog().debug("Generating " + fullClassName);

        String sourcePath = Util.convertClassToSourcePath(fullClassName, ".java");
        File targetFile = new File(generatedSourceDirectory, sourcePath);

        targetFile.getParentFile().mkdirs();
        StringWriter sw = new StringWriter();
        PrettyWriter out = new PrettyWriter(sw);

        String className = Util.getClassFromFullClass(fullClassName);
        String packageName = Util.getPackageFromFullClass(fullClassName);
        String fullSuperclassName = component.findJspTagSuperclass();
        if (fullSuperclassName == null)
          throw new IllegalArgumentException("Missing JSP Tag superclass");
        String superclassName = Util.getClassFromFullClass(fullSuperclassName);
        if (superclassName.equals(className))
          superclassName = fullSuperclassName;
        String componentFullClass = component.getComponentClass();
        String componentClass = Util.getClassFromFullClass(componentFullClass);

        // header/copyright
        writePreamble(out);

        // package
        out.println("package " + packageName + ";");

        out.println();
        _writeImports(out, fullSuperclassName, superclassName,
                      componentFullClass, component);

        out.println("/**");
        // TODO: remove this blank line.
        out.println();
        out.println(" * Auto-generated tag class.");
        out.println(" */");

        // TODO: eliminate <mfp:tag-class-modifier> metadata
        int modifiers = component.getTagClassModifiers();
        String classStart = Modifier.toString(modifiers);
        // TODO: use canonical ordering
        classStart = classStart.replaceAll("public abstract", "abstract public");
        out.println(classStart + " class " + className +
                                 " extends " + superclassName);
        out.println("{");
        out.indent();

        _writeConstructor(out, component);

        if (!Modifier.isAbstract(modifiers))
        {
          _writeGetComponentType(out, component);
          _writeGetRendererType(out, component);
        }
        _writePropertyMethods(out, component);
        _writeSetProperties(out, componentClass, component);
        _writeRelease(out, component);

        out.unindent();
        out.println("}");
        out.close();

        // delay write in case of error
        // timestamp should not be updated when an error occurs
        // delete target file first, because it is readonly
        targetFile.delete();
        FileWriter fw = new FileWriter(targetFile);
        StringBuffer buf = sw.getBuffer();
        fw.write(buf.toString());
        fw.close();
        targetFile.setReadOnly();
      }
      catch (Throwable e)
      {
        getLog().error("Error generating " + fullClassName, e);
      }
    }

    private void _writeImports(
      PrettyWriter   out,
      String         fullSuperclassName,
      String         superclassName,
      String         componentFullClass,
      ComponentBean  component)
    {
      Set imports = new TreeSet();

      Iterator properties = component.properties();
      properties = new FilteredIterator(properties, new TagAttributeFilter());
      if (properties.hasNext())
      {
        imports.add("org.apache.myfaces.trinidad.bean.FacesBean");
        imports.add(componentFullClass);
      }

      // TODO: only add FacesBean import when properties exist
      imports.add("org.apache.myfaces.trinidad.bean.FacesBean");

      // TODO: remove these imports
      imports.add("javax.faces.component.UIComponent");
      imports.add("javax.faces.el.ValueBinding");
      imports.add("org.apache.myfaces.trinidadinternal.taglib.util.VirtualAttributeUtils");

      // superclassName is fully qualified if it collides
      // with the generated class name and should not be
      // imported when such a collision would occur
      if (!superclassName.equals(fullSuperclassName))
        imports.add(fullSuperclassName);

      while (properties.hasNext())
      {
        PropertyBean property = (PropertyBean)properties.next();

        String   propertyClass = property.getPropertyClass();
        String[] propertyClassParams = property.getPropertyClassParameters();

        if (propertyClass != null)
          imports.add(propertyClass);

        if (_isConverter(propertyClass) ||
            _isKeyStroke(propertyClass) ||
            _isColorList(propertyClass, propertyClassParams) ||
            property.isVirtual())
        {
          imports.add("javax.faces.el.ValueBinding");
        }

        if (_isColorList(propertyClass, propertyClassParams))
        {
          imports.add("java.text.ParseException");
          imports.add("org.apache.myfaces.trinidadinternal.taglib.util.TagUtils");
        }

        // TODO: restore import and make reference to
        //       ConstantMethodBinding relative rather
        //       than absolute
        //if (property.isMethodBinding() &&
        //    isStringMethodBindingReturnType(property))
        //{
        //  imports.add("org.apache.myfaces.trinidadinternal.taglib.ConstantMethodBinding");
        //}

        if (property.isVirtual())
        {
          imports.add("org.apache.myfaces.trinidadinternal.taglib.util.VirtualAttributeUtils");
        }
      }

      // do not import implicit!
      imports.removeAll(Util.PRIMITIVE_TYPES);

      String tagClass = component.getTagClass();
      String packageName = Util.getPackageFromFullClass(tagClass);
      writeImports(out, packageName, imports);
    }

    private void _writeConstructor(
      PrettyWriter  out,
      ComponentBean component) throws IOException
    {
      String fullClassName = component.getTagClass();
      String className = Util.getClassFromFullClass(fullClassName);
      out.println();
      out.println("/**");
      // TODO: restore this correctly phrased comment (tense vs. command)
      //out.println(" * Constructs an instance of " + className + ".");
      out.println(" * Construct an instance of the " + className + ".");
      out.println(" */");
      out.println("public " + className + "()");
      out.println("{");
      out.println("}");
    }

    private void _writeGetComponentType(
      PrettyWriter  out,
      ComponentBean component) throws IOException
    {
      String componentType = component.getComponentType();
      out.println();
      out.println("public String getComponentType()");
      out.println("{");
      out.indent();
      out.println("return \"" + componentType + "\";");
      out.unindent();
      out.println("}");
    }

    private void _writeGetRendererType(
      PrettyWriter  out,
      ComponentBean component) throws IOException
    {
      String rendererType = component.getRendererType();
      out.println();
      out.println("public String getRendererType()");
      out.println("{");
      out.indent();
      out.println("return " + convertStringToLiteral(rendererType) + ";");
      out.unindent();
      out.println("}");
    }

    private void _writeRelease(
      PrettyWriter  out,
      ComponentBean component) throws IOException
    {
      Iterator properties = component.properties();
      properties = new FilteredIterator(properties, new TagAttributeFilter());
      // TODO: remove special case for UIXFormTag
      if (properties.hasNext() ||
          "org.apache.myfaces.trinidadinternal.taglib.UIXFormTag".equals(component.getTagClass()))
      {
        out.println();
        out.println("public void release()");
        out.println("{");
        out.indent();
        out.println("super.release();");
        while (properties.hasNext())
        {
          PropertyBean property = (PropertyBean)properties.next();
          String propName = property.getPropertyName();
          String propVar = "_" + Util.getVariableFromName(propName);
          out.println(propVar + " = null;");
        }
        out.unindent();
        out.println("}");
      }
    }

    private void _writePropertyMethods(
      PrettyWriter  out,
      ComponentBean component) throws IOException
    {
      Iterator properties = component.properties();
      properties = new FilteredIterator(properties, new TagAttributeFilter());
      while (properties.hasNext())
      {
        PropertyBean property = (PropertyBean)properties.next();
        out.println();
        _writePropertyMember(out, property);
        _writePropertySet(out, property);
      }
    }

    private void _writePropertyMembers(
      PrettyWriter  out,
      ComponentBean component) throws IOException
    {
      Iterator properties = component.properties();
      properties = new FilteredIterator(properties, new TagAttributeFilter());
      if (properties.hasNext())
      {
        out.println();
        while (properties.hasNext())
        {
          PropertyBean property = (PropertyBean)properties.next();
          _writePropertyMember(out, property);
        }
      }
    }

    private void _writePropertyMember(
     PrettyWriter  out,
     PropertyBean  property) throws IOException
    {
      String propName = property.getPropertyName();
      String propVar = "_" + Util.getVariableFromName(propName);
      out.println("private String " + propVar + ";");
    }

    private void _writePropertySet(
     PrettyWriter  out,
     PropertyBean  property) throws IOException
    {
      String propName = property.getPropertyName();
      String propVar = Util.getVariableFromName(propName);
      String setMethod = Util.getPrefixedPropertyName("set", propName);

      // TODO: restore coding standards, and make final
      out.println("public void " + setMethod + "(String " + propVar + ")");
      out.println("{");
      out.indent();
      out.println("_" + propVar + " = " + propVar + ";");
      out.unindent();
      out.println("}");
    }

    private void _writeSetProperties(
      PrettyWriter  out,
      String        componentClass,
      ComponentBean component) throws IOException
    {
      Iterator properties = component.properties();
      properties = new FilteredIterator(properties, new TagAttributeFilter());
      // TODO: only write out setProperties when properties exist
  //    if (properties.hasNext())
  //    {
        out.println();
        out.println("protected void setProperties(");
        out.indent();
        out.println("FacesBean bean)");
        out.unindent();
        out.println("{");
        out.indent();
        out.println("super.setProperties(bean);");

        while (properties.hasNext())
        {
          PropertyBean property = (PropertyBean)properties.next();
          _writeSetPropertiesCase(out, componentClass, property);
        }
        out.unindent();
        out.println("}");
  //    }
    }

    private void _writeSetPropertiesCase(
      PrettyWriter  out,
      String             componentClass,
      PropertyBean        property) throws IOException
    {
      String propName = property.getPropertyName();
      String propClass = property.getPropertyClass();
      String propVar = "_" + Util.getVariableFromName(propName);

      if (property.isVirtual())
      {
        _writeVirtualSetMethod(out, componentClass, propName);
      }
      else if (property.isMethodBinding())
      {
        _writeSetMethodBinding(out, componentClass, property);
      }
      else if (_isKeyStroke(propClass))
      {
        _writeSetKeyStroke(out, componentClass, propName);
      }
      else if (_isColorList(propClass, property.getPropertyClassParameters()))
      {
        _writeSetColorList(out, componentClass, propName);
      }
      else if (_isConverter(propClass))
      {
        _writeSetConverter(out, componentClass, propName);
      }
      else if (property.isLiteralOnly())
      {
        _writeSetLiteral(out, componentClass, propName, propClass, propVar);
      }
      else //if (_hasPropertySetter(property))
      {
        _writeSetProperty(out, componentClass, propName, propClass, propVar);
      }
  //    else
  //    {
  //      _writeSetValueBinding(out, componentClass, propName, propVar);
  //    }
    }

    /**
     * Returns true if this property is a complex Object.
     *
     * @return true  if this property is a complex Object,
     *         otherwise false
     */
    private boolean _hasPropertySetter(
      PropertyBean property)
    {
      String propertyClass = property.getPropertyClass();
      return (Util.PRIMITIVE_TYPES.contains(propertyClass) ||
              "java.lang.Object".equals(propertyClass) ||
              "java.lang.String".equals(propertyClass) ||
              "java.lang.String[]".equals(propertyClass));
    }

    private boolean _isConverter(
      String propClass)
    {
      return ("javax.faces.convert.Converter".equals(propClass));
    }

    private String[] _getAccessKeyPropertyKeys(
      String componentClass,
      String propName)
    {
      String[] propKeys = new String[2];

      int offset = propName.indexOf("AndAccessKey");
      if (offset != -1)
      {
        String mainProp = propName.substring(0, offset);
        propKeys[0] = componentClass + "." +
                      Util.getConstantNameFromProperty(mainProp, "_KEY");
        propKeys[1] = componentClass + "." +
                      Util.getConstantNameFromProperty("accessKey", "_KEY");
      }

      return propKeys;
    }

    private boolean _isKeyStroke(
      String propClass)
    {
      return ("javax.swing.KeyStroke".equals(propClass));
    }

    private boolean _isColorList(
      String   propClass,
      String[] propClassParams)
    {
      return ("java.util.List".equals(propClass) &&
              propClassParams.length == 1 &&
              "java.awt.Color".equals(propClassParams[0]));
    }

    private void _writeSetLiteral(
      PrettyWriter out,
      String       componentClass,
      String       propName,
      String       propClass,
      String       propVar)
    {
        // TODO: reject value binding expressions for literal-only
      _writeSetProperty(out, componentClass, propName, propClass, propVar);
    }

    private void _writeSetProperty(
      PrettyWriter out,
      String       componentClass,
      String       propName,
      String       propFullClass,
      String       propVar)
    {
      String propKey = Util.getConstantNameFromProperty(propName, "_KEY");
      String propClass = Util.getClassFromFullClass(propFullClass);
      String boxedClass = Util.getBoxedClass(propClass);
      String setProperty = "setProperty";
      if (!boxedClass.equals(propClass) ||
           "java.util.Date".equals(propFullClass) ||
          (boxedClass.indexOf("[]") != -1))
      {
        String propType = boxedClass.replaceAll("\\[\\]", "Array");
        setProperty = Util.getPrefixedPropertyName("set", propType + "Property");
      }
      out.println(setProperty + "(bean, " +
                                  componentClass + "." + propKey + ", " +
                                  propVar + ");" );
    }

    private void _writeSetValueBinding(
      PrettyWriter out,
      String       componentClass,
      String       propName,
      String       propVar)
    {
      String propKey = Util.getConstantNameFromProperty(propName, "_KEY");
      out.println("bean.setValueBinding(" + componentClass + "." + propKey + ", " +
                                        "createValueBinding(" + propVar + "));" );
    }

    private void _writeVirtualSetMethod(
      PrettyWriter  out,
      String        componentClass,
      String        propName) throws IOException
    {
      String[] propKeys = _getAccessKeyPropertyKeys(componentClass, propName);

      String propVar = "_" + propName;
      out.println("if (" + propVar + " != null)");
      out.println("{");
      out.indent();
      out.println("if (isValueReference(" + propVar + "))");
      out.println("{");
      out.indent();
      out.println("ValueBinding vb = createValueBinding(" + propVar + ");");
      out.println("VirtualAttributeUtils.setAccessKeyAttribute(");
      out.indent();
      out.println("bean,");
      out.println("vb,");
      out.println(propKeys[0] + ",");
      out.println(propKeys[1] +  ");");
      out.unindent();
      out.unindent();
      out.println("}");
      out.println("else");
      out.println("{");
      out.indent();
      out.println("VirtualAttributeUtils.setAccessKeyAttribute(");
      out.indent();
      out.println("bean,");
      out.println(propVar + ",");
      out.println(propKeys[0] + ",");
      out.println(propKeys[1] +  ");");
      out.unindent();
      out.unindent();
      out.println("}");
      out.unindent();
      out.println("}");
    }

    private void _writeSetMethodBinding(
      PrettyWriter  out,
      String             componentClass,
      PropertyBean        property) throws IOException
    {
      String propName = property.getPropertyName();
      String propKey = Util.getConstantNameFromProperty(propName, "_KEY");
      String propVar = "_" + propName;

      MethodSignatureBean signature = property.getMethodBindingSignature();
      String[] paramTypes = (signature != null) ? signature.getParameterTypes() : null;

      String classArray;

      if (paramTypes == null || paramTypes.length == 0)
      {
        classArray = "new Class[0]";
      }
      else
      {
        StringBuffer sb = new StringBuffer();
        sb.append("new Class[]{");
        for (int i=0; i < paramTypes.length; i++)
        {
          if (i > 0)
            sb.append(',');
          sb.append(paramTypes[i]);
          sb.append(".class");
        }

        // TODO: remove trailing comma
        sb.append(',');

        sb.append('}');
        classArray = sb.toString();
      }

      out.println("if (" + propVar + " != null)");
      out.println("{");
      out.indent();

      if (isStringMethodBindingReturnType(signature))
      {
        out.println("MethodBinding mb;");
        out.println("if (isValueReference(" + propVar + "))");
        out.indent();
        out.println("mb = createMethodBinding(" + propVar + ", " + classArray + ");");
        out.unindent();
        out.println("else");
        out.indent();
        out.println("mb = new org.apache.myfaces.trinidadinternal.taglib.ConstantMethodBinding(" + propVar + ");");
        out.unindent();
      }
      else
      {
        // never a literal, no need for ConstantMethodBinding
        out.println("MethodBinding mb = createMethodBinding(" + propVar + ", " +
                                                            classArray + ");");
      }

      out.println("bean.setProperty(" + componentClass + "." + propKey + ", mb);");
      out.unindent();
      out.println("}");
    }

    private void _writeSetKeyStroke(
      PrettyWriter  out,
      String        componentClass,
      String        propName) throws IOException
    {
      String propKey = Util.getConstantNameFromProperty(propName, "_KEY");
      String propVar = "_" + propName;

      out.println("if (" + propVar + " != null)");
      out.println("{");
      out.indent();
      out.println("if (isValueReference(" + propVar + "))");
      out.println("{");
      out.indent();
      out.println("ValueBinding vb = createValueBinding(" + propVar + ");");
      out.println("bean.setValueBinding(" + componentClass + "." + propKey + ", vb);");
      out.unindent();
      out.println("}");
      out.println("else");
      out.println("{");
      out.indent();
      out.println("bean.setProperty(" + componentClass + "." + propKey + ",");
      out.println("\tKeyStroke.getKeyStroke(" + propVar + "));");
      out.unindent();
      out.println("}");
      out.unindent();
      out.println("}");
    }


    private void _writeSetColorList(
      PrettyWriter  out,
      String        componentClass,
      String        propName) throws IOException
    {
      String propKey = Util.getConstantNameFromProperty(propName, "_KEY");
      String propVar = "_" + propName;

      out.println("if (" + propVar + " != null)");
      out.println("{");
      out.indent();
      out.println("if (isValueReference(" + propVar + "))");
      out.println("{");
      out.indent();
      out.println("ValueBinding vb = createValueBinding(" + propVar + ");");
      out.println("bean.setValueBinding(" + componentClass + "." + propKey + ", vb);");
      out.unindent();
      out.println("}");
      out.println("else");
      out.println("{");
      out.indent();
      out.println("try");
      out.println("{");
      out.indent();
      out.println("bean.setProperty(" + componentClass + "." + propKey + ",");
      out.println("                 TagUtils.getColorList(" + propVar + "));");
      out.unindent();
      out.println("}");
      out.println("catch (ParseException pe)");
      out.println("{");
      out.indent();
      out.println("setValidationError(");
      out.println("  pe.getMessage() + \": \" + \"Position \" + pe.getErrorOffset());");
      out.unindent();
      out.println("}");
      out.unindent();
      out.println("}");
      out.unindent();
      out.println("}");
    }


    private void _writeSetConverter(
      PrettyWriter  out,
      String             componentClass,
      String             propName) throws IOException
    {
      String propKey = Util.getConstantNameFromProperty(propName, "_KEY");
      String propVar = "_" + propName;

      out.println("if (" + propVar + " != null)");
      out.println("{");
      out.indent();
      out.println("if (isValueReference(" + propVar + "))");
      out.println("{");
      out.indent();
      out.println("ValueBinding vb = createValueBinding(" + propVar + ");");
      out.println("bean.setValueBinding(" + componentClass + "." + propKey + ", vb);");
      out.unindent();
      out.println("}");
      out.println("else");
      out.println("{");
      out.indent();
      out.println("Converter converter = getFacesContext().getApplication().");
      out.indent();
      out.println("createConverter(" + propVar + ");");
      out.unindent();
      out.println("bean.setProperty(" + componentClass + "." + propKey + ", converter);");
      out.unindent();
      out.println("}");
      out.unindent();
      out.println("}");
    }
  }

  private boolean isStringMethodBindingReturnType(
    MethodSignatureBean sig)
  {
    return (sig != null && "java.lang.String".equals(sig.getReturnType()));
  }

  private class IfComponentModifiedFilter extends ComponentFilter
  {
    protected boolean accept(
      ComponentBean component)
    {
      String tagClass = component.getTagClass();
      String sourcePath = Util.convertClassToSourcePath(tagClass, ".java");
      String templatePath = Util.convertClassToSourcePath(tagClass, "Template.java");
      File targetFile = new File(generatedSourceDirectory, sourcePath);
      File templateFile = new File(templateSourceDirectory, templatePath);

      // accept if templateFile is newer or component has been modified
      return (templateFile.lastModified() > targetFile.lastModified() ||
              component.isModifiedSince(targetFile.lastModified()));
    }
  }

  private class IfConverterModifiedFilter extends ConverterFilter
  {
    protected boolean accept(
      ConverterBean converter)
    {
      String tagClass = converter.getTagClass();
      String sourcePath = Util.convertClassToSourcePath(tagClass, ".java");
      String templatePath = Util.convertClassToSourcePath(tagClass, "Template.java");
      File targetFile = new File(generatedSourceDirectory, sourcePath);
      File templateFile = new File(templateSourceDirectory, templatePath);

      // accept if templateFile is newer or component has been modified
      return (templateFile.lastModified() > targetFile.lastModified() ||
              converter.isModifiedSince(targetFile.lastModified()));
    }
  }

  private class IfValidatorModifiedFilter extends ValidatorFilter
  {
    protected boolean accept(
      ValidatorBean validator)
    {
      String tagClass = validator.getTagClass();
      String sourcePath = Util.convertClassToSourcePath(tagClass, ".java");
      String templatePath = Util.convertClassToSourcePath(tagClass, "Template.java");
      File targetFile = new File(generatedSourceDirectory, sourcePath);
      File templateFile = new File(templateSourceDirectory, templatePath);

      // accept if templateFile is newer or component has been modified
      return (templateFile.lastModified() > targetFile.lastModified() ||
              validator.isModifiedSince(targetFile.lastModified()));
    }
  }

  /**
   * @parameter expression="${project}"
   * @required
   * @readonly
   */
  private MavenProject project;

  /**
   * @parameter
   * @required
   */
  private Map taglibs;

  /**
   * @parameter expression="META-INF/maven-faces-plugin/faces-config.xml"
   * @required
   * @readonly
   */
  private String resourcePath;

  /**
   * @parameter expression="src/main/conf"
   * @required
   */
  private File configSourceDirectory;

  /**
   * @parameter expression="src/main/java-templates"
   * @required
   */
  private File templateSourceDirectory;

  /**
   * @parameter expression="${project.build.directory}/maven-faces-plugin/main/java"
   * @required
   */
  private File generatedSourceDirectory;

  /**
   * @parameter expression="${project.build.directory}/maven-faces-plugin/main/resources"
   * @required
   */
  private File generatedResourcesDirectory;

  /**
   * @parameter
   * @required
   */
  private String packageContains;

  /**
   * @parameter
   */
  private boolean force;

  static private String _resolveType(
    String className)
  {
    return (String)_RESOLVABLE_TYPES.get(className);
  }

  static private Map _createResolvableTypes()
  {
    Map resolvableTypes = new HashMap();

    resolvableTypes.put("boolean", "Boolean");
    resolvableTypes.put("char", "Character");
    resolvableTypes.put("java.util.Date", "Date");
    resolvableTypes.put("int", "Integer");
    resolvableTypes.put("float", "Float");
    resolvableTypes.put("java.util.Locale", "Locale");
    resolvableTypes.put("long", "Long");
    resolvableTypes.put("java.lang.String", "String");
    resolvableTypes.put("java.lang.String[]", "StringArray");
    resolvableTypes.put("java.util.TimeZone", "TimeZone");

    return Collections.unmodifiableMap(resolvableTypes);
  }

  static final private Map _RESOLVABLE_TYPES = _createResolvableTypes();

  static final private String _JSP_TAG_LIBRARY_DOCTYPE_PUBLIC =
              "-//Sun Microsystems, Inc.//DTD JSP Tag Library 1.2//EN";

  static final private String _JSP_TAG_LIBRARY_DOCTYPE_SYSTEM =
              "http://java.sun.com/dtd/web-jsptaglibrary_1_2.dtd";

  static final private String _JSP_TAG_LIBRARY_DTD =
    "<!DOCTYPE taglib PUBLIC \n" +
    "  \"" + _JSP_TAG_LIBRARY_DOCTYPE_PUBLIC + "\"\n" +
    "  \"" + _JSP_TAG_LIBRARY_DOCTYPE_SYSTEM + "\" >\n";

  static final private String _XINCLUDE_JSP_TAG_LIBRARY_DTD =
    "<!DOCTYPE taglib PUBLIC\n" +
    "  \"" + _JSP_TAG_LIBRARY_DOCTYPE_PUBLIC + "\"\n" +
    "  \"" + _JSP_TAG_LIBRARY_DOCTYPE_SYSTEM + "\" [\n" +
    "      <!ELEMENT xi:include EMPTY>\n" +
    "      <!ATTLIST xi:include\n" +
    "          xmlns:xi CDATA #FIXED  \"" + XIncludeFilter.XINCLUDE_NAMESPACE + "\"\n" +
    "          href     CDATA #IMPLIED\n" +
    "          xpointer CDATA #IMPLIED>\n" +
    "]>\n";
}
