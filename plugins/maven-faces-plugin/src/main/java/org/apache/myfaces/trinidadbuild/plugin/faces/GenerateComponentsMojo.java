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
package org.apache.myfaces.trinidadbuild.plugin.faces;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.apache.myfaces.trinidadbuild.plugin.faces.io.PrettyWriter;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.ComponentBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.EventBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.EventRefBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.FacesConfigBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.FacetBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.parse.PropertyBean;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.ComponentFilter;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.FilteredIterator;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.PropertyFilter;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.SourceTemplate;
import org.apache.myfaces.trinidadbuild.plugin.faces.util.Util;

/**
 * @version $Id$
 * @requiresDependencyResolution compile
 * @goal generate-components
 * @phase generate-sources
 */
public class GenerateComponentsMojo extends AbstractFacesMojo
{
  /**
   * Execute the Mojo.
   */
  public void execute() throws MojoExecutionException
  {
    try
    {
      processIndex(project, resourcePath);
      _generateComponents();
    }
    catch (IOException e)
    {
      throw new MojoExecutionException("Error generating components", e);
    }
  }

  /**
   * Generates parsed components.
   */
  private void _generateComponents() throws IOException
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
      if (suppressListenerMethods)
        getLog().warn("Event listener methods will not be generated");

      Iterator components = facesConfig.components();
      components = new FilteredIterator(components, new SkipFilter());
      components = new FilteredIterator(components,
                                        new ComponentTypeFilter(typePrefix));

      // incremental unless forced
      if (!force)
        components = new FilteredIterator(components, new IfModifiedFilter());

      if (!components.hasNext())
      {
        getLog().info("Nothing to generate - all components are up to date");
      }
      else
      {
        int count = 0;
        while (components.hasNext())
        {
          _generateComponent((ComponentBean)components.next());
          count++;
        }
        getLog().info("Generated " + count + " component(s)");
      }
    }
  }

  /**
   * Generates a parsed component.
   *
   * @param component  the parsed component metadata
   */
  private void _generateComponent(
    ComponentBean component)
  {
    String fullClassName = component.getComponentClass();
    
    try
    {
      getLog().debug("Generating " + fullClassName);

      String sourcePath = Util.convertClassToSourcePath(fullClassName, ".java");
      File targetFile = new File(generatedSourceDirectory, sourcePath);

      StringWriter sw = new StringWriter();
      PrettyWriter out = new PrettyWriter(sw);

      String className = Util.getClassFromFullClass(fullClassName);
      String componentFamily = component.findComponentFamily();

      if (componentFamily == null)
      {
        getLog().error("Missing <component-family> for \"" +
                       fullClassName + "\"");
      }
      else
      {
        String packageName = Util.getPackageFromFullClass(fullClassName);
        String fullSuperclassName = component.findComponentSuperclass();
        String superclassName = Util.getClassFromFullClass(fullSuperclassName);

        // make class name fully qualified in case of collision
        if (superclassName.equals(className))
          superclassName = fullSuperclassName;

        // TODO: remove this bogosity
        if (superclassName.equals("UIXMenuHierarchy") ||
            superclassName.equals("UIXTable") ||
            superclassName.equals("UIXHierarchy") ||
            superclassName.equals("UIXMenuTree") ||
            className.equals("CoreTree"))
        {
          superclassName = fullSuperclassName;
        }


        String componentType = component.getComponentType();

        // Use template file if it exists
        String templatePath = Util.convertClassToSourcePath(fullClassName, "Template.java");
        File templateFile = new File(templateSourceDirectory, templatePath);

        SourceTemplate template = null;
        if (templateFile.exists())
        {
          getLog().debug("Using template " + templatePath);
          template = new SourceTemplate(templateFile);
          template.substitute(className + "Template", className);
          template.readPreface();
        }

        // header/copyright
        writePreamble(out);

        // package
        out.println("package " + packageName + ";");
        out.println();

        // imports
        _writeImports(out, template, packageName,
                      fullSuperclassName, superclassName,
                      component);

        // class
        _writeClassBegin(out, className, superclassName, component, template);

        // static final constants
        _writePropertyValueConstants(out, component);
        _writePropertyConstants(out, superclassName, component);
        _writeFacetConstants(out, component);
        _writeGenericConstants(out, componentFamily, componentType);

        // public constructors and methods
        _writeConstructor(out, component, Modifier.PUBLIC);

        // insert template code
        if (template != null)
        {
          template.writeContent(out);
          template.close();
        }

        _writeFacetMethods(out, component);
        _writePropertyMethods(out, component);

        if (!suppressListenerMethods)
          _writeListenerMethods(out, component);

        _writeGetFamily(out);

        // protected constructors and methods
        // TODO: reverse this order, to make protected constructor go first
        //       for now we want consistency with previous code generation
        _writeGetBeanType(out);
        _writeConstructor(out, component, Modifier.PROTECTED);

        // static initializer
        _writeTypeLock(out, component);

        _writeClassEnd(out);

        out.close();

        // delay write in case of error
        // timestamp should not be updated when an error occurs
        // delete target file first, because it is readonly
        targetFile.getParentFile().mkdirs();
        targetFile.delete();
        FileWriter fw = new FileWriter(targetFile);
        StringBuffer buf = sw.getBuffer();
        fw.write(buf.toString());
        fw.close();
        targetFile.setReadOnly();
      }
    }
    catch (IOException e)
    {
      getLog().error("Error generating " + fullClassName, e);
    }
  }

  private void _writeClassBegin(
    PrettyWriter   out,
    String         className,
    String         superclassName,
    ComponentBean  component,
    SourceTemplate template)
  {
    out.println("/**");

    // TODO: restore description (needs escaping?)
//    String description = component.getDescription();
//    if (description != null)
//    {
//      out.println(" *");
//      out.println(" * " + convertMultilineComment(description));
//    }

    String longDescription = component.getLongDescription();
    if (longDescription != null)
    {
      out.println(" *");
      out.println(" * " + convertMultilineComment(longDescription));
    }

    if (component.hasEvents(true))
    {
      // the events javadoc
      out.println(" *");
      out.println(" * <h4>Events:</h4>");
      out.println(" * <table border=\"1\" width=\"100%\" cellpadding=\"3\" summary=\"\">");
      out.println(" * <tr bgcolor=\"#CCCCFF\" class=\"TableHeadingColor\">");
      out.println(" * <th align=\"left\">Type</th>");
      out.println(" * <th align=\"left\">Phases</th>");
      out.println(" * <th align=\"left\">Description</th>");
      out.println(" * </tr>");
      Iterator events = component.events(true);
      while (events.hasNext())
      {
        EventRefBean eventRef = (EventRefBean)events.next();
        EventBean event = eventRef.resolveEventType();
        if (event != null)
        {
          String eventClass = event.getEventClass();
          String[] eventPhases = eventRef.getEventDeliveryPhases();
          String eventDescription = event.getDescription();
          out.println(" * <tr class=\"TableRowColor\">");
          out.println(" * <td valign=\"top\"><code>" + eventClass + "</code></td>");
          out.print(" * <td valign=\"top\" nowrap>");
          if (eventPhases != null)
          {
            for (int i=0; i < eventPhases.length; i++)
            {
              if (i > 0)
                out.print("<br>");
              out.print(eventPhases[i]);
            }
          }
          out.println("</td>");
          out.println(" * <td valign=\"top\">" + eventDescription + "</td>");
          out.println(" * </tr>");
        }
      }
      out.println(" * </table>");
    }

    if (!component.hasChildren())
    {
      out.println(" * <p>");
      out.println(" * It does not support any children.");
    }

    out.println(" */");

    // TODO: eliminate <mfp:component-class-modifier> metadata
    int modifiers = component.getComponentClassModifiers();
    String classStart = Modifier.toString(modifiers);
    // TODO: use canonical ordering
    classStart = classStart.replaceAll("public abstract", "abstract public");
    out.println(classStart + " class " + className +
                             " extends " + superclassName);

    Set interfaces = new HashSet();
    if (template != null)
      interfaces.addAll(template.getImplements());

    if (component.isNamingContainer())
      interfaces.add("javax.faces.component.NamingContainer");

    Iterator events = component.events();
    while (events.hasNext())
    {
      EventRefBean eventRef = (EventRefBean)events.next();
      EventBean event = eventRef.resolveEventType();
      if (event != null)
      {
        if (!eventRef.isIgnoreSourceInterface())
        {
          String source = event.getEventSourceInterface();
          if (source != null)
            interfaces.add(Util.getClassFromFullClass(source));
        }
      }
    }

    if (!interfaces.isEmpty())
    {
      Set implementsSet = new HashSet();
      for (Iterator iter=interfaces.iterator(); iter.hasNext();)
      {
        String fcqn = (String)iter.next();
        implementsSet.add(Util.getClassFromFullClass(fcqn));
      }

      // implements clause spans multiple lines
      char[] indent = new char[classStart.length() +
                               " class ".length() +
                               className.length() + 1];
      Arrays.fill(indent, ' ');
      out.print(indent);
      out.print("implements ");
      for (Iterator iter=implementsSet.iterator(); iter.hasNext();)
      {
        out.print((String)iter.next());
        if (iter.hasNext())
        {
          out.println(",");
          out.print(indent);
          out.print("           ");  // same length as "implements "
        }
      }
      out.println();
    }

    out.println("{");
    out.indent();
  }

  private void _writeClassEnd(
    PrettyWriter out)
  {
    out.unindent();
    out.println("}");
  }

  private void _writeImports(
    PrettyWriter   out,
    SourceTemplate template,
    String         packageName,
    String         fullSuperclassName,
    String         superclassName,
    ComponentBean  component)
  {
    Set imports = new TreeSet();

    // Use the template imports
    if (template != null)
      imports.addAll(template.getImports());

    // FacesBean is always needed to define the TYPE
    imports.add("org.apache.myfaces.trinidad.bean.FacesBean");

    // Detect NamingContainer
    if (component.isNamingContainer())
      imports.add("javax.faces.component.NamingContainer");

    Iterator properties = component.properties();
    properties = new FilteredIterator(properties, new NonVirtualFilter());
    // PropertyKey only needed if there are properties
    if (properties.hasNext())
    {
      imports.add("org.apache.myfaces.trinidad.bean.PropertyKey");

      PropertyFilter resolvable = new ResolvableTypeFilter();
      while (properties.hasNext())
      {
        PropertyBean property = (PropertyBean)properties.next();
        String propertyClass = property.getPropertyClass();
        if (propertyClass != null)
        {
          imports.add(propertyClass);
          // Check for generics
          String[] types = property.getAttributeClassParameters();
          if(types != null)
          {
            for(int i = types.length - 1; i >= 0; i--)
            {
              _addGenericImports(imports, types[i]);
            }
          }
        }

        // ComponentUtils only needed for resolvable properties
        if (resolvable.accept(property))
          imports.add("org.apache.myfaces.trinidad.util.ComponentUtils");
      }
    }

    Iterator facets = component.facets();
    // UIComponent needed if there are facets
    if (facets.hasNext())
      imports.add("javax.faces.component.UIComponent");

    Iterator events = component.events();
    while (events.hasNext())
    {
      EventRefBean eventRef = (EventRefBean)events.next();
      EventBean event = eventRef.resolveEventType();

      if (event == null)
      {
        getLog().warn("Unknown event type \"" + eventRef.getEventType() + "\""+
          " in component:"+component.getComponentType());
      }
      else
      {
        String listenerClass = event.getEventListenerClass();
        if (listenerClass != null)
          imports.add(listenerClass);

        if (!eventRef.isIgnoreSourceInterface())
        {
          String sourceInterface = event.getEventSourceInterface();
          if (sourceInterface != null)
            imports.add(sourceInterface);
        }
      }
    }

    // Import causes a collision if className and superclassName are equal
    if (!superclassName.equals(fullSuperclassName))
    {
      String superPackageName = Util.getPackageFromFullClass(fullSuperclassName);
      // component superclass only needed if not in
      // same package as component class
      if (superPackageName != packageName)
        imports.add(fullSuperclassName);
    }

    // do not import implicit types!
    imports.removeAll(Util.PRIMITIVE_TYPES);

    writeImports(out, packageName, imports);
  }
  
  private void _addGenericImports(Set imports, String type)
  {
    Matcher matcher = _GENERIC_TYPE.matcher(type);
    if(matcher.matches())
    {
      // Generic type
      imports.add(matcher.group(1));
      String[] types = matcher.group(2).split(",");
      for(int i = types.length - 1; i >= 0; i--)
      {
        _addGenericImports(imports, types[i]);
      }
    }
    else
    {
      // Non-generic type
      imports.add(type);
    }
  }

  private void _writeGenericConstants(
    PrettyWriter out,
    String       componentFamily,
    String       componentType) throws IOException
  {
    out.println();
    out.println("static public final String COMPONENT_FAMILY =");
    out.println("  \"" + componentFamily + "\";");
    out.println("static public final String COMPONENT_TYPE =");
    out.println("  \"" + componentType + "\";");
  }

  private void _writePropertyConstants(
    PrettyWriter   out,
    String         superclassName,
    ComponentBean  component) throws IOException
  {
    out.println("static public final FacesBean.Type TYPE = new FacesBean.Type(");
    out.indent();
    out.println(superclassName + ".TYPE);");
    out.unindent();

    //  component property keys
    Iterator properties = component.properties();
    properties = new FilteredIterator(properties, new NonVirtualFilter());
    while (properties.hasNext())
    {
      PropertyBean property = (PropertyBean)properties.next();
      String propName = property.getPropertyName();
      String propKey = Util.getConstantNameFromProperty(propName, "_KEY");
      String propAlias = property.getAliasOf();

      out.println("static public final PropertyKey " + propKey + " =");
      out.indent();
      if (propAlias != null)
      {
        String aliasKey = Util.getConstantNameFromProperty(propAlias, "_KEY");
        out.print("TYPE.registerAlias(" + aliasKey + ", \"" + propName + "\");");
      }
      else
      {
        out.print("TYPE.registerKey(\"" + propName + "\"");

        // property class
        String propFullClass = property.getPropertyClass();
        String propClass = Util.getClassFromFullClass(propFullClass);
        if (propClass == null)
        {
          propClass = "String";
        }
        String propDefault = property.getDefaultValue();

        if (!"Object".equals(propClass) || propDefault != null)
        {
          // TODO: do not use boxed class here
          String boxedClass = Util.getBoxedClass(propClass);
          out.print(", " + boxedClass + ".class");
        }

        if (propDefault != null)
        {
          out.print(", " + _convertStringToBoxedLiteral(propClass, propDefault));
        }

        // property capabilities
        String propCaps = _getPropertyCapabilities(property);
        if (propCaps != null)
          out.print(", " + propCaps);
        out.println(");");
      }
      out.unindent();
    }
  }

  private void _writePropertyValueConstants(
    PrettyWriter   out,
    ComponentBean  component) throws IOException
  {
    //  component property keys
    Iterator properties = component.properties();
    properties = new FilteredIterator(properties, new NonVirtualFilter());
    while (properties.hasNext())
    {
      PropertyBean property = (PropertyBean)properties.next();
      String[] propertyValues = property.getPropertyValues();

      if (propertyValues != null)
      {
        String propName = property.getPropertyName();

        for (int i=0; i < propertyValues.length; i++)
        {
          String propValue = propertyValues[i];
          String propValueName = propName +
                                 Character.toUpperCase(propValue.charAt(0)) +
                                 propValue.substring(1);
          String propValueKey = Util.getConstantNameFromProperty(propValueName);

          out.println("static public final String " + propValueKey + " = \"" + propValue + "\";");
        }

      }
    }
  }

  private void _writeFacetConstants(
    PrettyWriter  out,
    ComponentBean component) throws IOException
  {
    Iterator facets = component.facets();
    while (facets.hasNext())
    {
      FacetBean facet = (FacetBean)facets.next();
      String facetName = facet.getFacetName();
      String facetKey = Util.getConstantNameFromProperty(facetName, "_FACET");
      out.println("static public final " +
                    "String " + facetKey + " = \"" + facetName + "\";");
    }
  }

  private String _convertStringToBoxedLiteral(
    String  className,
    String  value)
  {
    if (value == null)
    {
      return null;
    }
    else if ("String".equals(className))
    {
      return "\"" + value.replaceAll("\'", "\\'") + "\"";
    }
    else if ("boolean".equals(className))
    {
      return ("true".equals(value)) ? "Boolean.TRUE" : "Boolean.FALSE";
    }
    else if ("char".equals(className))
    {
      return "Character.valueOf('" + value.replaceAll("\'", "\\'") + "')";
    }
    else if ("int".equals(className))
    {
      return "Integer.valueOf(" + value + ")";
    }
    else if ("short".equals(className))
    {
      return "Short.valueOf(" + value + ")";
    }
    else if ("long".equals(className))
    {
      return "Long.valueOf(" + value + ")";
    }
    else if ("double".equals(className))
    {
      return "Double.valueOf(" + value + ")";
    }
    else if ("float".equals(className))
    {
      return "Float.valueOf(" + value + ")";
    }
    else if ("Number".equals(className))
    {
      if(value.indexOf(".") == -1)
      {
        return "Integer.valueOf(" + value + ")";
      }
      else
      {
        return "Double.valueOf(" + value + ")";
      }
    }
    else
    {
      throw new IllegalStateException("property-class " + className + " not supported for auto-boxing");
    }
  }

  private String _convertVariableToBoxedForm(
    String  className,
    String  varName)
  {
    if ("boolean".equals(className))
    {
      return varName + " ? Boolean.TRUE : Boolean.FALSE";
    }
    else if ("char".equals(className))
    {
      return "Character.valueOf(" + varName + ")";
    }
    else if ("int".equals(className))
    {
      return "Integer.valueOf(" + varName + ")";
    }
    else if ("short".equals(className))
    {
      return "Short.valueOf(" + varName + ")";
    }
    else if ("long".equals(className))
    {
      return "Long.valueOf(" + varName + ")";
    }
    else if ("double".equals(className))
    {
      return "Double.valueOf(" + varName + ")";
    }
    else if ("float".equals(className))
    {
      return "Float.valueOf(" + varName + ")";
    }
    else
    {
      throw new IllegalStateException("property-class " + className + " not supported for auto-boxing");
    }
  }

  private void _writeConstructor(
    PrettyWriter   out,
    ComponentBean  component,
    int            modifiers) throws IOException
  {
    String fullClassName = component.getComponentClass();
    String className = Util.getClassFromFullClass(fullClassName);

    if (Modifier.isPublic(modifiers))
    {
      // TODO: eliminate this inconsistency
      if (!Modifier.isAbstract(component.getComponentClassModifiers()))
      {
        String rendererType = component.getRendererType();

        if (rendererType != null)
          rendererType = _convertStringToBoxedLiteral("String", rendererType);

        out.println();
        out.println("/**");
        // TODO: restore this correctly phrased comment (tense vs. command)
        //out.println(" * Constructs an instance of " + className + ".");
        out.println(" * Construct an instance of the " + className + ".");
        out.println(" */");
        out.println("public " + className + "()");
        out.println("{");
        out.indent();
        out.println("super(" + rendererType + ");");
        out.unindent();
        out.println("}");
      }
    }
    else if (Modifier.isProtected(modifiers))
    {
      out.println();
      out.println("/**");
      // TODO: restore this more descriptive comment with param docs
      //out.println(" * Construct an instance of the " + className);
      //out.println(" * with the specified renderer type.");
      //out.println(" * ");
      //out.println(" * @param rendererType  the renderer type");
      out.println(" * Construct an instance of the " + className + ".");
      out.println(" */");
      out.println("protected " + className + "(");
      out.indent();
      out.println("String rendererType");
      out.println(")");
      out.unindent();
      out.println("{");
      out.indent();
      out.println("super(rendererType);");
      out.unindent();
      out.println("}");

      // TODO: eliminate this inconsistency
      if (Modifier.isAbstract(component.getComponentClassModifiers()))
      {
        out.println();
        out.println("/**");
        // TODO: restore this correctly phrased comment (tense vs. command)
        //out.println(" * Constructs an instance of " + className + ".");
        out.println(" * Construct an instance of the " + className + ".");
        out.println(" */");
        out.println("protected " + className + "()");
        out.println("{");
        out.indent();
        out.println("this(null);");
        out.unindent();
        out.println("}");
      }
    }
  }

  private void _writeGetFamily(
    PrettyWriter out) throws IOException
  {
    out.println();
    out.println("@Override");
    out.println("public String getFamily()");
    out.println("{");
    out.indent();
    out.println("return COMPONENT_FAMILY;");
    out.unindent();
    out.println("}");
  }

  private void _writeGetBeanType(
    PrettyWriter out) throws IOException
  {
    out.println();
    out.println("@Override");
    out.println("protected FacesBean.Type getBeanType()");
    out.println("{");
    out.indent();
    out.println("return TYPE;");
    out.unindent();
    out.println("}");
  }

  private void _writePropertyMethods(
   PrettyWriter  out,
   ComponentBean component) throws IOException
  {
    Iterator properties = component.properties();
    properties = new FilteredIterator(properties, new NonVirtualFilter());
    while (properties.hasNext())
    {
      PropertyBean property = (PropertyBean)properties.next();
      if (property.isList())
        _writePropertyListMethods(out, property);
      else
      {
        _writePropertyGet(out, property);
        _writePropertySet(out, property);
      }
    }
  }

  private void _writePropertyListMethods(
   PrettyWriter  out,
   PropertyBean  property) throws IOException
  {
    String propName = property.getPropertyName();
    String propKey = Util.getConstantNameFromProperty(propName, "_KEY");
    String propertyClass = property.getPropertyClass();
    if (!"java.util.List".equals(propertyClass))
    {
      getLog().error("Invalid list type: " + propertyClass);
      return;
    }

    // Look for the generic type - if it doesn't exist, then
    // we'll be an Object.
    String[] params = property.getPropertyClassParameters();
    if ((params == null) || (params.length == 0))
      propertyClass = "java.lang.Object";
    else
      propertyClass = params[0];

    propertyClass = Util.getClassFromFullClass(propertyClass);

    String singularName = _getSingular(propName);
    String propVar = Util.getVariableFromName(singularName);
    String description = property.getDescription();
    String addMethod = Util.getPrefixedPropertyName("add", singularName);
    String removeMethod = Util.getPrefixedPropertyName("remove", singularName);
    String getMethod = Util.getPrefixedPropertyName("get", propName);

    out.println();
    out.println("/**");
    if (description != null)
    {
      out.println(" * Adds a " + convertMultilineComment(description));
    }
    out.println(" */");
    out.println("final public void " + addMethod + "(" + propertyClass + " " +
                propVar + ")");
    out.println("{");
    out.indent();
    out.println("if (" + propVar + " == null)");
    out.println("  throw new NullPointerException();");
    out.println();
    out.println("getFacesBean().addEntry(" + propKey + ", " + propVar + ");");
    out.unindent();
    out.println("}");

    out.println();
    out.println("/**");
    if (description != null)
    {
      out.println(" * Removes a " + convertMultilineComment(description));
    }
    out.println(" */");
    out.println("final public void " + removeMethod + "(" + propertyClass + " " +
                propVar + ")");
    out.println("{");
    out.indent();
    out.println("if (" + propVar + " == null)");
    out.println("  throw new NullPointerException();");
    out.println();
    out.println("getFacesBean().removeEntry(" + propKey + ", " + propVar + ");");
    out.unindent();
    out.println("}");

    out.println();
    out.println("/**");
    if (description != null)
    {
      out.println(" * Gets all " + convertMultilineComment(description));
    }
    out.println(" */");
    out.println("final public " + propertyClass + "[] " + getMethod + "()");
    out.println("{");
    out.indent();
    out.println("return (" + propertyClass + "[]) getFacesBean().getEntries(");
    out.println("         " + propKey + ", " + propertyClass + ".class);");
    out.unindent();
    out.println("}");
  }

  static private String _getSingular(String plural)
  {
    if (plural.endsWith("s"))
      return plural.substring(0, plural.length() - 1);
    return plural;
  }

  private void _writePropertySet(
   PrettyWriter  out,
   PropertyBean  property) throws IOException
  {
    String propertyClass = Util.getPropertyClass(property);
    _writePropertySet(out, property, propertyClass);

    if (property.getAlternateClass() != null)
    {
      String alternateClass = Util.getAlternatePropertyClass(property);
      _writePropertySet(out, property, alternateClass);
    }
  }

  private void _writePropertySet(
   PrettyWriter  out,
   PropertyBean  property,
   String        propertyClass) throws IOException
  {
    String propName = property.getPropertyName();
    String propKey = Util.getConstantNameFromProperty(propName, "_KEY");
    String propVar = Util.getVariableFromName(propName);
    String description = property.getDescription();
    String setMethod = Util.getPrefixedPropertyName("set", propName);

    out.println();
    out.println("/**");
    if (description != null)
    {
      out.println(" * Sets " + convertMultilineComment(description));
    }
    // TODO: restore this comment.
//    if (property.isRequired())
//    {
//      out.println(" * <p>");
//      out.println(" * This is a required property on the component.");
//    }
    // TODO: put this back in
    //out.println(" * ");
    //out.println(" * @param " + propName + "  the new " + propName + " value");
    out.println(" */");

    out.println("final public void " + setMethod + "(" + propertyClass + " " + propVar + ")");
    out.println("{");
    out.indent();
    if (Util.isPrimitiveClass(propertyClass))
    {
      out.println("setProperty(" + propKey + ", " +
                  _convertVariableToBoxedForm(propertyClass, propVar) +
                  ");");
    }
    else
    {
      out.println("setProperty(" + propKey + ", (" + propVar + "));");
    }
    out.unindent();
    out.println("}");
  }

  private void _writePropertyGet(
   PrettyWriter  out,
   PropertyBean  property) throws IOException
  {
    String propName = property.getPropertyName();
    String propKey = Util.getConstantNameFromProperty(propName, "_KEY");
    String propertyFullClass = property.getPropertyClass();
    String propertyClass = Util.getClassFromFullClass(propertyFullClass);
    String description = property.getDescription();
    String getMethod = Util.getMethodReaderFromProperty(propName, propertyClass);
    
    boolean isUnchecked = false;
    String[] genericTypes = property.getPropertyClassParameters();
    if(genericTypes != null && genericTypes.length > 0)
    {
      isUnchecked = true;
      propertyClass = Util.getPropertyClass(property);
    }
    
    out.println();
    out.println("/**");
    if (description != null)
    {
      out.println(" * Gets " + convertMultilineComment(description));
    }
    if (property.isRequired())
    {
      out.println(" * <p>");
      out.println(" * This is a required property on the component.");
      out.println(" * </p>");
    }
    // TODO: put this back in
    //out.println(" *");
    //out.println(" * @return  the new " + propName + " value");
    out.println(" */");

    if(isUnchecked)
    {
      out.println("@SuppressWarnings(\"unchecked\")");
    }
    
    out.println("final public " + propertyClass + " " + getMethod + "()");
    out.println("{");
    out.indent();

    String resolvableType = _resolveType(propertyFullClass);
    if (resolvableType != null)
    {
      // TODO: change signature of ComponentUtils.resolveCharacter
      //       to take Object instead of Character
      if (resolvableType.equals("Character"))
      {
        out.println("return ComponentUtils.resolveCharacter((Character)getProperty(" + propKey + "));");
      }
      else
      {
        // TODO: stop specifying default values in the getters
        String resolveMethod = Util.getPrefixedPropertyName("resolve", resolvableType);
        String propertyDefault = property.getDefaultValue();
        out.print("return ComponentUtils." + resolveMethod + "(getProperty(" + propKey + ")");
        if (propertyDefault != null)
        {
          out.print(", " + convertStringToLiteral(propertyClass,
                                                   propertyDefault));
        }
        out.println(");");
      }
    }
    else
    {
      if(propertyClass.equals("Object"))
      {
        // Cast is not necessary if the property class is Object
        out.println("return getProperty(" + propKey + ");");
      }
      else
      {
        out.println("return (" + propertyClass + ")" +
                    "getProperty(" + propKey + ");");
      }
    }
    out.unindent();
    out.println("}");
  }
  
  private void _writeFacetMethods(
   PrettyWriter  out,
   ComponentBean component) throws IOException
  {
    Iterator facets = component.facets();
    while (facets.hasNext())
    {
      FacetBean facet = (FacetBean)facets.next();
      _writeFacetGet(out, facet);
      _writeFacetSet(out, facet);
    }
  }

  private void _writeFacetSet(
   PrettyWriter  out,
   FacetBean     facet) throws IOException
  {
    String facetName = facet.getFacetName();
    // TODO: drop the unnecessary "Facet" suffix
    String facetVar = facetName + "Facet";
    String facetKey = Util.getConstantNameFromProperty(facetName, "_FACET");
    String setMethod = Util.getPrefixedPropertyName("set", facetName);
    String description = facet.getDescription();

    out.println();
    out.println("/**");
    if (description != null)
    {
      out.println(" * " + convertMultilineComment(description));
    }
    if (facet.isRequired())
    {
      out.println(" * <p>");
      out.println(" * This is a required facet on the component.");
    }
    // TODO: put this back in
    //out.println(" * ");
    //out.println(" * @param " + facetVar + "  the new " + facetName + " facet");
    out.println(" */");

    // Remove type safety warning since getFacets is not generics enabled 
    // under JSF 1.1 spec
    // TODO: Remove this line when Trinidad switch to JSF 1.2
    out.println("@SuppressWarnings(\"unchecked\")");
    
    out.println("final public void " + setMethod + "(UIComponent " + facetVar + ")");
    out.println("{");
    out.indent();
    out.println("getFacets().put(" + facetKey + ", " + facetVar + ");");
    out.unindent();
    out.println("}");
  }

  private void _writeFacetGet(
   PrettyWriter  out,
   FacetBean     facet) throws IOException
  {
    String facetName = facet.getFacetName();
    String facetKey = Util.getConstantNameFromProperty(facetName, "_FACET");
    String getMethod = Util.getPrefixedPropertyName("get", facetName);
    String description = facet.getDescription();

    out.println();
    out.println("/**");
    if (description != null)
    {
      out.println(" * " + convertMultilineComment(description));
    }
    if (facet.isRequired())
    {
      out.println(" * <p>");
      out.println(" * This is a required facet on the component.");
    }
    // TODO: put this back in
    //out.println(" * ");
    //out.println(" * @return  the " + facetName + " facet");
    out.println(" */");

    out.println("final public UIComponent " + getMethod + "()");
    out.println("{");
    out.indent();
    out.println("return getFacet(" + facetKey + ");");
    out.unindent();
    out.println("}");
  }

  private void _writeListenerMethods(
    PrettyWriter  out,
    ComponentBean component) throws IOException
  {
    Iterator events = component.events();
    while (events.hasNext())
    {
      EventRefBean eventRef = (EventRefBean)events.next();
      EventBean event = eventRef.resolveEventType();
      if (event != null)
      {
        _writeListenerAdd(out, event);
        _writeListenerRemove(out, event);
        _writeListenersGet(out, event);
      }
    }
  }

  private void _writeListenerAdd(
    PrettyWriter  out,
    EventBean     event) throws IOException
  {
    String listenerFullClass = event.getEventListenerClass();
    String listenerClass = Util.getClassFromFullClass(listenerFullClass);

    String eventName = event.getEventName();
    String addMethod = Util.getMethodNameFromEvent("add", eventName, "Listener");

    out.println();
    out.println("/**");
    out.println(" * Adds a " + eventName + " listener.");
    out.println(" *");
    out.println(" * @param listener  the " + eventName + " listener to add");
    out.println(" */");

    out.println("final public void " + addMethod + "(");
    out.indent();
    out.println(listenerClass + " listener)");
    out.unindent();
    out.println("{");
    out.indent();
    out.println("addFacesListener(listener);");
    out.unindent();
    out.println("}");
  }

  private void _writeListenerRemove(
    PrettyWriter  out,
    EventBean     event) throws IOException
  {
    String listenerFullClass = event.getEventListenerClass();
    String listenerClass = Util.getClassFromFullClass(listenerFullClass);

    String eventName = event.getEventName();
    String removeMethod = Util.getMethodNameFromEvent("remove", eventName, "Listener");

    out.println();
    out.println("/**");
    out.println(" * Removes a " + eventName + " listener.");
    out.println(" *");
    out.println(" * @param listener  the " + eventName + " listener to remove");
    out.println(" */");

    out.println("final public void " + removeMethod + "(");
    out.indent();
    out.println(listenerClass + " listener)");
    out.unindent();
    out.println("{");
    out.indent();
    out.println("removeFacesListener(listener);");
    out.unindent();
    out.println("}");
  }

  private void _writeListenersGet(
    PrettyWriter  out,
    EventBean     event) throws IOException
  {
    String listenerFullClass = event.getEventListenerClass();
    String listenerClass = Util.getClassFromFullClass(listenerFullClass);

    String eventName = event.getEventName();
    String getMethod = Util.getMethodNameFromEvent("get", eventName, "Listeners");

    out.println();
    out.println("/**");
    out.println(" * Returns an array of attached " + eventName + " listeners.");
    out.println(" *");
    out.println(" * @return  an array of attached " + eventName + " listeners.");
    out.println(" */");

    out.println("final public " + listenerClass + "[] " + getMethod + "()");
    out.println("{");
    out.indent();
    out.println("return (" + listenerClass + "[])" +
                        "getFacesListeners(" +  listenerClass + ".class);");
    out.unindent();
    out.println("}");
  }

  private void _writeTypeLock(
    PrettyWriter out, ComponentBean component) throws IOException
  {
    out.println();
    out.println("static");
    out.println("{");
    out.indent();
    String rendererType = component.getRendererType();
    if (rendererType == null)
    {
      out.println("TYPE.lock();");
    }
    else
    {
      String componentFamily = component.findComponentFamily();
      out.println("TYPE.lockAndRegister(\"" + componentFamily + "\"," +
                  "\"" + rendererType + "\");");
    }

    out.unindent();
    out.println("}");
  }

  private String _getPropertyCapabilities(
    PropertyBean property)
  {
    List caps = new ArrayList();

    if (property.isMethodBinding() ||
        property.isLiteralOnly())
    {
      caps.add("PropertyKey.CAP_NOT_BOUND");
    }

    if (property.isStateHolder())
    {
      caps.add("PropertyKey.CAP_STATE_HOLDER");
    }

    if (property.isTransient())
    {
      caps.add("PropertyKey.CAP_TRANSIENT");
    }

    if (property.isList())
    {
      caps.add("PropertyKey.CAP_LIST");
    }

    if (caps.isEmpty())
      return null;

    StringBuffer sb = new StringBuffer();
    for (int i=0; i < caps.size(); i++)
    {
      if (i > 0)
        sb.append(" | ");
      sb.append(caps.get(i));
    }
    return sb.toString();
  }

  private class NonVirtualFilter extends PropertyFilter
  {
    protected boolean accept(
      PropertyBean property)
    {
      return (!property.isVirtual());
    }
  }

  private class ResolvableTypeFilter extends PropertyFilter
  {
    protected boolean accept(
      PropertyBean property)
    {
      String propertyClass = property.getPropertyClass();
      String resolvableType = _resolveType(propertyClass);
      return (resolvableType != null);
    }
  }

  private class IfModifiedFilter extends ComponentFilter
  {
    protected boolean accept(
      ComponentBean component)
    {
      String componentClass = component.getComponentClass();
      String sourcePath = Util.convertClassToSourcePath(componentClass, ".java");
      String templatePath = Util.convertClassToSourcePath(componentClass, "Template.java");
      File targetFile = new File(generatedSourceDirectory, sourcePath);
      File templateFile = new File(templateSourceDirectory, templatePath);

      // accept if templateFile is newer or component has been modified
      return (templateFile.lastModified() > targetFile.lastModified() ||
              component.isModifiedSince(targetFile.lastModified()));
    }
  }

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
    // TODO: put this back in
    //resolvableTypes.put("java.util.Date", "Date");
    resolvableTypes.put("int", "Integer");
    resolvableTypes.put("float", "Float");
    resolvableTypes.put("double", "Double");
    resolvableTypes.put("java.util.Locale", "Locale");
    resolvableTypes.put("long", "Long");
    resolvableTypes.put("java.lang.String", "String");
    // TODO: put this back in
    //resolvableTypes.put("java.lang.String[]", "StringArray");
    resolvableTypes.put("java.util.TimeZone", "TimeZone");

    return Collections.unmodifiableMap(resolvableTypes);
  }

  /**
   * @parameter expression="${project}"
   * @readonly
   */
  private MavenProject project;

  /**
   * @parameter
   * @readonly
   */
  private String resourcePath = "META-INF/maven-faces-plugin/index.lst";

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
   * @parameter
   * @required
   */
  private String packageContains;

  /**
   * @parameter
   * @required
   */
    private String typePrefix;

  /**
   * @parameter
   */
  private boolean force;

  /**
   * @parameter
   */
  private boolean suppressListenerMethods;

  static private final Pattern _GENERIC_TYPE = Pattern.compile("([^<]+)<(.+)>");
  static final private Map _RESOLVABLE_TYPES = _createResolvableTypes();
}
