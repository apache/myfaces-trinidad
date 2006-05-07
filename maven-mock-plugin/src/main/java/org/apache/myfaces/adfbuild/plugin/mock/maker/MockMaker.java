package org.apache.myfaces.adfbuild.plugin.mock.maker;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintWriter;

import java.text.MessageFormat;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

import org.apache.myfaces.adfbuild.plugin.mock.maker.structure.ClassStructure;
import org.apache.myfaces.adfbuild.plugin.mock.maker.structure.MethodStructure;
import org.apache.myfaces.adfbuild.plugin.mock.maker.structure.ReflectionClassStructure;

/**
 * Title:        MockMaker
 * Description:  Creates MockObject class from interface.
 * Copyright:    Copyright (c) 2000, 2001 Ivan Moore with contributions from Scott Vlaminck and Henrik Karlsson
 * @author       <a href="mailto:mockivan@tadmad.co.uk">Ivan Moore</a>
 * @author       <a href="mailto:mockmaker@tadmad.co.uk">Matt Cooke</a>
 * @author       <a href="mailto:scott@vlaminck.com">Scott Vlaminck</a>
 * @author       <a href="mailto:maliksalman@hotmail.com">Salman Malik</a>
 * @author       Henrik Karlsson
 * @author       <a href="mailto:joew@thoughtworks.com">Joe Walnes</a>
 * @version 1.12
 */
/**
  COPYRIGHT AND PERMISSION NOTICE

  Copyright (c) 2000, 2001, 2002

  All rights reserved.

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
  associated documentation files (the "Software"), to deal in the Software without restriction, including
  without limitation the rights to use, copy, modify, merge, publish, distribute, and/or sell copies of the
  Software, and to permit persons to whom the Software is furnished to do so, provided that the
  above copyright notice(s) and this permission notice appear in all copies of the Software and that
  both the above copyright notice(s) and this permission notice appear in supporting documentation.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT OF THIRD PARTY RIGHTS. IN NO EVENT SHALL THE
  COPYRIGHT HOLDER OR HOLDERS INCLUDED IN THIS NOTICE BE LIABLE FOR
  ANY CLAIM, OR ANY SPECIAL INDIRECT OR CONSEQUENTIAL DAMAGES, OR
  ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
  PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
  PERFORMANCE OF THIS SOFTWARE.

  Except as contained in this notice, the name of a copyright holder shall not be used in advertising or
  otherwise to promote the sale, use or other dealings in this Software without prior written
  authorization of the copyright holder.
**/
public class MockMaker {

  public static final String VERSION = "1.12";

  private ClassLoader loader = null;
  private String interfaceToMakeMockFor;
  private CodeWriter output;
  private ClassStructure iface;
  private Vector instanceVariables = new Vector();
  private boolean includePackage;


  /**
   * Standard Constructor.
   */
  public MockMaker(
    String interfaceToMakeMockFor,
    CodeWriter output,
    Configuration aConfiguration) {
    this(interfaceToMakeMockFor, output, aConfiguration, null);
  }

  /**
   * Use an optional ClassLoader
   */
  public MockMaker(
    String interfaceToMakeMockFor,
    CodeWriter output,
    Configuration aConfiguration,
    ClassLoader classLoader) {
    super();
    this.interfaceToMakeMockFor = interfaceToMakeMockFor;
    this.output = output;
    myConfiguration = aConfiguration;
    loader = classLoader;
  }

  /**
   * Build from source code.
   */
  public MockMaker(
    ClassStructure iface,
    CodeWriter output,
    Configuration aConfiguration) {
    this.iface = iface;
    this.output = output;
    myConfiguration = aConfiguration;
  }

  /**
   * Make the MockObject.
   */
  public void make() throws ClassNotFoundException {
    if (iface == null) {
      iface = new ReflectionClassStructure(Class.forName(interfaceToMakeMockFor, true, getValidLoader()));
    }
    writeMock();
  }

  /**
   * Set our ClassLoader.
   */
  public void setClassLoader(ClassLoader classLoader) {
    loader = classLoader;
  }

  /**
   * Return our ClassLoader.
   */
  public ClassLoader getClassLoader() {
    return loader;
  }

  /**
   * Return a valid (non-null) ClassLoader.
   * This substitutes ClassLoader.getSystemClassLoader() for a null ClassLoader.
   * @return ClassLoader
   */
  public ClassLoader getValidLoader() {
    ClassLoader validLoader = getClassLoader();
    if (validLoader == null)
      validLoader = ClassLoader.getSystemClassLoader();
    return validLoader;
  }

  String capitalizedName(MethodStructure method) {
    String name = method.getName();
    String capitalizedName = capitalized(name);
    if (isOverloaded(name)) {
      return capitalizedName + parameterTypeNames(method);
    } else {
      return capitalizedName;
    }
  }

  String getLastPartOfName(ClassStructure aClass) {
    return getLastPartOfName(aClass, "[]");
  }

  String getLastPartOfName(String fullyQualifiedName) {
    int index = fullyQualifiedName.lastIndexOf(".");
    if (index == -1) {
      return fullyQualifiedName;
    }
    return fullyQualifiedName.substring(index + 1);
  }

  public static void main(String[] args) throws ClassNotFoundException, FileNotFoundException {
    if (args.length == 1) {
      PrintWriter pw = new PrintWriter(System.out);
      MockMaker mm = new MockMaker(args[0], new RealCodeWriter(pw), new RealConfiguration(RealConfiguration.DEFAULT_CONFIGURATION_FILENAME));
      mm.make();
    } else if (args.length == 3) {
      if (!args[0].equals("-o")) {
        printUsage();
        System.exit(1);
      } else {
        PrintWriter pw = new PrintWriter(new FileOutputStream(args[1]));
        MockMaker mm = new MockMaker(args[2], new RealCodeWriter(pw), new RealConfiguration(RealConfiguration.DEFAULT_CONFIGURATION_FILENAME));
        mm.make();
      }
    } else {
      printUsage();
      System.exit(1);
    }
  }


  static void printUsage() {
    System.out.println("MockMaker version " + VERSION + "\n");
    System.out.println("Usage: java mockmaker.MockMaker [-o output-file] some.package.SomeClassFile\n");
    System.out.println("Mockmaker will then create a mock object based on 'SomeClassFile'");
    System.out.println("and optionaly direct the output to output-file\n");
    System.out.println("For more information see : http://mockmaker.sourceforge.net");
    System.out.println("Copyright (2000 - 2002) Ivan Moore, Matthew Cooke. Read the license.");
  }


  String[] paramsFor(MethodStructure method) {
    ClassStructure[] paramTypes = method.getParameterTypes();
    String[] result = new String[paramTypes.length];
    for (int i = 0; i < paramTypes.length; i++) {
      result[i] = getLastPartOfName(paramTypes[i]) + " arg" + i;
    }
    return result;
  }

  private String getMethodSignature(MethodStructure method) {
    ClassStructure[] paramTypes = method.getParameterTypes();
    String[] result = new String[paramTypes.length];
    for (int i = 0; i < paramTypes.length; i++) {
      result[i] = getLastPartOfName(paramTypes[i]);
    }
    return nameOfMockClass() + "." + method.getName() + "(" + toCommaSeparatedList(result) + ")";
  }

  void writeCallsIV(MethodStructure method) {
    String name = capitalizedName(method);
    String ivName = "my" + name + "Calls";
    instanceVariables.addElement(ivName);
    output.writeInstanceVariableDeclaration(
      "ExpectationCounter",
      ivName,
      "new ExpectationCounter(\"" + getMethodSignature(method) + "\")");
  }
  void writeImplementingMethod(MethodStructure method) {
    String name = capitalizedName(method);
    String returnType = getLastPartOfName(method.getReturnType());
    String[] params = paramsFor(method);
    Vector statementsV = new Vector();
    statementsV.addElement("my" + name + "Calls.inc();");
    ClassStructure[] paramTypes = method.getParameterTypes();
    for (int i = 0; i < paramTypes.length; i++) {
      statementsV.addElement(
        "my"
          + name
          + "Parameter"
          + i
          + "Values.addActual("
          + wrappedArg(paramTypes[i], "arg" + i)
          + ");");
    }
    String[] exceptions = exceptionsFor(method);
    String returnValueIV = "myActual" + name + "ReturnValues";
    statementsV.addElement(
      "Object nextReturnValue = " + returnValueIV + ".getNext();");
        for ( int i = 0; i < exceptions.length; i++ )
        {
            statementsV.addElement( "if (nextReturnValue instanceof ExceptionalReturnValue && ((ExceptionalReturnValue)nextReturnValue).getException() instanceof " + exceptions[i] + ")" );
            statementsV.addElement( " throw (" + exceptions[i] + ")((ExceptionalReturnValue)nextReturnValue).getException();");
        }
        statementsV.addElement( "if (nextReturnValue instanceof ExceptionalReturnValue && ((ExceptionalReturnValue)nextReturnValue).getException() instanceof RuntimeException)" );
        statementsV.addElement( " throw (RuntimeException)((ExceptionalReturnValue)nextReturnValue).getException();");

        if ( !returnType.equals("void") ) {
            if ( PRIMITIVE_TYPES.containsKey(returnType)) {
                statementsV.addElement(
                    "return (("
                        + PRIMITIVE_TYPES.get(returnType)
                        + ") nextReturnValue)."
                        + returnType
                        + "Value();");
            } else {
                statementsV.addElement("return (" + returnType + ") nextReturnValue;");
            }
        }
    String[] statements = new String[statementsV.size()];
    statementsV.copyInto(statements);
    output.writeMethodDeclarationThrowsExceptions(
      returnType,
      method.getName(),
      params,
      exceptionsFor(method),
      statements);
  }
  void writeIVs(MethodStructure method) {
    writeCallsIV(method);
    writeReturnValueIV(method);
    writeParameterValueIVs(method);
  }
  void writeMethods(MethodStructure method) {
    writeSetExpectedCallsMethod(method);
    writeSetExpectedValuesMethod(method);
    writeImplementingMethod(method);
    writeSetActualMethod(method);
  }
  void writeMock() {
    if (includePackage) {
      output.writePackage(packageNameString());
    }
    output.writeImport("mockmaker.ReturnValues");
    output.writeImport("mockmaker.VoidReturnValues");
    output.writeImport("mockmaker.ExceptionalReturnValue");
    output.writeImport("com.mockobjects.*");
    writeImportsFromMethods();
    writeClassDeclaration();
    MethodStructure[] methods = getMethods();
    for (int i = 0; i < methods.length; i++) {
      writeIVs(methods[i]);
    }
    for (int i = 0; i < methods.length; i++) {
      writeMethods(methods[i]);
    }
    writeVerify();
    writeConstructors();
    output.finish();
  }
  void writeImportsFromMethods() {
    MethodStructure[] methods = getMethods();
    Vector importClasses = new Vector();
    // importing the class that we are mocking
    addClassForImport(importClasses, iface);
    // go through all methods and add the referenced classes
    for (int x = 0; x < methods.length; x++) {
      MethodStructure m = methods[x];
      // importing the return value class
      ClassStructure returnClass = m.getReturnType();

      addClassForImport(importClasses, returnClass);
      // importing all parameter classes
      ClassStructure[] paramClasses = m.getParameterTypes();
      for (int i = 0; i < paramClasses.length; i++)
        addClassForImport(importClasses, paramClasses[i]);
      // importing all exception classes
      ClassStructure[] excpetionClasses = m.getExceptions();
      for (int i = 0; i < excpetionClasses.length; i++)
        addClassForImport(importClasses, excpetionClasses[i]);
    }
    for (int x = 0; x < importClasses.size(); x++) {
      ClassStructure c = (ClassStructure) importClasses.elementAt(x);

      output.writeImport(c.getName());

    }
  }
  private void addClassForImport(Vector list, ClassStructure classToAdd) {
    /* no need to add primitives and anything in java.lang.*  */
    if (!classToAdd.isPrimitive()
      && !classToAdd.getName().startsWith("java.lang.")) {
      /* duplicates are not needed */
      if (!list.contains(classToAdd))
        list.addElement(classToAdd);
    }
  }
  void writeParameterValueIVs(MethodStructure method) {
    String name = capitalizedName(method);
    ClassStructure[] paramTypes = method.getParameterTypes();
    for (int i = 0; i < paramTypes.length; i++) {
      String ivName = "my" + name + "Parameter" + i + "Values";
      instanceVariables.addElement(ivName);
      String expectationSuffix = paramTypes[i].getExpectationName(name);
      output.writeInstanceVariableDeclaration(
        "ExpectationList",
        ivName,
        "new ExpectationList(\""
          + getMethodSignature(method)
          + expectationSuffix
          + "\")");
    }
  }
  void writeReturnValueIV(MethodStructure method) {
    String name = capitalizedName(method);
    String ivName = "myActual" + name + "ReturnValues";
    String type = method.getReturnType().getName().equals("void") ? "VoidReturnValues" : "ReturnValues";
    output.writeInstanceVariableDeclaration(
      "ReturnValues",
      ivName,
      "new " + type + "(\"" + getMethodSignature(method) + "\", " + myConfiguration.keepUsingLastReturnValue() + ")");
  }
  void writeSetActualMethod(MethodStructure method) {
    ClassStructure type = method.getReturnType();
    String returnType = getLastPartOfName(method.getReturnType());
        String name = capitalizedName(method);
        output.writeMethodDeclaration(
      "void",
      setActualReturnExceptionString(name),
      new String[] { "Throwable arg" },
      new String[] {
         "myActual" + name + "ReturnValues.add(new ExceptionalReturnValue(arg));" });
    if (returnType.equals("void")) {
      return;
    }
    output.writeMethodDeclaration(
      "void",
      setActualReturnValueString(name),
      new String[] { returnType + " arg" },
      new String[] {
         "myActual" + name + "ReturnValues.add(" + wrappedArg(type, "arg") + ");" });
  }
  void writeSetExpectedCallsMethod(MethodStructure method) {
    String name = capitalizedName(method);
    output.writeMethodDeclaration(
      "void",
      setExpectedCallsString(name),
      new String[] { "int calls" },
      new String[] { "my" + name + "Calls.setExpected(calls);" });
  }
  void writeSetExpectedValuesMethod(MethodStructure method) {
    ClassStructure[] paramTypes = method.getParameterTypes();
    if (paramTypes.length == 0) {
      return;
    }
    String name = capitalizedName(method);
    String[] params = paramsFor(method);
    Vector statementsV = new Vector();
    for (int i = 0; i < paramTypes.length; i++) {
      statementsV.addElement(
        "my"
          + name
          + "Parameter"
          + i
          + "Values.addExpected("
          + wrappedArg(paramTypes[i], "arg" + i)
          + ");");
    }
    String[] statements = new String[statementsV.size()];
    statementsV.copyInto(statements);
    output.writeMethodDeclaration(
      "void",
      addExpectedValuesString(name),
      params,
      statements);
  }
  void writeVerify() {
    Vector statementsV = new Vector();
    Enumeration e = instanceVariables.elements();
    while (e.hasMoreElements()) {
      Object ivName = e.nextElement();
      statementsV.addElement(ivName + ".verify();");
    }
    String[] statements = new String[statementsV.size()];
    statementsV.copyInto(statements);
    output.writeMethodDeclaration("void", "verify", new String[0], statements);
  }
  private Configuration myConfiguration;
  private static final Hashtable PRIMITIVE_TYPES = new Hashtable();
  static {
    PRIMITIVE_TYPES.put("int", "Integer");
    PRIMITIVE_TYPES.put("long", "Long");
    PRIMITIVE_TYPES.put("short", "Short");
    PRIMITIVE_TYPES.put("boolean", "Boolean");
    PRIMITIVE_TYPES.put("byte", "Byte");
    PRIMITIVE_TYPES.put("double", "Double");
    PRIMITIVE_TYPES.put("float", "Float");
    PRIMITIVE_TYPES.put("char", "Character");
  }
  private String addExpectedValuesString(String methodName) {
    return MessageFormat.format(
      myConfiguration.addExpectedValuesFormat(),
      new String[] { methodName });
  }
  private String capitalized(String aString) {
    String firstChar = aString.substring(0, 1);
    firstChar = firstChar.toUpperCase();
    return firstChar + aString.substring(1);
  }
  String[] exceptionsFor(MethodStructure method) {
    ClassStructure[] exceptionTypes = method.getExceptions();
    String[] result = new String[exceptionTypes.length];
    for (int i = 0; i < exceptionTypes.length; i++) {
      result[i] = getLastPartOfName(exceptionTypes[i]);
    }
    return result;
  }
  private String getLastPartOfName(ClassStructure aClass, String arrayString) {
    String result = aClass.getName();
    for (int i = 0; i < aClass.getArrayDimensions(); i++) {
      result += arrayString;
    }
    return getLastPartOfName(result);
  }
  private boolean isOverloaded(String name) {
    int numberOfMethodsWithThisName = 0;
    MethodStructure[] methods = iface.getMethods();
    for (int i = 0; i < methods.length; i++) {
      if (methods[i].getName().equals(name)) {
        numberOfMethodsWithThisName++;
        if (numberOfMethodsWithThisName >= 2) {
          return true;
        }
      }
    }
    return false;
  }
  private String parameterTypeNames(MethodStructure method) {
    StringBuffer result = new StringBuffer();
    ClassStructure[] paramTypes = method.getParameterTypes();
    for (int i = 0; i < paramTypes.length; i++) {
      result.append(capitalized(getLastPartOfName(paramTypes[i], "Array")));
    }
    return result.toString();
  }
  private String setActualReturnValueString(String methodName) {
    return MessageFormat.format(
      myConfiguration.setActualReturnValueFormat(),
      new String[] { methodName });
  }
  private String setActualReturnExceptionString(String methodName) {
    return MessageFormat.format(
      myConfiguration.setActualReturnExceptionFormat(),
      new String[] { methodName });
  }
  private String setExpectedCallsString(String methodName) {
    return MessageFormat.format(
      myConfiguration.setExpectedCallsFormat(),
      new String[] { methodName });
  }
  private String packageNameString() {
    String packageName = iface.getName().substring(0, iface.getName().lastIndexOf('.'));
    return MessageFormat.format(
      myConfiguration.packageNameFormat(),
      new String[] { packageName });
  }
  private String wrappedArg(ClassStructure paramType, String argName) {
    String type = getLastPartOfName(paramType);
    if (PRIMITIVE_TYPES.containsKey(type)) {
      return "new " + PRIMITIVE_TYPES.get(type) + "(" + argName + ")";
    }
    return argName;
  }
  private MethodStructure[] getMethods() {
    Vector myMethodsNotInObject = new Vector();
    MethodStructure[] myMethods = iface.getMethods();
    for (int i = 0; i < myMethods.length; i++) {
      MethodStructure myMethod = myMethods[i];
      if (myMethod.isMockTarget()) {
        myMethodsNotInObject.addElement(myMethod);
      }
    }
    MethodStructure[] result = new MethodStructure[myMethodsNotInObject.size()];
    myMethodsNotInObject.copyInto(result);
    return result;
  }
  private String nameOfMockClass() {
    return MessageFormat.format(
      myConfiguration.classNameFormat(),
      new String[] { getLastPartOfName(iface) });
  }
  private String[] paramsTypeNamesFor(MethodStructure method) {
    ClassStructure[] paramTypes = method.getParameterTypes();
    String[] result = new String[paramTypes.length];
    for (int i = 0; i < paramTypes.length; i++) {
      result[i] = getLastPartOfName(paramTypes[i]);
    }
    return result;
  }
  private void writeClassDeclaration() {
    if (iface.isInterface()) {
      output.writeClassDeclaration(nameOfMockClass(), getLastPartOfName(iface));
    } else {
      output.writeSubclassDeclaration(nameOfMockClass(), getLastPartOfName(iface));
    }
  }
  private void writeConstructors() {
    if (!iface.isInterface()) {
      MethodStructure[] constructors = iface.getConstructors();
      for (int i = 0; i < constructors.length; i++) {
        MethodStructure aConstructor = constructors[i];
        output.writeConstructorMethodDeclaration(
          nameOfMockClass(),
          paramsTypeNamesFor(aConstructor));
      }
    }
  }

  public void setIncludePackage(boolean b) {
    includePackage = true;
  }

  private String toCommaSeparatedList(String[] strings) {
    StringBuffer result = new StringBuffer();
    for (int i = 0; i < strings.length; i++){
      result.append(strings[i]);
      if(i<strings.length-1){
        result.append(", ");
      }
    }
    return result.toString();
  }
}
