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
package org.apache.myfaces.adfbuild.plugin.mock.maker;

import java.io.PrintWriter;

public class RealCodeWriter implements CodeWriter {
  private PrintWriter writer;
public RealCodeWriter(PrintWriter writer) {
  super();
  this.writer = writer;
}
public void finish() {
  writer.println("}");
  writer.flush();
}
public void writeClassDeclaration(String className, String interfaceName) {
  writer.println("\npublic class "+className+" implements "+interfaceName+"{");
}
public void writeImport(String importString) {
  writer.println("import "+importString+";");
}
public void writePackage(String packageString) {
  writer.println("package "+packageString+";");
}
public void writeInstanceVariableDeclaration(String typeName, String instanceVariableName) {
  writer.println("   private "+typeName+" "+instanceVariableName+";");
}
public void writeInstanceVariableDeclaration(String typeName, String instanceVariableName, String initialValue) {
  writer.println("   private "+typeName+" "+instanceVariableName+" = "+initialValue+";");
}


  private final static String[] NO_EXCEPTIONS = new String[0];

public void writeMethodDeclaration(String typeName, String methodName, String[] parameters, String[] statements) {
  writeMethodDeclarationThrowsExceptions(typeName, methodName, parameters, NO_EXCEPTIONS, statements);
}

public void writeMethodDeclarationThrowsExceptions(String typeName, String methodName, String[] parameters, String[] exceptions, String[] statements) {
  writer.print("   public "+typeName+" "+methodName+"(");
  writeCommaSeparatedList(parameters);
  writer.print(")");
  if (exceptions.length > 0){
    writer.print(" throws ");
    writeCommaSeparatedList(exceptions);
  }
  writer.println("{");
  for (int i = 0; i < statements.length; i++){
    writer.println("    "+statements[i]);
  }
  writer.println("  }");
}

private void writeCommaSeparatedList(String[] strings) {
  for (int i = 0; i < strings.length; i++){
    writer.print(strings[i]);
    if(i<strings.length-1){
      writer.print(", ");
    }
  }
}

public void writeConstructorMethodDeclaration(String className, String[] parameters) {
  writer.print("public "+className+"(");
  for (int i = 0; i < parameters.length; i++){
    writer.print(parameters[i]+" arg"+i);
    if(i<parameters.length-1){
      writer.print(", ");
    }
  }
  writer.println("){");
  writer.print("  super(");
  for (int i = 0; i < parameters.length; i++){
    writer.print("arg"+i);
    if(i<parameters.length-1){
      writer.print(", ");
    }
  }
  writer.println(");");
  writer.println("}");
}

public void writeSubclassDeclaration(String className, String superclassName) {
  writer.println("\npublic class "+className+" extends "+superclassName+"{");
}
}
