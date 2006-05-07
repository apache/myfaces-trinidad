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

public interface CodeWriter
{
void finish();
void writeClassDeclaration(String className,String interfaceName);
void writeImport(String importString);
void writePackage(String importString);
void writeInstanceVariableDeclaration(String typeName,String instanceVariableName);
void writeInstanceVariableDeclaration(String typeName,String instanceVariableName, String initialValue);
void writeMethodDeclaration(String typeName,String methodName, String[] parameters, String[] statements);

void writeMethodDeclarationThrowsExceptions(String typeName,String methodName, String[] parameters, String[] exceptions, String[] statements);

void writeConstructorMethodDeclaration(String methodName, String[] parameters);

void writeSubclassDeclaration(String className,String superclassName);
}
