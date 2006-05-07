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
package org.apache.myfaces.adfbuild.plugin.mock.maker.structure;

import com.thoughtworks.qdox.model.Type;
import com.thoughtworks.qdox.model.JavaMethod;

import java.util.Set;
import java.util.HashSet;

public class SourceTypeStructure implements ClassStructure {

  private Type type;
  private JavaMethod mth;
  private String paramName;

  private static Set primitives = new HashSet();

  static {
    primitives.add("void");
    primitives.add("boolean");
    primitives.add("char");
    primitives.add("byte");
    primitives.add("short");
    primitives.add("int");
    primitives.add("long");
    primitives.add("float");
    primitives.add("double");
  }

  SourceTypeStructure(Type type, JavaMethod mth, String paramName) {
    this.type = type;
    this.mth = mth;
    this.paramName = paramName;
  }

  public String getName() {
    return type.getValue();
  }

  public boolean isArray() {
    return type.isArray();
  }

  public int getArrayDimensions() {
    return type.getDimensions();
  }

  public boolean isPrimitive() {
    return primitives.contains(type.getValue());
  }

  public String getExpectationName(String name) {
    return " : " + getName() + " " + paramName;
  }

  // not implemented

  public boolean isInterface() {
    return false;
  }

  public MethodStructure[] getMethods() {
    return new MethodStructure[0];
  }

  public MethodStructure[] getConstructors() {
    return new MethodStructure[0];
  }
}
