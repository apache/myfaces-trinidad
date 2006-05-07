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

import java.lang.reflect.Method;
import java.lang.reflect.Constructor;
import java.lang.reflect.Member;
import java.lang.reflect.Modifier;
import java.util.Vector;

public class ReflectionClassStructure implements ClassStructure {

  private Class cls;
  private Class endOfArray;
  private int paramNumber;

  public ReflectionClassStructure(Class cls) {
    this(cls, 0);
  }

  public ReflectionClassStructure(Class cls, int paramNumber) {
    this.cls = cls;
    endOfArray = cls;
    this.paramNumber = paramNumber;
    while(endOfArray.isArray()) {
      endOfArray = endOfArray.getComponentType();
    }
  }

  public String getName() {
    return endOfArray.getName();
  }

  public boolean isInterface() {
    return endOfArray.isInterface();
  }

  public boolean isArray() {
    return cls.isArray();
  }

  public int getArrayDimensions() {
    Class current = cls;
    int result = 0;
    while (current.isArray()) {
      result++;
      current = current.getComponentType();
    }
    return result;
  }

  public boolean isPrimitive() {
    return endOfArray.isPrimitive();
  }

  public MethodStructure[] getMethods() {
    return convertMembers(endOfArray.getMethods());
  }

  public MethodStructure[] getConstructors() {
    return convertMembers(endOfArray.getConstructors());
  }

  public String getExpectationName(String name) {
    return " arg" + paramNumber + " values";
  }

  private MethodStructure[] convertMembers(Member[] members) {
    Vector validMembers = new Vector();
    for (int i = 0; i < members.length; i++) {
      if (members[i] instanceof Method) {
        Method meth = (Method)members[i];
        if (!Modifier.isStatic(meth.getModifiers())) {
          validMembers.addElement(new ReflectionMethodStructure(meth));
        }
      }
      else if (members[i] instanceof Constructor) {
        validMembers.addElement(new ReflectionConstructorStructure((Constructor)members[i]));
      }
    }
    MethodStructure[] result = new MethodStructure[validMembers.size()];
    validMembers.copyInto(result);
    return result;
  }

}
