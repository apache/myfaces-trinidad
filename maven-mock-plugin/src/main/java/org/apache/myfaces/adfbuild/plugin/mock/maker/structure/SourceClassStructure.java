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

import com.thoughtworks.qdox.model.JavaClass;
import com.thoughtworks.qdox.model.JavaMethod;
import com.thoughtworks.qdox.model.Type;

import java.util.HashMap;
import java.util.List;
import java.util.LinkedList;
import java.util.Map;

public class SourceClassStructure implements ClassStructure {

  private JavaClass cls;
  private MethodStructure[] methods;
  private MethodStructure[] constructors;

  static private Map structures = new HashMap();

  static public SourceClassStructure create(JavaClass cls)
  {
    SourceClassStructure structure = (SourceClassStructure) structures.get(cls);
    if (structure == null)
    {
      structure = new SourceClassStructure(cls);
      structures.put(cls, structure);
    }
    return structure;
  }

  private SourceClassStructure(JavaClass cls) {
    this.cls = cls;
  }

  public String getName() {
    return cls.getFullyQualifiedName();
  }

  public boolean isInterface() {
    return cls.isInterface();
  }

  public MethodStructure[] getMethods() {
    if (methods == null)
      methods = getMethodStructures(false);
    return methods;
  }

  public MethodStructure[] getConstructors() {
    if (constructors == null)
      constructors = getMethodStructures(true);
    return constructors;
  }

  private MethodStructure[] getMethodStructures(boolean constructors) {
    List result = new LinkedList();
    findMethods(cls, constructors, result);
    MethodStructure[] array = new MethodStructure[result.size()];
    result.toArray(array);
    return array;
  }

  private void findMethods(JavaClass currentCls, boolean constructors, List result) {
    // this sucks... refactor me
    JavaMethod[] methods = currentCls.getMethods();
    for(int i = 0; i < methods.length; i++) {
      JavaMethod m = methods[i];
      if (!m.isStatic() && constructors == m.isConstructor()) {
        SourceMethodStructure newMeth = new SourceMethodStructure(m);
        if (!result.contains(newMeth)) {
          result.add(newMeth);
        }
      }
    }

    if (!constructors)
    {
      Type superClassType = currentCls.getSuperClass();
      if (superClassType != null) {
        JavaClass superClass = cls.getClassLibrary().getClassByName(superClassType.getValue());
        if (superClass != null) {
          SourceClassStructure str = SourceClassStructure.create(superClass);
          addMethods(str, result);
        }
      }
      Type[] implementz = currentCls.getImplements();
      for (int i = 0; i < implementz.length; i++) {
        JavaClass imp = cls.getClassLibrary().getClassByName(implementz[i].getValue());
        if (imp != null) {
          SourceClassStructure str = SourceClassStructure.create(imp);
          addMethods(str, result);
        }
      }
    }
  }

  private void addMethods(ClassStructure cls, List result) {
    // this sucks... refactor me
    MethodStructure[] methods = cls.getMethods();
    for(int i = 0; i < methods.length; i++) {
      MethodStructure m = methods[i];
      if (!result.contains(m)) {
        result.add(m);
      }
    }
  }

  // not implemented

  public boolean isArray() {
    return false;
  }

  public int getArrayDimensions() {
    return 0;
  }

  public boolean isPrimitive() {
    return false;
  }

  public String getExpectationName(String name) {
    return null;
  }
}
