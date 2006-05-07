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
import java.lang.reflect.Modifier;

public class ReflectionMethodStructure extends ReflectionAbstractStructure {

  private Method mth;

  public ReflectionMethodStructure(Method mth) {
    this.mth = mth;
  }

  public String getName() {
    return mth.getName();
  }

  public boolean isMockTarget()
  {
    return (mth.getDeclaringClass() != Object.class) &&
           (!Modifier.isFinal(mth.getModifiers()) || mth.getDeclaringClass().isInterface());
  }

  public ClassStructure getReturnType() {
    return new ReflectionClassStructure(mth.getReturnType());
  }

  public ClassStructure[] getParameterTypes() {
    return convertClasses(mth.getParameterTypes());
  }

  public ClassStructure[] getExceptions() {
    return convertClasses(mth.getExceptionTypes());
  }

  public boolean equals(Object obj) {
    if (obj instanceof ReflectionMethodStructure) {
      ReflectionMethodStructure other = (ReflectionMethodStructure)obj;
      if (!other.getName().equals(getName())) return false;
      if (other.getParameterTypes().length != getParameterTypes().length) return false;
      for (int i = 0; i < other.getParameterTypes().length; i++) {
        ClassStructure otherStructure = other.getParameterTypes()[i];
        ClassStructure thisStructure = getParameterTypes()[i];
        if (!otherStructure.getName().equals(thisStructure.getName())) return false;
      }
      return true;
    }
    return false;
  }

}
