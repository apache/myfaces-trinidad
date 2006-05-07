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

import com.thoughtworks.qdox.model.JavaMethod;
import com.thoughtworks.qdox.model.JavaParameter;
import com.thoughtworks.qdox.model.Type;

public class SourceMethodStructure implements MethodStructure {

  private JavaMethod mth;

  SourceMethodStructure(JavaMethod mth) {
    this.mth = mth;
  }

  public String getName() {
    return mth.getName();
  }

  public boolean isMockTarget()
  {
    return (!mth.getParentClass().getFullyQualifiedName().equals("java.lang.Object")) &&
           (!mth.isFinal() || mth.getParentClass().isInterface());
  }

  public ClassStructure getReturnType() {
    return new SourceTypeStructure(mth.getReturns(), mth, null);
  }

  public ClassStructure[] getParameterTypes() {
    JavaParameter[]  params = mth.getParameters();
    ClassStructure[] result = new ClassStructure[params.length];
    for (int i = 0; i < params.length; i++) {
      result[i] = new SourceTypeStructure(params[i].getType(), mth, params[i].getName());
    }
    return result;
  }

  public ClassStructure[] getExceptions() {
    Type[] exceptions = mth.getExceptions();
    ClassStructure[] result = new ClassStructure[exceptions.length];
    for (int i = 0; i < exceptions.length; i++) {
      result[i] = new SourceTypeStructure(exceptions[i], mth, null);
    }
    return result;
  }

  public int hashCode() {
    return mth.hashCode();
  }

  public boolean equals(Object obj)
  {
    if (this == obj)
    {
      return true;
    }
    else if (obj instanceof SourceMethodStructure)
    {
      SourceMethodStructure s = (SourceMethodStructure)obj;
      return mth.equals(s.mth);
    }
    else
    {
      return false;
    }
  }
}
