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

import java.lang.reflect.Constructor;

public class ReflectionConstructorStructure extends ReflectionAbstractStructure implements MethodStructure {

  private Constructor con;

  public ReflectionConstructorStructure(Constructor con) {
    this.con = con;
  }

  public boolean isMockTarget()
  {
    return false;
  }

  public String getName() {
    return null;
  }

  public ClassStructure getReturnType() {
    return null;
  }

  public ClassStructure[] getParameterTypes() {
    return convertClasses(con.getParameterTypes());
  }

  public ClassStructure[] getExceptions() {
    return convertClasses(con.getExceptionTypes());
  }
}
