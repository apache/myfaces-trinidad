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
package org.apache.myfaces.trinidadinternal.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectStreamClass;

import org.apache.myfaces.trinidad.util.ClassLoaderUtils;

public class ObjectInputStreamResolveClass extends ObjectInputStream
{
  public ObjectInputStreamResolveClass()  throws IOException, SecurityException
  {
    super();
  }

  public ObjectInputStreamResolveClass(InputStream in) throws IOException 
  {
    super(in);
  }
  
  protected Class<?> resolveClass(ObjectStreamClass desc)
                           throws IOException,
                                  ClassNotFoundException
  {
    // TRINIDAD-1062 It has been noticed that in OC4J and Weblogic that the
    // classes being resolved are having problems by not finding
    // them using the context class loader. Therefore, we are adding
    // this work-around until the problem with these application
    // servers can be better understood
    return ClassLoaderUtils.loadClass(desc.getName());
  }
}
