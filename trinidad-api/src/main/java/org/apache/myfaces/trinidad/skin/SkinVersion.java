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
package org.apache.myfaces.trinidad.skin;

 
  /**
   * You can version skins. The skin version works tightly with the skin family.
   * This allows someone to create versions of their skin, like purple, purple-v2, 
   * purple-v3. Then the user can say which skin version they want, like:
   * <skin-family>purple</skin-family><skin-version>v3</skin-version> when they 
   * pick a skin in trinidad-config.xml.
   * When creating a skin, you give it a version if you care about versioning.
   * When extending this class, you must override equals and hashCode
   */
  abstract public class SkinVersion
  {

    // when extending this class, you must override equals and hashCode
    abstract public boolean equals(Object o);
    
    // when extending this class, you must override equals and hashCode
    abstract   public int hashCode();
    
    abstract public boolean isDefault();
    
    abstract public String getName();

  } 