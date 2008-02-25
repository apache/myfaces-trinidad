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
package org.apache.myfaces.trinidaddemo.email.resource;

import java.util.ListResourceBundle;

public class EmailDemoBundle extends ListResourceBundle
{
  @Override
  public Object[][] getContents()
  {
    return _CONTENTS;
  }

  static private final Object[][] _CONTENTS =
  {
    {"TODAY_MASK", "Today, {0}"},
    {"EMAIL_LIST_ERROR", "Illegal email address."},
    {"EMAIL_LIST_ERROR_detail", "{0} is not a legal email address."},
    {"MESSAGE_SENT", "The message was sent successfully."},
    {"COULD_NOT_DELETE", "Deletion failed."},
    {"COULD_NOT_DELETE_detail", "The server returned an error: {0}."},
    {"EMAIL_DEMO_TITLE", "Trinidad Email Demo"},   

    
  };
}

