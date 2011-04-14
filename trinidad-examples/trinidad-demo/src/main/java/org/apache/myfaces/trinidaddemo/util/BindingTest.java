/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
*/
package org.apache.myfaces.trinidaddemo.util;

import org.apache.myfaces.trinidad.component.core.CoreDocument;
import org.apache.myfaces.trinidad.component.core.output.CoreOutputText;
import org.apache.myfaces.trinidad.util.ComponentReference;

// the "bindingTester" backing bean
public class BindingTest
{
  public CoreDocument getDocument()
  {
    return document==null ? null : document.getComponent();
  }
  public void setDocument(CoreDocument document)
  {
    this.document = ComponentReference.newUIComponentReference(document);
  }
  public CoreOutputText getOutput()
  {
    return output==null ? null : output.getComponent();
  }
  public void setOutput(CoreOutputText output)
  {
    this.output = ComponentReference.newUIComponentReference(output);
  }

  private ComponentReference<CoreDocument> document;
  private ComponentReference<CoreOutputText> output;
}