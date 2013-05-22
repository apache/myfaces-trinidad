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
package org.apache.myfaces.trinidad.webapp;

import java.io.IOException;
import org.apache.myfaces.trinidad.model.UploadedFile;

 /**
  * Marker Interface responsible for processing file uploads by multiple processors 
  * one after another in a chained fashion.  An Apache Trinidad application could have
  * multiple <code>ChainedUploadedFileProcessor</code> instances. A composite UploadedFileProcessor 
  * is accessible from the {@link org.apache.myfaces.trinidad.context.RequestContext},
  * but will be invoked automatically by the framework as needed. Developers
  * can implement this interface and chain many of them up together using space
  * separated class names in <code>trinidad-config.xml</code> file under
  * <code>uploaded-file-processor</code> element. The order in which the processors
  * will be instantated and called will be the same as how it appears inside the element.
  * As such, it is expected that the input of the ChainedUploadedFileProcessor will be
  * the output of the last ChainedUploadedFileProcessor.
  * <p/>
  * UploadedFileProcessors which implement this interface are subject to the following
  * limitations/abilities:
  * <ul>
  * <li>
  *   Unlike in the <code>UploadedFileProcessor</code> the {@link UploadedFile} objects given
  *   to a chained UploadedFileProcessor should allow for multiple reads such that
  *   {@link UploadedFile#getInputStream()} may be called multiple times and must return a 
  *   new <code>InputStream</code> every time it is called.
  * </li>
  * <li>
  *   Chained processors need not return new <code>UploadedFile</code> instances.  They may,
  *   instead, return the same instance as its predecessor, thus saving the need to move
  *   content around with each call into the chain.  If a <code>ChainedUploadedFileProcessor</code>
  *   DOES return a new instance of UploadedFile, it must be allowed to be read multiple times.
  * </li>
  * <li>
  *   Chained processors may extend the {@link ExtendedUploadedFileProcessor} abstract
  *   class.  Any additional methods will be handled in accordance to the modifications listed above.
  * </li>
  * </ul>
  * 
  * @see org.apache.myfaces.trinidad.model.UploadedFile
  */
public interface ChainedUploadedFileProcessor extends UploadedFileProcessor {}
