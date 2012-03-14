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
package org.apache.myfaces.trinidadinternal.skin.pregen.variant;

import java.util.List;

import org.apache.myfaces.trinidadinternal.style.util.StyleSheetVisitUtils.StyleSheetVisitor;

/**
 * StyleSheetVisitor that is used to extract variant (eg. @-rule)
 * metadata from a skin style sheet document.
 * 
 * SkinVariantExtractors are typically single use - ie. they are 
 * used for a single call to StyleSheetVisitUtils.visitStyleSheets(),
 * after which visited variants are retrieved via a call to getVariants().
 *
 * @see org.apache.myfaces.trinidadinternal.style.util.StyleSheetVisitUtils
 */
interface SkinVariantExtractor <T> extends StyleSheetVisitor
{
  /**
   * Called after the style sheet visit to retrieve the variants
   * that were seen.
   */
  public List<T> getVariants();
}
