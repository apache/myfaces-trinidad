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
package org.apache.myfaces.trinidadinternal.skin.pregen;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidadinternal.skin.pregen.config.PregenConfig;
import org.apache.myfaces.trinidadinternal.style.StyleProvider;

/**
 * Interface for objects that perform skin pregeneration.
 */
interface SkinPregenerator
{

  /**
   * Pregenerate style sheets for the specified skin
   * @param context the FacesContext for the current request
   * @param skin the Skin to pregenerate
   * @param config configuration info that influences pregeneration
   */
  public void pregenerate(
    FacesContext  context,
    Skin          skin,
    PregenConfig  config,
    StyleProvider provider
    );
}
