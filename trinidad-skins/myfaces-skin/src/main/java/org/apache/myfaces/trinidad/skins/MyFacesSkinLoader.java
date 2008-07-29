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
package org.apache.myfaces.trinidad.skins;

import javax.faces.context.ExternalContext;

import org.apache.myfaces.trinidad.config.Configurator;
import org.apache.myfaces.trinidad.skin.SkinFactory;
import org.apache.myfaces.trinidadinternal.skin.SkinExtension;

/**
 * Loads the skins into the skin factory
 *
 * @author Andrew Robinson
 */
public class MyFacesSkinLoader
  extends Configurator
{
  /**
   * @see org.apache.myfaces.trinidad.config.Configurator#init(javax.faces.context.ExternalContext)
   */
  @Override
  public void init(ExternalContext externalContext)
  {
    super.init(externalContext);
    
    SkinFactory skinFactory = SkinFactory.getFactory();
    
    SkinExtension skin = new SkinExtension(
      skinFactory.getSkin(null, "simple.desktop"),
      "myfaces.desktop",
      "myfaces",
      "org.apache.myfaces.trinidad.desktop",
      "META-INF/style/myfaces-desktop.css");
    
    skinFactory.addSkin(skin.getId(), skin);
    
    skin = new SkinExtension(
      skinFactory.getSkin(null, "simple.pda"),
      "myfaces.pda",
      "myfaces",
      "org.apache.myfaces.trinidad.pda",
      "META-INF/style/myfaces-pda.css");
    
    skinFactory.addSkin(skin.getId(), skin);
  }
}
