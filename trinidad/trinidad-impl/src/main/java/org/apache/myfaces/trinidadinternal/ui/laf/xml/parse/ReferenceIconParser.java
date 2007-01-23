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
package org.apache.myfaces.trinidadinternal.ui.laf.xml.parse;

import org.apache.myfaces.trinidadinternal.share.xml.LeafNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.skin.icon.ReferenceIcon;
import org.apache.myfaces.trinidadinternal.ui.laf.xml.XMLConstants;
import org.xml.sax.Attributes;


/**
 * NodeParser for ReferenceIcons.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/ReferenceIconParser.java#0 $) $Date: 10-nov-2005.18:50:40 $
 * @author The Oracle ADF Faces Team
 */
public class ReferenceIconParser extends LeafNodeParser implements XMLConstants
{
  @Override
  protected Object getNodeValue(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs)
  {
    // name attr is required
    _name = getRequiredAttribute(context, attrs, NAME_ATTR);

    return new ReferenceIcon(_name);
  }
  
  private String _name;
}
