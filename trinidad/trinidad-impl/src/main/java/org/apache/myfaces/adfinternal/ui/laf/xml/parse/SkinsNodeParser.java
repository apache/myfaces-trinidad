/*
 * Copyright  2003-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.myfaces.adfinternal.ui.laf.xml.parse;

import java.util.ArrayList;

import org.apache.myfaces.adfinternal.skin.SkinExtension;
import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;


import org.apache.myfaces.adfinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.adfinternal.share.xml.NodeParser;
import org.apache.myfaces.adfinternal.share.xml.ParseContext;

import org.apache.myfaces.adfinternal.skin.SkinFactory;
import org.apache.myfaces.adfinternal.skin.SkinUtils;
import org.apache.myfaces.adfinternal.ui.laf.xml.XMLConstants;

/**
 * NodeParser for <skins> elements
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/SkinsNodeParser.java#0 $) $Date: 10-nov-2005.18:50:46 $
 * @author The Oracle ADF Faces Team
 */
public class SkinsNodeParser extends BaseNodeParser
  implements XMLConstants
{
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    return context.getParser(SkinExtension.class, namespaceURI, localName);
  }

  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    ) throws SAXParseException
  {
    assert ((child == null) || (child instanceof SkinExtension));

    SkinExtension skinExtension = (SkinExtension)child;
    
    // register skin with factory, this way when we create each skin
    // in SkinExtensionParser, we can extend any skin that has already
    // been registered with the skin factory.
    SkinFactory skinFactory = SkinUtils.getSkinFactory(context);
    skinFactory.addSkin(skinExtension.getId(), skinExtension);
    
    // add to list
    if (child instanceof SkinExtension)
      _skins.add(child);
  }

  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    ) throws SAXParseException
  {
    if (_skins.isEmpty())
      return null;

    SkinExtension[] skinExtension = new SkinExtension[_skins.size()];

    return _skins.toArray(skinExtension);
  }

  private ArrayList _skins = new ArrayList();
}
