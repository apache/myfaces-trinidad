
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

package org.apache.myfaces.trinidadinternal.ui.laf.xml.parse;

import java.util.ArrayList;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.share.xml.StringParser;
import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;

import org.apache.myfaces.trinidad.skin.Skin;
import org.apache.myfaces.trinidadinternal.skin.SkinExtension;

import org.apache.myfaces.trinidadinternal.skin.SkinUtils;
import org.apache.myfaces.trinidadinternal.ui.laf.xml.XMLConstants;

/**
 * NodeParser for SkinExtensions
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/SkinExtensionParser.java#0 $) $Date: 10-nov-2005.18:50:44 $
 * @author The Oracle ADF Faces Team
 * @todo ELIMINATE NAMESPACE
 */
public class SkinExtensionParser extends BaseNodeParser
  implements XMLConstants
{
  @Override
  public void startElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    _namespace = namespaceURI;
  }

  @Override
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    ) throws SAXParseException
  {

    // id and family are required. log a severe error if they are null.
    if ((_id == null) && (_LOG.isWarning()))
      _LOG.severe("Required element 'id' not found.");
    if ((_family == null) && (_LOG.isWarning()))
      _LOG.severe("Required element 'family' not found.");


    // if the renderKitId is not specified,
    // set it to _RENDER_KIT_ID_CORE.
    if (_renderKitId == null)
      _renderKitId = _RENDER_KIT_ID_DESKTOP;

    // figure out the base skin.
    Skin baseSkin = null;
    if (_extends != null)
      baseSkin = SkinUtils.getSkin(context, _extends);
    if (baseSkin == null)
    {
      if (_extends != null)
      {
        _LOG.severe("Unable to locate base skin \"{0}\" for " +
                    "use in defining skin of id \"{1}\", family " +
                    "\"{2}\", renderkit ID \"{3}\"",
                    new String[]{_extends, _id, _family, _renderKitId});
      }

      baseSkin = _getDefaultBaseSkin(context, _renderKitId);
    }

    SkinExtension skin = new SkinExtension(baseSkin,
                                           _id,
                                           _family,
                                           _renderKitId);

    // Set the style sheet
    if (_styleSheetName != null)
      skin.setStyleSheetName(_styleSheetName);
    // And the bundle
    if (_bundleName != null)
      skin.setBundleName(_bundleName);

    // Register icons
    for (int i = 0; i < _icons.size(); i++)
    {
      IconNode icon = (IconNode)_icons.get(i);
      String iconName = icon.getIconName();

      assert iconName != null;

      skin.registerIcon(iconName, icon.getIcon());
    }

    // Register the properties
    for (int i = 0; i < _properties.size(); i++)
    {
      SkinPropertyNode property = (SkinPropertyNode)_properties.get(i);
      skin.setProperty(property.getPropertyName(), property.getPropertyValue());

    }
    return skin;
  }

  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {
    if (!namespaceURI.equals(_namespace))
      return null;

    Class<?> expectedType = null;

    if (ICONS_NAME.equals(localName))
      expectedType = IconNode[].class;
    else if (PROPERTIES_NAME.equals(localName))
      expectedType = SkinPropertyNode[].class;

    if (expectedType != null)
       return context.getParser(expectedType, namespaceURI, localName);

    if ("id".equals(localName) ||
        "family".equals(localName) ||
        "render-kit-id".equals(localName) ||
        "style-sheet-name".equals(localName) ||
        "bundle-name".equals(localName) ||
        "extends".equals(localName))

    {
      return new StringParser();
    }

    return null;
  }

  @Override
  public void addCompletedChild(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Object       child
    ) throws SAXParseException
  {
    if (child instanceof IconNode[])
      _addToArrayList(_icons, (Object[])child);
    else if (child instanceof SkinPropertyNode[])
      _addToArrayList(_properties, (Object[])child);
    else if ("id".equals(localName))
      _id = (String) child;
    else if ("family".equals(localName))
      _family = (String) child;
    else if ("render-kit-id".equals(localName))
      _renderKitId = (String) child;
    else if ("style-sheet-name".equals(localName))
      _styleSheetName = (String) child;
    else if ("bundle-name".equals(localName))
      _bundleName = (String) child;
    else if ("extends".equals(localName))
      _extends = (String) child;
  }


  // Adds the objects in the array to the ArrayList
  private static void _addToArrayList(
    ArrayList<Object> list,
    Object[]  array
    )
  {
    assert list != null;

    if (array != null)
    {
      for (int i = 0; i < array.length; i++)
        list.add(array[i]);
    }
  }

  public Skin _getDefaultBaseSkin(
    ParseContext context,
    String renderKitId)
  {

    String baseSkinId = (_RENDER_KIT_ID_PDA.equals(renderKitId)) ?
                          _SIMPLE_PDA_SKIN_ID :
                          _SIMPLE_DESKTOP_SKIN_ID;

    Skin baseSkin = SkinUtils.getSkin(context, baseSkinId);

    // It is an error if we were unable to find the base LAF
    if (baseSkin == null)
      _LOG.severe(_UNKNOWN_BASE_SKIN_ERROR + baseSkinId);

    return baseSkin;
  }

  private String      _namespace;
  private String      _id;
  private String      _family;
  private String      _styleSheetName;
  private String      _renderKitId;
  private String      _bundleName;
  private String      _extends;


  // ArrayList of IconNodes
  private ArrayList<Object> _icons = new ArrayList<Object>();

  // ArrayList of PropertyNodes
  private ArrayList<Object> _properties = new ArrayList<Object>();

  // Error messages
  private static final String _UNKNOWN_BASE_SKIN_ERROR =
    "Unable to locate base skin: ";

  static private final String _RENDER_KIT_ID_DESKTOP = "org.apache.myfaces.trinidad.desktop";
  static private final String _RENDER_KIT_ID_PDA = "org.apache.myfaces.trinidad.pda";
  static private final String _SIMPLE_PDA_SKIN_ID = "simple.pda";
  static private final String _SIMPLE_DESKTOP_SKIN_ID = "simple.desktop";

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(SkinExtensionParser.class);

}
