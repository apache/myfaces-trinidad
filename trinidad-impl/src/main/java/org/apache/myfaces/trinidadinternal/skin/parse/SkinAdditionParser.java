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
package org.apache.myfaces.trinidadinternal.skin.parse;

import javax.el.ValueExpression;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.skin.CustomMetadata;
import org.apache.myfaces.trinidad.skin.SkinAddition;
import org.apache.myfaces.trinidad.skin.SkinFeatures;
import org.apache.myfaces.trinidadinternal.config.LazyValueExpression;
import org.apache.myfaces.trinidadinternal.share.xml.BaseNodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.NodeParser;
import org.apache.myfaces.trinidadinternal.share.xml.ParseContext;
import org.apache.myfaces.trinidadinternal.share.xml.StringParser;

import org.xml.sax.Attributes;
import org.xml.sax.SAXParseException;

/**
 * NodeParser for &lt;skin-addition&gt; node in trinidad-skins.xml
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/xml/parse/SkinExtensionParser.java#0 $) $Date: 10-nov-2005.18:50:44 $
 * @todo ELIMINATE NAMESPACE
 */
public class SkinAdditionParser extends BaseNodeParser
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
  }

  @Override
  public Object endElement(
    ParseContext context,
    String       namespaceURI,
    String       localName
    ) throws SAXParseException
  {

    // id is required for a SkinAddition. log a severe error if it is null.
    if (_skinId == null)
      _LOG.severe("REQUIRED_ELEMENT_SKINID_NOT_FOUND");

    if ((_resourceBundleName != null) && (_translationSourceExpression != null))
    {
      _LOG.severe("BOTH_BUNDLENAME_TRANSLATIONSOURCE_SET");
      _translationSourceExpression = null;
    }

    if (_translationSourceExpression != null &&
        !(_translationSourceExpression.startsWith("#{") &&
        _translationSourceExpression.endsWith("}")))
    {
      _LOG.severe("TRANSLATION_SOURCE_NOT_EL");
      _translationSourceExpression = null;
    }

    Object isMetaInf = context.getProperty(SKIN_NAMESPACE, META_INF);

    if (isMetaInf != null && _styleSheetName != null
      && Boolean.parseBoolean(isMetaInf.toString()) && !(_styleSheetName.startsWith("/")))
      _styleSheetName = META_INF_DIR.concat(_styleSheetName);

    SkinFeatures features = _skinFeatures;

    SkinAddition addition = null;
    if (_styleSheetName != null
            || _resourceBundleName != null
            || _translationSourceExpression != null)
    {
      if (_resourceBundleName != null)
      {
        // create SkinAddition with resourceBundleName
        addition = new SkinAddition.Builder().skinId(_skinId).styleSheetName(_styleSheetName)
          .resourceBundleName(_resourceBundleName).features(features).build();
      }
      else
      {
        ValueExpression translationSourceVE = null;

        if (_translationSourceExpression != null)
        {
          translationSourceVE = LazyValueExpression.createValueExpression(_translationSourceExpression.trim(), Object.class);
        }

        if (translationSourceVE != null)
        {
          // Create a SkinAddition with translationSourceVE
          addition = new SkinAddition.Builder().skinId(_skinId).styleSheetName(_styleSheetName)
            .translationSource(translationSourceVE).features(features).build();
        }
        else
        {
          // Create a SkinAddition with stylesheetName only
          addition = new SkinAddition.Builder().skinId(_skinId).styleSheetName(_styleSheetName).features(features).build();
        }
      }
    }
    else if (features != null)
    {
      addition = new SkinAddition.Builder().skinId(_skinId).features(features).build();
    }

    return addition;
  }

  @Override
  public NodeParser startChildElement(
    ParseContext context,
    String       namespaceURI,
    String       localName,
    Attributes   attrs
    ) throws SAXParseException
  {

    if ("skin-id".equals(localName) ||
        "style-sheet-name".equals(localName) ||
        "bundle-name".equals(localName) ||
        "translation-source".equals(localName))

    {
      return new StringParser();
    }
    else if ("features".equals(localName))
    {
      return context.getParser(SkinFeatures.class, namespaceURI, localName);
    }
    else if ("metadata".equals(localName))
    {
      return context.getParser(CustomMetadata.class, namespaceURI, localName);
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

    if ("skin-id".equals(localName))
      _skinId = (String) child;
    else if ("style-sheet-name".equals(localName))
      _styleSheetName = (String) child;
    else if ("bundle-name".equals(localName))
      _resourceBundleName = (String) child;
    else if ("translation-source".equals(localName))
      _translationSourceExpression = (String) child;
    else if ("features".equals(localName))
    {
      _skinFeatures = (SkinFeatures) child;
    }
  }

  private String _skinId;
  private String _styleSheetName;
  private String _resourceBundleName;
  private String _translationSourceExpression;
  private SkinFeatures _skinFeatures;


  private static final TrinidadLogger _LOG = 
    TrinidadLogger.createTrinidadLogger(SkinAdditionParser.class);

}