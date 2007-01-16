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
package org.apache.myfaces.trinidadbuild.plugin.faces.parse;

import java.lang.reflect.Modifier;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

/**
 * ValidatorBean is a Java representation of the faces-config validator
 * XML element.
 */
public class ValidatorBean extends ObjectBean
{
  /**
   * Creates a new ValidatorBean.
   */
  public ValidatorBean()
  {
    _properties = new LinkedHashMap();
  }

  /**
   * Sets the validator identifer for this component.
   *
   * @param validatorId  validator identifer
   */
  public void setValidatorId(
    String validatorId)
  {
    _validatorId = validatorId;
  }

  /**
   * Returns true if the validator identifier is specified, otherwise false.
   *
   * @return  true if the validator identifier is specified,
   *          otherwise false.
   */
  public boolean hasValidatorId()
  {
    return (_validatorId != null);
  }

  /**
   * Returns the validator identifier for this component.
   *
   * @return  the validator identifier
   */
  public String getValidatorId()
  {
    return _validatorId;
  }

  /**
   * Sets the validator class for this component.
   *
   * @param validatorClass  the validator class
   */
  public void setValidatorClass(
    String validatorClass)
  {
    _validatorClass = validatorClass;
  }

  /**
   * Returns the validator class for this component.
   *
   * @return  the validator class
   */
  public String getValidatorClass()
  {
    return _validatorClass;
  }

  /**
   * Sets the description of this property.
   *
   * @param description  the property description
   */
  public void setDescription(
    String description)
  {
    _description = description;
  }

  /**
   * Returns the description of this property.
   *
   * @return  the property description
   */
  public String getDescription()
  {
    return _description;
  }

  /**
   * Sets the long description of this property.
   *
   * @param longDescription  the long property description
   */
  public void setLongDescription(
    String longDescription)
  {
    _longDescription = longDescription;
  }

  /**
   * Returns the long description of this property.
   *
   * @return  the long property description
   */
  public String getLongDescription()
  {
    return _longDescription;
  }

  /**
   * Sets the JSP tag handler class for this component.
   *
   * @param tagClass  the JSP tag handler class
   */
  public void setTagClass(
    String tagClass)
  {
    _tagClass = tagClass;
  }

  /**
   * Returns the JSP tag handler class for this component.
   *
   * @return  the JSP tag handler class
   */
  public String getTagClass()
  {
    return _tagClass;
  }

  /**
   * Returns the JSP tag name for this component.
   *
   * @return  the JSP tag name
   */
  public QName getTagName()
  {
    return _tagName;
  }

  /**
   * Sets the JSP tag name for this component.
   *
   * @param tagClass  the JSP tag name
   */
  public void setTagName(
    QName tagName)
  {
    _tagName = tagName;
  }

  /**
   * Sets the validator super class for this component.
   *
   * @param validatorSuperClass  the validator super class
   */
  public void setValidatorSuperClass(
    String validatorSuperClass)
  {
    _validatorSuperClass = validatorSuperClass;
  }

  /**
   * Returns the validator super class for this component.
   *
   * @return  the validator super class
   */
  public String getValidatorSuperClass()
  {
    return _validatorSuperClass;
  }

  /**
   * Adds a property to this component.
   *
   * @param property  the property to add
   */
  public void addProperty(
    PropertyBean property)
  {
    _properties.put(property.getPropertyName(), property);
  }

  /**
   * Returns the property for this property name.
   *
   * @param propertyName  the property name to find
   */
  public PropertyBean findProperty(
    String propertyName)
  {
    return (PropertyBean)_properties.get(propertyName);
  }

  /**
   * Returns true if this component has any properties.
   *
   * @return  true   if this component has any properties,
   *          false  otherwise
   */
  public boolean hasProperties()
  {
    return !_properties.isEmpty();
  }

  /**
   * Returns an iterator for all properties on this component only.
   *
   * @return  the property iterator
   */
  public Iterator properties()
  {
    return _properties.values().iterator();
  }

  /**
   * Adds a Java Language class modifier to the validator class.
   *
   * @param modifier  the modifier to be added
   */
  public void addValidatorClassModifier(
    int modifier)
  {
    _validatorClassModifiers |= modifier;
  }

  /**
   * Returns the Java Language class modifiers for the validator class.
   * By default, these modifiers include Modifier.PUBLIC.
   *
   * @return  the Java Language class modifiers for the validator class
   */
  public int getValidatorClassModifiers()
  {
    int modifiers = _validatorClassModifiers;

    if (!Modifier.isPrivate(modifiers) &&
        !Modifier.isProtected(modifiers) &&
        !Modifier.isPublic(modifiers))
    {
      modifiers |= Modifier.PUBLIC;
    }

    return modifiers;
  }

  public void parseValidatorClassModifier(
    String modifier)
  {
    addValidatorClassModifier(_parseModifier(modifier));
  }

  public void parseTagClassModifier(
    String modifier)
  {
    addTagClassModifier(_parseModifier(modifier));
  }

  private int _parseModifier(
    String text)
  {
    if ("public".equals(text))
      return Modifier.PUBLIC;
    else if ("protected".equals(text))
      return Modifier.PROTECTED;
    else if ("private".equals(text))
      return Modifier.PRIVATE;
    else if ("abstract".equals(text))
      return Modifier.ABSTRACT;
    else if ("final".equals(text))
      return Modifier.FINAL;

    throw new IllegalArgumentException("Unrecognized modifier: " + text);
  }

  /**
   * Adds a Java Language class modifier to the tag class.
   *
   * @param modifier  the modifier to be added
   */
  public void addTagClassModifier(
    int modifier)
  {
    _tagClassModifiers |= modifier;
  }

  /**
   * Returns the Java Language class modifiers for the tag class.
   * By default, these modifiers include Modifier.PUBLIC.
   *
   * @return  the Java Language class modifiers for the tag class
   */
  public int getTagClassModifiers()
  {
    int modifiers = _tagClassModifiers;

    if (!Modifier.isPrivate(modifiers) &&
        !Modifier.isProtected(modifiers) &&
        !Modifier.isPublic(modifiers))
    {
      modifiers |= Modifier.PUBLIC;
    }

    return modifiers;
  }

  private String  _description;
  private String  _longDescription;
  private String  _validatorId;
  private String  _validatorClass;
  private String  _validatorSuperClass;
  private QName   _tagName;
  private String  _tagClass;
  private Map     _properties;
  private int     _validatorClassModifiers;
  private int     _tagClassModifiers;


  static private final Logger _LOG = Logger.getLogger(ValidatorBean.class.getName());
}
