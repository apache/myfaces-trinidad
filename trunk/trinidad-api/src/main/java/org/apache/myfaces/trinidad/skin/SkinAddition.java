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
package org.apache.myfaces.trinidad.skin;

import java.util.Map;

import javax.el.ValueExpression;

import javax.faces.el.ValueBinding;

import org.apache.myfaces.trinidad.util.ToStringHelper;

/**
 * <p>
 * SkinAddition objects are used by custom component developers who have created custom
 * components, and they need a way to 'push' in their own stylesheet and
 * resource bundle for these components into some skin of their choosing,
 * most likely the simple skin.</p>
 * <p>
 * A Skin object contains zero or more SkinAdditions. The SkinAdditions' stylesheets
 * are merged into the Skin's own stylesheet. The SkinAdditions' resource
 * bundle is looked at along with the Skin's own resource bundle when Skin's
 * getTranslatedValue is called.
 * </p>
 * <p>
 * If you want to 'push' your styles into a specific skin, then you would create a skin-addition in the trinidad-skins.xml file.
 * You specify a &lt;skin-addition&gt;. The children are: &lt;skin-id&gt;,
 * &lt;style-sheet-name&gt;, &lt;bundle-name&gt;, and &lt;translation-source&gt;.
 * The &lt;skin-id&gt; is used to specify which skin you want to 'push' your stylesheet/resource bundle into.
 * Most likely this is the simple.desktop skin.
 * The other elements are used to create a SkinAddition object.
 * </p>
 */
public class SkinAddition implements Comparable
{

  /**
   * Constructor takes a styleSheet name and a resourceBundle name.
   * @deprecated Use builder to create a SkinAddition object.
   * @See Builder
   */
  @Deprecated
  public SkinAddition (
    String styleSheetName,
    String resourceBundleName
    )
  {
    _styleSheetName = styleSheetName;
    _resourceBundleName = resourceBundleName;
    _translationSourceVE = null;
    _translationSourceVB = null;
    _skinFeatures = null;
    _skinId = null;
  }

  /**
   * Constructor takes a styleSheet name and a translationSource ValueExpression.
   * @deprecated Use builder to create a SkinAddition object.
   * @See Builder
   */
  @Deprecated
  public SkinAddition (
    String       styleSheetName,
    ValueExpression translationSourceValueExpression
    )
  {
    _styleSheetName = styleSheetName;
    _resourceBundleName = null;
    _translationSourceVE = translationSourceValueExpression;
    _translationSourceVB = null;
    _skinFeatures = null;
    _skinId = null;
  }
  /**
   * Constructor takes a styleSheet name. resource bundle name and
   * translation source value expression will be null.
   * @deprecated Use builder to create a SkinAddition object.
   * @See Builder
   */
  @Deprecated
  public SkinAddition (
    String styleSheetName
    )
  {
    _styleSheetName = styleSheetName;
    _resourceBundleName = null;
    _translationSourceVE = null;
    _translationSourceVB = null;
    _skinFeatures = null;
    _skinId = null;
  }

  /**
   * Constructor takes a styleSheet name and a resourceBundle name.
   * @deprecated Use builder to create a SkinAddition object.
   * @See Builder
   */
  @Deprecated
  public SkinAddition (
    String styleSheetName,
    String resourceBundleName,
    Map<String, String> features
    )
  {
    _styleSheetName = styleSheetName;
    _resourceBundleName = resourceBundleName;
    _translationSourceVE = null;
    _translationSourceVB = null;
    _skinFeatures = features;
    _skinId = null;
  }

  /**
   * Constructor takes a styleSheet name and a translationSource ValueExpression.
   * @deprecated Use builder to create a SkinAddition object.
   * @See Builder
   */
  @Deprecated
  public SkinAddition (
    String       styleSheetName,
    ValueExpression translationSourceValueExpression,
    Map<String, String> features
    )
  {
    _styleSheetName = styleSheetName;
    _resourceBundleName = null;
    _translationSourceVE = translationSourceValueExpression;
    _translationSourceVB = null;
    _skinFeatures = features;
    _skinId = null;
  }
  /**
   * Constructor takes a styleSheet name. resource bundle name and
   * translation source value expression will be null.
   * @deprecated Use builder to create a SkinAddition object.
   * @See Builder
   */
  @Deprecated
  public SkinAddition (
    String styleSheetName,
    Map<String, String> features
    )
  {
    _styleSheetName = styleSheetName;
    _resourceBundleName = null;
    _translationSourceVE = null;
    _translationSourceVB = null;
    _skinFeatures = features;
    _skinId = null;
  }

  /**
   * Constructor takes only features.
   * @deprecated Use builder to create a SkinAddition object.
   * @See Builder
   */
  @Deprecated
  public SkinAddition (
    Map<String, String> features
    )
  {
    _styleSheetName = null;
    _resourceBundleName = null;
    _translationSourceVE = null;
    _translationSourceVB = null;
    _skinFeatures = features;
    _skinId = null;
  }

  /**
   * Constructor takes a styleSheet name and a translationSource ValueBinding.
   * ValueBinding is deprecated, use ValueExpression for setting the translationSource
   * @deprecated Use builder to create a SkinAddition object.
   * @See Builder
   */
  @Deprecated
  public SkinAddition (
    String       styleSheetName,
    ValueBinding translationSourceValueBinding
    )
  {
    _styleSheetName = styleSheetName;
    _resourceBundleName = null;
    _translationSourceVE = null;
    _translationSourceVB = translationSourceValueBinding;
    _skinFeatures = null;
    _skinId = null;
  }

  /**
   * Gets the skin id to which this skin addition belongs to.
   */
  public String getSkinId()
  {
    return _skinId;
  }
  
  /**
   * Gets the SkinAddition's style sheet name.
   * Note that in order to avoid infinite call loop the implementation of getStylesheetName() in 
   * this class or sub classes should not call toString().
   */
  public String getStyleSheetName()
  {
    return _styleSheetName;
  }
  
  /**
   * Gets the SkinAddition's resource bundle. 
   * A skin cannot have both a resourceBundleName and a translation source
   * value expression. If they do, then the resourceBundleName takes precedence.
   * Note that in order to avoid infinite call loop the implementation of getResourceBundleName() 
   * in this class or sub classes should not call toString().
   */
  public String getResourceBundleName()
  {
    return _resourceBundleName;
  } 

  /**
   * @inheritDoc
   * Note that in order to avoid infinite call loop the implementation of getStyleSheetName() 
   * and getResourceBundleName() in this class or its sub classes should not call toString().
   */
  @Override
  public String toString()
  {
    ToStringHelper helper = 
      new ToStringHelper(this).
      append("styleSheetName", getStyleSheetName()).
      append("bundleName", getResourceBundleName());

    if (_translationSourceVE != null)
    {
      helper.append("translationExpr",  _translationSourceVE.getExpressionString());
    }

    if (_translationSourceVB != null)
    {
      helper.append("translationBindingExpr",  _translationSourceVB.getExpressionString());
    }

    return helper.toString();
  }
 
  
  /**
   * Gets the SkinAddition's translation source ValueExpresion. The 
   * ValueExpression can point to a Map or a ResourceBundle.
   * Note: A skin cannot have both a resourceBundleName and a translation source
   * value expression. If they do, then the resourceBundleName takes precedence.
   */
  public ValueExpression getTranslationSourceValueExpression()
  {
    return _translationSourceVE;
  } 
  
  /**
   * Gets the SkinAddition's translation source ValueBinding. The 
   * ValueBinding can point to a Map or a ResourceBundle.
   * Note: A skin cannot have both a resourceBundleName and a translation source
   * value binding. If they do, then the resourceBundleName takes precedence.
   * @deprecated
   */
  @Deprecated
  public ValueBinding getTranslationSourceValueBinding()
  {
    return _translationSourceVB;
  }

  /**
   * Gets any skin features added through the skin addition
   */
  public Map<String, String> getSkinFeatures()
  {
    return _skinFeatures;
  }

  /**
   * Compares two skinning additions for the purposes of ordering.  Currently this
   * method compares the stylesheet name.  If both names are null, then they are
   * considered equal.  If one of the names is null and the other isn't, the null
   * value is always less then the real value.  Other then that the values will be
   * returned according to the string's natural order.
   * 
   * @param t
   * @return
   * 
   * @throws NullPointerException if t is null
   * @throws ClassCastException id t is not an instance of StyleSheetAddition
   */
  public int compareTo(Object t)
  {
    String comparedName = ((SkinAddition)t).getStyleSheetName();
    String thisName     = getStyleSheetName();
    
    
    if(null == thisName)
    {
      if(null == comparedName)
      {
        //Both stylesheet names are null, so they are equal
        return 0;
      }
      
      //This stylesheet name is null and the other isn't.  This is always less
      return -1;
    }
    
    if(null == comparedName)
    {
      //Compared object is null, this one isn't
      return 1;
    }
    
    int result = thisName.compareTo(comparedName);
    
    return result;
  }

  /**
   * convenience builder for SkinAddition
   * does not support the deprecated ValueBinding for translationSource
   */
  public static class Builder
  {

    public Builder()
    {
      _skinFeatures = new SkinFeatures();
    }

    public Builder skinId(String skinId)
    {
      _skinId = skinId;
      return this;
    }

    public Builder styleSheetName(String styleSheetName)
    {
      _styleSheetName = styleSheetName;
      return this;
    }

    public Builder resourceBundleName(String resourceBundleName)
    {
      _resourceBundleName = resourceBundleName;
      return this;
    }

    public Builder translationSource(ValueExpression translationSource)
    {
      _translationSource = translationSource;
      return this;
    }

    public Builder features(SkinFeatures skinFeatures)
    {
      // set only a non null SkinFeatures object
      if (skinFeatures != null)
        _skinFeatures = skinFeatures;

      return this;
    }

    public SkinAddition build()
    {
      return new SkinAddition(this);
    }

    private String       _styleSheetName;
    private String       _skinId;
    private String       _resourceBundleName;
    private ValueExpression _translationSource;
    private SkinFeatures _skinFeatures;
  }

  @Override
  public boolean equals(Object o)
  {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    SkinAddition addition = (SkinAddition) o;

    if (_resourceBundleName != null ? !_resourceBundleName.equals(addition._resourceBundleName) : addition._resourceBundleName != null)
      return false;
    if (_skinFeatures != null ? !_skinFeatures.equals(addition._skinFeatures) : addition._skinFeatures != null)
      return false;
    if (_skinId != null ? !_skinId.equals(addition._skinId) : addition._skinId != null)
      return false;
    if (_styleSheetName != null ? !_styleSheetName.equals(addition._styleSheetName) : addition._styleSheetName != null)
      return false;
    if (_translationSourceVE != null ? !_translationSourceVE.equals(addition._translationSourceVE) : addition._translationSourceVE != null)
      return false;

    return true;
  }

  @Override
  public int hashCode()
  {
    int result = 17;
    result = 37 * result + (null == _styleSheetName ? 0 : _styleSheetName.hashCode());
    result = 37 * result + (null == _skinId ? 0 : _skinId.hashCode());
    result = 37 * result + (null == _resourceBundleName ? 0 : _resourceBundleName.hashCode());
    result = 37 * result + (null == _translationSourceVE ? 0 : _translationSourceVE.hashCode());
    result = 37 * result + (null == _skinFeatures ? 0 : _skinFeatures.hashCode());
    return result;
  }

  /**
   * Constructor for SkinAddition based on Builder
   * This constructor can be used to create SkinAddition object for any combination of parameters
   * @param builder
   * @See Builder
   */
  private SkinAddition (Builder builder)
  {
    _skinId = builder._skinId;
    _styleSheetName = builder._styleSheetName;
    _resourceBundleName = builder._resourceBundleName;
    _translationSourceVE = builder._translationSource;
    _skinFeatures = builder._skinFeatures.getFeatures();

    // this is deprecated so we do not support this in the new API
    _translationSourceVB = null;

  }

  private final String       _styleSheetName;
  private final String       _skinId;
  private final String       _resourceBundleName;
  private final ValueExpression _translationSourceVE;
  private final ValueBinding _translationSourceVB;
  private final Map<String, String> _skinFeatures;
  
}
