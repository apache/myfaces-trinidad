/*
 * Copyright  2004-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.uinode;

import java.util.HashSet;
import java.util.Set;

import javax.faces.el.ValueBinding;


import org.apache.myfaces.adf.bean.FacesBean;
import org.apache.myfaces.adf.bean.FacesBeanImpl;
import org.apache.myfaces.adf.bean.PropertyKey;
import org.apache.myfaces.adf.bean.PropertyMap;
import org.apache.myfaces.adf.bean.util.FlaggedPropertyMap;

import org.apache.myfaces.adf.component.UIXChoose;
import org.apache.myfaces.adf.component.UIXCommand;
import org.apache.myfaces.adf.component.UIXComponent;
import org.apache.myfaces.adf.component.UIXFacesBean;
import org.apache.myfaces.adf.component.UIXGo;
import org.apache.myfaces.adf.component.UIXNavigationLevel;
import org.apache.myfaces.adf.component.UIXNavigationTree;
import org.apache.myfaces.adf.component.UIXInput;
import org.apache.myfaces.adf.component.UIXMenu;
import org.apache.myfaces.adf.component.UIXMessages;
import org.apache.myfaces.adf.component.UIXPanel;
import org.apache.myfaces.adf.component.UIXPoll;
import org.apache.myfaces.adf.component.UIXProcess;
import org.apache.myfaces.adf.component.UIXSelectItem;
import org.apache.myfaces.adf.component.UIXSelectMany;
import org.apache.myfaces.adf.component.UIXShowDetail;
import org.apache.myfaces.adf.component.UIXShowMany;
import org.apache.myfaces.adf.component.UIXShowOne;
import org.apache.myfaces.adf.component.UIXSingleStep;
import org.apache.myfaces.adf.component.UIXTree;

import org.apache.myfaces.adfinternal.ui.AttributeKey;
import org.apache.myfaces.adfinternal.ui.UIConstants;
import org.apache.myfaces.adfinternal.ui.UINode;

import org.apache.myfaces.adfinternal.ui.collection.AttributeMap;
import org.apache.myfaces.adfinternal.ui.collection.FlaggedAttributeMap;

import org.apache.myfaces.adfinternal.ui.data.BoundValue;

import org.apache.myfaces.adfinternal.uinode.bind.ClientIdBoundValue;
import org.apache.myfaces.adfinternal.uinode.bind.PropertyBoundValue;
import org.apache.myfaces.adfinternal.uinode.bind.URLBoundValue;

public class UINodeFacesBean extends FacesBeanImpl
                             implements UIXFacesBean
{
  static public class Type extends FacesBean.Type
  {
    public Type()
    {
      super(null);
    }


    protected PropertyKey createPropertyKey(
      String name,
      Class  type,
      Object defaultValue,
      int    capabilities,
      int    index)
    {
      return new UINodePropertyKey(name, type, defaultValue, 
                                   capabilities, index);
    }
  }

  public UINodeFacesBean()
  {
  }

  public void init(
    UIXComponent   component,
    FacesBean.Type type)
  {
    _type = type;
    _component = component;
    _node = createUINode(component,
                         getNamespaceURI(),
                         createAttributeMap(component.getFamily()));

    // Show the ID _only_ if the componentId is explicitly set,
    // or the component always wants IDs
    boolean alwaysRenderId = _ALWAYS_RENDER_ID.contains(component.getFamily());
    _node.setAttributeValue(
          UIConstants.ID_ATTR,
          new ClientIdBoundValue(component,
                                 !alwaysRenderId));

  }

  public FacesBean.Type getType()
  {
    return _type;
  }

  public UIXComponent getUIXComponent()
  {
    return _component;
  }

  public UINode getUINode()
  {
    return _node;
  }

  public void setUINodeAttribute(AttributeKey key, Object value)
  {
    _node.setAttributeValue(key, value);
  }


  protected String getNamespaceURI()
  {
    return UIConstants.MARLIN_NAMESPACE;
  }

  protected PropertyMap createPropertyMap()
  {
    return new FlaggedPropertyMap()
    {
      public Object put(PropertyKey key, Object value)
      {
        if (value == null)
        {
          return remove(key);
        }
        else
        {
          Object o = super.put(key, value);
          // Push the value into the UINode if necessary
          AttributeKey attrKey = getAttributeKey(key);
          if (attrKey != null)
            _node.setAttributeValue(attrKey, value);
          return o;
        }
      }

      public Object remove(PropertyKey key)
      {
        Object o = super.remove(key);
        if (o == null)
          return null;

        AttributeKey attrKey = getAttributeKey(key);
        if (attrKey != null)
        {
          // We're no longer shadowing a ValueBinding (if one's present);
          // re-establish that binding, or reset to null
          Object attrValue;
          ValueBinding binding = getValueBinding(key);
          if (binding == null)
            attrValue = null;
          else
            attrValue = new ValueBindingBoundValue(binding);

          _node.setAttributeValue(attrKey, attrValue);
        }

        return o;
      }

    };
  }

  protected PropertyMap createBindingsMap()
  {
    FlaggedPropertyMap bindings = new FlaggedPropertyMap()
    {
      public Object put(PropertyKey key, Object value)
      {
        if (value == null)
        {
          return remove(key);
        }
        else
        {
          Object o = super.put(key, value);
          AttributeKey attrKey = getAttributeKey(key);
          if (attrKey != null)
          {
            // If there's no local value, then we're not shadowing;
            // set up a new ValueBindingBoundValue
            if (getLocalProperty(key) == null)
            {
              ValueBinding binding = (ValueBinding) value;
              _node.setAttributeValue(attrKey,
                                      new ValueBindingBoundValue(binding));
            }
          }

          return o;
        }
      }

      public Object remove(PropertyKey key)
      {
        Object o = super.remove(key);
        if (o == null)
          return null;

        AttributeKey attrKey = getAttributeKey(key);
        if (attrKey != null)
        {
          // If there's no local value, then we're not shadowing,
          // and the value is just null now
          if (getLocalProperty(key) == null)
            _node.setAttributeValue(attrKey, null);
        }

        return o;
      }
    };

    bindings.setUseStateHolder(true);
    return bindings;
  }


  /**
   * Hook for creating the AttributeMap.  Add type-specific
   * AttributeKey overrides here.
   */
  protected AttributeMap createAttributeMap(String componentFamily)
  {
    FlaggedAttributeMap attrMap = new FlaggedAttributeMap();

    _setURLAttribute(attrMap, "source", UIConstants.SOURCE_ATTR);
    _setURLAttribute(attrMap, "destination", UIConstants.DESTINATION_ATTR);
    _setURLAttribute(attrMap, "longDescURL", UIConstants.LONG_DESC_URL_ATTR);

    if (_HAS_ICON_URL.contains(componentFamily))
      _setURLAttribute(attrMap, "icon", UIConstants.ICON_ATTR);

    return attrMap;
  }


  /**
   * Hook for converting PropertyKeys into AttributeKeys
   */
  protected AttributeKey getAttributeKey(PropertyKey key)
  {
    if (key instanceof UINodePropertyKey)
    {
      return ((UINodePropertyKey) key).getAttributeKey();
    }

    return null;
  }

  protected UIXComponentUINode createUINode(
    UIXComponent component,
    String       namespaceURI,
    AttributeMap attrMap)
  {
    return new UIXComponentUINode(component, namespaceURI, attrMap);
  }

  private void _setURLAttribute(
    AttributeMap attrMap,
    String       propKeyName,
    AttributeKey attrKey)
  {
    PropertyKey key = getType().findKey(propKeyName);
    if (key == null)
      return;

    _setURLAttribute(attrMap,
                     key,
                     attrKey);
  }

  private void _setURLAttribute(
    AttributeMap attrMap,
    PropertyKey  propKey,
    AttributeKey attrKey)
  {
    BoundValue propBV = new PropertyBoundValue(this, propKey);
    attrMap.setAttribute(attrKey, new URLBoundValue(propBV));
  }

  // If any of these "Sets" are small, eliminate and move to
  // custom subclass
  static private Set _HAS_ICON_URL = new HashSet();
  // Move this to a protected boolean hook?
  static private Set _ALWAYS_RENDER_ID = new HashSet();

  static
  {
    // For "GlobalButton"
    _HAS_ICON_URL.add(UIXCommand.COMPONENT_FAMILY);
    _HAS_ICON_URL.add(UIXGo.COMPONENT_FAMILY);
    // For "box" and "header"
    _HAS_ICON_URL.add(UIXPanel.COMPONENT_FAMILY);
    // For "header"
    _HAS_ICON_URL.add(UIXShowDetail.COMPONENT_FAMILY);
    
    // One reason components need ids when they submit events with source.
    // Another is PPR support.
    _ALWAYS_RENDER_ID.add(UIXShowDetail.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(UIXProcess.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(UIXMenu.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(UIXMessages.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(UIXNavigationLevel.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(UIXSingleStep.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(UIXShowMany.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(UIXShowOne.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(UIXPoll.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(UIXTree.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(UIXNavigationTree.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(UIXChoose.COMPONENT_FAMILY);

    /* "Core" doesn't have a component family!
    _ALWAYS_RENDER_ID.add(CoreTable.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(CoreTreeTable.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(CoreMessages.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(CoreSingleStepButtonBar.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(CoreShowOneTab.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(CoreSelectRangeChoiceBar.COMPONENT_FAMILY);*/

    _ALWAYS_RENDER_ID.add(UIXInput.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(UIXSelectItem.COMPONENT_FAMILY);
    _ALWAYS_RENDER_ID.add(UIXSelectMany.COMPONENT_FAMILY);
  }

  private FacesBean.Type      _type;
  private UIXComponent        _component;
  private UIXComponentUINode  _node;
}
