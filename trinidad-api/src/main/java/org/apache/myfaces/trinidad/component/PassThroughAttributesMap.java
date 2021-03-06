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

package org.apache.myfaces.trinidad.component;

import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import org.apache.myfaces.trinidad.bean.AttachedObjects;
import org.apache.myfaces.trinidad.bean.FacesBean;

class PassThroughAttributesMap implements Map<String, Object>, Serializable
{
    private static final long serialVersionUID = -9106932179394257866L;

    private FacesBean _facesBean;

    PassThroughAttributesMap(FacesBean facesBean)
    {
        _facesBean = facesBean;
    }


    /**
     * Return the map containing the attributes.
     * <p/>
     * This method is package-scope so that the UIComponentBase class can access it
     * directly when serializing the component.
     */
    Map<String, Object> getUnderlyingMap()
    {
        AttachedObjects<String, Object> passThroughAttributes
                = (AttachedObjects<String, Object>) _facesBean.getProperty(UIXComponentBase._PASS_THROUGH_ATTRIBUTES_KEY);

        return passThroughAttributes == null ? Collections.EMPTY_MAP : passThroughAttributes.getAttachedObjectMap();
    }

    @Override
    public boolean equals(Object obj)
    {
        return getUnderlyingMap().equals(obj);
    }

    @Override
    public int hashCode()
    {
        return getUnderlyingMap().hashCode();
    }

    public int size()
    {
        return getUnderlyingMap().size();
    }

    public boolean isEmpty()
    {
        return getUnderlyingMap().isEmpty();
    }

    public boolean containsKey(Object key)
    {
        return getUnderlyingMap().containsKey(key);
    }

    public boolean containsValue(Object value)
    {
        return getUnderlyingMap().containsValue(value);
    }

    public Object get(Object key)
    {
        return getUnderlyingMap().get(key);
    }

    public Object put(String key, Object value)
    {
        AttachedObjects<String, Object> passThroughAttributesMap
                = (AttachedObjects<String, Object>) _facesBean.getProperty(UIXComponentBase._PASS_THROUGH_ATTRIBUTES_KEY);
        if (passThroughAttributesMap == null)
        {
            passThroughAttributesMap = new AttachedObjects<String, Object>();
            _facesBean.setProperty(UIXComponentBase._PASS_THROUGH_ATTRIBUTES_KEY, passThroughAttributesMap);
        }

        passThroughAttributesMap.addAttachedObject(key, value);
        return null;
    }

    public Object remove(Object key)
    {
        AttachedObjects<String, Object> passThroughAttributesMap
                = (AttachedObjects<String, Object>) _facesBean.getProperty(UIXComponentBase._PASS_THROUGH_ATTRIBUTES_KEY);
        if (passThroughAttributesMap != null)
        {
            passThroughAttributesMap.removeAttachedObject((String) key, get(key));
        }
        return null;
    }

    /**
     * Call put(key, value) for each entry in the provided map.
     */
    public void putAll(Map<? extends String, ?> t)
    {
        for (Map.Entry<? extends String, ?> entry : t.entrySet())
        {
            put(entry.getKey(), entry.getValue());
        }
    }

    /**
     * Return a set of all <i>attributes</i>. Properties of the underlying
     * UIComponent are not included, nor value-bindings.
     */
    public Set<Map.Entry<String, Object>> entrySet()
    {
        return getUnderlyingMap().entrySet();
    }

    public Set<String> keySet()
    {
        return getUnderlyingMap().keySet();
    }

    public void clear()
    {
        getUnderlyingMap().clear();
    }

    public Collection<Object> values()
    {
        return getUnderlyingMap().values();
    }
}
