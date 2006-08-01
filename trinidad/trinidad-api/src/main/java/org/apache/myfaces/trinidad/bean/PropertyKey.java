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
package org.apache.myfaces.trinidad.bean;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.bean.util.StateUtils;

/**
 * Key for an entry in a FacesBean.
 * 
 * @author The Oracle ADF Faces Team
 * 
 * @todo default values?
 * @todo some deeper concept of type?
 * @todo can list entries ensure that 
 */
public class PropertyKey
{
  /**
   * Capability indicating this property does not support bindings.
   */
  static public final int CAP_NOT_BOUND = 1;

  /**
   * Capability indicating this property is transient.
   */
  static public final int CAP_TRANSIENT = 2;

  /**
   * Capability indicating this property describes a list.  List
   * PropertyKeys will automatically be marked as not supporting
   * bindings.
   */
  static public final int CAP_LIST = 4;

  /**
   * Capability indicating this property can use the StateHolder API.
   */
  static public final int CAP_STATE_HOLDER = 8;

  /**
   * Create a named PropertyKey, not attached to any type.
   */
  static public PropertyKey createPropertyKey(String name)
  {
    return new PropertyKey(name);
  }
  
  //
  // Constructors, all package-private.  Only the constructor
  // that takes a simple String could be exposed at all safely;
  // note that state saving cannot possibly re-create an
  // anonymous PropertyKey with anything other than default metadata,
  // so it is only safe to create such a PropertyKey as part
  // of a FacesBean.Type.
  //
  PropertyKey(
    String name)
  {
    this(name, _TYPE_DEFAULT);
  }
  
  PropertyKey(
    String   name,
    Class<?> type)
  {
    this(name, type, null);
  }

  PropertyKey(
    String   name,
    Class<?> type,
    Object   defaultValue)
  {
    this(name, type, defaultValue, _CAPS_DEFAULT, -1);
  }

  // Needs to be protected for UINodePropertyKey implementation
  protected PropertyKey(
    String   name,
    Class<?> type,
    Object   defaultValue,
    int      capabilities,
    int      index)
  {
    if (name == null)
      throw new NullPointerException();

    if (type == null)
      throw new NullPointerException();

    if (defaultValue != null)
    {
      // Ensure that default value is legal for this property type.
      Class<?> boxedType = _getBoxedType(type);
      if (!boxedType.isAssignableFrom(defaultValue.getClass()))
      {
        throw new IllegalStateException(
            "Default value " + defaultValue +
            " is not assignable to type " + type);
      }
    }
    else
    {
      // Default the default value according to Java Language Specification
      defaultValue = _getJavaDefault(type);

      // simplify equality testing in .equals()
      if (defaultValue == null)
        defaultValue = _OBJECT_NULL;
    }
        
    if ((capabilities & ~_CAPS_ALL) != 0)
      throw new IllegalStateException(
          "Capability mask " + (capabilities & ~_CAPS_ALL) +
          " not understood");

    // Lists cannot be bound
    if ((capabilities & CAP_LIST) != 0)
      capabilities = capabilities | CAP_NOT_BOUND;

    _name = name;
    _type = type;
    _default = defaultValue;
    _capabilities = capabilities;
    _index = index;
    
    _hashCode = _name.hashCode();
  }

  /**
   * Returns the type of this property.
   */
  public Class<?> getType()
  {
    return _type;
  }

  /**
   * Returns the default value of this property.
   */
  public Object getDefault()
  {
    return (_default != _OBJECT_NULL) ? _default : null;
  }

  /**
   * Returns the owning type for this property key.
   */
  public FacesBean.Type getOwner()
  {
    return _owner;
  }

  /**
   * Returns true if the property supports being bound.
   */
  public boolean getSupportsBinding()
  {
    return (_capabilities & CAP_NOT_BOUND) == 0;
  }

  /**
   * Returns true if the property is transient.
   */
  public boolean isTransient()
  {
    return (_capabilities & CAP_TRANSIENT) != 0;
  }

  /**
   * Returns true if the property is used to store a list.
   */
  public boolean isList()
  {
    return (_capabilities & CAP_LIST) != 0;
  }

  /**
   * Returns the name of this property.
   */
  public String getName()
  {
    return _name;
  }

  /**
   * Returns the index of this property.
   */
  public int getIndex()
  {
    return _index;
  }


  public Object saveValue(
    FacesContext context,
    Object value)
  {
    if ((_capabilities & CAP_STATE_HOLDER) != 0)
      return StateUtils.saveStateHolder(context, value);

    if (isList())
      return StateUtils.saveList(context, value);

    return value;
  }

  public Object restoreValue(
    FacesContext context,
    Object savedValue)
  {
    if ((_capabilities & CAP_STATE_HOLDER) != 0)
      return StateUtils.restoreStateHolder(context, savedValue);

    if (isList())
      return StateUtils.restoreList(context, savedValue);

    return savedValue;
  }

  
  @Override
  public boolean equals(Object o)
  {
    if (o == this)
      return true;

    if (!(o instanceof PropertyKey))
      return false;

    PropertyKey that = (PropertyKey)o;

    // If we're not both in the same Type, we're not equals.
    if (_owner != that._owner)
      return false;

    // If the index is -1, then it might be an anonymous
    // type, in which case we have to compare names
    int index = this._index;
    if (index == -1)
    {
      return ((that._index == -1) && 
              (that._name.equals(_name)) &&
              (that._type.equals(_type)) &&
              (that._default.equals(_default)));
    }

    // But otherwise, since the types are the same, then the
    // same index would imply the same instance - which would
    // have been caught up above.  So, again, this ain't the same equal
    return false;
  }

  @Override
  public int hashCode()
  {
    return _hashCode;
  }

  @Override
  public String toString()
  {
    String className = getClass().getName();
    int lastPeriod = className.lastIndexOf('.');
    if (lastPeriod >= 0)
      className = className.substring(lastPeriod + 1);

    if (_index >= 0)
      return className + "[" + _name + "," + _index + "]";
    return className + "[" + _name + "]";
  }

  void __setOwner(FacesBean.Type owner)
  {
    _owner = owner;
  }

  static private Object _getJavaDefault(
    Class<?> type)
  {
    return _PRIMITIVE_DEFAULTS.get(type);
  }
  
  static private Class<?> _getBoxedType(
    Class<?> type)
  {
    Class<?> boxedType = _BOXED_PRIMITIVES.get(type);
    return (boxedType != null ? boxedType : type);
  }
  
  static private Map<Class<?>, Object> _createPrimitiveDefaults()
  {
    Map<Class<?>, Object> map = new HashMap<Class<?>, Object>();
    map.put(Boolean.TYPE, Boolean.FALSE);
    map.put(Byte.TYPE, new Byte((byte)0));
    map.put(Character.TYPE, new Character('\0'));
    map.put(Double.TYPE, new Double(0.0));
    map.put(Float.TYPE, new Float(0.0f));
    map.put(Integer.TYPE, new Integer(0));
    map.put(Long.TYPE, new Long(0L));
    map.put(Short.TYPE, new Short((short)0));

    return Collections.unmodifiableMap(map);
  }

  static private Map<Class<?>, Class<?>> _createBoxedPrimitives()
  {
    Map<Class<?>, Class<?>> map = new HashMap<Class<?>, Class<?>>();
    map.put(Boolean.TYPE, Boolean.class);
    map.put(Byte.TYPE, Byte.class);
    map.put(Character.TYPE, Character.class);
    map.put(Double.TYPE, Double.class);
    map.put(Float.TYPE, Float.class);
    map.put(Integer.TYPE, Integer.class);
    map.put(Long.TYPE, Long.class);
    map.put(Short.TYPE, Short.class);

    return Collections.unmodifiableMap(map);
  }

  static private final Map<Class<?>, Object>   _PRIMITIVE_DEFAULTS = _createPrimitiveDefaults();
  static private final Map<Class<?>, Class<?>> _BOXED_PRIMITIVES   = _createBoxedPrimitives();

  private final int     _hashCode;
  private final String   _name;
  private final int      _index;
  private final int      _capabilities;
  private final Class<?> _type;
  private final Object   _default;
  private       FacesBean.Type _owner;

  static private final int _CAPS_DEFAULT = 
    0;
  
  static private final int _CAPS_ALL = 
    CAP_NOT_BOUND |
    CAP_TRANSIENT |
    CAP_LIST |
    CAP_STATE_HOLDER;

  static private final Class<Object> _TYPE_DEFAULT = Object.class;

  static private final Object _OBJECT_NULL = new Object();
}

