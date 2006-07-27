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
package org.apache.myfaces.trinidad.component;

import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * This class saves the state of stamp components.
 * @author The Oracle ADF Faces Team
 */
final class StampState implements Externalizable
{


  public StampState()
  {
  }

  /**
   * Clears all state except for the state associated with the
   * give currencyObj
   * @param skipCurrencyObj
   */
  public void clear(Object skipCurrencyObj)
  {
    if (!_rows.isEmpty())
    {
      Iterator iter = _rows.keySet().iterator();
      while(iter.hasNext())
      {
        DualKey dk = (DualKey) iter.next();
        if (_eq(dk._key1, skipCurrencyObj))
          continue;
        iter.remove();
      }
    }
  }

  public void put(Object currencyObj, String key, Object value)
  {
    if (_rows == Collections.EMPTY_MAP)
    {
      // =-=AEW Better default sizes
      _rows = new HashMap(109);
    }

    DualKey dk = new DualKey(currencyObj, key);
    _rows.put(dk, value);
  }

  public int size()
  {
    return _rows.size();
  }

  public Object get(Object currencyObj, String key)
  {
    DualKey dk = new DualKey(currencyObj, key);
    return _rows.get(dk);
  }

  /**
   * Save the per-row state of a given stamp.
   */
  public static Object saveStampState(FacesContext context, UIComponent stamp)
  {
    RowState state = _createState(stamp);
    if (state != null)
      state.saveRowState(stamp);
    return state;
  }

  /**
   * Restore the per-row state of a given stamp.
   */
  public static void restoreStampState(FacesContext context, UIComponent stamp,
                                       Object stampState)
  {
    if (stampState != null)
    {
      String stampId = stamp.getId();
      // Force the ID to be reset to reset the client identifier (needed
      // for UIComponentBase implementation which caches clientId too
      // aggressively)
      stamp.setId(stampId);

      RowState state = (RowState) stampState;
      state.restoreRowState(stamp);
    }
  }

  /**
   * save the stamp state of the children of the given column in the given table.
   */
  public static Object saveChildStampState(
    FacesContext context,
    UIComponent column,
    UIXCollection table)
  {
    List kids = column.getChildren();
    int sz = kids.size();
    Object[] state = new Object[sz];
    for(int i=0; i<sz; i++)
    {
      state[i] = table.saveStampState(context, (UIComponent) kids.get(i));
    }
    return state;
  }

  /**
   * restore the stamp state of the children of the given column in the given table.
   */
  public static void restoreChildStampState(
    FacesContext context,
    UIComponent column,
    UIXCollection table,
    Object stampState)
  {
    List kids = column.getChildren();
    Object[] state = (Object[]) stampState;
    for(int i=0; i<state.length; i++)
    {
      table.restoreStampState(context, (UIComponent) kids.get(i), state[i]);
    }
  }

  /**
   * @todo can do better...
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeInt(_rows.size());

    if (_rows.isEmpty())
      return;

    HashMap map = new HashMap(_rows.size());
    map.putAll(_rows);

    if (_LOG.isFinest())
    {
      Iterator entries = map.entrySet().iterator();
      while (entries.hasNext())
      {
        Map.Entry entry = (Map.Entry) entries.next();
        _LOG.finest("Saving " + entry.getKey() + ", " + entry.getValue());
      }
    }

    out.writeObject(map);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int size = in.readInt();

    if (size > 0)
    _rows = (Map) in.readObject();

    if (_LOG.isFinest())
    {
      Iterator entries = _rows.entrySet().iterator();
      while (entries.hasNext())
      {
        Map.Entry entry = (Map.Entry) entries.next();
        _LOG.finest("Restoring " + entry.getKey() + ", " + entry.getValue());
      }
    }
  }

  private static RowState _createState(UIComponent child)
  {
    if (child instanceof EditableValueHolder)
      return new EVHState();
    if (child instanceof UIXShowDetail)
      return new SDState();
    if (child instanceof UIXCollection)
      return new TableState();
    return null;
  }

  private static boolean _eq(Object k1, Object k2)
  {
    if (k1 == null)
      return k2 == null;
    return k1.equals(k2);
  }

  // State for a single row
  static private abstract class RowState implements Serializable
  {

    public RowState()
    {
    }

    abstract public void saveRowState(UIComponent child);

    abstract public void restoreRowState(UIComponent child);

    abstract public boolean isNull();
  }

  static private final class SDState extends RowState
  {
    public SDState()
    {
    }

    public void saveRowState(UIComponent child)
    {
      _disclosed = ((UIXShowDetail) child).isDisclosed();
    }

    public void restoreRowState(UIComponent child)
    {
      ((UIXShowDetail) child).setDisclosed(_disclosed);
    }

    public boolean isNull()
    {
      return !_disclosed;
    }

    public String toString()
    {
      return "SDState[disclosed=" + _disclosed + "]";
    }

    private boolean _disclosed;
  }

  static private final class TableState extends RowState
  {
    public TableState()
    {
    }

    public void saveRowState(UIComponent child)
    {
      _state = ((UIXCollection) child).__getMyStampState();
    }

    public void restoreRowState(UIComponent child)
    {
      ((UIXCollection) child).__setMyStampState(_state);
    }

    public boolean isNull()
    {
      return _state == null;
    }

    private Object _state = null;
  }

  static private class EVHState extends RowState
  {
    public EVHState()
    {
      _valid = true;
    }

    public void saveRowState(UIComponent child)
    {
      assert _assertIsStampCorrect(child);

      EditableValueHolder evh = (EditableValueHolder) child;
      _submitted = evh.getSubmittedValue();
      _localSet = evh.isLocalValueSet();
      _local = evh.getLocalValue();
      _valid = evh.isValid();
    }

    public void restoreRowState(UIComponent child)
    {
      assert _assertIsStampCorrect(child);

      EditableValueHolder evh = (EditableValueHolder) child;
      evh.setSubmittedValue(_submitted);
      evh.setValue(_local);
      evh.setLocalValueSet(_localSet);
      evh.setValid(_valid);

      assert _assertStampHonoursState(evh);
    }

    public boolean isNull()
    {
      return (_valid && (!_localSet) && (_submitted == null));
    }


    public String toString()
    {
      return "EVHState[value=" + _local + ",submitted=" + _submitted + "]";
    }

    private boolean _assertStampHonoursState(EditableValueHolder evh)
    {
      return (evh.getSubmittedValue() == _submitted) &&
        (evh.getLocalValue() == _local) &&
        (evh.isLocalValueSet() == _localSet) &&
        (evh.isValid() == _valid);
    }

    /**
     * Make sure that this stampState is used to save/restore state from the
     * same stamp each time.
     */
    private boolean _assertIsStampCorrect(UIComponent stamp)
    {
      if (_assertStamp != null)
      {
        String stampId = stamp.getId();
        String assertStampId = _assertStamp.getId();
        assert (((assertStampId == null) && (stampId == null)) ||
                ((assertStampId != null) && assertStampId.equals(stampId))) :
          "Current stamp:"+stamp+
          " with id:"+stamp.getId()+
          ". Previously stamp was:"+_assertStamp+
          " with id:"+_assertStamp.getId();
      }
      else
      {
        _assertStamp = stamp;
      }
      return true;
    }

    private Object _submitted;
    private Object _local;
    private boolean _localSet;
    private boolean _valid;
    private transient UIComponent _assertStamp = null;
  }

  private static final class DualKey implements Serializable
  {
    public DualKey(Object key1, Object key2)
    {
      _key1 = key1;
      _key2 = key2;

      _hash = _hash(key1) + _hash(key2);
    }

    public boolean equals(Object other)
    {
      if (other == this)
        return true;
      if (other instanceof DualKey)
      {
        DualKey otherKey = (DualKey) other;
        if (hashCode() != otherKey.hashCode())
          return false;

        return _eq(_key1, otherKey._key1) && _eq(_key2, otherKey._key2);
      }
      return false;
    }

    public int hashCode()
    {
      return _hash;
    }

    public String toString()
    {
      return "<"+_key1+","+_key2+">";
    }

    private static int _hash(Object k)
    {
      return (k==null) ? 0 : k.hashCode();
    }

    private final Object _key1, _key2;
    private final int _hash;
  }

  private Map _rows = Collections.EMPTY_MAP;

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(StampState.class);
}
