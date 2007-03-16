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
package org.apache.myfaces.trinidadinternal.util;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;

import java.util.Map;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import javax.servlet.http.HttpSession;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;


/**
 * A simple tokenized cache
 */
public class TokenCache implements Serializable
{
  /**
   * Character guaranteed to not be used in tokens
   */
  static public char SEPARATOR_CHAR = '.';

  /**
   * Gets a TokenCache from the session, creating it if needed.
   */
  @SuppressWarnings("unchecked")
  static public TokenCache getTokenCacheFromSession(
    FacesContext context,
    String       cacheName,
    boolean      createIfNeeded,
    int          defaultSize)
  {
    ExternalContext external = context.getExternalContext();
    Object session = external.getSession(true);
    assert(session != null);

    TokenCache cache;
    // Synchronize on the session object to ensure that
    // we don't ever create two different caches
    synchronized (session)
    {
      cache = (TokenCache) external.getSessionMap().get(cacheName);
      if ((cache == null) && createIfNeeded)
      {
        // Seed the token cache with the session ID (if available)
        int seed = 0;
        if (_USE_SESSION_TO_SEED_ID)
        {
          if (session instanceof HttpSession)
          {
            String id = ((HttpSession) session).getId();
            if (id != null)
              seed = id.hashCode();
          }
        }

        cache = new TokenCache(defaultSize, seed);

        external.getSessionMap().put(cacheName, cache);
      }
    }

    return cache;
  }


  /**
   * For serialization only
   */
  public TokenCache()
  {
    this(_DEFAULT_SIZE, 0);
  }


  /**
   * Create a TokenCache that will store the last "size" entries.
   */
  public TokenCache(int size)
  {
    this(size, 0);
  }

  public TokenCache(int size, int seed)
  {
    _cache = new LRU(size);
    _count = seed;
  }

  /**
   * Create a new token;  and use that token to store a value into
   * a target Map.
   */
  public String addNewEntry(
      Object value, 
      Map<String, Object> targetStore)
  {
    Object remove = null;
    String token = null;
    synchronized (this)
    {
      token = _getNextToken();

      assert(_removed == null);
      _cache.put(token, token);
      remove = _removed;
      _removed = null;
    }

    if (remove != null)
      targetStore.remove(remove);

    targetStore.put(token, value);

    return token;
  }


  /**
   * Returns true if an entry is still available.
   */
  public boolean isAvailable(String token)
  {
    synchronized (this)
    {
      return _cache.get(token) != null;
    }
  }

  /**
   * Removes a value from the cache.
   * @return previous value associated with the token, if any
   */
  public Object removeOldEntry(
      String token, 
      Map<String, Object> targetStore)
  {
    synchronized (this)
    {
      _LOG.finest("Removing token {0} from cache", token);
      _cache.remove(token);
      return targetStore.remove(token);
    }
  }

  /**
   * Clear a cache, without resetting the token.
   */
  public void clear(Map<String, Object> targetStore)
  {
    synchronized (this)
    {
      for(String keyToRemove : _cache.keySet())
      {
        _LOG.finest("Clearing token {0} from cache", keyToRemove);
        targetStore.remove(keyToRemove);
      }

      _cache.clear();
    }
  }

  private String _getNextToken()
  {
    synchronized (_lock)
    {
      _count = _count + 1;
      return Integer.toString(_count, 16);
    }
  }

  private void readObject(ObjectInputStream in)
    throws ClassNotFoundException, IOException
  {
    in.defaultReadObject();
    // Re-create the lock, which was transient
    _lock = new Object();
  }

  private class LRU extends LRUCache<String, String>
  {
    public LRU(int maxSize)
    {
      super(maxSize);
    }

    @Override
    protected void removing(Object key)
    {
      _removed = key;
    }
  }

  private final Map<String, String> _cache;
  private int      _count;
  private transient Object   _lock = new Object();

  // Hack instance parameter used to communicate between the LRU cache's
  // removing() method, and the addNewEntry() method that may trigger it
  private transient Object _removed;

  static private final boolean _USE_SESSION_TO_SEED_ID = true;

  static private final int _DEFAULT_SIZE = 15;
  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(TokenCache.class);
}
