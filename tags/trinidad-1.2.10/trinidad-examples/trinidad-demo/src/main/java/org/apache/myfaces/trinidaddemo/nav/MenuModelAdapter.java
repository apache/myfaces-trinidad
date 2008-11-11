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
package org.apache.myfaces.trinidaddemo.nav;

import java.util.List;

import org.apache.myfaces.trinidad.model.ViewIdPropertyMenuModel;

/**
 * This class facilitates calling ViewIdPropertyMenuModel.addViewId via
 * an "aliasList".
 */
public class MenuModelAdapter implements java.io.Serializable
{
  public MenuModelAdapter()
  {
  }

  private ViewIdPropertyMenuModel _model = null;
  private List<Object> _aliasList = null;
  private boolean _aliasListAdded = false;

  /**
   *
   * @param model an instance of ViewIdPropertyMenuModel
   */
  public void setModel(ViewIdPropertyMenuModel model)
  {
    _model = model;
    _aliasListAdded = false;

  }

  public ViewIdPropertyMenuModel getModel()
  {
    if (_model != null && !_aliasListAdded)
    {
      _aliasListAdded = true;
      if(_aliasList != null && !_aliasList.isEmpty())
      {
        int size = _aliasList.size();
        if (size % 2 == 1)
          size = size - 1;

        for ( int i = 0; i < size; i=i+2)
        {
          _model.addViewId(_aliasList.get(i).toString(),
                         _aliasList.get(i+1).toString());
        }
      }
    }
    return _model;
  }

  public List<Object> getAliasList()
  {
    return _aliasList;
  }

  /**
   * aliasList is just a list of viewId strings grouped into pairs.
   * We iterate over the list like so:
   * ViewIdPropertyMenuModel.addViewId(aliasList.get(i), aliasList.get(i+1))
   */
  public void setAliasList(List<Object> aliasList)
  {
    _aliasList = aliasList;
  }
}
