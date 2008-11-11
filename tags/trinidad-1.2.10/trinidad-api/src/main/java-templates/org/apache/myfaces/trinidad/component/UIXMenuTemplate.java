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
package org.apache.myfaces.trinidad.component;

import javax.faces.FacesException;
import javax.faces.context.FacesContext;
import javax.faces.component.ContextCallback;
import javax.faces.component.NamingContainer;

/**
 * <p>
 * @version $Name:  $ ($Revision: 518820 $) $Date: 2007-03-15 18:02:36 -0700 (Thu, 15 Mar 2007) $
 */
abstract public class UIXMenuTemplate extends UIXComponentBase
                                      implements NamingContainer
{


  @Override
  public boolean invokeOnComponent(FacesContext context,
                                   String clientId,
                                   ContextCallback callback)
    throws FacesException
  {
    String thisClientId = getClientId(context);

    if (clientId.equals(thisClientId))
    {
      callback.invokeContextCallback(context, this);
      return true;
    }

    // This component is a naming container. If the client id shows it's inside this naming container,
    // then process further.
    // Otherwise we know the client id we're looking for is not in this naming container,
    // so for improved performance short circuit and return false.
    else if (clientId.startsWith(thisClientId) &&
             (clientId.charAt(thisClientId.length()) ==
              NamingContainer.SEPARATOR_CHAR))
    {

      return super.invokeOnComponent(context, clientId, callback);
    }

    return false;
  }

}
