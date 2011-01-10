package org.apache.myfaces.trinidadinternal.lifecycle;

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

import javax.faces.context.FacesContext;
import java.util.Arrays;

public final class PartialLifecycleUtils
{
  private static final TrinidadLogger LOG = TrinidadLogger.createTrinidadLogger(PartialLifecycleUtils.class);
  private PartialLifecycleUtils()
  {
    // no-op
  }

  public static String[] getPartialTargets(FacesContext facesContext)
  {
    if (facesContext.getExternalContext().getRequestMap().containsKey(PartialLifecycleUtils.class.getName()))
    {
      return (String[]) facesContext.getExternalContext().getRequestMap().get(PartialLifecycleUtils.class.getName());
    }
    return null;
  }

  public static void setPartialTargets(FacesContext facesContext, String... partialTargets)
  {
    if (LOG.isFine()) {
      LOG.fine("Found Partial Targets {0}", Arrays.asList(partialTargets));
    }
    facesContext.getExternalContext().getRequestMap().put(PartialLifecycleUtils.class.getName(), partialTargets);
  }
}
