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

import javax.faces.lifecycle.Lifecycle;
import javax.faces.lifecycle.LifecycleFactory;
import java.util.Iterator;


public class PartialLifecycleFactory extends LifecycleFactory
{

  private static final TrinidadLogger LOG = TrinidadLogger.createTrinidadLogger(PartialLifecycleFactory.class);

  private LifecycleFactory factory;
  private PartialLifecycle defaultLifecycle;

  public PartialLifecycleFactory(LifecycleFactory factory)
  {
    this.factory = factory;
    defaultLifecycle = new PartialLifecycle();
    if (LOG.isInfo())
    {
      LOG.info("new PartialLifecycleFactory");
    }
  }

  public void addLifecycle(String lifecycleId, Lifecycle lifecycle)
  {
    factory.addLifecycle(lifecycleId, lifecycle);
    if (LOG.isInfo())
    {
      LOG.info("Lifecycle added : " + lifecycleId + " = " + lifecycle.getClass().getName() + "");
    }
  }

  public Lifecycle getLifecycle(String lifecycleId)
  {
    if (LifecycleFactory.DEFAULT_LIFECYCLE.equals(lifecycleId))
    {
      if (LOG.isInfo())
      {
        LOG.info("getLifecycle(\"" + lifecycleId + "\")  -> PartialLifecycle");
      }
      return defaultLifecycle;
    } else
    {
      if (LOG.isInfo())
      {
        LOG.info("getLifecycle(\"" + lifecycleId + "\")  -> other Lifecycle");
      }
      return factory.getLifecycle(lifecycleId);
    }
  }

  public Iterator getLifecycleIds()
  {
    if (LOG.isInfo())
    {
      LOG.info("getLifecycleIds()");
    }
    return factory.getLifecycleIds();
  }
}
