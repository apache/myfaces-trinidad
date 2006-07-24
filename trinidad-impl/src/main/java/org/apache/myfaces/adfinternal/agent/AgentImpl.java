/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.adfinternal.agent;

import java.util.HashMap;
import java.util.Map;

import org.apache.myfaces.adf.logging.ADFLogger;


/**
 * The implementation of agent interface
 * <p></p>
 * This implementation supports agents recognized by all uix22
 * This class returns name strings (instead of int's)
 * Certain agents/platforms have been renamed
 * - using "webkit" (instead of safari), on recommendation from uix team
 * - using "gecko" for all gecko based browsers
 * - using "ppc" (instead of windows) for platform
 * <p></p>
 *
 */

public class AgentImpl extends DefaultAgent
{

  public AgentImpl()
  {
    this(false);
  }

  public AgentImpl(boolean nullAgentEntry)
  {
    if (nullAgentEntry)
    {
      _LOG.warning(
      "The agent type is unknown; creating an agent with null agent attributes.");
      _entry = _NULL_AGENT_ENTRY;
    }
    else
    {
      _entry = new AgentEntry();
    }
  }

/*  public AgentImpl(String userAgent, String accept)
  {
    _entry = _getAgentEntry(userAgent, accept);
  }
  */
  public Object getType()
  {
    return _entry._type;
  }

  public String getAgentName()
  {
    return _entry._agent;
  }

  public String getAgentVersion()
  {
    return _entry._agentVersion;
  }

  public String getPlatformName()
  {
    return _entry._platform;
  }

  public String getPlatformVersion()
  {
    return _entry._platformVersion;
  }

  public String getHardwareMakeModel()
  {
    return _entry._makeModel;
  }

  public Map getCapabilities()
  {
    return _requestCapabilities;
  }

  //setter methods for AgentImpl
  public void setType(Object type)
  {
    _entry._type = type;
  }

  public void setAgent(String agent)
  {
    _entry._agent = agent;
  }

  public void setAgentVersion(String version)
  {
    _entry._agentVersion = version;
  }

  public void setPlatform(String platform)
  {
    _entry._platform = platform;
  }

  public void setPlatformVersion(String version)
  {
    _entry._platformVersion = version;
  }

  public void setMakeModel(String makemodel)
  {
    _entry._makeModel = makemodel;
  }

  public void setAgentEntryToNULL()
  {
    _entry = _NULL_AGENT_ENTRY;
  }

  //Private entry structure to
  //store the Agent attributes
  private static class AgentEntry
  {
    Object _type = TYPE_UNKNOWN;
    String _agent;
    String _agentVersion;
    String _platform;
    String _platformVersion;
    String _makeModel;
  }

  void __addRequestCapability(CapabilityKey key,Object value)
  {
     if (_requestCapabilities == null)
     {
       _requestCapabilities = new HashMap();
     }
    _requestCapabilities.put(key,value);
  }

  private HashMap _requestCapabilities;
  private AgentEntry _entry;
  static private final AgentEntry _NULL_AGENT_ENTRY = new AgentEntry();
  static final private ADFLogger _LOG = ADFLogger.createADFLogger(AgentImpl.class);

}
