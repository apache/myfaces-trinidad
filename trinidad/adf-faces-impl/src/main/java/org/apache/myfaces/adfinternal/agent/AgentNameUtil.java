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

import org.apache.myfaces.adf.context.Agent;


/**
 * An utility class that maps agent/platform name strings to AdFacesAgent
 * Application constants and vice versa
 *
 */
public class AgentNameUtil
{
  /**
   * utility method to get AdfFacesAgent application constant (int) from agent name strings
   *
   * @param agentName
   * @return
   */
  public static int getAgent(String agentName)
  {
    if (AdfFacesAgent.AGENT_NETSCAPE.equals(agentName))
    {
      return AdfFacesAgent.APPLICATION_NETSCAPE;
    }

    if (AdfFacesAgent.AGENT_IE.equals(agentName))
    {
      return AdfFacesAgent.APPLICATION_IEXPLORER;
    }

    if (AdfFacesAgent.AGENT_GECKO.equals(agentName))
    {
      return AdfFacesAgent.APPLICATION_GECKO;
    }

    if (AdfFacesAgent.AGENT_ELAINE.equals(agentName))
    {
      return AdfFacesAgent.APPLICATION_WEB_CLIPPING;
    }

    if (AdfFacesAgent.AGENT_ICE_BROWSER.equals(agentName))
    {
      return AdfFacesAgent.APPLICATION_ICE;
    }

    if (AdfFacesAgent.AGENT_PIXO.equals(agentName))
    {
      return AdfFacesAgent.APPLICATION_PIXO;
    }

    if ((AdfFacesAgent.AGENT_NETFRONT.equals(agentName)) ||
        (AdfFacesAgent.AGENT_WEBPRO.equals(agentName)))
    {
      return AdfFacesAgent.APPLICATION_NET_FRONT;
    }

    if (AdfFacesAgent.AGENT_WEBKIT.equals(agentName))
    {
      return AdfFacesAgent.APPLICATION_SAFARI;
    }

    return AdfFacesAgent.APPLICATION_UNKNOWN;
  }

  /**
   * utility method to get AdfFacesAgent application constant (int) from platform/os name strings
   *
   * @param platformName
   * @return
   */
  public static int getPlatform(String platformName)
  {
    if (AdfFacesAgent.PLATFORM_WINDOWS.equals(platformName))
    {
      return AdfFacesAgent.OS_WINDOWS;
    }

    if (AdfFacesAgent.PLATFORM_MAC.equals(platformName))
    {
      return AdfFacesAgent.OS_MACOS;
    }

    if (AdfFacesAgent.PLATFORM_LINUX.equals(platformName))
    {
      return AdfFacesAgent.OS_LINUX;
    }

    if (AdfFacesAgent.PLATFORM_SOLARIS.equals(platformName))
    {
      return AdfFacesAgent.OS_SOLARIS;
    }

    if (AdfFacesAgent.PLATFORM_PALM.equals(platformName))
    {
      return AdfFacesAgent.OS_PALM;
    }

    if (AdfFacesAgent.PLATFORM_PPC.equals(platformName))
    {
      //return unknown for now
      return AdfFacesAgent.OS_PPC;
    }

    return AdfFacesAgent.OS_UNKNOWN;
  }

  /**
   * utility method to get AdfFacesAgent application constant (int) from Agent type objects
   *
   * @param otype
   * @return
   */
  public static int getAgentType(Object otype)
  {
    if (otype == Agent.TYPE_PDA)
    {
      return AdfFacesAgent.TYPE_PDA;
    }

    if (otype == Agent.TYPE_PHONE)
    {
      return AdfFacesAgent.TYPE_PHONE;
    }

    //Default to desktop (This is UIX 2.2 logic)
    return AdfFacesAgent.TYPE_DESKTOP;
  }

  /**
   * utility method to get agent name string from AdfFacesAgent application constant (int)
   *
   * @param agentId
   * @return
   */
  public static String getAgentName(int agentId)
  {
    switch (agentId) {
      case AdfFacesAgent.APPLICATION_UNKNOWN:
        return null;
      case AdfFacesAgent.APPLICATION_NETSCAPE:
        return AdfFacesAgent.AGENT_NETSCAPE;
      case AdfFacesAgent.APPLICATION_IEXPLORER:
        return AdfFacesAgent.AGENT_IE;
      case AdfFacesAgent.APPLICATION_GECKO:
        return AdfFacesAgent.AGENT_GECKO;
      case AdfFacesAgent.APPLICATION_WEB_CLIPPING:
        return AdfFacesAgent.AGENT_ELAINE;
      case AdfFacesAgent.APPLICATION_ICE:
        return AdfFacesAgent.AGENT_ICE_BROWSER;
      case AdfFacesAgent.APPLICATION_PIXO:
        return AdfFacesAgent.AGENT_PIXO;
      case AdfFacesAgent.APPLICATION_NET_FRONT:
        return AdfFacesAgent.AGENT_NETFRONT;
      case AdfFacesAgent.APPLICATION_SAFARI:
        return AdfFacesAgent.AGENT_WEBKIT;
       default:
        return null;
    }
  }



  /**
   * utility method to get platform name string from AdfFacesAgent application constant (int)
   *
   * @param platformId
   * @return
   */
  public static String getPlatformName(int platformId)
  {
    switch (platformId) {
      case AdfFacesAgent.OS_UNKNOWN:
        return null;
      case AdfFacesAgent.OS_WINDOWS:
        return AdfFacesAgent.PLATFORM_WINDOWS;
      case AdfFacesAgent.OS_MACOS:
        return AdfFacesAgent.PLATFORM_MAC;
      case AdfFacesAgent.OS_LINUX:
        return AdfFacesAgent.PLATFORM_LINUX;
      case AdfFacesAgent.OS_SOLARIS:
        return AdfFacesAgent.PLATFORM_SOLARIS;
      case AdfFacesAgent.OS_PALM:
        return AdfFacesAgent.PLATFORM_PALM;
      case AdfFacesAgent.OS_PPC:
        return AdfFacesAgent.PLATFORM_PPC;
      default:
        return null;
    }
  }

  /**
   * utility method to get type obejct from AdfFacesAgent application constant (int)
   *
   * @param type
   * @return
   */
  public Object getType (int type) {
    switch (type) {
      case AdfFacesAgent.TYPE_DESKTOP:
        return null;
      case AdfFacesAgent.TYPE_PDA:
        return Agent.TYPE_PDA;
      case AdfFacesAgent.TYPE_PHONE:
        return Agent.TYPE_PHONE;
      case AdfFacesAgent.TYPE_VOICE:
      default:
        return Agent.TYPE_UNKNOWN;
    }
  }

}
