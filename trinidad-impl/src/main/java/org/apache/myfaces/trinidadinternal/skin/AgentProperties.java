package org.apache.myfaces.trinidadinternal.skin;

import java.util.Arrays;
import java.util.Set;

import org.apache.myfaces.trinidad.context.Version;


/**
 * Container to hold "@agent" properties like {@link Version}and "touchScreen"
 * capabilities.
 */
public final class AgentProperties
{
  public AgentProperties(Set<Version> versions,
                         Set<String> capabilityTouchScreen)
  {
    if (versions == null)
      throw new NullPointerException("versions must be non-null");

    if (capabilityTouchScreen == null)
      throw new NullPointerException("capabilityTouchScreen must be non-null");

    _versions = versions;
    _capabilityTouchScreen = capabilityTouchScreen;
    _hashCode = _versions.hashCode() * 37 + _capabilityTouchScreen.hashCode();
  }

  public Set<Version> getVersions()
  {
    return _versions;
  }

  public Set<String> getCapabilityTouchScreen()
  {
    return _capabilityTouchScreen;
  }

  @Override
  public boolean equals(Object o)
  {
    if (o == this)
      return true;
    else if (!(o instanceof AgentProperties))
      return false;
    else
    {
      AgentProperties otherAgentProperties = (AgentProperties) o;
      return _versions.equals(otherAgentProperties._versions) &&
        _capabilityTouchScreen.equals(otherAgentProperties._capabilityTouchScreen);
    }

  }

  @Override
  public String toString()
  {
    StringBuilder builder = new StringBuilder();
    builder.append("versions: ").append(Arrays.toString(_versions.toArray()));
    builder.append(" touchScreen: ").append(Arrays.toString(_capabilityTouchScreen.toArray()));
    return builder.toString();
  }

  @Override
  public int hashCode()
  {
    return _hashCode;
  }

  private final Set<Version> _versions;
  private final Set<String> _capabilityTouchScreen;
  private final int _hashCode;
}
