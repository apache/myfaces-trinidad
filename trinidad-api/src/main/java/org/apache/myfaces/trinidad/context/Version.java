/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidad.context;

import java.util.Arrays;
import java.util.regex.Pattern;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.Range;

/**
 * Immutable Representation of a dot-separated version.
 *
 * This representation
 * allows individual sections of the version to be wild-carded and allows
 * for comparisons between Versions with different numbers of version
 * subsections to be compared.  When comparing Versions, each version
 * subsection is compared from left to right.  If one Version doesn't have
 * a version subsection at the current index, the value of versionPadding
 * is used for this comparison.  Version subsections with the wild-card value "*"
 * are considered equal.  The value returned by compareTo() is the value of the
 * first non-equal version subsection or zero if all subsections match.
 *
 * Due to the support for wild-cards, this class has a natural ordering
 * that is inconsistent with equals.  For example,
 * <code>Version("5", "*").compareTo(Version("5.0", "*") == 0</code>
 * <code>Version("5", "*").equals(Version("5.0", "*") == false;</code>
 * 
 * The concrete versions produced by toMinimumVersion() and toMaximumVersion()
 * do have consistent compareTo()/equals() behavior, as these versions are
 * guaranteed to not contain wildcards.
 * 
 * @author Blake Sullivan
 */
public final class Version implements Comparable<Version>
{

  /**
   * A constant value holding the minimum value a version can have: 0.
   */
  public static final Version MIN_VERSION;
  
  /**
   * A constant value holding a maximum upper bound for versions.
   *
   * In theory there is no upper limit to version string values, ie. version
   * strings could be infinitely long.  However, in practice it can be
   * helpful to have some way to identify a concrete maximum upper bound to
   * a range of versions.  Version.MAX_VERSION specifies the Integer.MAX_VALUE
   * version for this purpose.
   */
  public static final Version MAX_VERSION;
  
  /**
   * A range of versions from MIN_VERSION to MAX_VERSION.
   */
  public static final Range<Version> ALL_VERSIONS;

  /**
   * Creates a Version instance from the dot-separated Version String using null as the padding
   * @param version The dot-separated version to represent
   * @throws NullPointerException if the version is null
   * @throws IllegalArgumentException if the version is an empty String
   * @see #Version(String, String)
   */
  public Version(String version)
  {
    this(version, null);
  }
  
  /**
   * Creates a Version instance from the dot-separated Version String and the
   * versionPadding.
   * @param version The dot-separated version to represent
   * @param versionPadding The value to return for sub-version sections
   * requested beyond the sub-version sections present in the version String.
   * If null or empty, no padding will be performed.
   * @throws NullPointerException if version is null
   * @throws IllegalArgumentException if version is the empty String
   */
  public Version(String version, String versionPadding)
  {
    _checkNonEmptyString(version, "version");
    if (versionPadding == null)
    {
      versionPadding = "";
    }
    
    // build the array of subversions
    _versions = _DOT_SPLITTER.split(version, 0);
    
    // We also store away int representations of version strings, since
    // this is necessary for more accurate comparison - ie. when comparing
    // version segments, we want to compare int 9 vs 10, since comparing
    // "9".compareTo("10") produces undesirable results.
    _intVersions = _toIntVersions(_versions, version);

    _versionPadding = versionPadding;
    
    // since we're immutable, we might as well calculate this up front
    // while we still have the String version around
    _hashCode = version.hashCode() * 37 + versionPadding.hashCode();
  }

  /**
   * When comparing Versions, each version
   * subsection is compared from left to right.  If one Version doesn't have
   * a version subsection at the current index, the value of versionPadding
   * is used for this comparison.  Version subsections with the wild-card value "*"
   * care considered equal.  The value returned by compareTo() is the value of the
   * first non-equal version subsection or zero if all subsections match.
   * @param otherVersion The Version object to compare this Version Object with
   * @return a negative integer, zero, or a positive integer as this object
   *         is less than, equal to, or greater than the specified object.
   */
  public int compareTo(Version otherVersion)
  {
    int ourVersionCount = _versions.length;
    int otherVersionCount = otherVersion._versions.length;
    
    int compareCount = (ourVersionCount > otherVersionCount)
                         ? ourVersionCount
                         : otherVersionCount;
    
    for (int versionIndex = 0; versionIndex < compareCount; versionIndex++)
    {
      int result = _compareVersions(otherVersion, versionIndex);

      // not equal, so return the result
      if (result != 0)
        return result;
    }
    
    // equivalent
    return 0;
  }
  
  /**
   * Converts this Version to an equivalent "minimum" instance.
   * 
   * Interior wildcard segements are replaced with "0".
   * The trailing wildcard segment (if present) is dropped.
   * Wildcard version padding is replaced with null padding.
   * 
   * If no wilcards are present, returns this Version instance.
   */
  public Version toMinimumVersion()
  {
    if (!_containsWildcard() && !_isWildcard(_versionPadding))
    {
      return this;
    }
    
    return new Version(_toString("0", true));
  }

  /**
   * Converts this Version to an equivalent "maximum" instance.
   * 
   * Both wildcard segements and wilcard padding are replaced with
   * Integer.MAX_VALUE.
   * If no wilcards are present, returns this Version instance.
   */
  public Version toMaximumVersion()
  {
    if (!_containsWildcard() && !_isWildcard(_versionPadding))
    {
      return this;
    }
    
    return new Version(_toString(_MAX_STRING, false), _MAX_STRING);
  }

  @Override
  public String toString()
  {
    return _toString(_WILDCARD, false);
  }
  
  @Override
  public boolean equals(Object o)
  {
    if (o == this)
      return true;
    else if (!(o instanceof Version))
      return false;
    else
    {
      Version otherVersion = (Version)o;
      
      // we are equal if all of version content and padding are equal
      return _versionPadding.equals(otherVersion._versionPadding) &&
             Arrays.equals(_versions, otherVersion._versions);
    }
  }
  
  @Override
  public int hashCode()
  {
    // used cached version
    return _hashCode;
  }

  // Converts an array of String version segments to
  // an array of int versions more suitable for use in
  // comparisons.
  // 
  // Note that not all version segments can be converted
  // to an int.  For example, the version could include
  // non-numeric characters.  Also wildcard segments will 
  // fail to convert.  We identify these segments by setting
  // the int value to _NON_INT_VERSION.  We can then fall
  // back on using String comparisions for these segments.
  //
  // The fullVersion is provided for error handling only.
  private static int[] _toIntVersions(String[] versions, String fullVersion)
  {
    assert(versions != null);
    int[] intVersions = new int[versions.length];
    
    for (int i = 0; i < versions.length; i++)
    {
      intVersions[i] = _toIntVersion(versions[i], fullVersion);
    }
    
    return intVersions;
  }
  
  // Converts a String version segment to the corresponding
  // int.  Returns _NON_INT_VERSION if the String cannot be
  // converted (eg. wildcard character).
  //
  // The fullVersion is provided for error handling only.
  private static int _toIntVersion(String versionSegment, String fullVersion)
  {
    if (_DIGITS_PATTERN.matcher(versionSegment).matches())
    {
      try
      {
        return Integer.parseInt(versionSegment);
      }
      catch (NumberFormatException e)
      {
        // Since we already filtered out version strings
        // that didn't match the _DIGITS_PATTERN, the only
        // case where we should arrive here is if the version
        // string overflows int.
        _LOG.warning("UNEXPECTED_VERSION_VALUE", new Object[] { versionSegment, fullVersion });                
      }
    }
    
    return _NON_INT_VERSION;
  }

  /**
   * Compares the version segment at the specified index.
   * 
   * @param otherVersion the Version instance to which we are comparing.
   * @param versionIndex the index of the version segment that we are testing
   * @return < 0 if this Version's segment is < otherVersion's segment.  0 if 
   *   equal.  Otherwise, > 1.
   */
  private int _compareVersions(Version otherVersion, int versionIndex)
  {
    if (_isIntComparable(otherVersion, versionIndex))
    {
      return _compareIntVersions(otherVersion, versionIndex);
    }
    
    return _compareStringVersions(otherVersion, versionIndex);
  }
  
  /**
   * Tests whether an int comparison can be performed for a particular
   * version segment.
   * 
   * @param otherVersion the Version instance to which we are comparing.
   * @param versionIndex the index of the version segment that we are testing
   * @return true if both this Version and otherVersion have an int value
   *   for the specified version index.
   */
  private boolean _isIntComparable(Version otherVersion, int versionIndex)
  {
    int[] ourIntVersions = _intVersions;
    int[] otherIntVersions = otherVersion._intVersions;
    
    return ((versionIndex < ourIntVersions.length)             &&
            (versionIndex < otherIntVersions.length)           &&
            (ourIntVersions[versionIndex] != _NON_INT_VERSION) &&
            (otherIntVersions[versionIndex] != _NON_INT_VERSION));
  }

  /**
   * Compares the int version segments at the specified index.
   * 
   * @param otherVersion the Version instance to which we are comparing.
   * @param versionIndex the index of the version segment that we are testing.
   * @return < 0 if this Version's segment is < otherVersion's segment.  0 if 
   *   equal.  Otherwise, > 1.
   */
  private int _compareIntVersions(Version otherVersion, int versionIndex)
  {
    assert(_isIntComparable(otherVersion, versionIndex));
    int ourIntVersion = _intVersions[versionIndex];
    int otherIntVersion = otherVersion._intVersions[versionIndex];
    
    return (ourIntVersion < otherIntVersion) ? -1 : (ourIntVersion > otherIntVersion ? 1 : 0);
  }

  /**
   * Compares the String version segment at the specified index.
   * 
   * @param otherVersion the Version instance to which we are comparing.
   * @param versionIndex the index of the version segment that we are testing
   * @return < 0 if this Version's segment is < otherVersion's segment.  0 if 
   *   equal.  Otherwise, > 1.
   */
  private int _compareStringVersions(Version otherVersion, int versionIndex)
  {
    String ourSubVersion = _getSubVersion(versionIndex);
    String otherSubVersion = otherVersion._getSubVersion(versionIndex);
      
    // treat "*" wildcard as equals
    if (_isWildcard(ourSubVersion) || _isWildcard(otherSubVersion))
    {
      return 0;
    }
    
    return ourSubVersion.compareTo(otherSubVersion);
  }

  /**
   * Returns the string representation of the this Version, replacing
   * wildcards with the specified value.
   *
   * @param wildcardReplacement non-null String to substitute for wildcard
   *   version segments.
   * @param dropTrailingWildcard flag indicating whether trailing wildcards
   *   should be dropped in the returned string.
   */
  private String _toString(
    String  wildcardReplacement,
    boolean dropTrailingWildcard
    )
  {
    assert(wildcardReplacement != null);

    // rebuild the initial version string from the split array
    StringBuilder versionBuilder = new StringBuilder();
    int versionCount = _versions.length;
    
    for (int i = 0;;)
    {
      String subVersion = _versions[i];
      if (_isWildcard(subVersion))
      {
        subVersion = wildcardReplacement;
      }

      versionBuilder.append(subVersion);
      
      i++;
      
      if (i != versionCount)
      {
        if (dropTrailingWildcard && (i == versionCount - 1) && _isWildcard(_versions[i]))
        {
          break;
        }

        versionBuilder.append('.');
      }
      else
        break;
    }
        
    return versionBuilder.toString();    
  }

  /**
   * Returns the contents of the sub-version section of the overall version,
   * padding the result with the version padding if the version section
   * index is greater than the number of actual version sections in this
   * version
   * @param versionIndex index of the "." version section from the left side
   * of the version string.
   * @return The content of the version section if available, otehrwise the
   * versionPadding
   */
  private String _getSubVersion(int versionIndex)
  {
    if (versionIndex >= _versions.length)
      return _versionPadding;
    else
      return _versions[versionIndex];
  }

  
  private void _checkNonEmptyString(String checkedString, String identifier)
  {
    if (checkedString == null)
      throw new NullPointerException(identifier + " must be non-null");
    
    if (checkedString.length() == 0)
      throw new IllegalArgumentException(identifier + " must be non-empty");
  }
  
  // Tests wheter this version contains a wildcard segment
  private boolean _containsWildcard()
  {
    for (int i = 0; i < _versions.length; i++)
    {
      if (_isWildcard(_versions[i]))
      {
        return true;
      }
    }
    
    return false;
  }
  
  // Tests whether the specified version segment is the wildcard.
  private static boolean _isWildcard(String subVersion)
  {
    return _WILDCARD.equals(subVersion);
  }

  private final String[] _versions;
  private final int[] _intVersions;
  private final String _versionPadding;
  private final int _hashCode;

  // Constant for the wildcard character  
  private static final String _WILDCARD = "*";

  // cache the compiled splitter
  private static final Pattern _DOT_SPLITTER = Pattern.compile("\\.");
  
  // A pattern for testing whether a version string is numeric.
  private static final Pattern _DIGITS_PATTERN = Pattern.compile("\\d+");
  
  // Placeholder used by _intVersions[] for non-numeric/non-int segments.  Note
  // that we can use -1 to mark non-int versions since the _DIGITS_PATTERN will
  // filter out any negative values - ie. _intVersions[] will only contain
  // non-negative version ints, plus _NON_INT_VERSION.
  private static final int _NON_INT_VERSION = -1;
  
  private static final String _MAX_STRING = Integer.toString(Integer.MAX_VALUE);
  
  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(Version.class);

  static
  {
    MIN_VERSION = new Version("0");
    MAX_VERSION = new Version(_MAX_STRING, _MAX_STRING);
    ALL_VERSIONS = Range.of(MIN_VERSION, MAX_VERSION);
  }

}
