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
package org.apache.myfaces.trinidad.render;

import java.io.IOException;

import java.util.ArrayList;
import java.util.List;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.component.UIForm;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.component.UIXForm;
import org.apache.myfaces.trinidad.context.Agent;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.ComponentUtils;


/**
 * Generic utilities for rendering.
 *
 */
public class RenderUtils
{
  private RenderUtils()
  {
  }


  /**
   * Encodes a component and all of its children.
   */
  @SuppressWarnings("unchecked")
  static public void encodeRecursive(FacesContext context,
                                     UIComponent component)
    throws IOException
  {
    if (component.isRendered())
    {
      component.encodeBegin(context);
      if (component.getRendersChildren())
      {
        component.encodeChildren(context);
      }
      else
      {
        if (component.getChildCount() > 0)
        {
          for(UIComponent child : (List<UIComponent>)component.getChildren())
          {
            encodeRecursive(context, child);
          }
        }
      }

      component.encodeEnd(context);
    }
  }

  /**
   *  Retrieves id of the form the component is contained in.
   *
   * @param context the faces context object
   * @param component UIComponent whose container form id is to be retuirned
   *
   * @return String id of the form if one exists in component's hierarchy,
   *                otherwise null
   */
  public static String getFormId(
    FacesContext context,
    UIComponent  component)
  {
    UIComponent form = null;
    while (component != null)
    {
      if ((component instanceof UIForm) ||
          (component instanceof UIXForm))
      {
        form = component;
        break;
      }

      component = component.getParent();
    }

    if (form == null)
      return null;

    return form.getClientId(context);
  }

  /**
   * Given a 'from' component and a relativeId,
   * return the clientId for use at rendering time that will identify the
   * id of the component you are referencing on the client.
   * This is used for attributes like e.g. "for" and "chooseId".
   *
   * <p>
   * e.g., given this hierarchy
   * <br/>
   *  &lt;f:subview id="aaa"&gt;
   *    &lt;f:subview id="xxx"&gt;<br/>
           &lt;tr:chooseColor id="cp1" .../&gt;<br/>
            &lt;f:subview id="yyy"><br/>
               &lt;tr:inputColor id="sic1" chooseId="::cp1" .../&gt;<br/>
            &lt;/f:subview&gt;<br/>
         &lt;/f:subview&gt;
      &lt;/f:subview&gt;<br/>
    </p>
    <p>
   * The 'from' component is the inputColor component.
   * The 'relativeId' is "::cp1". ('::' pops up one naming container)
   * The return value is 'aaa:xxx:cp1' when
   * the clientId of the 'xxx' component is 'aaa:xxx'.
   *
   * </p>
   * <p>
   * It does not assume that the target component can be located, although it does
   * check. If it can't be found, it returns the correct relativeId anyway.
   * </p>
   * <p>
   * A relativeId starting with
   * NamingContainer.SEPARATOR_CHAR (that is, ':') will be
   * treated as absolute (after dropping that character).
   * A relativeId with no colons means it is within the same naming container
   * as the 'from' component (this is within the 'from' component if 'from'
   * is a naming container).
   * A relativeId starting with '::' pops out of the 'from' component's
   * naming container. If the 'from' component is a naming container, then
   * '::' will pop out of the 'from' component itself. A relativeId with ':::' pops up two naming containers, etc.
   * ComponentUtils.findRelativeComponent finds and returns the component, whereas
   * this method returns a relativeId that can be used during renderering
   * so the component can be found in javascript on the client.
   * </p>
   * @param context
   * @param from the component to search relative to
   * @param scopedId the relative id path from the 'from' component to the
   *                 component to find
   * @return the clientId for the 'relative' component.
   * @see ComponentUtils#findRelativeComponent
   * @see javax.faces.component.UIComponent#findComponent

   */
  public static String getRelativeId(
    FacesContext context,
    UIComponent  from,
    String       scopedId)
  {
    if (from == null)
        return null;

    if ((scopedId == null) || (scopedId.length() == 0))
      return null;

    // Figure out how many colons
    int colonCount = _getColonCount(scopedId);

    // colonCount == 0: fully relative
    // colonCount == 1: absolute
    // colonCount > 1: for each extra colon after 1, pop out of
    // the naming container (to the view root, if naming containers run out)

    if (colonCount == 1)
      return scopedId.substring(1);
    if (colonCount == 0 && !(from instanceof NamingContainer))
    {
      // we do it the fast way if there
      // are no colons and the from isn't a NamingContainer.
      // the reason is this use case hasn't changed between the previous
      // logic and the current logic for finding the component, so it
      // is already backward compatible, and therefore we don't have to
      // call the findComponent code for backward compatibility.
      return _getRelativeId(context, from, scopedId, colonCount);
    }

    //
    // We need to make it backward compatible, and
    // the only way is to use the findRelativeComponent code.
    // This way we'll have a hint that the syntax is 'old' if
    // it can't be found. Plus, findRelativeComponent code has
    // backward compatibilty built in.
    UIComponent component =
      ComponentUtils.findRelativeComponent(from, scopedId);
    if (component == null && from instanceof NamingContainer)
    {
      component = ComponentUtils.findRelativeComponent(from.getParent(), scopedId);
      if (component != null)
      {
        _LOG.warning("DEPRECATED_RELATIVE_ID_SYNTAX",
          new Object[] {scopedId, from});
      }
    }

    // the component wasn't found, but go ahead and return something smart
    if (component == null)
    {
      return _getRelativeId(context, from, scopedId, colonCount);
    }
    else
    {
      return component.getClientId(context);
    }

  }

  /**
   * Return the chained JavaScript
   */
  public static String getChainedJS(
    boolean   shortCircuit,
    String... scripts
    )
  {
    if (scripts.length == 0)
    {
      return null;
    }

    if (scripts.length == 1)
    {
      return scripts[1];
    }
    // Strip out null & 0-length scripts
    List<String> filteredScripts = new ArrayList<String>(scripts.length);
    for (String script : scripts)
    {
      if (script != null)
      {
        script = script.trim();
        if (script.length() > 0)
        {
          filteredScripts.add(script);
        }
      }
    }
    if (filteredScripts.isEmpty())
    {
      return null;
    }
    else if (filteredScripts.size() == 1)
    {
      return filteredScripts.get(0);
    }

    StringBuilder builder = new StringBuilder(100);
    builder.append("return _chainMultiple([");
    boolean first = true;
    for (String script : filteredScripts)
    {
      if (first)
      {
        builder.append('\'');
        first = false;
      }
      else
      {
        builder.append(",'");
      }
      escapeJS(builder, script, true);
      builder.append('\'');
    }
    RenderingContext rc = RenderingContext.getCurrentInstance();
    if (rc.getAgent().getType().equals(Agent.TYPE_DESKTOP))
    {
      if (shortCircuit)
        builder.append("],this,event,true);");
      else
        builder.append("],this,event);");
    }
    else
    {
      // Some mobile browsers do not support DOM Event object.
      // If event is passed, the script crushes before the function gains
      // control.
      if (shortCircuit)
        builder.append("],this,null,true);");
      else
        builder.append("],this,null);");
    }

    return builder.toString();
  }

  /**
   * Handle escaping '/', and single quotes, plus escaping text inside of
   * quotes with just a String for input.  If a String in and a String out is
   * all that is required, this version is more efficient if the String
   * does not need to be escaped.
   */
  public static String escapeJS(
    String inString
    )
  {
    return escapeJS(inString, false /* inQuotes */);
  }

  /**
   * Handle escaping '/', and single quotes, plus escaping text inside of
   * quotes with just a String for input.  If a String in and a String out is
   * all that is required, this version is more efficient if the String
   * does not need to be escaped.
   */
  public static String escapeJS(
    String  inString,
    boolean inQuotes
    )
  {
    int charCount = inString.length();

    StringBuilder outBuilder = new StringBuilder(charCount * 2);

    escapeJS(outBuilder, inString, inQuotes);

    // since we only add characters, if the character count is different, we
    // will have a different output string, otherwise, reuse the input string,
    // as it is unchanged
    if (charCount != outBuilder.length())
    {
      return outBuilder.toString();
    }
    else
    {
      return inString;
    }
  }

  /**
   * Handle escaping '/', and single quotes, plus escaping text inside of
   * quotes.
   */
  public static void escapeJS(
    StringBuilder outBuilder,
    String       inString
    )
  {
    escapeJS(outBuilder, inString, false /* inQuotes */);
  }

  /**
   * Handle escaping '/', and single quotes, plus escaping text inside of
   * quotes.
   */
  public static void escapeJS(
    StringBuilder outBuilder,
    String       inString,
    boolean      inQuotes)
  {
    escapeJS(outBuilder, inString, inQuotes, 1 /* escapeCount */);
  }

  /**
   * Handle escaping '/', and single quotes, plus escaping text inside of
   * quotes.
   */
  public static void escapeJS(
    StringBuilder outBuilder,
    String       inString,
    boolean      inQuotes,
    int          escapeCount
    )
  {
    int leadSlashCount = (int)Math.pow(2, escapeCount) - 2;
    int charCount = inString.length();

    char    prevChar  = '\u0000';

    //
    // loop through the string escaping the single quotes at the \'s as
    // necessary
    //
    for (int i = 0; i < charCount; i++)
    {
      char currChar = inString.charAt(i);

      if (currChar == '\'')
      {
        if (!(inQuotes && (prevChar == '\\')))
        {
          // only toggle whetehr we are in quotes if the quote isn't escaped
          inQuotes = !inQuotes;
        }

        // handle double-escaping case
        // eg. "\'" + escapeJS(builder,"a'b",true,2) + "\'" -> "\'a\\\'b\'"
        for (int j=0; j < leadSlashCount; j++)
        {
          outBuilder.append('\\');
        }

        // always escape quotes
        outBuilder.append('\\');

        // output the current character
        outBuilder.append(currChar);
      }
      else
      {
        if (inQuotes)
        {
          if (currChar > 255)
          {
            outBuilder.append("\\u");
            _appendHexString(outBuilder, currChar, 4);
          }
          else
          {
            if ((currChar > 31) &&
                (currChar < 128))
            {
              if (currChar == '\\')
              {
                // escape all \'s in strings
                outBuilder.append('\\');
              }

              // output the current character
              outBuilder.append(currChar);
            }
            else
            {
              outBuilder.append("\\x");
              _appendHexString(outBuilder, currChar, 2);
            }
          }
        }
        else
        {
          // Double up backslashes (see bug 1676002)
          if (currChar == '\\')
            outBuilder.append('\\');

          // output the current character
          outBuilder.append(currChar);
        }
      }

      // keep track of the previous character to determine whether
      // single quotes are escaped
      prevChar = currChar;
    }
  }

  private static void _escapeSingleQuotes(
    StringBuilder outBuilder,
    String       inString
    )
  {
    int     charCount = inString.length();
    char    prevChar  = '\u0000';
    boolean inQuotes  = false;

    //
    // loop through the string escaping the single quotes at the \'s as
    // necessary
    //
    for (int i = 0; i < charCount; i++)
    {
      char currChar = inString.charAt(i);

      if (currChar == '\'')
      {
        if (!(inQuotes && (prevChar == '\\')))
        {
          // only toggle whetehr we are in quotes if the quote isn't escaped
          inQuotes = !inQuotes;
        }

        // always escape quotes
        outBuilder.append('\\');
      }
      else if ((currChar == '\\') && inQuotes)
      {
        // escape all \'s in strings
        outBuilder.append('\\');
      }

      // output the current character
      outBuilder.append(currChar);

      // keep track of the previous character to determine whether
      // single quotes are escaped
      prevChar = currChar;
    }
  }

  private static void _appendHexString(
    StringBuilder builder,
    int          number,
    int          minDigits
    )
  {
    String hexString = Integer.toHexString(number);

    int hexLength = hexString.length();

    int zeroPadding = minDigits - hexLength;

    if (zeroPadding > 0)
    {
      builder.append('0');

      while (zeroPadding > 1)
      {
        builder.append('0');
        zeroPadding--;
      }
    }
    else
    {
      if (zeroPadding < 0)
      {
        throw new IllegalArgumentException();
      }
    }

    builder.append(hexString);
  }

  // This does NOT use findComponent
  // ComponentUtils.findRelativeComponent finds the component, whereas
  // this method returns a relativeId that can be used during renderering
  // so the component can be found in javascript on the client.
  // This code is faster because it doesn't have to find the component.
  // It is used when the getRelativeId's findRelativeComponent cannot find
  // the component. This way we can return the relativeId anyway.
  private static String _getRelativeId(
    FacesContext context,
    UIComponent  from,
    String       relativeId,
    int          colonCount)
  {


    if (colonCount == 1)
      return relativeId.substring(1);
    else if (colonCount > 1)
    {
      relativeId = relativeId.substring(colonCount);
    }

    // if the component is not a NamingContainer, then we need to
    // get the component's naming container and set this as the 'from'.

    if (!(from instanceof NamingContainer))
    {
      from = _getParentNamingContainer(from);
    }
    // pop out of the naming containers if there are multiple colons
    // from will be null if there are no more naming containers
    for (int j = 1; j < colonCount; j++)
    {
      from = _getParentNamingContainer(from);
    }

    // assumption is no one but the parent naming container modifies its
    // client id
    if (from == null)
      return relativeId;
    else
    {
      return (from.getClientId(context) +
              NamingContainer.SEPARATOR_CHAR + relativeId);
    }


  }


  // Given a component, get its naming container. If the component
  // is a naming container, it will get its naming container.
  // This is different than the one in ComponentUtils. This one
  // returns null if there are no NamingContainers. The other one
  // returns the ViewRoot.
  private static UIComponent _getParentNamingContainer (
    UIComponent from)
  {

    while (from != null && from.getParent() != null)
    {
      from = from.getParent();
      if (from instanceof NamingContainer)
        return from;
    }

    return null;
  }

  // Figure out how many colons
  private static int _getColonCount(String relativeId)
  {
    int idLength = relativeId.length();
    int colonCount = 0;
    while (colonCount < idLength)
    {
      if (relativeId.charAt(colonCount) != NamingContainer.SEPARATOR_CHAR)
        break;
      colonCount++;
    }
    return colonCount;
  }
  static private final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(RenderUtils.class);

}
