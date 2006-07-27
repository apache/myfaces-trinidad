/*
 * Copyright  2002-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.trinidadinternal.ui.laf.base.xhtml;

import java.io.IOException;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;

import javax.faces.context.ResponseWriter;


import org.apache.myfaces.trinidadinternal.util.IntegerUtils;

import org.apache.myfaces.trinidadinternal.agent.TrinidadAgent;

import org.apache.myfaces.trinidadinternal.ui.UIXRenderingContext;
import org.apache.myfaces.trinidadinternal.ui.UINode;


/**
 * Base class for audio and video rendering.
 *
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/ui/laf/base/xhtml/MediaRenderer.java#0 $) $Date: 15-nov-2005.19:26:37 $
 * @author The Oracle ADF Faces Team
 */
public class MediaRenderer extends XhtmlLafRenderer
{
  /**
   * Returns an URL to the media to display
   */
  protected Object getSource(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return XhtmlLafUtils.getLocalAttribute(context, node, SOURCE_ATTR);
  }

  /**
   * Returns the expected content type of the media to display
   */
  protected Object getContentType(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object type =  node.getAttributeValue(context, CONTENT_TYPE_ATTR);
    return type;
  }

  /**
   * Returns true if the media should start playing as soon as it is loaded.
   */
  protected Object getAutostart(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, AUTOSTART_ATTR);
  }

  /**
   * Returns the number of times that the media should play
   */
  protected Number getPlayCount(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return (Number)node.getAttributeValue(context, PLAY_COUNT_ATTR);
  }

  /**
   * Returns an enumeration determining the set of controls to render
   */
  protected Object getControls(
    UIXRenderingContext context,
    UINode           node
    )
  {
    return node.getAttributeValue(context, CONTROLS_ATTR);
  }


  /**
   * Renders the MediaBean asa either an <embed> or <object> as appropriate.
   *
   * =-= bts For convenience, I overrode render() as opposed to overriding the
   *         different renderAttributes(), prerender(), renderContent(),
   *         routines as appropriate.  This was done for convenience, since
   *         the <embed> and <object> tags attributes need to be written
   *         in different routines and I didn't want to copy the code.
   *         However, it also has side-effects for both subclassing and support
   *         for things such as JavaScript events.  Given my redesign to
   *         drive almost everything off of meta-data, the decision to
   *         do all of the work in render() could be revisited.
   */
  public void render(
    UIXRenderingContext context,
    UINode           node
    ) throws IOException
  {

    // get the mime type
    String mimeType = _getMimeType(context, node);

    // extract the primary cotnent type from the content mime type
    String primaryContentType = _getPrimaryContentType(mimeType);

    MediaRenderer.PlayerData playerData = _getPlayerData(context,
                                           node,
                                           mimeType,
                                           primaryContentType);

    ResponseWriter writer = context.getResponseWriter();

    Object  source         = getSource(context, node);
    Object  contentType    = getContentType(context, node);
    Object  shortDesc      = node.getAttributeValue(context, SHORT_DESC_ATTR);
    Object  id             = getID(context, node);

    if (_LINK_PLAYER_DATA == playerData)
    {
      _renderLink(writer,
                  node,
                  supportsID(context) ? id : null,
                  source,
                  null,
                  shortDesc,
                  contentType);
    }
    else
    {
     // determine which controls to display based on the primary
      // mime type
      MediaRenderer.ControlSet  controlSet = playerData.getControlSet(primaryContentType);



      MediaRenderer.ControlData imageWindowControlData =
               playerData.getImageWindowControlData(primaryContentType);

      MediaRenderer.ControlData controlData = (controlSet != null)
                                  ? controlSet.getControlData(
                                                 getControls(context, node))
                                  : null;

      //
      // Determine which tags to use
      //
      boolean useEmbedTag = true;
      boolean useObjectTag = true;

      boolean isImage = _IMAGE_PLAYER_DATA == playerData;

      if (isImage)
      {
        useEmbedTag  = false;
        useObjectTag = false;
      }
      else
      {
        useEmbedTag  = useEmbed(context);
        useObjectTag = !useEmbedTag;
      }

      Object width = node.getAttributeValue(context, WIDTH_ATTR);

      // since current players don't add to width, treat innerWidth
      // identically to width for now
      if (width == null)
        width = node.getAttributeValue(context, INNER_WIDTH_ATTR);

      // get whether this set of controls can be autosized.
      // embed elements can not be autosized.
      boolean canAutosize = playerData.canAlwaysAutosize ||
                            (controlData.canAutosize && !useEmbedTag);

      // compute default width for players that don't autosize
      if ((width == null) && !canAutosize)
      {
        Number defaultInnerWidth = getDefaultInnerWidth(context,
                                                        node,
                                                        primaryContentType);

        // use the greater of the default width of the content
        // and the preferred width of the controls
        width = (defaultInnerWidth.intValue() >
                 controlData.preferredWidth.intValue())
                  ? defaultInnerWidth
                  : controlData.preferredWidth;
      }



      // default the autostarting value
      String  autostartValue  = null;

      // default the playcount param name and value
      String playCountParamName = null;
      Object playCountParamValue = null;

      if (!isImage)
      {
        //
        // Handle looping attributes
        //
        Number playCountValue = getPlayCount(context, node);

        if (playCountValue != null)
        {
          int playCount = playCountValue.intValue();

          if ((playCount != 1) && (playCount >= 0))
          {
            if (playCount == 0)
            {
              // some players handle infinite looping specially
              playCountParamName  = playerData.infiniteLoopParamName;
              playCountParamValue = playerData.infiniteLoopParamValue;
            }
            else
            {
              playCountParamName  = playerData.playCountParamName;
              playCountParamValue = playCountValue;
            }
          }
        }

        //
        // determine whether we should autostart
        //
        Object autostart = getAutostart(context, node);

        if (autostart != null)
        {
          boolean autostarts = Boolean.TRUE.equals(autostart);

          if (autostarts != playerData.autostartByDefault)
          {
            // only write out an autostart value, if its different than
            // the default
            autostartValue = autostarts
                               ? playerData.autostartTrueValue
                               : playerData.autostartFalseValue;
          }
        }
        else
        {
          // if player autostarts by default, turn it off
          if (playerData.autostartByDefault)
          {
            autostartValue = playerData.autostartFalseValue;
          }
        }
      }


      Object standbyText = node.getAttributeValue(context,
                                                   STANDBY_TEXT_ATTR);

      Object height = null;
      // if image window and controls rendered separately
      if ( imageWindowControlData != null )
      {

        height = _getInnerHeight( context,
                                  node,
                                  canAutosize,
                                  primaryContentType,
                                  controlData,
                                  playerData );

        // since image and controls are rendered separately, a value
        // is needed to associate them. The id will be used if it is not null,
        // otherwise an id will be generated for the value.
        Object wireImageToControlsParamValue = id;

        if ( wireImageToControlsParamValue == null )
        {
          wireImageToControlsParamValue =
                                      XhtmlLafUtils.generateUniqueID(context);
        }

        _render( context,
                 node,
                 contentType,
                 id,
                 wireImageToControlsParamValue,
                 source,
                 shortDesc,
                 standbyText,
                 width,
                 height,
                 autostartValue,
                 playCountParamName,
                 playCountParamValue,
                 playerData,
                 imageWindowControlData,
                 useEmbedTag,
                 useObjectTag,
                 isImage,
                 false);


        if ( controlData != _NULL_CONTROL_DATA )
        {
          writer.startElement(DIV_ELEMENT, node.getUIComponent());
          writer.endElement(DIV_ELEMENT);


          height = IntegerUtils.getInteger( controlData.height );
          _render( context,
                   node,
                   contentType,
                   id,
                   wireImageToControlsParamValue,
                   null,
                   shortDesc,
                   null,
                   width,
                   height,
                   null,
                   null,
                   null,
                   playerData,
                   controlData,
                   useEmbedTag,
                   useObjectTag,
                   isImage,
                   true);
        }
      }
      // if image window and controls rendered together
      else
      {
        height = _getHeight( context,
                             node,
                             canAutosize,
                             primaryContentType,
                             controlData,
                             playerData );

        _render( context,
                 node,
                 contentType,
                 id,
                 null,
                 source,
                 shortDesc,
                 standbyText,
                 width,
                 height,
                 autostartValue,
                 playCountParamName,
                 playCountParamValue,
                 playerData,
                 controlData,
                 useEmbedTag,
                 useObjectTag,
                 isImage,
                 false);
      }


      if (useEmbedTag)
      {
        //
        // render the alternative link content
        //
        // =-=bts I feel that our backup rendering should be an icon that
        //        we retrieve using a pretected method
        //
        writer.startElement("noembed", node.getUIComponent());
        _renderLink(writer, node, null, source, null, shortDesc, contentType);
        writer.endElement("noembed");
      }
    }
  }

  /**
   * Returns the default inner width to use for players when no width is
   * specified.
   */
  protected Number getDefaultInnerWidth(
    UIXRenderingContext context,
    UINode           node,
    String           primaryContentType
    )
  {
    return _getDefaultSizeArray(primaryContentType)[_DEFAULT_INNER_WIDTH_INDEX];
  }


  /**
   * Returns the default inner height to use for players when no height is
   * specified.
   */
  protected Number getDefaultInnerHeight(
    UIXRenderingContext context,
    UINode           node,
    String           primaryContentType
    )
  {
    return _getDefaultSizeArray(primaryContentType)[_DEFAULT_INNER_HEIGHT_INDEX];
  }


  /**
   * Returns true if we should use the <embed> tag instead of the
   * <object> tag to embed the viewer
   */
  protected boolean useEmbed(
    UIXRenderingContext context
    )
  {
    TrinidadAgent agent = context.getAgent();

    // =-= bts only desktop IE seems to support this
    //         Move this to capability
    return !((agent.getAgentType()       == TrinidadAgent.TYPE_DESKTOP) &&
            (agent.getAgentApplication() == TrinidadAgent.APPLICATION_IEXPLORER) &&
            (agent.getAgentOS()          == TrinidadAgent.OS_WINDOWS));
  }

  private Number[] _getDefaultSizeArray(
    String primaryContentType
    )
  {
    Object sizeArray = _sDefaultInnerSizes.get(primaryContentType);

    if (sizeArray == null)
    {
      // try again with video if no entry found for this primary content type
      sizeArray = _sDefaultInnerSizes.get("video");
    }

    return (Number[])sizeArray;
  }


  /**
   * Renders a link
   */
  private void _renderLink(
    ResponseWriter writer,
    UINode node,
    Object       id,
    Object       source,
    Object       iconUrl,
    Object       shortDescription,
    Object       contentType
    ) throws IOException
  {
    // Don't render a link without an "href".
    // But, for PPR, always render a span if we have an ID.
    if (source == null)
    {
      if (id != null)
      {
        writer.startElement("span", node.getUIComponent());
        writer.writeAttribute("id", id, null);
        writer.endElement("span");
      }
    }
    else
    {
      writer.startElement("a", node.getUIComponent());

      // write the ID
      writer.writeAttribute("id", id, null);

      // link to the content
      writer.writeURIAttribute("href", source, null);

      // mime-type of content
      writer.writeAttribute("type", contentType, null);

      if (iconUrl != null)
      {
        // =-=AEW Just noticed: this code fails to render "alt"; before
        // using, check accessibility!!!
        assert false;
        writer.startElement("img", node.getUIComponent());
        writer.writeURIAttribute("src", iconUrl, null);
        writer.endElement("img");
      }

      writer.writeText(shortDescription, null);

      writer.endElement("a");
    }
  }


  /**
   * Renders a name value pair as a <param> element
   */
  private void _renderParamAttribute(
    UIXRenderingContext context,
    String           paramName,
    Object           paramValue,
    boolean          isURL
    ) throws IOException
  {
    if ( paramValue != null )
    {
      ResponseWriter writer = context.getResponseWriter();

      writer.startElement("param", null);
      writer.writeAttribute("name", paramName, null);

      if (isURL)
      {
        writer.writeURIAttribute("value", paramValue, null);
      }
      else
      {
        writer.writeAttribute("value", paramValue, null);
      }

      writer.endElement("param");
    }
  }


  /**
   * Returns the constant identifying the player to use
   *
   * =-= bts Do we care about AccessibilityMode?
   */
  private MediaRenderer.PlayerData _getPlayerData(
    UIXRenderingContext context,
    UINode           node,
    String           mimeType,
    String           primaryMimeType
    )
  {
    Object playerData = context.getLocalProperty(0, _PLAYER_DATA_KEY, null);

    if (playerData == null)
    {
      // the default player is the link player
      TrinidadAgent agent = context.getAgent();

      int agentOSInt = agent.getAgentOS();

      Integer agentOS = IntegerUtils.getInteger(agentOSInt);

      // get the set of supported players for this OS
      HashSet supportedPlayers = (HashSet)
                                _sSupportedOSPlayers.get(agentOS);

      // are we trying to display an image?
      boolean isImage = "image".equals(primaryMimeType);

      // check if this OS supports any non-link style players
      if (supportedPlayers != null)
      {
        // get the desired player, if any
        Object playerAttr = XhtmlLafUtils.getLocalAttribute(
                                                context,
                                                node,
                                                PLAYER_ATTR);

        if (playerAttr != null)
        {
          String playerString = playerAttr.toString();

          if (PLAYER_LINK.equals(playerString))
          {
            playerData = _LINK_PLAYER_DATA;
          }
          else
          {
            // check that the specified player is supported
            if (!isImage && supportedPlayers.contains(playerString))
            {
              playerData = _sPlayerData.get(playerString);

              if (playerData != null)
              {
                if (mimeType != null)
                {
                  // check that the format is supported by this player
                  if (!((PlayerData)playerData).isSupportedMimeType(mimeType))
                  {
                    playerData = null;
                  }
                }
              }
            }
          }
        }

        // assume that all images can be played by the browser's built-in
        // image player
        if (isImage)
          playerData = _IMAGE_PLAYER_DATA;
        else
        {
          // if the primary content type isn't image, video, or audio,
          // use the link player
          if (!"video".equals(primaryMimeType) &&
              !"audio".equals(primaryMimeType))
            playerData = _LINK_PLAYER_DATA;
        }

        //
        // guess the player based on the mime type
        //
        if (playerData == null)
        {
          Object preferredPlayer = _sPreferredMimePlayers.get(mimeType);

          if ((preferredPlayer != null) &&
              supportedPlayers.contains(preferredPlayer))
          {
            playerData = (PlayerData)_sPlayerData.get(preferredPlayer);
          }
        }

        //
        // guess the player based on the operating system
        //
        if (playerData == null)
        {
          Object preferredPlayer = _sPreferredOSPlayers.get(agentOS);

          if (preferredPlayer != null)
          {
            playerData = (PlayerData)_sPlayerData.get(preferredPlayer);

            // check that the format is supported by this player
            if (!((PlayerData)playerData).isSupportedMimeType(mimeType))
            {
              playerData = null;
            }
          }
        }
      }

      // if all else fails, use the link player
      if (playerData == null)
        playerData = _LINK_PLAYER_DATA;

      context.setLocalProperty(_PLAYER_DATA_KEY, playerData);
    }

    return (PlayerData)playerData;
  }


  /**
   * Returns the primary content type of the result:
   * i.e. "audio", "video"
   */
  private String _getPrimaryContentType(
    String mimeType
    )
  {
    if (mimeType != null)
    {
      int slashIndex = mimeType.indexOf('/');

      if (slashIndex != -1)
      {
        return mimeType.substring(0, slashIndex);
      }
    }

    return null;
  }


  /**
   * Returns the secondary content type of the result:
   * i.e. "quicktime", "au"
   */
  private String _getSecondaryContentType(
    String mimeType
    )
  {
    if (mimeType != null)
    {
      int slashIndex = mimeType.indexOf('/');

      if (slashIndex != -1)
      {
        return mimeType.substring(slashIndex + 1);
      }
    }

    return null;
  }


  /**
   * Returns the most likely MIME type for the content
   */
  private String _getMimeType(
    UIXRenderingContext context,
    UINode           node
    )
  {
    // try any provided content mime type first
    Object mimeTypeAttr = getContentType(context, node);

    String mimeType = "";

    // primary mime type to force mime type to
    String primaryForced = null;

    if (mimeTypeAttr != null)
    {
      mimeType = mimeTypeAttr.toString();

      // convert mime types to lower case
      mimeType = mimeType.toLowerCase(Locale.US);

      // Check that the user hasn't forced the primary mime-type
      int mimeLength = mimeType.length();

      if ((mimeType.charAt(mimeLength - 1) == '*') &&
          (mimeType.charAt(mimeLength - 2) == '/'))
      {
        // force the primary mime type
        primaryForced = mimeType.substring(0, mimeLength - 2);
      }
      else
      {
        Object remapped = _sMimeTypeRemapper.get(mimeType);

        if (remapped != null)
        {
          // we remapped the in mime-type to the cannonical mime type
          return remapped.toString();
        }
        else
        {
          // return the mime-type as is
          return mimeType;
        }
      }
    }

    // try to determine the mime-type off of the source extension
    String extension = _getSourceExtension(context, node);

    if (extension != null)
    {
      // try to map the extension to a mime type
      Object mimeTypeObject = _sExtensionMap.get(extension);

      if (mimeTypeObject != null)
      {
        mimeType = mimeTypeObject.toString();

        if (primaryForced != null)
        {
          // merge the primary forced type with the extension mapped secondary
          // type
          mimeType = primaryForced + '/' + _getSecondaryContentType(mimeType);
        }
      }
    }

    return mimeType;
  }


  /**
   * Returns the lowercase extension of any source URL
   */
  private String _getSourceExtension(
    UIXRenderingContext context,
    UINode           node
    )
  {
    Object sourceURL = getSource(context, node);

    if (sourceURL != null)
    {
      String sourceURLString = sourceURL.toString();

      int extensionIndex = sourceURLString.lastIndexOf('.');

      if ((extensionIndex != -1) &&
          (extensionIndex != sourceURLString.length() -1))
      {
        return sourceURLString.substring(extensionIndex + 1).toLowerCase();
      }
    }

    // no extension
    return null;
  }


  /**
   * Initializes a Hashset with the contents of an array
   */
  private static HashSet _createHashSet(
    Object[] contents
    )
  {
    if (contents != null)
    {
      int contentLength = contents.length;

      HashSet set = new HashSet((int)(contentLength * 1.5));

      for (int i = 0; i < contentLength; i++)
      {
        set.add(contents[i]);
      }

      return set;
    }
    else
    {
      return null;
    }
  }

  /**
   * Initializes a HashMap with the name/value contents of an array
   */
  private static HashMap _createHashMap(
    Object[] contents
    )
  {
    if (contents != null)
    {
      int contentLength = contents.length;

      HashMap map = new HashMap((int)(contentLength * 0.75));

      for (int i = 0; i < contentLength; i += 2)
      {
        map.put(contents[i], contents[i + 1]);
      }

      return map;
    }
    else
    {
      return null;
    }
  }


  // get the height of the image window
  private Object _getInnerHeight(
    UIXRenderingContext context,
    UINode           node,
    boolean          canAutosize,
    String           primaryContentType,
    ControlData      controlData,
    PlayerData       playerData
    )
  {

    // get the height without checking min
    Object height = _getHeightNoMin(context,
                                    node,
                                    canAutosize,
                                    primaryContentType,
                                    controlData);



    boolean useMinHeight = (height == null);

    if (!useMinHeight)
    {
      int intHeight = _getIntHeight(height);

      // - if intHeight equals Integer.MAX_VALUE then height could not be
      //   turned to an int, return height
      // - if intHeight is less than playerData.minHeight.intValue()
      //   then return player min - contorl data height if possible
      // - if intHeight is between the player min and Integer.MAX_VALUE
      //   then return inHeight - control data height

      if (playerData.minHeight != null &&
          intHeight < playerData.minHeight.intValue())
      {
        useMinHeight = true;
      }
      else if ( intHeight < Integer.MAX_VALUE )
      {
        return IntegerUtils.getInteger(intHeight - controlData.height);
      }
    }

    if (useMinHeight)
    {
      return IntegerUtils.getInteger(playerData.minHeight.intValue() -
                                     controlData.height);
    }

    return height;
  }

  //
  // determine the height without checking the min height
  //
  private Object _getHeightNoMin(
    UIXRenderingContext context,
    UINode           node,
    boolean          canAutosize,
    String           primaryContentType,
    ControlData      controlData
    )
  {
    Object height = node.getAttributeValue(context, HEIGHT_ATTR);

    // compute default height for players that don't autosize
    if (height == null)
    {
      // get the inner height from the attribute
      Object innerHeight = node.getAttributeValue(context,
                                                  INNER_HEIGHT_ATTR);

      if (!canAutosize && (innerHeight == null))
      {
        // get the default inner height
        innerHeight = getDefaultInnerHeight(context,
                                            node,
                                            primaryContentType);
      }

      //
      // convert from inner height to outer height
      //
      int extraHeight = (controlData != null)
        ? controlData.height
        : 0;
      if ((innerHeight != null) && (extraHeight != 0))
      {
        height = IntegerUtils.getInteger(((Number)innerHeight).intValue() +
                                         extraHeight);
      }
      else
      {
        height = innerHeight;
      }
    }

    return height;
  }


  //
  // determine the height including the controls
  //
  private Object _getHeight(
    UIXRenderingContext context,
    UINode           node,
    boolean          canAutosize,
    String           primaryContentType,
    ControlData      controlData,
    PlayerData       playerData
    )
  {

    // get the height
    Object height = _getHeightNoMin(context,
                                    node,
                                    canAutosize,
                                    primaryContentType,
                                    controlData);

    //
    // apply minimum player size to height
    //
    if (playerData.minHeight != null)
    {
      boolean useMinHeight = (height == null);

      if (!useMinHeight)
      {
        int intHeight = _getIntHeight(height);

        useMinHeight = (playerData.minHeight.intValue() > intHeight);
      }

      if (useMinHeight)
      {
        height = playerData.minHeight;
      }
    }

    return height;
  }

  // try to transform height to an int
  // returns Integer.MAX_VALUE if unable to transform height to an int
  private int _getIntHeight(
    Object height
  )
  {
    int intHeight;

    if (height instanceof Number)
    {
      intHeight = ((Number)height).intValue();
    }
    else
    {
      String stringHeight = height.toString();

      // don't parse  0.000000e+000ights
      if (stringHeight.indexOf('%') == -1)
      {
        try
        {
          intHeight = new Integer(stringHeight).intValue();
        }
        catch (NumberFormatException e)
        {
          // not a number
          intHeight = Integer.MAX_VALUE;
        }
      }
      else
      {
        intHeight = Integer.MAX_VALUE;
      }
    }

    return intHeight;
  }

  private void _render(
    UIXRenderingContext context,
    UINode           node,
    Object           contentType,
    Object           id,
    Object           wireImageToControls,
    Object           source,
    Object           shortDesc,
    Object           standbyText,
    Object           width,
    Object           height,
    String           autostartValue,
    String           playCountParamName,
    Object           playCountParamValue,
    PlayerData       playerData,
    ControlData      controlData,
    boolean          useEmbedTag,
    boolean          useObjectTag,
    boolean          isImage,
    boolean          isNotMainID
    )throws IOException
  {
    String elementName = (isImage)
                             ? "img"
                             : (useEmbedTag)
                                 ? "embed"
                                 : "object";

    // name of attribute to use to write out source URL
    String sourceAttrName = "src";

    ResponseWriter writer = context.getResponseWriter();

    // start the element
    writer.startElement(elementName, node.getUIComponent());

    // render the short description
    writer.writeAttribute("title", shortDesc, null);

    // render the intrinsic events
    if (supportsIntrinsicEvents(context))
    {
      renderEventHandlers(context, node);
    }

    writer.writeAttribute("width", width, null);
    writer.writeAttribute("height", height, null);

    if (!isImage)
    {

      //
      // <embed> uses direct attributes
      //
      if (useEmbedTag)
      {

        // embed doesn't support id, use name instead
        writer.writeAttribute("name", id, null );

        // =-= bts, we might only want to do this if the client has specified
        //     that they want this particular player
        writer.writeAttribute("type", playerData.playerMimeType, null);

        // location to download new plug-in from
        writer.writeAttribute("pluginspage", playerData.pluginsPage, null);

        // autostart
        writer.writeAttribute(playerData.autostartParamName, autostartValue, null);

        // looping
        writer.writeAttribute(playCountParamName, playCountParamValue, null);

        // wire image window to controls
        writer.writeAttribute(playerData.wireImageToControlsParamName,
                              wireImageToControls,
							  null);

        // add in control attributes
        if (controlData != null)
        {
          String[] paramNameValues = controlData.paramNameValues;

          if (paramNameValues != null)
          {
            for (int i = 0; i < paramNameValues.length; i += 2)
            {
              writer.writeAttribute(paramNameValues[i], paramNameValues[i+1], null);
            }
          }
        }
      }
      else
      {
        if (!isNotMainID )
          renderID( context, node );

        // Object tag doesn't take a source attribute
        sourceAttrName = null;

        // override the content type with a player-specified type
        // =-= bts What does this do?
        if (playerData.overrideContentType != null)
        {
          contentType = playerData.overrideContentType;
        }

        // =-= bts maybe we should have a default message
        writer.writeAttribute("standby", standbyText, null);

        // mime-type of content
        writer.writeAttribute("type", contentType, null);

        // Class ID
        writer.writeAttribute("classid", playerData.classID, null);

        // Download location
        writer.writeAttribute("codebase", playerData.codeBase, null);
      }
    }

    // write out the source if any
    if (sourceAttrName != null)
    {
      writer.writeURIAttribute(sourceAttrName, source, null);
    }

    //
    // write out Object tag content
    //
    if (useObjectTag)
    {
      //
      // render the parameters to the player
      //

      // source of content
      _renderParamAttribute(context,
                            playerData.sourceParamName,
                            source,
                            true);

      // autostart
      _renderParamAttribute(context,
                            playerData.autostartParamName,
                            autostartValue,
                            false);

      // looping
      _renderParamAttribute(context,
                            playCountParamName,
                            playCountParamValue,
                            false);

      // wire image window to controls
      if ( playerData.wireImageToControlsParamName != null )
      {
        _renderParamAttribute(context,
                              playerData.wireImageToControlsParamName,
                              wireImageToControls,
                              false);
      }

      // add in control attributes
      if (controlData != null )
      {
        String[] paramNameValues = controlData.paramNameValues;

        if (paramNameValues != null)
        {
          for (int i = 0; i < paramNameValues.length; i += 2)
          {
            _renderParamAttribute(context,
                                  paramNameValues[i],
                                  paramNameValues[i + 1],
                                  false);
          }
        }
      }

      //
      // render the link as alternative content
      //
      // =-=bts I feel that our backup rendering should be an icon that
      //        we retrieve using a pretected method
      //
      _renderLink(writer, node, null, source, null, shortDesc, contentType);
    }

    // close the element
    writer.endElement(elementName);
  }



  //
  // known mime types
  //
  private static final String _AIFF_MIME_TYPE = "audio/aiff";
  private static final String _ART_MIME_TYPE = "image/x-art";
  private static final String _ASF_MIME_TYPE = "video/x-ms-asf";
  private static final String _AVI_MIME_TYPE = "video/avi";
  private static final String _BMP_MIME_TYPE = "image/bmp";
  private static final String _GIF_MIME_TYPE = "image/gif";
  private static final String _HTML_MIME_TYPE = "text/html";
  private static final String _IEF_IMAGE_MIME_TYPE = "image/ief";
  private static final String _JPEG_MIME_TYPE = "image/jpeg";
  private static final String _MIDI_MIME_TYPE = "audio/mid";
  private static final String _MPEG_AUDIO_MIME_TYPE = "audio/mpeg";
  private static final String _MPEG_VIDEO_MIME_TYPE = "video/mpeg";
  private static final String _MPEG2_VIDEO_MIME_TYPE = "video/x-mpeg2";
  private static final String _PLAIN_TEXT_MIME_TYPE = "text/plain";
  private static final String _PORTABLE_ANYMAP_MIME_TYPE = "image/x-portable-anymap";
  private static final String _PORTABLE_BITMAP_MIME_TYPE = "image/x-portable-bitmap";
  private static final String _PORTABLE_GRAYMAP_MIME_TYPE = "image/x-portable-graymap";
  private static final String _PICT_MIME_TYPE = "image/pict";
  private static final String _PNG_MIME_TYPE = "image/png";
  private static final String _QUICKTIME_MIME_TYPE = "video/quicktime";
  private static final String _CMU_RASTER_MIME_TYPE = "image/x-cmu-raster";
  private static final String _REAL_AUDIO_MIME_TYPE = "audio/x-realaudio";
  private static final String _REAL_AUDIO_PLUGIN_MIME_TYPE = "audio/x-pn-realaudio-plugin";
  private static final String _REAL_AUDIO_PN_MIME_TYPE = "audio/x-pn-realaudio";
  private static final String _REAL_VIDEO_MIME_TYPE = "video/x-realaudio";
  private static final String _REAL_VIDEO_PLUGIN_MIME_TYPE = "video/x-pn-realaudio-plugin";
  private static final String _REAL_VIDEO_PN_MIME_TYPE = "video/x-pn-realaudio";
  private static final String _RGB_MIME_TYPE = "image/x-rgb";
  private static final String _SGI_MOVIE_MIME_TYPE = "video/x-sgi-movie";
  private static final String _SMIL_MIME_TYPE = "application/smil";
  private static final String _TIFF_MIME_TYPE = "image/tiff";
  private static final String _ULAW_MIME_TYPE = "audio/basic";
  private static final String _WAV_MIME_TYPE = "audio/wav";
  private static final String _WMA_MIME_TYPE = "audio/x-ms-wma";
  private static final String _WMV_MIME_TYPE = "video/x-ms-wmv";
  private static final String _WVX_MIME_TYPE = "video/x-ms-wvx";
  private static final String _X_BITMAP_MIME_TYPE = "image/x-xbitmap";
  private static final String _X_PIXMAP_MIME_TYPE = "image/x-xpixmap";
  private static final String _X_WINDOW_DUMP_MIME_TYPE = "image/x-xwindowdump";

  // Maps MIME type variants to the cannonical MIME type
  private static final String[] _MIME_TYPE_REMAPPER = new String[]
  {
    "audio/vnd.rn-realaudio", _REAL_AUDIO_PN_MIME_TYPE,
    "video/vnd.rn-realvideo", _REAL_VIDEO_PN_MIME_TYPE,
    "audio/x-aiff",           _AIFF_MIME_TYPE,
    "audio/x-mpeg",           _MPEG_AUDIO_MIME_TYPE,
    "audio/x-wav",            _WAV_MIME_TYPE,
    "video/msvideo",          _AVI_MIME_TYPE,
    "video/x-mpeg",           _MPEG_VIDEO_MIME_TYPE,
    "video/x-msvideo",        _AVI_MIME_TYPE,
  };


  /**
   * Maintains attribute and size information for a particular control
   * configuration.
   */
  private static class ControlData
  {
    public ControlData(
      String[] paramNameValues,
      boolean  canAutosize,
      int      height,
      Number   preferredWidth
      )
    {
      if (height < 0)
        throw new IllegalArgumentException();

       if (preferredWidth.intValue() < 0)
        throw new IllegalArgumentException();

      this.paramNameValues = paramNameValues;
      this.canAutosize     = canAutosize;
      this.height          = height;
      this.preferredWidth  = preferredWidth;
    }

    // name values necessary to get to this control state
    public String[] paramNameValues;
    public boolean  canAutosize;
    public int      height;
    public Number   preferredWidth;
  }

  /**
   * Maintains default innersize information and ControlData information
   * for a set of ControlData's for a primary content type
   */
  private static class ControlSet
  {
    public ControlSet(
      Object[] controls
      )
    {
      _controls = _createHashMap(controls);
    }

    /**
     * Returns the ControlData Object to use to build up the set of controls
     * for this player, based on the client-specified set of controls to render.
     */
    public MediaRenderer.ControlData getControlData(
      Object controlValue
      )
    {
      Object controlData = null;

      // get the specified control data
      if (controlValue != null)
      {
        controlData = _controls.get(controlValue.toString());
      }

      // if we failed for some reason, default to the typical controls
      if (controlData == null)
      {
        controlData = _controls.get(CONTROLS_TYPICAL);
      }

      return (ControlData)controlData;
    }

    private final HashMap _controls;
  }


  /**
   * Class containing information about the different players
   */
  private static class PlayerData
  {
    public PlayerData(
      String   player,
      boolean  autostartByDefault,
      String   autostartTrueValue,
      String   autostartFalseValue,
      boolean  canAlwaysAutosize,
      Number   minHeight,
      String   autostartParamName,
      String   sourceParamName,
      String   infiniteLoopParamName,
      Object   infiniteLoopParamValue,
      String   playCountParamName,
      String   pluginsPage,
      String   codeBase,
      String   playerMimeType,
      String   classID,
      String   overrideContentType,
      String   wireImageToControlsParamName,
      ControlData imageWindowControlData,
      Object[] controlSets,
      Object[] unsupportedMimeTypes
      )
    {
      if ((minHeight != null) && (minHeight.intValue() < 0))
        throw new IllegalArgumentException();

      this.player                 = player;
      this.autostartByDefault     = autostartByDefault;
      this.autostartTrueValue     = autostartTrueValue;
      this.autostartFalseValue    = autostartFalseValue;
      this.canAlwaysAutosize      = canAlwaysAutosize;
      this.minHeight              = minHeight;
      this.autostartParamName     = autostartParamName;
      this.sourceParamName        = sourceParamName;
      this.infiniteLoopParamName  = infiniteLoopParamName;
      this.infiniteLoopParamValue = infiniteLoopParamValue;
      this.playCountParamName     = playCountParamName;
      this.pluginsPage            = pluginsPage;
      this.codeBase               = codeBase;
      this.classID                = classID;
      this.playerMimeType         = playerMimeType;
      this.overrideContentType    = overrideContentType;
      this.wireImageToControlsParamName = wireImageToControlsParamName;

      _imageWindowControlData = imageWindowControlData;
      _controlSets = _createHashMap(controlSets);
      _unsupportedMimeTypes = _createHashSet(unsupportedMimeTypes);
    }

    public MediaRenderer.ControlData getImageWindowControlData(
      String primaryContentType
      )
    {
      if ( "video".equals(primaryContentType))
        return _imageWindowControlData;

      return null;
    }

    /**
     * Returns the ControlSet Object to use to build up the set of controls
     * for this player, based on the primary content MimeType
     */
    public MediaRenderer.ControlSet getControlSet(
      String primaryContentType
      )
    {
      if (_controlSets != null)
      {
        Object controls = null;

        // get the primary hashmap
        if (primaryContentType != null)
          controls = _controlSets.get(primaryContentType);

        // if we have an unknown primary typoe, try again with video,
        // which shouldn't fail
        if (controls == null)
          controls = _controlSets.get("video");

        return (ControlSet)controls;
      }

      return null;
    }

    /**
     * Returns true if the player supports rendering this format
     */
    public boolean isSupportedMimeType(
      String mimeType
      )
    {
      if ((mimeType != null) && (mimeType.length() != 0))
      {
        if (_unsupportedMimeTypes != null)
        {
          // extension isn't listed as an unsupported extension
          return !_unsupportedMimeTypes.contains(mimeType);
        }
      }

      return true;
    }

    public  final String  player;
    public  final boolean autostartByDefault;
    public  final String  autostartTrueValue;
    public  final String  autostartFalseValue;
    public  final boolean canAlwaysAutosize;
    public  final Number  minHeight;
    public  final String  autostartParamName;
    public  final String  sourceParamName;
    public  final String  infiniteLoopParamName;
    public  final Object  infiniteLoopParamValue;
    public  final String  playCountParamName;
    public  final String  pluginsPage;
    public  final String  codeBase;
    public  final String  playerMimeType;
    public  final String  classID;
    public  final String  overrideContentType;
    public  final String  wireImageToControlsParamName;
    private final ControlData _imageWindowControlData;
    private final HashMap _controlSets;
    private final HashSet _unsupportedMimeTypes;
  }


  // Real Player Class ID
  private static final String _RP_CLASS_ID =
                                  "clsid:CFCDAA03-8BE4-11cf-B84B-0020AFBBCCFA";

  // QuickTime class id
  private static final String _QT_CLASS_ID =
                                  "clsid:02BF25D5-8C17-4B23-BC80-D3488ABDDC6B";

  private static final String _QT_CODE_BASE =
                              "http://www.apple.com/qtactivex/qtplugin.cab";

  private static final String _WMP_6_4_CLASS_ID =
                               "clsid:22D6F312-B0F6-11D0-94AB-0080C74C7E95";

  private static final String _WMP_CODE_BASE =
    "http://activex.microsoft.com/activex/controls/mplayer/en/nsmp2inf.cab#Version=6,0,02,902";

  private static final Number _MIN_QT_HEIGHT = TWO;
  private static final int _QT_CONTROL_HEIGHT = 16;
  private static final int _WMP_CONTROL_HEIGHT = 40;

  //
  // Meta Data for the Link Player
  //
  private static final MediaRenderer.PlayerData _LINK_PLAYER_DATA =
    new MediaRenderer.PlayerData(PLAYER_LINK, // player type
                   false,       // no autostart by default
                   "true",      // autostartTrueValue
                   "false",     // autostartFalseValue
                   true,        // can always autosize
                   null,        // min player height
                   null,        // autostartParamName
                   null,        // sourceParamName
                   null,        // infiniteLoopParamName,
                   null,        // infiniteLoopParamValue
                   null,        // playCountParamName
                   null,        // pluginsPage URL
                   null,        // <Object> CodeBase
                   null,        // playerMimeType
                   null,        // <Object> ClassID
                   null,        // overrideContentType
                   null,        // wireImageToControlsParamName
                   null,        // imageWindowControlData
                   null,        // ControlData[]
                   null);       // Unsupported MimeTypes

  //
  //
  private static final MediaRenderer.PlayerData _IMAGE_PLAYER_DATA =
    new MediaRenderer.PlayerData("image",     // player type
                   false,       // no autostart by default
                   "true",      // autostartTrueValue
                   "false",     // autostartFalseValue
                   true,        // can always autosize
                   null,        // min player height
                   null,        // autostartParamName
                   null,        // sourceParamName
                   null,        // infiniteLoopParamName,
                   null,        // infiniteLoopParamValue
                   null,        // playCountParamName
                   null,        // pluginsPage URL
                   null,        // <Object> CodeBase
                   null,        // playerMimeType
                   null,        // <Object> ClassID
                   null,        // overrideContentType
                   null,        // wireImageToControlsParamName
                   null,        // imageWindowControlData
                   null,        // ControlData[]
                   null);       // Unsupported MimeTypes

  //
  // Meta Data for the Quicktime Player
  //

  // ControlData for the QuickTime all controls case
  private static final MediaRenderer.ControlData _QUICKTIME_ALL_CONTROL_DATA =
     new MediaRenderer.ControlData(null,                  // all controls is the default
                     false,                 // no autosize
                     _QT_CONTROL_HEIGHT,    // extra height
                     new Integer(275));     // preferred width

  private static final MediaRenderer.PlayerData _QUICKTIME_PLAYER_DATA =
    new MediaRenderer.PlayerData(
    PLAYER_QUICKTIME,                           // player type
    true,                                       // autostart by default
    "true",                                     // autostartTrueValue
    "false",                                    // autostartFalseValue
    false,                                      // can't always autosize
    _MIN_QT_HEIGHT,                             // min player height
    "autoplay",                                 // autostartParamName
    "src",                                      // sourceParamName
    "loop",                                     // infiniteLoopParamName,
    "true",                                     // infiniteLoopParamValue
    null,                                       // no loop count support
    "http://www.apple.com/quicktime/download/", // pluginsPage
    _QT_CODE_BASE,                              // <Object> CodeBase
    null,                                       // playerMimeType
    _QT_CLASS_ID,                               // <Object> ClassID
    null,                                       // overrideContentType
    null,                                       // wireImageToControlsParamName
    null,                                       // imageWindowControlData
    new Object[]
    {
      "audio", new MediaRenderer.ControlSet(new Object[]
      {
        CONTROLS_NONE,         new MediaRenderer.ControlData(new String[]
                                               {
                                                 "kioskmode", "true",
                                                 "controller","false",
                                               },
                                               false, // no autosize
                                               0,
                                               ZERO),
        CONTROLS_NONE_VISIBLE, new MediaRenderer.ControlData(new String[]
                                               {
                                                 "controller", "false"
                                               },
                                               false, // no autosize
                                               0,
                                               ZERO),
        CONTROLS_MINIMAL,      new MediaRenderer.ControlData(null, // all controls default
                                               false, // no autosize
                                               _QT_CONTROL_HEIGHT,
                                               new Integer(18)),  // play button only
        CONTROLS_TYPICAL,      _QUICKTIME_ALL_CONTROL_DATA,
        CONTROLS_ALL,          _QUICKTIME_ALL_CONTROL_DATA,
      }),
      "video", new MediaRenderer.ControlSet(new Object[]
      {
        CONTROLS_NONE,         new MediaRenderer.ControlData(new String[]
                                               {
                                                 "kioskmode", "true",
                                                 "controller","false",
                                               },
                                               false, // no autosize
                                               0,
                                               ZERO),
        CONTROLS_NONE_VISIBLE, new MediaRenderer.ControlData(new String[]
                                               {
                                                 "controller", "false"
                                               },
                                               false, // no autosize
                                               0,
                                               ZERO),
        CONTROLS_MINIMAL,      _QUICKTIME_ALL_CONTROL_DATA,
        CONTROLS_TYPICAL,      _QUICKTIME_ALL_CONTROL_DATA,
        CONTROLS_ALL,          _QUICKTIME_ALL_CONTROL_DATA,
      }),
    },
    new String[] // mime-types not supported by Quicktime 5
    {
      _WMA_MIME_TYPE,
      _WMV_MIME_TYPE,
      _REAL_AUDIO_MIME_TYPE,
      _REAL_AUDIO_PN_MIME_TYPE,
      _REAL_AUDIO_PLUGIN_MIME_TYPE,
      _REAL_VIDEO_MIME_TYPE,
      _REAL_VIDEO_PN_MIME_TYPE,
      _REAL_VIDEO_PLUGIN_MIME_TYPE,
    });


  //
  // Meta Data for the Windows Media Player 6.4
  //

  // preferred width of WMP showing all controls
  private static final Number _WMP_ALL_CONTROLS_WIDTH = new Integer(275);

  private static final Number _WMP_MINIMAL_CONTROLS_WIDTH =
                                                IntegerUtils.getInteger(72);
  private static final int _WMP_MINIMAL_CONTROLS_HEIGHT = 21;
  private static final int _WMP_ALL_CONTROLS_HEIGHT = 170;

  // control data for showing no visible controls on Windows Meida Player 6.4
  private static final MediaRenderer.ControlData _WMP_6_4_NONE_VISIBLE_CONTROL_DATA =
     new MediaRenderer.ControlData(new String[]
                     {
                       "showcontrols", "false"
                     },
                     true, // autosizes
                     0,
                     ZERO);

  // =-= bts what about close captioning?
  // =-= bts Is unsupported mime types the right way to go, how about supported
  //         mimetypes instead?

  //
  // ControlSet shared by all primary content types on WMP 6.4
  //
  private static final MediaRenderer.ControlSet _WINDOWS_6_4_CONTROL_SET = new MediaRenderer.ControlSet(
    new Object[]
    {
      CONTROLS_NONE,         _WMP_6_4_NONE_VISIBLE_CONTROL_DATA,
      CONTROLS_NONE_VISIBLE, _WMP_6_4_NONE_VISIBLE_CONTROL_DATA,
      CONTROLS_MINIMAL,      new MediaRenderer.ControlData(new String[]
                                             {
                                               "showcontrols",         "true",
                                               "showaudiocontrols",    "false",
                                               "showpositioncontrols", "false",
                                               "showtracker",          "false",
                                             },
                                             false, // no autosize
                                             _WMP_MINIMAL_CONTROLS_HEIGHT,
                                             _WMP_MINIMAL_CONTROLS_WIDTH),
      CONTROLS_TYPICAL,      new MediaRenderer.ControlData(null,// this is the default
                                             true, // autosizes
                                             _WMP_CONTROL_HEIGHT,
                                             _WMP_ALL_CONTROLS_WIDTH),
      CONTROLS_ALL,          new MediaRenderer.ControlData(new String[]
                                             {
                                               "showdisplay",   "true",
                                               "showgotobar",   "true",
                                               "showstatusbar", "true",
                                             },
                                             true, // autosizes
                                             _WMP_ALL_CONTROLS_HEIGHT,
                                             _WMP_ALL_CONTROLS_WIDTH),
    });

  private static final MediaRenderer.PlayerData _WINDOWS_6_4_PLAYER_DATA =
    new MediaRenderer.PlayerData(
    PLAYER_WINDOWS,                                  // player type
    true,                                            // autostart by default
    "1",                                             // autostartTrueValue
    "0",                                             // autostartFalseValue
    false,                                           // can't always autosize
    null,                                            // min player height
    "autostart",                                     // autostartParamName
    "filename",                                      // sourceParamName
    "playcount",                                     // infiniteLoopParamName,
    ZERO,                                            // infiniteLoopParamValue
    "playcount",                                     // playCountParamName
    "http://www.microsoft.com/Windows/MediaPlayer/", // pluginsPage
    _WMP_CODE_BASE,                                  // <Object> CodeBase
    "application/x-mplayer2",                        // playerMimeType
    _WMP_6_4_CLASS_ID,                               // <Object> ClassID
    "application/x-oleobject",                       // overrideContentType
    null,                                            // wireImageToControlsParamName
    null,                                            // imageWindowControlData
    new Object[]
    {
      "audio", _WINDOWS_6_4_CONTROL_SET,
      "video", _WINDOWS_6_4_CONTROL_SET,
    },
    new String[] // mime-types not supported by Windows Media Player 6.4
    {
      _QUICKTIME_MIME_TYPE,
      _REAL_AUDIO_MIME_TYPE,
      _REAL_AUDIO_PN_MIME_TYPE,
      _REAL_AUDIO_PLUGIN_MIME_TYPE,
      _REAL_VIDEO_MIME_TYPE,
      _REAL_VIDEO_PN_MIME_TYPE,
      _REAL_VIDEO_PLUGIN_MIME_TYPE,
    });

  //
  // Meta Data for the Windows Media Player 7.1
  //

  /* WMP 7.1 is not currently activated in any way in this class.
  private static final String _WMP_7_1_CLASS_ID =
                               "clasid:6BF52A52-394A-11D3-B153-00C04F79FAA6";


  // control data for showing no visible controls on Windows Media Player 7.1
  private static final MediaRenderer.ControlData _WMP_7_1_NONE_VISIBLE_CONTROL_DATA =
     new MediaRenderer.ControlData(new String[]{"uimode", "none"},
                     true, // autosizes
                     0,
                     ZERO);

  // control data for showing typical controls on Windows Media Player 7.1
  private static final MediaRenderer.ControlData _WMP_7_1_TYPICAL_CONTROL_DATA =
     new MediaRenderer.ControlData(null,// typical is the default
                     true, // autosizes
                     _WMP_CONTROL_HEIGHT,
                     _WMP_ALL_CONTROLS_WIDTH);

  //
  // ControlSet shared by all primary content types on WMP 7.1
  //
  private static final MediaRenderer.ControlSet _WINDOWS_7_1_CONTROL_SET = new MediaRenderer.ControlSet(
    new Object[]
    {
      CONTROLS_NONE,         _WMP_7_1_NONE_VISIBLE_CONTROL_DATA,
      CONTROLS_NONE_VISIBLE, _WMP_7_1_NONE_VISIBLE_CONTROL_DATA,
      CONTROLS_MINIMAL,      new MediaRenderer.ControlData(new String[]{"uimode", "mini"},
                                             true, // autosizes
                                             _WMP_CONTROL_HEIGHT,
                                             _WMP_ALL_CONTROLS_WIDTH),
      CONTROLS_TYPICAL,      _WMP_7_1_TYPICAL_CONTROL_DATA,
      CONTROLS_ALL,          _WMP_7_1_TYPICAL_CONTROL_DATA,
    });

  private static final MediaRenderer.PlayerData _WINDOWS_7_1_PLAYER_DATA =
    new MediaRenderer.PlayerData(
    PLAYER_WINDOWS,                                  // player type
    true,                                            // autostart by default
    "1",                                             // autostartTrueValue
    "0",                                             // autostartFalseValue
    true,                                            // can always autosize
    null,                                            // min player height
    "autostart",                                     // autostartParamName
    "filename",                                      // sourceParamName
    "playcount",                                     // infiniteLoopParamName,
    ZERO,                                            // infiniteLoopParamValue
    "playcount",                                     // playCountParamName
    "http://www.microsoft.com/Windows/MediaPlayer/", // pluginsPage
    _WMP_CODE_BASE,                                  // <Object> CodeBase
    "application/x-mplayer2",                        // playerMimeType
    _WMP_7_1_CLASS_ID,                               // <Object> ClassID
    "application/x-oleobject",                       // overrideContentType
    null,                                            // wireImageToControlsParamName
    null,                                            // imageWindowControlData
    new Object[]
    {
      "audio", _WINDOWS_7_1_CONTROL_SET,
      "video", _WINDOWS_7_1_CONTROL_SET,
    },
    new String[] // mime-types not supported by Windows Media Player 7.0.1
    {
      _QUICKTIME_MIME_TYPE,
      _QUICKTIME_MIME_TYPE,
      _REAL_AUDIO_MIME_TYPE,
      _REAL_AUDIO_PN_MIME_TYPE,
      _REAL_AUDIO_PLUGIN_MIME_TYPE,
      _REAL_VIDEO_MIME_TYPE,
      _REAL_VIDEO_PN_MIME_TYPE,
      _REAL_VIDEO_PLUGIN_MIME_TYPE,
   });
  */

  //
  // Meta Data for the Real Player
  //
  private static final Number _REAL_MINIMAL_WIDTH  = new Integer(44);
  private static final int    _REAL_MINIMAL_HEIGHT = 26;

  private static final Number _REAL_TYPICAL_WIDTH  = new Integer(220);
  private static final int    _REAL_TYPICAL_HEIGHT = 36;

  private static final Number _REAL_ALL_WIDTH  = new Integer(375);
  private static final int    _REAL_ALL_HEIGHT = 100;


 // null control data
  private static final MediaRenderer.ControlData _NULL_CONTROL_DATA =
     new MediaRenderer.ControlData(null,
                     false,           // no autosize
                     0,
                     ZERO);



  // control data for showing image window on Real Player
  private static final MediaRenderer.ControlData _REAL_VIDEO_IMAGE_WINDOW_CONTROL_DATA =
     new MediaRenderer.ControlData(new String[]
                     {
                        "controls", "ImageWindow",
                        "maintainAspect", "true"
                     },
                     false,           // no autosize
                     0,
                     ZERO);



  private static final MediaRenderer.ControlData _REAL_MINIMAL_CONTROL_DATA =
  new MediaRenderer.ControlData(new String[]
                               {
                                 "controls", "PlayButton"
                               },
                               false,           // no autosize
                               _REAL_MINIMAL_HEIGHT,
                               _REAL_MINIMAL_WIDTH);


  // control data for showing typical controls on Real Player
  private static final MediaRenderer.ControlData _REAL_TYPICAL_CONTROL_DATA =
     new MediaRenderer.ControlData(new String[]{"controls", "ControlPanel"},
                     false,           // no autosize
                     _REAL_TYPICAL_HEIGHT,
                     _REAL_TYPICAL_WIDTH);



  // control data for showing all controls on Real Player
  private static final MediaRenderer.ControlData _REAL_ALL_CONTROL_DATA =
     new MediaRenderer.ControlData(new String[]{"controls", "All"},
                     false,           // no autosize
                     _REAL_ALL_HEIGHT,
                     _REAL_ALL_WIDTH);

  private static final MediaRenderer.ControlData _REAL_AUDIO_NONE_VISIBLE_CONTROL_DATA =
     new MediaRenderer.ControlData(new String[]{"controls", ""},
                     false,           // no autosize
                     0,
                     ZERO);


  private static final MediaRenderer.PlayerData _REAL_PLAYER_DATA =
    new MediaRenderer.PlayerData(
    PLAYER_REAL,                                // player type
    false,                                      // no autostart by default
    "true",                                     // autostartTrueValue
    "false",                                    // autostartFalseValue
    false,                                      // can't always autosize
    null,                                       // min player height
    "autostart",                                // autostartParamName
    "src",                                      // sourceParamName
    "loop",                                     // infiniteLoopParamName,
    "true",                                     // infiniteLoopParamValue
    "numloop",                                  // playCountParamName
    "http://www.real.com/player/",              // pluginsPage
    "http://www.real.com/player/",              // <Object> CodeBase
    "audio/x-pn-realaudio-plugin",              // playerMimeType
    _RP_CLASS_ID,                               // <Object> ClassID
    null,                                       // overrideContentType
    "console",                                  // wireImageToControlsParamName
    _REAL_VIDEO_IMAGE_WINDOW_CONTROL_DATA,  // imageWindowControlData
    new Object[]
    {
      "audio", new MediaRenderer.ControlSet(new Object[]
      {
        CONTROLS_NONE,         _REAL_AUDIO_NONE_VISIBLE_CONTROL_DATA,
        CONTROLS_NONE_VISIBLE, _REAL_AUDIO_NONE_VISIBLE_CONTROL_DATA,
        CONTROLS_MINIMAL,      _REAL_MINIMAL_CONTROL_DATA,
        CONTROLS_TYPICAL,      _REAL_TYPICAL_CONTROL_DATA,
        CONTROLS_ALL,          _REAL_ALL_CONTROL_DATA,
      }),
      "video", new MediaRenderer.ControlSet(new Object[]
      {
        CONTROLS_NONE,         _NULL_CONTROL_DATA,
        CONTROLS_NONE_VISIBLE, _NULL_CONTROL_DATA,
        CONTROLS_MINIMAL,      _REAL_MINIMAL_CONTROL_DATA,
        CONTROLS_TYPICAL,      _REAL_TYPICAL_CONTROL_DATA,
        CONTROLS_ALL,          _REAL_ALL_CONTROL_DATA,
      }),
    },
    null); // mime types not supported by real

  private static final Object[] _PLAYER_DATA = new Object[]
  {
    PLAYER_LINK,      _LINK_PLAYER_DATA,
    PLAYER_QUICKTIME, _QUICKTIME_PLAYER_DATA,
    PLAYER_WINDOWS,   _WINDOWS_6_4_PLAYER_DATA,
    PLAYER_REAL,      _REAL_PLAYER_DATA,
  };

  // maps extensions to MIME types
  private static final String[] _EXTENSION_TO_MIME_TYPE = new String[]
  {
    "aif",  _AIFF_MIME_TYPE,
    "aifc", _AIFF_MIME_TYPE,
    "aiff", _AIFF_MIME_TYPE,
    "art",  _ART_MIME_TYPE,
    "asf",  _ASF_MIME_TYPE,
    "asx",  _ASF_MIME_TYPE,
    "au",   _ULAW_MIME_TYPE,
    "avi",  _AVI_MIME_TYPE,
    "bmp",  _BMP_MIME_TYPE,
    "gif",  _GIF_MIME_TYPE,
    "htm",  _HTML_MIME_TYPE,
    "html", _HTML_MIME_TYPE,
    "ief",  _IEF_IMAGE_MIME_TYPE,
    "jfif", _JPEG_MIME_TYPE,
    "jpe",  _JPEG_MIME_TYPE,
    "jpg",  _JPEG_MIME_TYPE,
    "jpeg", _JPEG_MIME_TYPE,
    "kar",  _MIDI_MIME_TYPE,
    "m15",  _MPEG_VIDEO_MIME_TYPE,
    "m75",  _MPEG_VIDEO_MIME_TYPE,
    "m1a",  _MPEG_VIDEO_MIME_TYPE,
    "m1s",  _MPEG_VIDEO_MIME_TYPE,
    "m1v",  _MPEG_VIDEO_MIME_TYPE,
    "mid",  _MIDI_MIME_TYPE,
    "midi", _MIDI_MIME_TYPE,
    "mov",  _QUICKTIME_MIME_TYPE,
    "movie",_SGI_MOVIE_MIME_TYPE,
    "mpe",  _MPEG_VIDEO_MIME_TYPE,
    "mpg",  _MPEG_VIDEO_MIME_TYPE,
    "mpga", _MPEG_AUDIO_MIME_TYPE,
    "mpeg", _MPEG_VIDEO_MIME_TYPE,
    "mp2",  _MPEG_AUDIO_MIME_TYPE,
    "mp3",  _MPEG_AUDIO_MIME_TYPE,
    "mpa",  _MPEG_AUDIO_MIME_TYPE,
    "mpm",  _MPEG_AUDIO_MIME_TYPE,
    "mp2v", _MPEG2_VIDEO_MIME_TYPE,
    "mpv2", _MPEG2_VIDEO_MIME_TYPE,
    "pbm",  _PORTABLE_BITMAP_MIME_TYPE,
    "pgm",  _PORTABLE_GRAYMAP_MIME_TYPE,
    "pnm",  _PORTABLE_ANYMAP_MIME_TYPE,
    "pic",  _PICT_MIME_TYPE,
    "pict", _PICT_MIME_TYPE,
    "png",  _PNG_MIME_TYPE,
    "qt",   _QUICKTIME_MIME_TYPE,
    "ra",   _REAL_AUDIO_MIME_TYPE,
    "ram",  _REAL_AUDIO_PN_MIME_TYPE,
    "ras",  _CMU_RASTER_MIME_TYPE,
    "rgb",  _RGB_MIME_TYPE,
    "rm",   _REAL_AUDIO_PN_MIME_TYPE,
    "rpg",  _REAL_AUDIO_PLUGIN_MIME_TYPE,
    "rpm",  _REAL_AUDIO_PLUGIN_MIME_TYPE,
    "rv",   _REAL_VIDEO_MIME_TYPE,
    "smf",  _MIDI_MIME_TYPE,
    "smi",  _SMIL_MIME_TYPE,
    "smil", _SMIL_MIME_TYPE,
    "snd",  _ULAW_MIME_TYPE,
    "tif",  _TIFF_MIME_TYPE,
    "tiff", _TIFF_MIME_TYPE,
    "txt",  _PLAIN_TEXT_MIME_TYPE,
    "ulw",  _ULAW_MIME_TYPE,
    "wav",  _WAV_MIME_TYPE,
    "wma",  _WMA_MIME_TYPE,
    "wmv",  _WMV_MIME_TYPE,
    "wvx",  _WVX_MIME_TYPE,
    "xbm",  _X_BITMAP_MIME_TYPE,
    "xpm",  _X_PIXMAP_MIME_TYPE,
    "xwd",  _X_WINDOW_DUMP_MIME_TYPE,
  };



  // Preferred Players for MimeTypes
  private static final Object[] _PREFERRED_MIME_PLAYERS = new Object[]
  {
    _AIFF_MIME_TYPE,              PLAYER_QUICKTIME,
    _ASF_MIME_TYPE,               PLAYER_WINDOWS,
    _AVI_MIME_TYPE,               PLAYER_WINDOWS,
    _QUICKTIME_MIME_TYPE,         PLAYER_QUICKTIME,
    _REAL_AUDIO_MIME_TYPE,        PLAYER_REAL,
    _REAL_AUDIO_PN_MIME_TYPE,     PLAYER_REAL,
    _REAL_AUDIO_PLUGIN_MIME_TYPE, PLAYER_REAL,
    _REAL_VIDEO_MIME_TYPE,        PLAYER_REAL,
    _REAL_VIDEO_PN_MIME_TYPE,     PLAYER_REAL,
    _REAL_VIDEO_PLUGIN_MIME_TYPE, PLAYER_REAL,
    _WMA_MIME_TYPE,               PLAYER_WINDOWS,
    _WMV_MIME_TYPE,               PLAYER_WINDOWS,
    _WVX_MIME_TYPE,               PLAYER_WINDOWS,
  };

  //
  // Object constants for OS's
  //
  private static final Integer _WINDOWS_OS =
                                  IntegerUtils.getInteger(TrinidadAgent.OS_WINDOWS);

  private static final Integer _MAC_OS =
                                  IntegerUtils.getInteger(TrinidadAgent.OS_MACOS);

  private static final Integer _SOLARIS_OS =
                                  IntegerUtils.getInteger(TrinidadAgent.OS_SOLARIS);

  // Preferred Players for Operating Systems
  /* CURRENTLY UNUSED
  private static final Object[] _PREFERRED_OS_PLAYERS = new Object[]
  {
    _WINDOWS_OS, PLAYER_WINDOWS,
    _MAC_OS,     PLAYER_QUICKTIME,
    _SOLARIS_OS, PLAYER_WINDOWS,
  };
  */

  // Supported players on each OS, in prference order
  private static final Object[] _SUPPORTED_OS_PLAYERS = new Object[]
  {
    _WINDOWS_OS, new Object[]{PLAYER_WINDOWS,   PLAYER_QUICKTIME, PLAYER_REAL},
    _MAC_OS,     new Object[]{PLAYER_QUICKTIME, PLAYER_WINDOWS, PLAYER_REAL},
    _SOLARIS_OS, new Object[]{PLAYER_WINDOWS},
  };

  //
  // default sizes for different primary mime types
  //
  private static Object[] _DEFAULT_INNER_SIZES = new Object[]
  {
    "audio", new Number[]{ZERO, ZERO},
    "video", new Number[]{IntegerUtils.getInteger(200),
                          IntegerUtils.getInteger(150)},
  };

  private static final int _DEFAULT_INNER_WIDTH_INDEX  = 0;
  private static final int _DEFAULT_INNER_HEIGHT_INDEX = 1;


  // local property for the player identification information
  private static final Object _PLAYER_DATA_KEY = new Object();

  // map of mime types to preferred players
  private static HashMap _sPreferredMimePlayers;

  // map of Operating Systems to preferred players
  private static HashMap _sPreferredOSPlayers;

  // map of Operating Systems to HashSet of supported players
  private static HashMap _sSupportedOSPlayers;

  // map the in mime type to the cannonical mime type
  private static HashMap  _sMimeTypeRemapper;

  // maps the extension to the mime type
  private static HashMap _sExtensionMap;

  // maps a player name to its player data
  private static HashMap _sPlayerData;

  // maps a primary content type to a default inner size
  private static HashMap _sDefaultInnerSizes;

  static
  {
    // initialize data for supported players
    _sPlayerData = _createHashMap(_PLAYER_DATA);

    // initialize the mapping of mime types to preferred players
    _sPreferredMimePlayers = _createHashMap(_PREFERRED_MIME_PLAYERS);

    //
    // create the set of all OS's that have players.  The first player
    // listed is the preferred player for that OS.
    //
    int supportedOSPlayersLength = _SUPPORTED_OS_PLAYERS.length;
    int hashSize = (int)(supportedOSPlayersLength * 0.75);

    _sSupportedOSPlayers = new HashMap(hashSize);
    _sPreferredOSPlayers = new HashMap(hashSize);

    for (int i = 0; i < supportedOSPlayersLength; i += 2)
    {
      Object   os      = _SUPPORTED_OS_PLAYERS[i];
      Object[] players = (Object[])_SUPPORTED_OS_PLAYERS[i + 1];

      if ((players != null) && (players.length > 0))
      {
        // add the players for this OS to the mapping of OSs to
        // player sets
        _sSupportedOSPlayers.put(os, _createHashSet(players));

        // record the preferred player for this OS
        _sPreferredOSPlayers.put(os, players[0]);
      }
    }

    // initialize the mime type remapper
    _sMimeTypeRemapper = _createHashMap(_MIME_TYPE_REMAPPER);

    // initialize the extension map
    _sExtensionMap = _createHashMap(_EXTENSION_TO_MIME_TYPE);

    // initialize the map of default inner sizes
    _sDefaultInnerSizes = _createHashMap(_DEFAULT_INNER_SIZES);
  }
}
