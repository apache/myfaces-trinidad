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
package org.apache.myfaces.trinidaddemo.email;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.mail.Address;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.internet.InternetAddress;

import org.apache.myfaces.trinidad.context.RequestContext;

/**
 * Backing bean for the "reply-to message" page.
 * @todo For now, the original contents are not included in the reply
 * We could add a preference so the user can choose to include
 * the original contents in replies
 */
public class ReplyToMessageBackingBean extends NewMessageBackingBean
{
  public ReplyToMessageBackingBean()
  {
    // if the user pressed the Reply button, then the pageFlowScope's
    // replyToAll value will be false. If the user pressed the
    // Reply to All button, then the pageFlowScope's replyToAll value
    // will be true. We set this in the showMessage.jspx page.
    RequestContext afContext = RequestContext.getCurrentInstance();

    Object replyToAll = afContext.getPageFlowScope().get("replyToAll");

    _setUpReplyToMessage("true".equals(replyToAll));
  }

  private void _setUpReplyToMessage(boolean replyToAll)
  {
    RequestContext afContext = RequestContext.getCurrentInstance();

    MessageData message =
      (MessageData) afContext.getPageFlowScope().get("message");

    if (message == null) return;
    Message msg = message.getMessage();

    try
    {
        msg.getFolder().open(Folder.READ_ONLY);
        Message replyMessage = msg.reply(replyToAll);

        setSubject(replyMessage.getSubject());
        Address[] replyToAddresses = replyMessage.getAllRecipients();
        setTo(_getAddressString(replyToAddresses));

    }
    catch (MessagingException e)
    {
      _LOG.log(Level.WARNING, "Couldn't create reply-to message", e);
    }
    finally
    {

        try
        {
          msg.getFolder().close(false);
        }
        catch (Exception e)
        {

        }

    }
  }

  /**
   * Given Address[], return a comma-separated string of the email addresses.
   * @param replyToAddresses
   * @return return a comma-separated string of the email addresses.
   */
  private String _getAddressString(Address[] replyToAddresses)
  {

    StringBuffer to = new StringBuffer(100);

    for (int i = 0; i < replyToAddresses.length; i++)
    {
      if (i > 0)
        to.append(",");
      to.append(((InternetAddress)replyToAddresses[i]).getAddress());
    }

    return to.toString();

  }

  static private final Logger _LOG =
    Logger.getLogger(ReplyToMessageBackingBean.class.getName());

}
