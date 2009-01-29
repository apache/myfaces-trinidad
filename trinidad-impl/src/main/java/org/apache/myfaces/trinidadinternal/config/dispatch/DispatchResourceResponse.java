package org.apache.myfaces.trinidadinternal.config.dispatch;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.faces.context.ExternalContext;

import javax.portlet.RenderRequest;
import javax.portlet.RenderResponse;
import javax.portlet.ResourceRequest;
import javax.portlet.ResourceResponse;
import javax.portlet.filter.ResourceResponseWrapper;

public class DispatchResourceResponse
  extends ResourceResponseWrapper
{
  public DispatchResourceResponse(ExternalContext ec)
  {
    super((ResourceResponse)ec.getResponse());
    _request = (ResourceRequest)ec.getRequest();
  }

  @Override
  public void setContentType(
    String contentTypeAndCharset)
  {
    if(contentTypeAndCharset != null)
    {
      Matcher matcher = _CONTENT_TYPE_PATTERN.matcher(contentTypeAndCharset);
      if (matcher.matches())
      {
        String contentType = matcher.group(1);
        String charset = (matcher.groupCount() > 1) ? matcher.group(2) : null;

        // capture the content type on the request
        _request.setAttribute(DispatchResponseConfiguratorImpl.__CONTENT_TYPE_KEY, contentType);

        // TODO: use Agent APIs when available
        if ("application/xhtml+xml".equals(contentType))
        {
          //TODO: Is this still needed in IE7??
          String userAgent = _request.getProperty("User-agent");
          if (userAgent != null && userAgent.indexOf("compatible; MSIE") != -1)
          {
            // IE must serve XHTML as text/html
            contentTypeAndCharset = "text/html";

            if (charset != null)
              contentTypeAndCharset += ";charset=" + charset;
          }
        }
      }
    }
    super.setContentType(contentTypeAndCharset);
  }

  private final ResourceRequest _request;

  static private final Pattern _CONTENT_TYPE_PATTERN = Pattern.compile("([^;]+)(?:;charset=(.*))?");
}
