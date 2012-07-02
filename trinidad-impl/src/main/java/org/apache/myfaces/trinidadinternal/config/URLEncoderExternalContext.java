package org.apache.myfaces.trinidadinternal.config;

import javax.faces.context.ExternalContext;

import org.apache.myfaces.trinidad.context.ExternalContextDecorator;
import org.apache.myfaces.trinidad.util.URLEncoder;
import org.apache.myfaces.trinidad.util.URLEncoderFactory;

public class URLEncoderExternalContext
  extends ExternalContextDecorator
{
  public URLEncoderExternalContext(ExternalContext ec)
  {
    _ec = ec;
  }

  protected ExternalContext getExternalContext()
  {
    return _ec;
  }

  @Override
  public String encodeResourceURL(String url)
  {
    URLEncoder encoder = URLEncoderFactory.getFactory().getURLEncoder(_ec);
    return encoder.encodeResourceURL(url);
  }

  @Override
  public String encodeActionURL(String url)
  {
    URLEncoder encoder = URLEncoderFactory.getFactory().getURLEncoder(_ec);
    return encoder.encodeActionURL(url);
  }
  
  private ExternalContext _ec;
}
