package org.apache.myfaces.trinidad.util;

public abstract class URLEncoderWrapper
  extends URLEncoder
{
  public abstract URLEncoder getWrapped();
  
  public String encodeActionURL(String url)
  {
    return getWrapped().encodeActionURL(url);
  }

  public String encodeResourceURL(String url)
  {
    return getWrapped().encodeResourceURL(url);
  }

  @Override
  public String encodeInProtocolResourceURL(String url)
  {
    return getWrapped().encodeInProtocolResourceURL(url);
  }

  @Override
  public String encodePartialActionURL(String url)
  {
    return getWrapped().encodePartialActionURL(url);
  }

  @Override
  public String encodeRedirectURL(String url)
  {
    return getWrapped().encodeRedirectURL(url);
  }

  @Override
  public String encodeSkinResourceURL(String url)
  {
    return getWrapped().encodeSkinResourceURL(url);
  }
}
