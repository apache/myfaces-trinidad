package org.apache.myfaces.trinidad.util;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

class ExternalContextURLEncoder extends URLEncoder
{
  public ExternalContextURLEncoder(ExternalContext ec)
  {
    assert(ec != null);
    _context = ec;
  }
  
  @Override
  public String encodePartialActionURL(String url)
  {
    return _context.encodeActionURL(url);
  }

  public String encodeRedirectURL(String url)
  {
    return url;
  }

  public String encodeInProtocolResourceURL(String url)
  {
    return _context.encodeResourceURL(url);
  }

  public String encodeSkinResourceURL(String url)
  {
    return url;
  }

  @Override
  public String encodeActionURL(String url)
  {
    return _context.encodeActionURL(url);
  }
  
  @Override
  public String encodeResourceURL(String url)
  {
    return _context.encodeResourceURL(url);
  }
  
  private ExternalContext _context;
}
