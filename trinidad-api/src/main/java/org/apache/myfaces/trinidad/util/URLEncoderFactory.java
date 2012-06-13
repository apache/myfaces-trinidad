package org.apache.myfaces.trinidad.util;

import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

/**
 * This is a factory for the URLEncoder.  The URL encoder is a convenience class that
 * handles URL encoding in a container independant fashion.  The Portlet bridge spec
 * and later JSF specifications have tried to make URL encoding more specific, but
 * these technologies do not always address the needs of Trinidad URL encoding in
 * all circumstances.  This method aims to do just that.
 */
public class URLEncoderFactory
{
  public static URLEncoderFactory getFactory()
  {
    return _ENCODER;
  }
  
  public URLEncoder getURLEncoder()
  {
    FacesContext fc = FacesContext.getCurrentInstance();
    if(null == fc)
    {
      return getURLEncoder(null);
    }

    return getURLEncoder(fc.getExternalContext());
  }
  
  /**
   * Returns a URLEncoder for this request.
   * 
   * @return a URLEncoder for the given request
   * @throws IllegalStateException when there is not FacesContext available and
   *         a URLEncoder has not been manually set.  This can also be thrown if
   *         the URLEncoder has already been cleaned up at the end of the request.
   */
  public URLEncoder getURLEncoder(ExternalContext ec)
  {
    //even though we should wait until we have a faces context and throw an error
    //if we don't, go ahead and fudge it if the threadlocal is not null.  This just
    //means that a URLEncoder has already been set for this thread.
    URLEncoder enc = _local.get();
    
    if(null != enc)
    {
      return enc;
    }
    
    if(null == ec)
    {
      throw new IllegalStateException("An ExternalContext must be a available");
    }
    
    if(ExternalContextUtils.isPortlet(ec))
    {
      setURLEncoder(new PortletURLEncoder(ec));
    }
    else
    {
      setURLEncoder(new ExternalContextURLEncoder(ec));
    }
    
    return _local.get();
  }
  
  public void setURLEncoder(URLEncoder encoder)
  {
    _local.set(encoder);
  }
  
  private static final URLEncoderFactory _ENCODER = new URLEncoderFactory();
  
  //This threadlocal should get cleaned up when the request is done.  It's handled
  //by the configurators.
  private static final ThreadLocal<URLEncoder> _local = ThreadLocalUtils.newRequestThreadLocal();
  
}
