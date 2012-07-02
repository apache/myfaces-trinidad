package org.apache.myfaces.trinidad.util;

import java.net.URI;

import javax.faces.context.ExternalContext;

/**
 *
 */
class PortletURLEncoder extends ExternalContextURLEncoder
{
  public PortletURLEncoder(ExternalContext ec)
  {
    super(ec);
  }

  @Override
  public String encodeInProtocolResourceURL(String url)
  {
    return super.encodeInProtocolResourceURL(_addParam(url, "javax.portlet.faces.InProtocolResourceLink", "true"));
  }

  @Override
  public String encodePartialActionURL(String url)
  {
    //Portlet AJAX requests are encoded as InProtocolResource urls
    return encodeInProtocolResourceURL(url);
  }
  
  //Adds or replaces a parameter value
  private String _addParam(String url, String name, String value)
  {
    int queryPos = url.indexOf('?');
    int fragmentPos = url.lastIndexOf('#');
    name += "="; //Add the equals sign to this, every use of it below needs the "=" sign.
    
    StringBuilder sb = new StringBuilder();
    
    
    if(queryPos == -1)
    {
      sb.append((fragmentPos == -1)?url:url.substring(0, fragmentPos))
        .append("?")
        .append(name)
        .append(value);
      
    }
    else
    {
      sb.append(url.substring(0, ++queryPos)); //We must include the "?"
      String queryString = (fragmentPos == -1)?url.substring(queryPos):url.substring(queryPos, fragmentPos);
      
      //Search the queryString to see if the param already exists.  If it does, change it.
      if(queryString.indexOf(name) != -1)
      {
        //We have this query param already.  Replace all occurances of it with the new name/value
        queryString = queryString.replaceAll(name + "[\\w+.~*$\'()*\\-+;,?/]*", name + value);
      }
      else
      {
        queryString += "&" + name + value;
      }
      
      sb.append(queryString);
    }
    
    //Append the fragment if there is one
    if(fragmentPos >= 0)
    {
      sb.append(url.substring(fragmentPos));
    }
    
    return sb.toString();
  }
}

