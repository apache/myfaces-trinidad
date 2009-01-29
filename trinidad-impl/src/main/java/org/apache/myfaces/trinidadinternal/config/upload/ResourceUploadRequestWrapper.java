package org.apache.myfaces.trinidadinternal.config.upload;

import java.io.UnsupportedEncodingException;

import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

import javax.faces.context.ExternalContext;

import javax.portlet.ResourceRequest;
import javax.portlet.ResourceResponse;
import javax.portlet.filter.ResourceRequestWrapper;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidadinternal.share.util.CaboHttpUtils;

public class ResourceUploadRequestWrapper
  extends ResourceRequestWrapper
{
  public ResourceUploadRequestWrapper(ExternalContext ec,
                                      Map<String, String[]> params)
  {
    super((ResourceRequest) ec.getRequest());
    _response = (ResourceResponse) ec.getResponse();

    @SuppressWarnings("unchecked")
    Map<String, String[]> origionalMap = super.getParameterMap();

    _extractedParams = new HashMap<String, String[]>(origionalMap);
    _extractedParams.putAll(params);

    _encoding = super.getCharacterEncoding();
  }

  /**
   * Hide the content type so that no one tries to re-download the
   * uploaded files.
   */
  @Override
  public String getContentType()
  {
    return _WWW_FORM_URLENCODED_TYPE;
  }

  @Override
  public String getCharacterEncoding()
  {
    return _encoding;
  }


  /**
   * Trap calls to setCharacterEncoding() to decode parameters correctly
   */
  @Override
  public void setCharacterEncoding(String encoding)
    throws UnsupportedEncodingException
  {
    // If the encoding is already right, we can bail
    if (encoding.equals(_encoding))
      return;

    // Don't call super.setCharacterEncoding() - it's too late
    // and we'll get a warning
    _encoding = encoding;
    if (_LOG.isFine())
      _LOG.fine("Switching encoding of wrapper to " + encoding);

    _extractedAndDecodedParams =
        new HashMap<String, String[]>(_extractedParams.size());

    byte[] buffer = new byte[256];

    for (Map.Entry<String, String[]> entry: _extractedParams.entrySet())
    {
      String key = entry.getKey();
      key = CaboHttpUtils.decodeRequestParameter(key, encoding, buffer);

      String[] oldValue = entry.getValue();
      int length = oldValue.length;
      String[] newValue = new String[length];
      for (int i = 0; i < length; i++)
      {
        newValue[i] =
            CaboHttpUtils.decodeRequestParameter(oldValue[i], encoding,
                                                 buffer);
        if (_LOG.isFinest())
          _LOG.finest("Parameter " + key + ":" + newValue[i]);
      }

      _extractedAndDecodedParams.put(key, newValue);
    }

    // Let the UploadedFiles know, so it can fix up filenames
    UploadedFiles.setCharacterEncoding(this, encoding);
  }

  @Override
  public String getParameter(String param)
  {
    String[] value = _getParameterValues(param);
    if (value == null)
      return null;

    return value[0];
  }

  @Override
  public Map<String, String[]> getParameterMap()
  {
    Map<String, String[]> map = _getMap();
    return Collections.unmodifiableMap(map);
  }

  @Override
  public Enumeration<String> getParameterNames()
  {
    return Collections.enumeration(_getMap().keySet());
  }

  @Override
  public String[] getParameterValues(String param)
  {
    String[] value = _getParameterValues(param);
    if (value == null)
      return null;

    return (String[]) value.clone();
  }

  private String[] _getParameterValues(String param)
  {
    return _getMap().get(param);
  }

  /**
   * Get the correct map of parameters whether or not setCharacterEncoding()
   * was called.
   */
  private Map<String, String[]> _getMap()
  {
    if (_extractedAndDecodedParams != null)
      return _extractedAndDecodedParams;

    return _extractedParams;
  }

  private Map<String, String[]> _extractedAndDecodedParams;
  private Map<String, String[]> _extractedParams;
  private ResourceResponse _response;
  private String _encoding;
  private static final String _WWW_FORM_URLENCODED_TYPE =
    "application/x-www-form-urlencoded";
  private static final TrinidadLogger _LOG =
    TrinidadLogger.createTrinidadLogger(ResourceUploadRequestWrapper.class);
}
