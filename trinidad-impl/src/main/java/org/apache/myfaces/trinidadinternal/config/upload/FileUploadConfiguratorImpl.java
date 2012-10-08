/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidadinternal.config.upload;

import java.io.IOException;
import java.io.InputStream;

import java.io.SequenceInputStream;

import java.lang.reflect.Proxy;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.faces.context.ExternalContext;

import javax.portlet.faces.annotation.ExcludeFromManagedRequestScope;

import javax.servlet.http.HttpServletRequest;

import org.apache.myfaces.trinidad.config.Configurator;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.model.ExtendedUploadedFile;
import org.apache.myfaces.trinidad.model.UploadedFile;
import org.apache.myfaces.trinidad.util.ClassLoaderUtils;
import org.apache.myfaces.trinidad.util.ExternalContextUtils;
import org.apache.myfaces.trinidad.util.RequestStateMap;
import org.apache.myfaces.trinidad.util.RequestType;
import org.apache.myfaces.trinidad.webapp.UploadedFileProcessor;
import org.apache.myfaces.trinidadinternal.share.util.MultipartFormHandler;
import org.apache.myfaces.trinidadinternal.share.util.MultipartFormItem;
import org.apache.myfaces.trinidadinternal.ui.marshal.MarshalingService;

/**
 * This configurator will handle the FileUploads for Trinidad.
 *
 * @version $Revision$ $Date$
 */
public class FileUploadConfiguratorImpl extends Configurator
{

  /**
   * Returns the added parameters
   *
   * @param externalContext
   * @return
   */
  static public Map<String, String[]> getAddedParameters(ExternalContext externalContext)
  {
    @SuppressWarnings("unchecked")
    Map<String, String[]> map = (Map<String, String[]>) externalContext.getRequestMap().get(_PARAMS);

    return map;
  }

  /**
   * Returns <code>true</code> if the request wrapper has been applied.
   *
   * @param context
   * @return
   */
  static public boolean isApplied(ExternalContext context)
  {
    return (RequestStateMap.getInstance(context).get(_APPLIED)!=null);
  }

  /**
   *
   */
  @SuppressWarnings("unchecked")
  static public void apply(ExternalContext context)
  {
    RequestStateMap.getInstance(context).put(_APPLIED, AppliedClass.APPLIED);
  }

  /* (non-Javadoc)
   * @see org.apache.myfaces.trinidad.config.Configurator#beginRequest(javax.faces.context.ExternalContext)
   */
  @Override
  @SuppressWarnings("unchecked")
  public void beginRequest(ExternalContext externalContext)
  {
    /*
     * Note: This class does not do a dispose on the file uploads.  The
     * reason for this is that in a portal environment, multiple render-requests
     * may depend on the same set of files being available to the view layer.
     * Instead the files will be automatically cleaned up when the portlet
     * generated the next request.  If we need to clean up sooner then we should
     * clean up on the end-request.
     */
    // FIXME AdamWiner We should clean up ASAP - these are potentially very
    // large allocations of memory and file, so cleaning up as soon
    // as possible is a good thing
    //Process MultipartForm if need be
    if (MultipartFormHandler.isMultipartRequest(externalContext) &&
       (externalContext.getRequest() instanceof HttpServletRequest || ExternalContextUtils.isPortlet(externalContext)))
    {
      try
      {
        final MultipartFormHandler mfh = new MultipartFormHandler(externalContext);
        RequestContext requestContext = RequestContext.getCurrentInstance(); 

        // TODO: How is this set?
        // AdamWiner: looks like the previous Trinidad incarnation
        // of this code didn't have any allowed configuration...
        mfh.setMaximumAllowedBytes(_maxAllowedBytes);
        mfh.setCharacterEncoding(ExternalContextUtils.getCharacterEncoding(externalContext));

        final HashMap<String, String[]> parameters = new HashMap<String, String[]>();
        MultipartFormItem item;
        UploadedFiles files = new UploadedFiles(externalContext);
        while ((item = mfh.getNextPart()) != null)
        {
          final String name = item.getName();
          String value = null;
          // No filename - it's not a file uploaded field
          if (item.getFilename() == null)
          {
            value = item.getValue();
            final Object oldValue = parameters.get(name);
            if (oldValue == null)
            {
              parameters.put(name, new String[]{value});
            }
            else
            {
              final String[] oldArray = (String[]) oldValue;
              final String[] newArray = new String[oldArray.length + 1];
              System.arraycopy(oldArray, 0, newArray, 1, oldArray.length);
              newArray[0] = value;
              parameters.put(name, newArray);
            }
          }
          else if (item.getFilename().length() > 0)
          {
            // Upload a file
            _doUploadFile(requestContext, externalContext, files, item, parameters);
          }
        }
        if (parameters.containsKey(_MULTIPLE_UPLOAD_PARAM))
        {
          // This is a multiple file upload request. We will only be processing one request at a time
          // from any particular window since multi file upload requests are queue on the client side
          // per window. So we do not need to implement extra locking logic.
          String uploadType = parameters.get(_MULTIPLE_UPLOAD_PARAM)[0];
          if (uploadType != null)
          {
            UploadedFiles windowFiles = UploadedFiles.getUploadedFilesForWindow(externalContext);
            if (uploadType.equals("multipleAdd"))
            {
              // Add files
              Map<String, List<UploadedFile>> uploadedMapFile = files.getUploadedFileMap();
              Iterator iterator = uploadedMapFile.keySet().iterator();
              while (iterator.hasNext())
              {
                // Add all the files in the request to the window
                String name = (String) iterator.next();
                List<UploadedFile> fileList = uploadedMapFile.get(name);
                for (UploadedFile file: fileList)
                {
                  windowFiles.__put(name, file);
                }
              }
              UploadedFiles.__saveUploadedFilesForWindow(windowFiles, externalContext, requestContext);
              uploadedMapFile.clear();
            }
            else if (uploadType.equals("multipleDelete"))
            {
              // Delete a file
              String itemName = parameters.get("itemName")[0];
              String fileName = parameters.get("fileName")[0];
              List<UploadedFile> uploadedFiles = windowFiles.getUploadedFileList(itemName);
              if (uploadedFiles != null)
              {
                for (UploadedFile uploadedFile: uploadedFiles)
                {
                  if (uploadedFile.getFilename().equals(fileName))
                  {
                    // Remove the file from the window, delete the tmp file from disk/memory
                    // and save the file map
                    uploadedFiles.remove(uploadedFile);
                    uploadedFile.dispose();
                    UploadedFiles.__saveUploadedFilesForWindow(windowFiles, externalContext, requestContext);
                    break;
                  }
                }
              }
            }
            else if (uploadType.equals("multipleAddChunk"))
            {
              // Add a chunk
              String itemName = parameters.get("itemName")[0];
              String fileName = externalContext.getRequestHeaderMap().get(_MULTIPLE_UPLOAD_CHUNK_FILENAME_PARAM);
              Long chunkNum = Long.parseLong(externalContext.getRequestHeaderMap().get(_MULTIPLE_UPLOAD_CHUNK_NUM_PARAM));
              Long chunkCount = Long.parseLong(externalContext.getRequestHeaderMap().get(_MULTIPLE_UPLOAD_CHUNK_COUNT_PARAM));
              UploadedFile file = files.getUploadedFile(itemName);
              Map<String, Object> windowMap = requestContext.getWindowMap();
              List<UploadedFile> chunkList = (List<UploadedFile>) windowMap.get(_UPLOADED_CHUNK_FILES_LIST_KEY);
              if (chunkList == null)
              {
                chunkList = new ArrayList<UploadedFile>();
                windowMap.put(_UPLOADED_CHUNK_FILES_LIST_KEY, chunkList);
              }
              chunkList.add(file);
              if (chunkNum == chunkCount - 1)
              {
                // if it's the last chunk then create a combined file
                UploadedFile combinedFile = new ChunkedUploadedFile(fileName, file.getContentType(), chunkList);
                windowFiles.__put(itemName, combinedFile);
                windowMap.remove(_UPLOADED_CHUNK_FILES_LIST_KEY);
                UploadedFiles.__saveUploadedFilesForWindow(windowFiles, externalContext, requestContext);
              }
              files.getUploadedFileMap().clear();
            }
          }
        }
        externalContext.getRequestMap().put(_PARAMS, parameters);
      }
      catch (Throwable t)
      {
        if(_LOG.isSevere())
        {
          _LOG.severe(t);
        }
      }
    }
  }

  /* (non-Javadoc)
   * @see org.apache.myfaces.trinidad.config.Configurator#getExternalContext(javax.faces.context.ExternalContext)
   */
  @Override
  public ExternalContext getExternalContext(ExternalContext externalContext)
  {
    //Wrap only if there are parameters present
    Map<String, String[]> addedParams = getAddedParameters(externalContext);

    if(addedParams != null)
    {
      return _getExternalContextWrapper(externalContext, addedParams);
    }

    return externalContext;
  }
  
  /* (non-Javadoc)
   * @see org.apache.myfaces.trinidad.config.Configurator#endRequest(javax.faces.context.ExternalContext)
   */
  @Override
  public void endRequest(ExternalContext externalContext)
  {
    // TODO matzew check portlet env.
    if(!ExternalContextUtils.isPortlet(externalContext))
    {
      UploadedFiles files = UploadedFiles.getUploadedFiles(externalContext);
      if(files != null)
        files.dispose();
    }
  }

  /* (non-Javadoc)
   * @see org.apache.myfaces.trinidad.config.Configurator#init(javax.faces.context.ExternalContext)
   */
  @Override
  public void init(ExternalContext externalContext)
  {
    super.init(externalContext);
    //TODO initialize _maxAllowedBytes
    
    // Get the marshaling service
    _marshalingService = _getMarshalingService();
  }

  private void _doUploadFile(
      final RequestContext   requestContext,
      final ExternalContext  externalContext,
      final UploadedFiles     files,
      final MultipartFormItem item,
      final Map<String, String[]> parameters) throws IOException
  {
    String filename = item.getFilename();
    Map<String, Object> properties = _getPropertiesMapFromParameters(parameters, item.getName(), filename);
    String chunkFilename = externalContext.getRequestHeaderMap().get(_MULTIPLE_UPLOAD_CHUNK_FILENAME_PARAM);
    if (chunkFilename != null)
    {
      // We store the filename in a special header when sending chunked data. The reason is that
      // browsers do not have an API for setting the filename on Blob objects. Also, append the chunk num
      // so it's easier to keep track of.
      String chunkNum = externalContext.getRequestHeaderMap().get(_MULTIPLE_UPLOAD_CHUNK_NUM_PARAM);
      chunkFilename = chunkFilename + ".part" + chunkNum;
      filename = chunkFilename;
    }
    final UploadedFile temp = new TempUploadedFile(filename, item, properties);
    Map<String, Object> sessionMap = externalContext.getSessionMap();
    Map<String, Object> requestMap = externalContext.getRequestMap();
    
    _copyParamsFromSessionToRequestMap(sessionMap, requestMap,
      UploadedFileProcessor.MAX_MEMORY_PARAM_NAME,
      UploadedFileProcessor.MAX_DISK_SPACE_PARAM_NAME,
      UploadedFileProcessor.TEMP_DIR_PARAM_NAME,
      UploadedFileProcessor.MAX_FILE_SIZE_PARAM_NAME,
      UploadedFileProcessor.MAX_CHUNK_SIZE_PARAM_NAME);
    
    final UploadedFile file =
      requestContext.getUploadedFileProcessor().processFile(externalContext.getRequest(), temp);

    if (file != null)
    {
      // Store the file.
      files.__put(item.getName(), file);

      if (_LOG.isFine())
      {
        _LOG.fine("Uploaded file " + file.getFilename() + "(" +
            file.getLength() + " bytes) for ID " + item.getName());
      }
    }
  }

  /**
   * copies some params (varargs) from the session map to the request map 
   */
  private void _copyParamsFromSessionToRequestMap(Map<String, Object> sessionMap, Map<String, Object> requestMap, String... params)
  {
    for(String param : params)
    {
      requestMap.put(param,  sessionMap.get(param));
    }
  }

  static private ExternalContext _getExternalContextWrapper(ExternalContext externalContext, Map<String, String[]> addedParams)
  {
    if(!isApplied(externalContext))
    {
      RequestType type = ExternalContextUtils.getRequestType(externalContext);
      
      switch(type)
      {
        case SERVLET:
          externalContext.setRequest(new UploadRequestWrapper(externalContext, addedParams));
          break;
        case RESOURCE:
          externalContext.setRequest(new UploadResourceRequest(externalContext, addedParams));
          break;
        case ACTION:
          //Portlet 2.0 should use the framework provided wrapper.  Portlet 1.0 needs to implement
          //the interface.  Because we need to compile against Portlet 2.0, we implement the Portlet
          //1.0 scenario using a Proxy.
          Object req;
          
          if(_ENHANCED_PORTLET_SUPPORTED)
          {
            req = _getActionRequestWrapper(externalContext, addedParams);
          }
          else
          {
            req = _getActionRequestProxy(externalContext, addedParams);
          }
          
          externalContext.setRequest(req);
      }
      apply(externalContext);        
    }

    //If we don't have any wrapped params or we have a render portal request,
    //return the origional external context
    return externalContext;
  }
  
  static private Object _getActionRequestProxy(ExternalContext ec, Map<String, String[]> params)
  {
    try
    {
      Class<?> actionRequestClass = ClassLoaderUtils.loadClass("javax.portlet.ActionRequest");
      return Proxy.newProxyInstance(ClassLoaderUtils.getContextClassLoader(), new Class<?>[]{actionRequestClass}, new UploadActionInvocationHandler(ec, params));
    }
    catch (ClassNotFoundException e)
    {
      throw new RuntimeException(e);
    }
  }
  
  static private Object _getActionRequestWrapper(ExternalContext ec, Map<String, String[]> params)
  {
    try
    {
      Class<?> wrapperClass = ClassLoaderUtils.loadClass("org.apache.myfaces.trinidadinternal.config.upload.UploadActionRequestWrapper");
      return wrapperClass.getConstructor(ExternalContext.class, Map.class).newInstance(ec, params);
    }
    catch (Exception e)
    {
      throw new RuntimeException(e);
    }
  }
  
  static private Map<String, Object> _getPropertiesMapFromParameters(Map<String, String[]> parameters, String name, String filename)
  {
    Map<String, Object> properties = new HashMap<String, Object>();
    StringBuilder propertiesParamBuilder = new StringBuilder(_MULTIPLE_UPLOAD_FILE_PROPERTY_PARAM);
    propertiesParamBuilder.append(":");
    propertiesParamBuilder.append(name);
    propertiesParamBuilder.append(":");
    propertiesParamBuilder.append(filename);
    String propertiesParam = propertiesParamBuilder.toString();
    if (parameters.get(propertiesParam) == null || parameters.get(propertiesParam).length == 0)
      return Collections.emptyMap();
    
    String marshaledProperties = parameters.get(propertiesParam)[0];

    if (marshaledProperties != null)
    {
      if (_marshalingService != null)
      {
        Object propertiesList = _marshalingService.unmarshal(marshaledProperties);
        if (propertiesList instanceof List)
        {
          for (int i = 0; i < ((List) propertiesList).size(); i = i + 2)
          {
            String propertyName = (String) ((List) propertiesList).get(i);
            Object propertyValue = ((List) propertiesList).get(i + 1);
            properties.put(propertyName, propertyValue);
          }
        }
        return properties;
      }
    }
    return Collections.emptyMap();
  }
  
  static private MarshalingService _getMarshalingService()
  {
    MarshalingService service = null;
    List<MarshalingService> marshalingServices = ClassLoaderUtils.getServices(MarshalingService.class.getName());
    if (marshalingServices != null && !marshalingServices.isEmpty())
    {
      service = marshalingServices.get(0);
    }
    return service;
  }
  
  //This will ensure the property is removed on the next request
  @ExcludeFromManagedRequestScope
  static private class AppliedClass
  {
    static public final AppliedClass APPLIED = new AppliedClass();
  }

  static private final class TempUploadedFile extends ExtendedUploadedFile
  {
    public TempUploadedFile(String filename, MultipartFormItem item, Map<String, Object> properties)
    {
      _filename = filename;
      _item = item;
      if (properties != null)
        _properties = properties;
      assert(properties == null);
      assert(item.getValue() == null);
    }

    public String getFilename()
    {
      return _filename != null ? _filename : _item.getFilename();
    }

    public String getContentType()
    {
      return _item.getContentType();
    }

    public long getLength()
    {
      // The length is not known yet.
      return -1L;
    }

    public Object getOpaqueData()
    {
      return null;
    }
    
    @Override
    public Map<String, Object> getProperties()
    {
      return _properties;
    }

    public InputStream getInputStream() throws IOException
    {
      return _item.getInputStream();
    }

    public void dispose()
    {
      throw new UnsupportedOperationException();
    }

    private MultipartFormItem _item;
    private String _filename = null;
    private Map<String, Object> _properties = new HashMap<String, Object>();
  }
  static private class ChunkedUploadedFile extends ExtendedUploadedFile
  {
    private List<UploadedFile> _uploadedFileChunkList = null;
    private String _filename = null;
    private String _contentType = null;
    
    static private final long serialVersionUID = 4534545227734904589L;
    
    public ChunkedUploadedFile(String filename, String contentType, List<UploadedFile> uploadedFileChunkList)
    {
      _filename = filename;
      _contentType = contentType;
      _uploadedFileChunkList = uploadedFileChunkList;
    }
    
    public String getFilename()
    {
      return _filename;
    }

    public String getContentType()
    {
      return _contentType;
    }
    
    public long getLength()
    {
      Long totalLength = 0L;
      for (UploadedFile file : _uploadedFileChunkList)
      {
        if (file.getLength() == -1L)
        {
          // there was an error so return -1
          return -1L;
        }
        totalLength = totalLength + file.getLength();
      }
      return totalLength;
    }

    public Object getOpaqueData()
    {
      for (UploadedFile file : _uploadedFileChunkList)
      {
        if (file.getLength() == -1L)
        {
          // there was an error so return the data
          return file.getOpaqueData();
        }
      }
      return null;
    }
    
    @Override
    public Map<String, Object> getProperties()
    {
      if (_uploadedFileChunkList.size() > 0)
      {
        UploadedFile file = _uploadedFileChunkList.get(0);
        if (file instanceof ExtendedUploadedFile)
          return ((ExtendedUploadedFile) file).getProperties();
      }
      return Collections.emptyMap();
    }

    public InputStream getInputStream() throws IOException
    {
      List<InputStream> inputSteamList = new ArrayList<InputStream>(_uploadedFileChunkList.size());
      for (UploadedFile uploadedFileChunk : _uploadedFileChunkList)
        inputSteamList.add(uploadedFileChunk.getInputStream());
      
      return new SequenceInputStream(Collections.enumeration(inputSteamList));
    }

    public void dispose()
    {
      for (UploadedFile uploadedFileChunk : _uploadedFileChunkList)
      {
        try
        {
          uploadedFileChunk.dispose();
        }
        catch (Exception e)
        {
          // Just keep trying to dispose of the rest of the chunks
        }
      }
    }
  }
  static private final String _APPLIED = FileUploadConfiguratorImpl.class.getName()+".APPLIED";
  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(FileUploadConfiguratorImpl.class);
  static private final String _PARAMS = FileUploadConfiguratorImpl.class.getName()+".PARAMS";
  static private final boolean _ENHANCED_PORTLET_SUPPORTED = ExternalContextUtils.isRequestTypeSupported(RequestType.RESOURCE);
  static private final String _MULTIPLE_UPLOAD_PARAM = "org.apache.myfaces.trinidad.UploadedFiles";
  static private final String _MULTIPLE_UPLOAD_FILE_PROPERTY_PARAM = "org.apache.myfaces.trinidad.UploadedFiles.FileProperty";
  static private final String _MULTIPLE_UPLOAD_CHUNK_NUM_PARAM = "X-Trinidad-chunkNum";
  static private final String _MULTIPLE_UPLOAD_CHUNK_COUNT_PARAM = "X-Trinidad-chunkCount";
  static private final String _MULTIPLE_UPLOAD_CHUNK_FILENAME_PARAM = "X-Trinidad-chunkFilename";
  static private final String _UPLOADED_CHUNK_FILES_LIST_KEY = "org.apache.myfaces.trinidadinternal.webapp.UploadedFiles.ChunkList";
  static private volatile MarshalingService _marshalingService = null;
  
  private long _maxAllowedBytes = 1L << 27;
}
