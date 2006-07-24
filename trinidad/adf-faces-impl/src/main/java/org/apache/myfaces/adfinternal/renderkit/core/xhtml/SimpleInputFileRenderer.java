/*
 * Copyright  2005,2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.renderkit.core.xhtml;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.adf.bean.FacesBean;

import org.apache.myfaces.adf.component.core.input.CoreInputFile;

import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.adfinternal.webapp.UploadedFiles;

/**
 */
public class SimpleInputFileRenderer extends SimpleInputTextRenderer
{
  public SimpleInputFileRenderer()
  {
    super(CoreInputFile.TYPE);
  }

  //
  // DECODE BEHAVIOR
  // 
  public Object getSubmittedValue(
    FacesContext context,
    UIComponent  component)
  {
    Object result = null;

    UploadedFiles files = UploadedFiles.getUploadedFiles(context);
    if (files != null)
    {
      String clientId = component.getClientId(context);
      result = files.getUploadedFile(clientId);
    }

    // If we couldn't find a file, return "FALSE" to indicate that
    // the file upload *was* available, but didn't upload anything
    // this time.
    if (result == null)
      result = Boolean.FALSE;

    return result;
  }

  public Object getConvertedValue(
    FacesContext context,
    UIComponent  component,
    Object       submittedValue)
  {
    // Convert FALSE back into null
    if (submittedValue == Boolean.FALSE)
      return null;

    return submittedValue;
  }


  //
  // ENCODE BEHAVIOR
  // 

  /**
   * <inputFile> cannot show a value.
   */
  protected String getConvertedString(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean)
  {
    return null;
  }

  protected String getDefaultInputType()
  {
    return "file";
  }


  //
  // Overrides disabling all the things you can't do on
  // an inputFile
  // 

  public boolean isTextArea(
    FacesBean bean)
  {
    return false;
  }

  protected boolean isAutoSubmit(
    FacesBean bean)
  {
    return false;
  }

  protected boolean getSecret(FacesBean bean)
  {
    return false;
  }

  protected Number getMaximumLength(FacesBean bean)
  {
    return null;
  }


  protected boolean getReadOnly(FacesContext context, FacesBean bean)
  {
    return false;
  }
  
  protected String getRootStyleClass(FacesBean bean)  
  {
    return "af|inputFile";
  }

  protected String getContentStyleClass(FacesBean bean)
  {
    return "af|inputFile::content";
  }

  protected Integer getDefaultColumns(AdfRenderingContext arc, FacesBean bean)
  {
    return null;
  }
}
