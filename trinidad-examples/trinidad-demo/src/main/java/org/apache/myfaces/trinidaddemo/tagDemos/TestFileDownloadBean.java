/** Copyright (c) Oracle Corporation 2008. All rights reserved */
package org.apache.myfaces.trinidaddemo.tagDemos;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;

import javax.faces.context.FacesContext;


/**
 * Bean class used to demo the fileDownloadActionListener tag
 */
public class TestFileDownloadBean
{
  public TestFileDownloadBean()
  {
  }


  public void sendHelloFile(FacesContext context, 
                            OutputStream outputStream) throws IOException
  {
    Writer out = new OutputStreamWriter(outputStream, "UTF-8");
    out.write("Hi there!");
    out.close();
  }
  
  public void errorHelloFile(FacesContext context, 
                            OutputStream outputStream) throws IOException
  {
    throw new IOException("Error occurred");
  }


}
