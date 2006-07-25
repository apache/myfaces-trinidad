/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.adfdemo;
import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;

import org.apache.myfaces.adf.context.AdfFacesContext;

public class DataBean implements java.io.Serializable
{
  public DataBean()
  {
    // This isn't thread-safe.  I don't care. :)
    _int = _sCount++;
    _string = "String " + _int;
    _boolean = ((_int % 2) == 0);
  }

  public String action()
  {
    FacesContext context = FacesContext.getCurrentInstance();
    FacesMessage message = new FacesMessage("CLICKED ON ROW " + _int + ", " +
                                            _string);
    context.addMessage(null, message);
    return null;
  }

  public String showDetail()
  {
    AdfFacesContext.getCurrentInstance().
      getPageFlowScope().put("detail", this);
    return "showDetail";
  }

  public boolean getBoolean()
  {
    return _boolean;
  }

  public void setBoolean(boolean aBoolean)
  {
    _boolean = aBoolean;
  }


  public int getInt()
  {
    return _int;
  }

  public void setInt(int anInt)
  {
    _int = anInt;
  }

  public String getString()
  {
    return _string;
  }

  public void setString(String aString)
  {
    _string = aString;
  }

  private int _int;
  private boolean  _boolean;
  private String _string;

  static private int _sCount = 0;
}
