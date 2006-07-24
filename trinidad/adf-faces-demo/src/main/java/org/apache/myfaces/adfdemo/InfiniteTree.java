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
import java.io.Serializable;
import java.util.AbstractList;
import java.util.List;
import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;

public class InfiniteTree extends AbstractList implements Serializable
{
  public InfiniteTree(String id)
  {
    _id = id;
  }

  public InfiniteTree()
  {
    this("");
  }

  public int size()
  {
    return _GENES.length;
  }

  public Object get(int index)
  {
    return new Bean(index);
  }

  // must be public for bean introspection:
  public final class Bean
  {
    public Bean(int index)
    {
      _index = index;
    }

    public String action()
    {
      FacesContext context = FacesContext.getCurrentInstance();
      FacesMessage message = new FacesMessage("Clicked on Gene " + getLabel());
      context.addMessage(null, message);
      return null;
    }

    public String getLabel()
    {
      char ch = _GENES[_index];
      return _id + ch;
    }

    public String getParentLabel()
    {
      return _id;
    }

    public int getIndex()
    {
      return _index;
    }

    public int getDepth()
    {
      return getLabel().length();
    }

    public List getKids()
    {
      return new InfiniteTree(getLabel());
    }

    private final int _index;
  }

  private final String _id;
  private static final char[] _GENES = {'A', 'C', 'G', 'T'};
}
