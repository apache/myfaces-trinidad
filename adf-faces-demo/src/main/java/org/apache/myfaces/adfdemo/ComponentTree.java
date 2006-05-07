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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.StringTokenizer;

import javax.faces.component.UIComponent;

import org.apache.myfaces.adf.component.UIXTree;
import org.apache.myfaces.adf.model.RowKeySetTreeImpl;

/**
 * Creates a tree of components from a flat list of jspx files in a folder.
 * @author Arjuna Wijeyekoon
 */
public class ComponentTree 
{
  // public no-arg constructor needed for managed-bean.
  public ComponentTree()
  {
  }
  
  public List getComponents()
  {
    if (_components == null)
    {
      Node root = new Node();
      List files = getFileList();
      for(int i=0; i<files.size(); i++)
      {
        String file = (String) files.get(i);
        try
        {
          _processFile(root, file);
        }
        catch(RuntimeException e)
        {
          throw new RuntimeException("error with filename:"+file, e);
        }
      }
      setFileList(null);
      _components = _createChildList(root, "");
    }
    return _components;
  }
  
  public List getFileList()
  {
    return _fileList;
  }

  public void setFileList(List files)
  {
    _fileList = files;
  }

  public UIComponent getTree()
  {
    // this bean's lifecycle is application. this method is called
    // per user, so don't change any state on this bean.
    return null;
  }
  
  public void setTree(UIComponent treeComp)
  {
    // this bean's lifecycle is application. this method is called
    // per user, so don't change any state on this bean.
    UIXTree tree = (UIXTree) treeComp;
    Map attrs = tree.getAttributes();
    final String _key = "org.apache.myfaces.adfdemo.vuew.faces.ComponentTree";
    // has this tree been initialized?
    if (attrs.get(_key) == null)
    {
      // tree has not been initialized.
      attrs.put(_key, Boolean.TRUE); // mark as initialized
      RowKeySetTreeImpl state = new RowKeySetTreeImpl(true);
      tree.setDisclosedRowKeys(state);
    }
  }

//  private static File _getFile(String path)
//  {
//    if (path == null)
//      throw new NullPointerException("path");
//    FacesContext fContext = FacesContext.getCurrentInstance();
//    ServletContext sContext = 
//      (ServletContext) fContext.getExternalContext().getContext();
//    String pathString = sContext.getRealPath(path);
//    return new File(pathString);
//  }
  
  private static void _processFile(Node node, String filename)
  {
    StringTokenizer tokens = _getTokens(filename);
    while(tokens.hasMoreTokens())
    {
      String token = tokens.nextToken();
      // check to see if the token is a delimiter. Delimiters are a single char:
      if (token.length() == 1)
      {
        char ch = token.charAt(0);
        if (ch == '.')
        {
          node.filename = filename;
          break;
        }
        String next = tokens.nextToken();
        token += next;
      }
      node = _procureChildNode(node, token);
    }
  }
  
  // Create or get a child node, under the specified parent node.
  // the specified token string is used as the key to store the child
  // under.
  private static Node _procureChildNode(Node parent, String token)
  {
    Map parentMap = parent.kids;
    if (parentMap == null)
    {
      parentMap = new HashMap(3);
      parent.kids = parentMap;
    }
  
    Node childNode = (Node) parentMap.get(token);
    if (childNode == null)
    {
      childNode = new Node();
      parentMap.put(token, childNode);
    }
    return childNode;
  }
  
  private static StringTokenizer _getTokens(String filename)
  {
    StringTokenizer tokens = 
      new StringTokenizer(filename, "ABCDEFGHIJKLMNOPQRSTUVWXYZ._", true);
    return tokens;
  }
  
  private static List _createChildList(Node root, String label)
  {
    Map kids = root.kids;
    if (kids == null)
    {
      return null;
    }
    else
    {
      List kidList = new ArrayList(kids.size());
      Iterator entries = kids.entrySet().iterator();
      while(entries.hasNext())
      {
        
        Entry e = (Entry) entries.next();
        String token = (String) e.getKey();
        Node child = (Node) e.getValue();
        kidList.add(new Bean(label + token, child));
      }
      Collections.sort(kidList);
      return kidList;
    }
  }
  
  // must be public for introspection;  must be serializable
  // for storing on the session.
  public static final class Bean implements Comparable, Serializable
  {
    // No-arg constructor just for serialization
    public Bean()
    {
    }

    public Bean(String label, Node node)
    {
      String fn = node.filename;
      List kids = _createChildList(node, label);
      // if this bean has no demo, and it has only one child, then pull
      // the child up to this bean's level:
      if ((fn==null) && (kids != null) && (kids.size() == 1))
      {
        Bean child = (Bean) kids.get(0);
        fn = child.getFilename();
        _token = child.getLabel();
        kids = null;
      }
      else
      {
        _token = label;
      }

      _filename = fn;
      _kids = kids;
    }

    public int compareTo(Object obj)
    {
      Bean other = (Bean) obj;
      return getLabel().compareTo(other.getLabel());
    }

    /**
     * Some demos are not components. They are additional demos of
     * other components. EG: table_selection.jspx
     */
    public boolean isExample()
    {
      if (_filename == null)      
        return false;
      return (_filename.indexOf('_') > 0);
    }
    
    public String getFilename()
    {
      return _filename;
    }
    
    public String getViewId()
    {
      return "/components/" +_filename;
    }

    public String getLabel()
    {
      return _token;
    }
    
    public List getComponents()
    {
      return _kids;
    }
    
    public String view()
    {
      if (_filename == null)
        return null;
        
      String action = _filename.substring(0, _filename.lastIndexOf('.'));
      return "guide."+action;
    }
    
    private String _filename;
    private String _token;
    private List _kids;
  }
  
  private static final class Node
  {
    public String filename = null;
    public Map kids = null;
  }
   
  private List _components = null;
  private List _fileList = null; 
}
