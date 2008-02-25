/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidaddemo;

import java.io.Serializable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.StringTokenizer;

import javax.faces.component.UIComponent;

import org.apache.myfaces.trinidad.component.UIXTree;
import org.apache.myfaces.trinidad.model.RowKeySetTreeImpl;

/**
 * Creates a tree of components from a flat list of jspx files in a folder.
 */
public class ComponentTree 
{
  // public no-arg constructor needed for managed-bean.
  public ComponentTree()
  {
  }
  
  public List<Bean> getComponents()
  {
    if (_components == null)
    {
      Node root = new Node();
      List<String> files = getFileList();
      for(int i=0; i<files.size(); i++)
      {
        String file = files.get(i);
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
  
  public List<String> getFileList()
  {
    return _fileList;
  }

  public void setFileList(List<String> files)
  {
    _fileList = files;
  }

  public UIComponent getTree()
  {
    // this bean's lifecycle is application. this method is called
    // per user, so don't change any state on this bean.
    return null;
  }
  
  @SuppressWarnings("unchecked")
  public void setTree(UIComponent treeComp)
  {
    // this bean's lifecycle is application. this method is called
    // per user, so don't change any state on this bean.
    UIXTree tree = (UIXTree) treeComp;
    Map<String, Object> attrs = tree.getAttributes();
    final String _key = "org.apache.myfaces.trinidaddemo.vuew.faces.ComponentTree";
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
    Map<String, Node> parentMap = parent.kids;
    if (parentMap == null)
    {
      parentMap = new HashMap<String, Node>(3);
      parent.kids = parentMap;
    }
  
    Node childNode = parentMap.get(token);
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
  
  private static List<Bean> _createChildList(Node root, String label)
  {
    Map<String, Node> kids = root.kids;
    if (kids == null)
    {
      return null;
    }
    else
    {
      List<Bean> kidList = new ArrayList<Bean>(kids.size());
      for(Entry<String, Node> entry : kids.entrySet())
      {
        kidList.add(new Bean(label + entry.getKey(), entry.getValue()));
      }
      
      Collections.sort(kidList);
      return kidList;
    }
  }
  
  // must be public for introspection;  must be serializable
  // for storing on the session.
  public static final class Bean implements Comparable<Bean>, Serializable
  {
    // No-arg constructor just for serialization
    public Bean()
    {
    }

    public Bean(String label, Node node)
    {
      String fn = node.filename;
      List<Bean> kids = _createChildList(node, label);
      // if this bean has no demo, and it has only one child, then pull
      // the child up to this bean's level:
      if ((fn==null) && (kids != null) && (kids.size() == 1))
      {
        Bean child = kids.get(0);
        fn = child.getFilename();
        _token = child.getLabel();
        kids = kids.get(0).getComponents();
      }
      else
      {
        _token = label;
      }

      _filename = fn;
      _kids = kids;
    }

    public int compareTo(Bean obj)
    {
      return getLabel().compareTo(obj.getLabel());
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
    
    public List<Bean> getComponents()
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
    private List<Bean> _kids;
  }
  
  private static final class Node
  {
    public String filename = null;
    public Map<String, Node> kids = null;
  }
   
  private List<Bean> _components = null;
  private List<String> _fileList = null; 
}
