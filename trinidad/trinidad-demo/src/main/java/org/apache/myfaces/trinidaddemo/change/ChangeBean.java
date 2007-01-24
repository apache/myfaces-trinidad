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
package org.apache.myfaces.trinidaddemo.change;

import java.io.IOException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.myfaces.trinidad.change.AddChildComponentChange;
import org.apache.myfaces.trinidad.change.AddChildDocumentChange;
import org.apache.myfaces.trinidad.change.SetFacetChildComponentChange;
import org.apache.myfaces.trinidad.change.AttributeComponentChange;
import org.apache.myfaces.trinidad.change.ChangeManager;
import org.apache.myfaces.trinidad.change.ComponentChange;
import org.apache.myfaces.trinidad.change.DocumentChange;
import org.apache.myfaces.trinidad.change.RemoveChildComponentChange;
import org.apache.myfaces.trinidad.change.RemoveFacetComponentChange;
import org.apache.myfaces.trinidad.change.ReorderChildrenComponentChange;
import org.apache.myfaces.trinidad.component.core.nav.CoreCommandButton;

import org.apache.myfaces.trinidad.component.core.output.CoreImage;
import org.apache.myfaces.trinidad.component.core.output.CoreOutputFormatted;
import org.apache.myfaces.trinidad.context.RequestContext;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;

import org.xml.sax.SAXException;
import java.io.ByteArrayInputStream;

/**
 * Managed bean for Change persistence demos.
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-demo/src/main/java/oracle/adfdemo/view/faces/change/ChangeBean.java#1 $) $Date: 16-aug-2005.15:12:29 $
 */
public class ChangeBean 
{
  /**
   * Picks up an image randomly from a list and changes it on the image.
   */
  @SuppressWarnings("unchecked")
  public void modifyObjectImage(ActionEvent event)
  {
    UIComponent uic = event.getComponent().findComponent("oi1");
    String source = "/components/images/" + _images[_getRandIndex()];
    uic.getAttributes().put("source", source);
    _addAttributeChange(uic, "source", source);
  }
  
  /**
   * Picks up a string randomly from a list and changes the text attribute value
   *  of the panelBox.
   */
  @SuppressWarnings("unchecked")
  public void modifyPanelBox(ActionEvent event)
  {
    UIComponent uic = event.getComponent().findComponent("pb1");
    String text = _texts[_getRandIndex()];
    uic.getAttributes().put("text", text);
    _addAttributeChange(uic, "text", text);
  }

  /**
   * Modifies the sortable property of the column.
   */
  @SuppressWarnings("unchecked")
  public void modifyColumn(ActionEvent event)
  {
    //=-=pu: 'uic1' gets null, while 'uic' gets valid component, maybe a bug ?.
    //UIComponent uic1 = event.getComponent().findComponent("c1");
    UIComponent uic2 = event.getComponent().findComponent("t1");
    UIComponent uic = uic2.findComponent("c1");
    
    Object sortableAttrib = uic.getAttributes().get("sortable");
    Boolean isSortable = 
      (sortableAttrib == null)? Boolean.TRUE:(Boolean)sortableAttrib;
    Boolean newSortableValue = 
      Boolean.TRUE.equals(isSortable)? Boolean.FALSE:Boolean.TRUE;
    uic.getAttributes().put("sortable", newSortableValue);
    _addAttributeChange(uic, "sortable", newSortableValue);
  }

  /**
   * Picks up a string randomly from a list and changes the label attribute 
   *  value of the inputText.
   */
  @SuppressWarnings("unchecked")
  public void modifyInputText(ActionEvent event)
  {
    UIComponent uic = event.getComponent().findComponent("it1");
    String label = _labels[_getRandIndex()];
    uic.getAttributes().put("label", label);
    _addAttributeChange(uic, "label", label);
  }

  /**
   * Appends an image child to the panelGroup in the underlying JSP document
   */
  public void appendChildToDocument(ActionEvent event)
  {
    UIComponent eventSource = event.getComponent();
    UIComponent uic = eventSource.findComponent("pg1");
    
    // only allow the image to be added once
    if (_findChildById(uic,"oi3") != null)
      return;
      
    FacesContext fc = FacesContext.getCurrentInstance();

    DocumentFragment imageFragment = _createDocumentFragment(_IMAGE_MARK_UP);
    
    if (imageFragment != null)
    {
      DocumentChange change = new AddChildDocumentChange(imageFragment);
      
      ChangeManager apm = RequestContext.getCurrentInstance().getChangeManager();
      
      apm.addDocumentChange(fc, uic, change);
    }
  }

  /**
   * Appends an image child to the panelGroup.
   */
  @SuppressWarnings("unchecked")
  public void appendChild(ActionEvent event)
  {
    UIComponent eventSource = event.getComponent();
    UIComponent uic = eventSource.findComponent("pg1");
    if (_findChildById(uic,"oi2") != null)
      return;
    FacesContext fc = FacesContext.getCurrentInstance();
    
    CoreImage newChild = 
      (CoreImage) fc.getApplication().createComponent(
        "org.apache.myfaces.trinidad.CoreImage");
    newChild.setId("oi2");
    newChild.setInlineStyle("height: 100px, width: 120px");
    newChild.setSource(
      "http://homepage.mac.com/awiner/.Pictures/WindyHill/PaleSwallowtail.jpg");  
    uic.getChildren().add(newChild);

    ComponentChange aca = new AddChildComponentChange(newChild);

    ChangeManager apm = RequestContext.getCurrentInstance().getChangeManager();
    apm.addComponentChange(fc, uic, aca);
  }

  /**
   * Adds a 'brandingAppContextual' facet  to the panelGroup.
   */
  @SuppressWarnings("unchecked")
  public void addFacet(ActionEvent event)
  {
    UIComponent eventSource = event.getComponent();
    UIComponent uic = eventSource.findComponent("pp1");
    FacesContext fc = FacesContext.getCurrentInstance();
    CoreOutputFormatted newFacetComponent = 
      (CoreOutputFormatted) fc.getApplication().createComponent(
        "org.apache.myfaces.trinidad.CoreOutputFormatted");
    newFacetComponent.setStyleUsage("inContextBranding" );
    newFacetComponent.setValue(
      "Customer Company - Menlo Park");
    uic.getFacets().put("brandingAppContextual", newFacetComponent);

    ComponentChange afa = new SetFacetChildComponentChange("brandingAppContextual", newFacetComponent);

    ChangeManager apm = RequestContext.getCurrentInstance().getChangeManager();
    apm.addComponentChange(fc, uic, afa);
  }

  /**
   * Reverses the order of children of the panelGroup.
   */
  @SuppressWarnings("unchecked")
  public void reorderChildren(ActionEvent event)
  {
    UIComponent uic = event.getComponent().findComponent("pg1");
    int numChildren = uic.getChildCount();
    if (numChildren == 0)
      return;
    List<UIComponent> children = uic.getChildren();
    Collections.reverse(children);
    List<String> reorderedChildIdList = new ArrayList<String>();
    for(UIComponent child : children)
    {
      reorderedChildIdList.add(child.getId());
    }
    
    ComponentChange ra = new ReorderChildrenComponentChange(reorderedChildIdList);

    FacesContext fc = FacesContext.getCurrentInstance();
    ChangeManager apm = RequestContext.getCurrentInstance().getChangeManager();
    apm.addComponentChange(fc, uic, ra);
  }

  /**
   * Removes a pair of children, based on some characteristic of the
   *  event source.
   */
  public void removeChildren(ActionEvent event)
  {
    UIComponent eventSource = event.getComponent();
    UIComponent uic = eventSource.findComponent("pg1");
    int numChildren = uic.getChildCount();
    if (numChildren == 0)
      return;
    String eventSourceId = eventSource.getId();    
    if (eventSourceId.equals("cb2"))
    {
      _removeChild(uic, "sic1");
      _removeChild(uic, "cc1");
    }
    else if (eventSourceId.equals("cb3"))
    {
      _removeChild(uic, "cd1");
      _removeChild(uic, "sid1");
    }
  }

  /**
   * Removes one or more facets, based on some characteristic of the
   *  event source.
   */
  @SuppressWarnings("unchecked")
  public void removeFacets(ActionEvent event)
  {
    CoreCommandButton eventSource = (CoreCommandButton) event.getComponent();
    //pu: Anything until ":" in the button text represents the facet name/s
    String facetNameFromButtonText = (eventSource.getText().split(":"))[0];
    //pu: In case of the button that removes multiple facets, this is again 
    //  delimited by "_"
    String removableFacetNames[] = facetNameFromButtonText.split("_");
    
    //pu: Get the CorePanelPage components that has all the removable facets
    UIComponent uic = eventSource.findComponent("pp1");
    Map<String, UIComponent> facets = uic.getFacets();
    if (facets.keySet().size() == 0)
      return;

    for (int i=0; i<removableFacetNames.length; i++)
    {
      if (facets.get(removableFacetNames[i]) != null)
      {
        facets.remove(removableFacetNames[i]);
        ComponentChange rfa = new RemoveFacetComponentChange(removableFacetNames[i]);
        FacesContext fc = FacesContext.getCurrentInstance();
        ChangeManager apm = RequestContext.getCurrentInstance().getChangeManager();
        apm.addComponentChange(fc, uic, rfa);
      }
    }
  }

  /**
   * Creates a DocumentFragment containing the parsed content
   * @param markUp JSP Document markup
   * @return DocumentFragment containing the parsed content
   */
  private static DocumentFragment _createDocumentFragment(
    String markUp)
  {
    // prepend XML declaration
    markUp = "<?xml version = '1.0' encoding = 'ISO-8859-1'?>" + markUp;

    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware(true);
    factory.setValidating(false);

    DocumentBuilder builder;
    
    try
    {
      builder = factory.newDocumentBuilder();
    }
    catch (ParserConfigurationException pce)
    {
      _LOG.log(Level.WARNING, "Unable to get XML Parser:", pce);
      
      return null;
    }
    
    try
    {
      // use a version explicitly with ISO-8859-1 instead
      byte[] markupBytes = markUp.getBytes();
      Document newDoc = builder.parse(new ByteArrayInputStream(markupBytes));
    
      DocumentFragment fragment = newDoc.createDocumentFragment();
      
      // add the document's root element to the fragment
      fragment.appendChild(newDoc.getDocumentElement());
      
      return fragment;
    }
    catch (SAXException se)
    {      
      _LOG.log(Level.WARNING, "Unable to parse markup:" + markUp, se);
      
      return null;
    }
    catch (IOException ioe)
    {
      _LOG.log(Level.WARNING, "IO Problem with markup:" + markUp, ioe);

      return null;
    }
  }

  @SuppressWarnings("unchecked")
  private static void _removeChild(UIComponent uic, String removableChildId)
  {
    UIComponent removableChild = _findChildById(uic, removableChildId);
    if (removableChild != null)
    {
      List<UIComponent> children = uic.getChildren();
      children.remove(removableChild);
      ComponentChange rca = new RemoveChildComponentChange(removableChildId);
      FacesContext fc = FacesContext.getCurrentInstance();
      ChangeManager apm = RequestContext.getCurrentInstance().getChangeManager();
      apm.addComponentChange(fc, uic, rca);
    }
  }
  
  @SuppressWarnings("unchecked")
  private static UIComponent _findChildById(UIComponent uic, String id)
  {
    int numChildren = uic.getChildCount();
    if (numChildren == 0)
      return null;
    List<UIComponent> children = uic.getChildren();
    UIComponent child = null;
    for (int i=0; i<numChildren; i++)
    {
      child = children.get(i);
      if (id.equals(child.getId()))
        return child;
    }
    return null;
  }

  private static void _addAttributeChange(
    UIComponent uic, 
    String attribName, 
    Object attribValue
    )
  {
    FacesContext fc = FacesContext.getCurrentInstance();
    ChangeManager apm = RequestContext.getCurrentInstance().getChangeManager();
    ComponentChange aa = new AttributeComponentChange(attribName, attribValue);
    apm.addComponentChange(fc, uic, aa);
  }
  
  private static int _getRandIndex()
  {
    return (int) (Math.random()*10)/2;
  }
  
  private static final String _images[] = 
    {"cobrand.gif","corporateBrand.gif","largeAd.gif","mediumAd.gif","new.gif"};
  private static final String _labels[] = 
    {"Label One","Label Two","Label Three","Label Four","Label Five"};
  private static final String _texts[] = 
    {"PanelBoxText One",
     "PanelBoxText Two",
     "PanelBoxText Three",
     "PanelBoxText Four",
     "PanelBoxText Five"};

  // markup to use for image added to document
  private static final String _IMAGE_MARK_UP = 
   "<tr:Image id='oi3' inlineStyle='height: 100px; width: 120px;' " +
   "source='http://homepage.mac.com/awiner/.Pictures/WindyHill/PaleSwallowtail.jpg' " +
   "xmlns:af='http://myfaces.apache.org/adf/faces/EA17'/>";

  static private final Logger _LOG = Logger.getLogger(ChangeBean.class.getName());
}
