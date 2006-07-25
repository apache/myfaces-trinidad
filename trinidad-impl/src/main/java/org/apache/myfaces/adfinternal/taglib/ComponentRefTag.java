/*
 * Copyright  2004-2006 The Apache Software Foundation.
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

package org.apache.myfaces.adfinternal.taglib;

import java.io.IOException;

import java.io.Serializable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;

import javax.servlet.ServletException;
import javax.servlet.jsp.JspException;

import org.apache.myfaces.adf.logging.ADFLogger;
import org.apache.myfaces.adf.component.UIXComponentRef;

import org.apache.myfaces.adfinternal.metadata.RegionMetadata;
import org.apache.myfaces.adfinternal.metadata.RegionMetadata.AttributeMetaData;
import org.apache.myfaces.adfinternal.metadata.RegionMetadata.ComponentMetaData;
import org.apache.myfaces.adfinternal.share.expl.Coercions;


/**
 * This tag inserts a region (defined by &lt;af:regionDef&gt;)
 * into the current page. The "id" attribute is required on this
 * tag.
 *
 * @author The Oracle ADF Faces Team
 */
public class ComponentRefTag extends UIXComponentRefTag
{


  public String getComponentType()
  {
    return "org.apache.myfaces.adf.ComponentRef";
  }

  public String getRendererType()
  {
    return null;
  }

  /**
   * Indicate that a facet was moved. The region component must keep track
   * of all facets that were moved so that they may be restored before the
   * jsf jsp tag framework notices. If the jsf jsp tag framework notices
   * that the facets are missing, it will recreate them, and that is undesireable
   * as that will break the "binding" attribute support
   * (on the facet) and will have performance consequences.
   * @param region the component that the facet was moved from.
   * @param facet the facet that was moved.
   * @param component the component that the facet was moved to.
   */
  public static void addRelocatedFacet(
    UIComponent region,
    String facet,
    UIComponent component)
  {
    Map map = (Map) region.getAttributes().get(_RELOCATED_FACETS_ATTRIBUTE);
    if (map == null)
    {
      map = new HashMap(3);
      region.getAttributes().put(_RELOCATED_FACETS_ATTRIBUTE, map);
    }

    // compute the ID to retrieve this relocated facet later.
    // this is because we don't always have the relocated facet instance
    // For example, after Serialization, the relocated facet instance is lost:
    StringBuffer findId = new StringBuffer(component.getId());
    for(UIComponent c = component.getParent(); c!=region; c = c.getParent())
    {
      if (c instanceof NamingContainer)
      {
        findId.insert(0, NamingContainer.SEPARATOR_CHAR);
        findId.insert(0, c.getId());
      }
    }

    map.put(facet, new RelocatedFacet(component, findId.toString()));
  }

  public int doStartTag() throws JspException
  {
    int returnVal = super.doStartTag();
    UIComponent region = getComponentInstance();
    Map<String,RelocatedFacet> map = (Map) region.getAttributes().get(_RELOCATED_FACETS_ATTRIBUTE);
    if ((map != null) && (!map.isEmpty()))
    {
      // restore all the facets that were moved. Otherwise
      // the jsf jsp tag framework will recreate them and that will
      // break support for the "binding" attribute on the facets that were
      // moved:
      for(Entry<String,RelocatedFacet> e:map.entrySet())
      {
        String facet = e.getKey();
        RelocatedFacet rf = e.getValue();
        rf.restoreFacet(region, facet);
      }
      map.clear();
    }

    return returnVal;
  }

  public int doEndTag() throws JspException
  {
    UIXComponentRef region = (UIXComponentRef) getComponentInstance();
    String regionType = region.getComponentType();

    if (regionType == null)
    {
      _LOG.severe("'componentType' attribute is required");
    }
    else
processRegion:
    {
      _LOG.finest("componentType is:{0}", regionType);
      RegionMetadata rmd =
        RegionMetadata.getRegionMetadata(getFacesContext());
      ComponentMetaData cmd = (ComponentMetaData) rmd.getRegionConfig(regionType);
      if (cmd == null)
      {
        _LOG.severe("Could not find metadata for componentType:{0} in region-metadata",
          regionType);
        break processRegion;
      }

      boolean hasErrors = _typeConvertAndDefaultAttrs(regionType, cmd);
      if (hasErrors)
        break processRegion;

      String page = cmd.getJspUIDef();

      _LOG.finest("Including page:{0}", page);

      try
      {
        if (page != null)
        {
          String oldRegion = (String)
            region.getAttributes().put(_REGION_TYPE_ATTRIBUTE, regionType);
          if (!regionType.equals(oldRegion))
          {
            region.getChildren().clear(); // bug 4508595
          }
          pageContext.getRequest().getRequestDispatcher(page).include(
            pageContext.getRequest(), pageContext.getResponse());
        }
        else
          _LOG.severe("There was no jspUri for componentType:"+regionType);
      }
      catch (IOException e)
      {
        _LOG.severe(e);
        throw new JspException(e);
      }
      catch (ServletException e)
      {
        _LOG.severe(_unwrap(e));
        throw new JspException(e);
      }
    }

    return super.doEndTag();
  }

  private boolean _typeConvertAndDefaultAttrs(
    String regionType,
    ComponentMetaData cmd)
  {
    boolean hasErrors = false;
    UIComponent region = getComponentInstance();
    Map compAttrs = region.getAttributes();
    List attrs = cmd.getAttributes();
    int sz = attrs.size();
    for(int i=0; i<sz; i++)
    {
      AttributeMetaData attr = (AttributeMetaData) attrs.get(i);
      String name = attr.getAttrName();
      Class klass = attr.getAttrClass();
      if (region.getValueBinding(name) != null)
        continue;

      Object compValue = compAttrs.get(name);
      if (compValue == null)
      {
        // if attribute value was not specified then try to default it:
        String defaultValue = attr.getDefaultValue();
        if (defaultValue != null)
        {
          hasErrors |= _typeConvert(compAttrs, name, defaultValue, klass);
        }
        // if no default value was found then make sure the attribute was not
        // required:
        else if (attr.isRequired())
        {
          _LOG.severe("attribute:{0} is missing on componentType:{1}",
                      new Object[] {name, regionType});
          hasErrors = true;
        }
      }
      // if a value was specified see if it needs to be type converted:
      else if (compValue instanceof String)
      {
        hasErrors |= _typeConvert(compAttrs, name, (String) compValue, klass);
      }
    }
    return hasErrors;
  }

  private boolean _typeConvert(Map compAttrs, String name, String value, Class klass)
  {
    try
    {
      Object converted = Coercions.coerce(value, klass);
      compAttrs.put(name, converted);
      return false;
    }
    catch (IllegalArgumentException e)
    {
      _LOG.severe(e);
    }
    return true;
  }

  private static Throwable _unwrap(Throwable t)
  {
    while(true)
    {
      Throwable causedBy = null;
      // OC4J does not unwrap the following exceptions:
      if (t instanceof JspException)
      {
        causedBy = ((JspException) t).getRootCause();
      }
      else if (t instanceof ServletException)
      {
        causedBy = ((ServletException) t).getRootCause();
      }
      if ((causedBy == null) || (causedBy == t))
        return t;
      else
        t = causedBy;
    }
  }

  private static final class RelocatedFacet implements Serializable
  {
    public RelocatedFacet(UIComponent facet, String findComponentId)
    {
      _facet = facet;
      // since we can't serialize the facet UIComponent, we need
      // another way to retrieve it, in the case that we are
      // serialized. The solution is to use UIComponent.findComponent if that
      // happens. So we need the id to pass into findComponent:
      _findId = findComponentId;
      assert findComponentId != null;
    }

    public void restoreFacet(UIComponent region, String facet)
    {
      UIComponent relocatedFacet = _facet;
      if (relocatedFacet == null)
      {
        relocatedFacet = region.findComponent(_findId);
      }
      region.getFacets().put(facet, relocatedFacet);
    }

    // must be transient because UIComponent is not Serializable.
    private transient final UIComponent _facet;
    private final String _findId;
  }

  private static final String _REGION_TYPE_ATTRIBUTE =
    "org.apache.myfaces.adfinternal.taglib.RegionTag.regionType.old";
  private static final String _RELOCATED_FACETS_ATTRIBUTE =
    "org.apache.myfaces.adfinternal.taglib.RegionTag.facets.relocated";
  private static final ADFLogger _LOG = ADFLogger.createADFLogger(ComponentRefTag.class);
}
