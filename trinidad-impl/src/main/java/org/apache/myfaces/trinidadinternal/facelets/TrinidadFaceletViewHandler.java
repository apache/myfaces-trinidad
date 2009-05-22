package org.apache.myfaces.trinidadinternal.facelets;


import com.sun.facelets.FaceletViewHandler;

import java.io.IOException;

import java.util.HashSet;
import java.util.Set;

import javax.faces.FacesException;
import javax.faces.application.ViewHandler;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.change.ChangeManager;
import org.apache.myfaces.trinidad.context.PageResolver;
import org.apache.myfaces.trinidad.context.RequestContext;

/**
 * ViewHandler implementation for Trinidad in Facelets mode
 */
public class TrinidadFaceletViewHandler
  extends FaceletViewHandler
{
  
  // A context parameter for controlling which views should be handled by Facelets 
  // based on the supplied extensions and prefixes.
  // We process this parameter exactly like Facelets does with "facelets.VIEW_MAPPINGS" 
  // with the exception that we get a physical view Id from the PageResolver before checking
  // its extension/prefix.
  //
  // Unfortunately, Facelets provides no hook for plugging the PageResolver into the logic
  // handling "facelets.VIEW_MAPPINGS". We will recommend that our users leave "facelets.VIEW_MAPPINGS" 
  // unset and use "org.apache.myfaces.trinidad.FACELETS_VIEW_MAPPINGS" instead.
  static public final String FACELETS_VIEW_MAPPINGS =
                            "org.apache.myfaces.trinidad.FACELETS_VIEW_MAPPINGS";
  
  public TrinidadFaceletViewHandler(ViewHandler parent)
  {
    super(parent);
    _parent = parent;
  }
  
  /**
   * Overridden to apply changes from the ChangeManager
   */
  @Override
  protected void buildView(FacesContext context, UIViewRoot viewToRender)
    throws IOException, FacesException 
  {
    super.buildView(context, viewToRender);
    
    // Apply changes once we have a stable view tree built. This is the earliest 
    // opportunity, the document was just attached to the view root.
    ChangeManager cm = RequestContext.getCurrentInstance().getChangeManager();
    cm.applyComponentChangesForCurrentView(FacesContext.getCurrentInstance());
  }
  
  
  /**
   * Overridden to check wthether the physical viewId (as determined by PageResolver)
   * should be handled by Facelets
   */
  @Override
  public UIViewRoot restoreView(FacesContext context, String viewId)
  {
    _initMappings(context);
    
    if (_handledByFacelets(viewId))
    {
      return super.restoreView(context, viewId);
    }
    else
    {
      return _parent.restoreView(context, viewId);
    }
  }
  
  /**
   * Overridden to check wthether the physical viewId (as determined by PageResolver)
   * should be handled by Facelets
   */
   @Override
  public void renderView(FacesContext context, UIViewRoot viewToRender)
    throws IOException
  {
    if (!viewToRender.isRendered()) 
    {
      return;
    }
    
    _initMappings(context);
    
    if (_handledByFacelets(viewToRender.getViewId())) 
    {
      super.renderView(context, viewToRender);
    }
    else
    {
      _parent.renderView(context, viewToRender);
    }
  }
  
  /**
   * Overridden to check wthether the physical viewId (as determined by PageResolver)
   * should be handled by Facelets
   */
  @Override
  public void writeState(FacesContext context) 
    throws IOException
  {
    _initMappings(context);
    
    if (_handledByFacelets(context.getViewRoot().getViewId()))
    {
      super.writeState(context);
    }
    else
    {
      _parent.writeState(context);
    }
  }
  
  /**
   * This method uses double-check locking (DLC) to avoid synchronization
   * when accessing _extensionMappings and _prefixMappings
   * Note that _mappingsInitialized, _extensionMappings and _prefixMappings
   * are declared 'volatile', and _extensionMappings/_prefixMappings are not modified
   * since they are constructed. 
   * JDK5 and later extends the semantics for volatile so 
   * that the system will not allow a write of a volatile to be reordered with respect 
   * to any previous read or write, and a read of a volatile cannot be reordered with 
   * respect to any following read or write
   */
  private void _initMappings(FacesContext context)
  {
    if (_mappingsInitialized)
    {
      return;
    }
    synchronized(this)
    {
      if (!_mappingsInitialized)
      {
        ExternalContext external = context.getExternalContext();
        String viewMappings = external.getInitParameter(FACELETS_VIEW_MAPPINGS);
        if ((viewMappings != null) && (viewMappings.length() > 0)) 
        {
          String[] mappingsArray = viewMappings.split(";");
          
          Set<String> extensionMappings = new HashSet<String>(mappingsArray.length);
          Set<String> prefixMappings = new HashSet<String>(mappingsArray.length);
          
          for (int i = 0; i < mappingsArray.length; i++) 
          {
            String mapping = mappingsArray[i].trim();
            int mappingLength = mapping.length();
            if (mappingLength <= 1) 
            {
              continue;
            }
      
            if (mapping.charAt(0) == '*') 
            {
              extensionMappings.add(mapping.substring(1));
            } 
            else if (mapping.charAt(mappingLength - 1) == '*') 
            {
              prefixMappings.add(mapping.substring(0, mappingLength - 1));
            }
          }
          
          if (extensionMappings.size() > 0)
          {
            _extensionMappings = new HashSet<String>(extensionMappings);
          }
          
          if (prefixMappings.size() > 0)
          {
            _prefixMappings = new HashSet<String>(prefixMappings);
          }
        }
       
        _mappingsInitialized = true;
      }
    }
  }
  
  private boolean _handledByFacelets(String viewId) 
  {
    // If there's no extension or prefixe mappings, then
    // just make Facelets handle everything
    if ((_extensionMappings == null) && (_prefixMappings == null)) 
    {
      return true;
    }
    
    // Since extension/prefix mappings apply to physical URIs, we need
    // to get a physical viewId. It is assumed that getPhysicalURI() lookup is
    // relatively cheap
    RequestContext afc = RequestContext.getCurrentInstance();
    if (afc != null)
    {
      viewId = afc.getPageResolver().getPhysicalURI(viewId);
    }
    
    if (_extensionMappings != null) 
    {
      for (String extension: _extensionMappings) 
      {
        if (viewId.endsWith(extension)) 
        {
          return true;
        }
      }
    }
    
    if (_prefixMappings != null) 
    {
      for (String prefix: _prefixMappings) 
      {
        if (viewId.startsWith(prefix)) 
        {
            return true;
        }
      }
    }
    
    return false;
  }
  
  
  private final ViewHandler _parent;
  
  // Set of viewId extensions that should be handled by Facelets
  private volatile Set<String> _extensionMappings;

  // Set of viewId prefixes that should be handled by Facelets
  private volatile Set<String> _prefixMappings;
  
  private volatile boolean _mappingsInitialized = false;
}
