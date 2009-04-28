package org.apache.myfaces.trinidadinternal.facelets;


import com.sun.facelets.FaceletViewHandler;

import java.io.IOException;

import javax.faces.FacesException;
import javax.faces.application.ViewHandler;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;

import org.apache.myfaces.trinidad.change.ChangeManager;
import org.apache.myfaces.trinidad.context.RequestContext;

/**
 * ViewHandler implementation for Trinidad in Facelets mode
 */
public class TrinidadFaceletViewHandler
  extends FaceletViewHandler
{
  public TrinidadFaceletViewHandler(ViewHandler parent)
  {
    super(parent);
  }
  
  @Override
  protected void buildView(FacesContext context, UIViewRoot viewToRender)
    throws IOException, FacesException 
  {
    super.buildView(context, viewToRender);
    
    // Apply changes once we have a stable view tree built. This is the earliest 
    //  opportunity, the document was just attached to the view root.
    ChangeManager cm = RequestContext.getCurrentInstance().getChangeManager();
    cm.applyComponentChangesForCurrentView(FacesContext.getCurrentInstance());
  }
}
