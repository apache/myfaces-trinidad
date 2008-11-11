package org.apache.myfaces.trinidadinternal.application;

import javax.faces.component.UIViewRoot;

import javax.portlet.faces.component.PortletNamingContainerUIViewRoot;

import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * A set of utilities for dealing with portlets.  These are not as generic
 * as those found in the ExternalContextUtils, they are designed to be much
 * more specific to Trinidad usecases and don't operate on an ExternalContext.
 *
 * Methods are extracted into this class in order to allow Trinidad to be run
 * both with and without the bridge jars in place.  The methods here should be
 * executed only within a portlet environment.
 */
class PortletUtils
{
  private PortletUtils()
  {
  }
  
  /**
   * This should only be executed if we are currently in a Portlet Request.
   * If executed, this method introduces a dependency on the JSR-301 bridge
   * which is required for Trinidad to run in a portal environment.  If this
   * method is not run, then the bridge api's remain optional at runtime.
   * 
   * This method checks the current UIViewRoot to see if it is a 
   * PortletNamingContainer.  If it is, then this class simply returns the
   * UIViewRoot.  If it does not then the current UIViewRoot is used to create
   * a new PortletNamingContainerUIViewRoot.
   */
  public static final UIViewRoot getPortletViewRoot(UIViewRoot root) 
  {
    Class<? extends UIViewRoot> rootClass = root.getClass();
    //If the current root is not the real UIViewRoot object in faces then
    //is no need to escape it.  It is assumed it handles namespacing on its
    //own.  This is the same as the logic in the JSR-301 Bridge spec.
    if(rootClass == UIViewRoot.class) 
    {
      _LOG.fine("Creating PortletUIViewRoot for use with the portal.");
      root = new PortletNamingContainerUIViewRoot(root);
    }
    
    //TODO: Do we need a warning here if the view root is not annotated 
    //properly?  This could happen if another renderkit is involved and does
    //not correctly implement JSR-301.  This will NOT be an issue in Trin only
    //environments.
    return root;
  }

  private static final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(PortletUtils.class);
}
