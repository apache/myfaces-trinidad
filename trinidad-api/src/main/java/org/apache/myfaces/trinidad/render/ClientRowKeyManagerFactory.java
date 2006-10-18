package org.apache.myfaces.trinidad.render;

import javax.faces.component.UIComponent;

import javax.faces.context.FacesContext;


/**
 * A producer of ClientRowKeyManagers.
 * Typically this interface will be implemented by Renderers of stamping 
 * components (like UIXCollection subclasses) that need to provide
 * string-row-keys that can be used to identify data rows on the client.
 */
public interface ClientRowKeyManagerFactory
{
  /**
   * Create a new ClientRowKeyManager for the given UIComponent
   */
  public ClientRowKeyManager createClientRowKeyManager(
    FacesContext context, 
    UIComponent component);
}
