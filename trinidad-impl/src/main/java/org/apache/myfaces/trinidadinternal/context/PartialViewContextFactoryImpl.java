package org.apache.myfaces.trinidadinternal.context;

import javax.faces.context.FacesContext;
import javax.faces.context.FacesContextFactory;
import javax.faces.context.PartialViewContext;
import javax.faces.context.PartialViewContextFactory;

public class PartialViewContextFactoryImpl
  extends PartialViewContextFactory
{
  public PartialViewContextFactoryImpl(PartialViewContextFactory factory)
  {
    _factory = factory;
  }

  /**
   * Creates a PartialViewContext instance that is optimized for Trinidad PPR.
   * @param context 
   * @return a PartialViewContext instance
   */
  public PartialViewContext getPartialViewContext(FacesContext context)
  {
    return new PartialViewContextImpl(context);
  }
  
  private final PartialViewContextFactory _factory;
  
}
