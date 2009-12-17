package org.apache.myfaces.trinidadinternal.context;

import java.util.Collection;
import java.util.Set;

import javax.faces.context.FacesContext;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.component.visit.VisitContext;
import org.apache.myfaces.trinidad.component.visit.VisitHint;


public final class MVisitContextFactory
{
  private MVisitContextFactory() {}

  public static VisitContext createVisitContext(
   FacesContext       context,
   Collection<String> ids,
   Set<VisitHint>     hints,
   PhaseId            phaseId)
  {
    if ((ids == null) || ids.isEmpty())
      return new FullVisitContext(context, hints, phaseId);
    else
      return new PartialVisitContext(context, ids, hints, phaseId);
  }
}
