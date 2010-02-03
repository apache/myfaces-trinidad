package org.apache.myfaces.trinidaddemo;

import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;

import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;

/**
 * This class is used in panelPageSkinDemo.jspx. It sets the Skin to dirty so that if you
 * change a skin css file it gets picked up immediately without the need to set the web.xml
 * CHECK_FILE_MODIFICATION flag.
 */
public class SkinDirtyPhaseListener
  implements PhaseListener
{
  public SkinDirtyPhaseListener()
  {
    super();
  }

  public void afterPhase(PhaseEvent phaseEvent)
  {
  }

  public void beforePhase(PhaseEvent phaseEvent)
  {
    // Add event code here...
    System.out.println("***PhaseTracker: Before Phase: " + phaseEvent.getPhaseId());
    RenderingContext rContext = RenderingContext.getCurrentInstance();
    if (rContext != null)
    {
      System.out.println("Set Skin to dirty");
      rContext.getSkin().setDirty(true);
    }
    else
      System.out.println("rContext in _beforePhase is null!");

  }

  public PhaseId getPhaseId()
  {
    return PhaseId.ANY_PHASE;
  }
}
