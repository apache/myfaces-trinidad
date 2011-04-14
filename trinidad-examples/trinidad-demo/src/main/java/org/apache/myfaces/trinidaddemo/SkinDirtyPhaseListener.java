package org.apache.myfaces.trinidaddemo;

import javax.faces.event.ActionEvent;
import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;

import javax.servlet.ServletContext;

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
    System.out.println("***PhaseTracker: After Phase: " + phaseEvent.getPhaseId());
    RenderingContext rContext = RenderingContext.getCurrentInstance();

    if (rContext != null)
    {
      System.out.println("Set Skin to dirty if button was pressed, and it is set to :" + _pressedButton);
      if (_pressedButton)
      {
        System.out.println("Setting the skin to be dirty will allow you to refresh your browser and see any changes to the skin's css file immediately.");
        rContext.getSkin().setDirty(true);
        _pressedButton = false;
      }
      else 
      {
        System.out.println("Set Skin to not be dirty");
        rContext.getSkin().setDirty(false);
      }
    }
    else
      System.out.println("rContext in _afterPhase is null!");

  }

  public PhaseId getPhaseId()
  {
    return PhaseId.RESTORE_VIEW;
  }
  
  public void buttonAction(ActionEvent action) {
    System.out.println("You pressed the button");
    _pressedButton = true;
    
  }
  
  
  private boolean _pressedButton = false;
}
