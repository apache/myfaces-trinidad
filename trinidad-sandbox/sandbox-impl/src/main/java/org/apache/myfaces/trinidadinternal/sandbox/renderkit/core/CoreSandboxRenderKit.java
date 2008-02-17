package org.apache.myfaces.trinidadinternal.sandbox.renderkit.core;

import org.apache.myfaces.trinidadinternal.renderkit.RenderKitDecorator;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;

public class CoreSandboxRenderKit extends RenderKitDecorator
{

    public CoreSandboxRenderKit(){
        
    }
    
    @Override
    protected String getDecoratedRenderKitId()
    {
      return CoreRenderKit.BASE_RENDER_KIT_ID;
    }
}
