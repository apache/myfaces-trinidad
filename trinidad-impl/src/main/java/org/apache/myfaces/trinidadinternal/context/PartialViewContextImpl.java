/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidadinternal.context;

import java.io.IOException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;

import java.util.Set;

import javax.faces.FacesException;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.context.PartialResponseWriter;
import javax.faces.context.PartialViewContext;
import javax.faces.context.ResponseWriter;
import javax.faces.event.PhaseId;

import org.apache.myfaces.trinidad.component.UIXComponent;
import javax.faces.component.visit.VisitCallback;
import javax.faces.component.visit.VisitContext;
import javax.faces.component.visit.VisitHint;
import javax.faces.component.visit.VisitResult;

import org.apache.myfaces.trinidad.component.visit.VisitTreeUtils;
import org.apache.myfaces.trinidad.context.PartialPageContext;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidadinternal.renderkit.core.ppr.PPRResponseWriter;
import org.apache.myfaces.trinidadinternal.renderkit.core.xhtml.PartialPageUtils;

public class PartialViewContextImpl extends PartialViewContext
{
  public PartialViewContextImpl(FacesContext context) 
  {
    _context = context;
  }

  /**
   * To force a full execute we always return an empty execute list.
   * @return
   */
  public Collection<String> getExecuteIds()
  {
    /*
    if (_executeIds == null) 
    {
      String executeStr = _facesContext.getExternalContext().getRequestParameterMap().get(
        PartialViewContext.PARTIAL_EXECUTE_PARAM_NAME);
    }
    else
    {
      _executeIds = new ArrayList<String>();
    }
    */ 
    _executeIds = new ArrayList<String>();
    return _executeIds;
  }

  public Collection<String> getRenderIds()
  {
    return Collections.emptySet();
  }

  /**
   * Return a PartialResponseWriter that consumes the Trinidad PprResponseWriter. 
   * @return
   */
  public PartialResponseWriter getPartialResponseWriter()
  {
    if (_partialResponseWriter == null) 
    {
      _partialResponseWriter = new PPRPartialResponseWriter(_context.getResponseWriter());
    }
    return _partialResponseWriter;
  }

  public boolean isAjaxRequest()
  {
    _assertNotReleased();
    if (_ajaxRequest == null) 
    {
        _ajaxRequest = _PARTIAL_AJAX.equals(_context.
            getExternalContext().getRequestHeaderMap().get(_FACES_REQUEST));
    }
    return _ajaxRequest;
  }

  public boolean isPartialRequest()
  {
    _assertNotReleased();
    if (_partialRequest == null) {
        _partialRequest = isAjaxRequest() ||
                _PARTIAL_PROCESS.equals(_context.
                getExternalContext().getRequestHeaderMap().get(_FACES_REQUEST));
    }
    return _partialRequest;  
  }

  /**
   * Trinidad PPR currently does a full decode/validate/update. So we return 'false' here, but in 
   * processPartial, turn into a 'full tree visit'.
   * @return true as the default executeList is set to be '@all'.
   */
  public boolean isExecuteAll()
  {
    _assertNotReleased();
    /*String execute = _context.getExternalContext().getRequestParameterMap()
      .get(PARTIAL_EXECUTE_PARAM_NAME);
    
    return execute.equals(this.ALL_PARTIAL_PHASE_CLIENT_IDS);*/
    return true;
  }

  public boolean isRenderAll()
  {
    _assertNotReleased();
    if (_renderAll == null) {
        String render = _context.getExternalContext().getRequestParameterMap()
                .get(PARTIAL_RENDER_PARAM_NAME);
        _renderAll = (ALL_PARTIAL_PHASE_CLIENT_IDS.equals(render));
    }

    return _renderAll;  
  }

  public void setRenderAll(boolean renderAll)
  {
    _renderAll = renderAll;
  }

  public void setPartialRequest(boolean isPartialRequest)
  {
    _partialRequest = isPartialRequest;
  }

  public void release()
  {
    _ajaxRequest = null;
    _executeIds = null;
    _context = null;
    _partialRequest = null;
    _partialResponseWriter = null;
    _released = true;
    _renderAll = null;
    _renderIds = null;
  }

  /**
   * Handles the render phase alone as we perform a full 'execute' always.
   * 
   * @param phaseId
   */
  public void processPartial(PhaseId phaseId)
  {
    UIViewRoot viewRoot = _context.getViewRoot();
    if (phaseId == PhaseId.APPLY_REQUEST_VALUES ||
        phaseId == PhaseId.PROCESS_VALIDATIONS ||
        phaseId == PhaseId.UPDATE_MODEL_VALUES)
    {
      // Assume the executeIds is set to @all to mimic a full tree visit. 
      // Create a FullVisitContext
      Collection <String> executeIds = getExecuteIds();

      try 
      {
        _processExecute(viewRoot, phaseId, executeIds);
      } 
      catch (Exception e) 
      {
          // RELEASE_PENDING LOG EXCEPTION
      }
      
    }
    else if (phaseId == PhaseId.RENDER_RESPONSE)
    {
      _processRender(viewRoot, phaseId);
    }

  }

  /**
   * Performs a 'full' execute if executeIds is empty, in order to build the partialTrigger 
   * listeners. Later, support for an 'optimized execute' will be added that 'visits' the executeId 
   * components using the Trinidad visitTree implementation (that accounts for setup/teardown of 
   * contexts.)
   * @param component the UIViewRoot
   * @param phaseId
   * @param executeIds the Collection of ids to execute
   */
  private void _processExecute(
    UIViewRoot component, 
    PhaseId phaseId,
    Collection<String> executeIds)
  {
    if (executeIds == null || executeIds.isEmpty())
    {
      // We use the tree visitor mechanism to locate the components to process. For now we ignore the 
      // executeIds as we always want to perform a full decode. So a FullVisitContext is created and 
      // the VisitCallback invoked for every component in the tree. 
      VisitContext visitContext = 
                              VisitTreeUtils.createVisitContext(_context, 
                                                               executeIds, 
                                                               _EXECUTE_VISIT_HINTS);
      VisitCallback visitCallback = new ExecuteAllCallback(_context, phaseId);
      UIXComponent.visitTree(visitContext, component, visitCallback);
    }
    else
    {
      // TODO: optimized decode??  
    }
  }

  /**
   * Performs a partial optimized rendering, if PPROptimization is enabled, otherwise defaults to a 
   * full render. 
   * 
   * @param viewRoot
   * @param phaseId
   */
  private void _processRender(UIComponent viewRoot, PhaseId phaseId)
  {
    boolean documentStarted = false;
    
    PartialResponseWriter partialResponseWriter = getPartialResponseWriter();
    // Hold onto the original writer for later
    ResponseWriter origResponseWriter = _context.getResponseWriter();
    _context.setResponseWriter(partialResponseWriter);

    ExternalContext extContext = _context.getExternalContext();
    extContext.setResponseContentType(_RESPONSE_CONTENT_TYPE);
    extContext.addResponseHeader("Pragma", "no-cache");
    extContext.addResponseHeader("Cache-control", "no-cache");

    try
    {
      partialResponseWriter.startDocument();
      documentStarted = true;
    
      // determine whether we should try and optimize the PPR rendering
      boolean encodeAllChildren = !PartialPageUtils.isOptimizedPPREnabled(_context, true);
      
      if (encodeAllChildren)
      {
        // We are guaranteed to be called on UIViewRoot. 
        if (!viewRoot.isRendered()) 
        {
          return;
        }
      
        // No PPR optimization, so encode all children
        Iterator<UIComponent> children = viewRoot.getFacetsAndChildren();
        while (children.hasNext()) 
        {
          children.next().encodeAll(_context);
        }
        
        // TODO: Explcitly render out the view state as an update element.
        _renderViewState(partialResponseWriter);
      }
      else
      {
        // TODO 
        // HINT: PartialPageContext already creates a partial visit context. Reuse it??
        
        // Only visit tree if renderIds exist
        Collection <String> renderIds = getRenderIds();
        if (renderIds != null && !renderIds.isEmpty())
        {
          // perform an optimized partial visit of the children
          RenderingContext rContext = RenderingContext.getCurrentInstance();
          PartialPageContext pprContext = rContext.getPartialPageContext();
         
          VisitContext visitContext = pprContext.getVisitContext();
          // component.visitTree(visitContext, new PhaseAwareVisitCallback(_context, phaseId));
        }
      }
    }
    catch (IOException e)
    {
      // launder the IOException as a FacesException, we'll unwrap this later
      throw new FacesException(e);        
    }
    finally
    {
      try
      {
        if (documentStarted)
        {
          partialResponseWriter.endDocument();
          documentStarted = false;
        }
      }
      catch (IOException ioe2)
      {
        // TODO log exception
      }
      _context.setResponseWriter(origResponseWriter);
    }
  }
  
  private void _renderViewState(PartialResponseWriter writer)
    throws IOException
  {
    // Get the view state and write it to the response..
    writer.startUpdate(PartialResponseWriter.VIEW_STATE_MARKER);
    String state = _context.getApplication().getStateManager().getViewState(_context);
    writer.write(state);
    writer.endUpdate();    
  }
  
  @SuppressWarnings({"FinalPrivateMethod"})
  private final void _assertNotReleased() 
  {
    if (_released) {
        throw new IllegalStateException();
    }
  }




  /**
   * A simple PartialRepsonseWriter implementation that delegates most of its calls to the
   * PPRResponseWriter as it already has the smarts to generate XML partial Ajax responses using the
   * Trinidad partialTriggers mechanism.
   */
  private static final class PPRPartialResponseWriter extends PartialResponseWriter 
  {
    public PPRPartialResponseWriter(ResponseWriter writer)
    {
      super(null);
      assert(writer instanceof PPRResponseWriter);
      _pprWriter = (PPRResponseWriter) writer;
    }

    @Override
    public void endDocument() throws IOException
    {
      getWrapped().endDocument();
    }

    @Override
    public ResponseWriter getWrapped()
    {
      return _pprWriter;
    }

    @Override
    public void startDocument() throws IOException
    {
      getWrapped().startDocument();
    }


    @Override
    public void endUpdate()
      throws IOException
    {
      _pprWriter.endUpdate();
    }
    
    @Override
    public void startUpdate(String string) throws IOException 
    {
      _pprWriter.startUpdate(string);
    }

    private PPRResponseWriter _pprWriter;
  }
  
  /**
   * Callback for encoding subtrees during optimized PPR tree visits
   */
  private static final class ExecuteAllCallback implements VisitCallback
  {
    public ExecuteAllCallback(
      FacesContext context, 
      PhaseId phaseId)
    {
      _context= context;
      _phaseId = phaseId;
    }
    
    public VisitResult visit(VisitContext context, UIComponent target)
    {

      // For UIViewRoot it's important to return VisitResult.ACCEPT to indicate that the tree 
      // visit start processing its children. This is also required to not get into a recursive 
      // hell as we got here to begin from UIViewRoot.processDecodes()
      if (target instanceof UIViewRoot)
        return VisitResult.ACCEPT;

      // we have the subtree we want, execute it
      if (_phaseId == PhaseId.APPLY_REQUEST_VALUES) 
      {
        target.processDecodes(_context);
      } 
      else if (_phaseId == PhaseId.PROCESS_VALIDATIONS) 
      {
        target.processValidators(_context);
      } 
      else if (_phaseId == PhaseId.UPDATE_MODEL_VALUES) 
      {
        target.processUpdates(_context);
      }        

      // we have finished processing desired target
      return VisitResult.REJECT;
    }
    
    private FacesContext _context;
    private PhaseId      _phaseId;
  }  
  
  private Boolean _ajaxRequest;
  private List<String> _executeIds;
  private FacesContext _context = null; 
  private Boolean _partialRequest;
  private PartialResponseWriter _partialResponseWriter;
  private boolean _released;
  private Boolean _renderAll;
  private Collection<String> _renderIds;

  private static final Set<VisitHint> _EXECUTE_VISIT_HINTS = EnumSet.of(VisitHint.SKIP_UNRENDERED,
                                                                        VisitHint.EXECUTE_LIFECYCLE);  
  
  private static final String _RESPONSE_CONTENT_TYPE = "text/xml";
  private static final String _FACES_REQUEST = "Faces-Request";
  private static final String _PARTIAL_AJAX = "partial/ajax";
  private static final String _PARTIAL_PROCESS = "partial/process";

}
