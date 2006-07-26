/*
 * Copyright  2001-2006 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.myfaces.trinidadinternal.renderkit.core.pages;

import java.io.IOException;

import java.util.Map;

import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;

import org.apache.myfaces.trinidad.component.core.CoreDocument;
import org.apache.myfaces.trinidad.component.core.CoreForm;
import org.apache.myfaces.trinidad.component.core.input.CoreChooseColor;
import org.apache.myfaces.trinidad.component.core.input.CoreSelectInputColor;
import org.apache.myfaces.trinidad.component.core.layout.CorePanelButtonBar;
import org.apache.myfaces.trinidad.component.core.nav.CoreGoButton;
import org.apache.myfaces.trinidad.component.core.output.CoreObjectSpacer;
import org.apache.myfaces.trinidad.component.html.HtmlCellFormat;
import org.apache.myfaces.trinidad.component.html.HtmlRowLayout;
import org.apache.myfaces.trinidad.component.html.HtmlScript;
import org.apache.myfaces.trinidad.component.html.HtmlTableLayout;

import org.apache.myfaces.trinidadinternal.convert.ColorConverter;
import org.apache.myfaces.trinidadinternal.renderkit.AdfRenderingContext;

/**
 * Entry point for the "colorPicker" JSP.
 * <p>
 * Parameters:
 * <ul>
 * <li>Java locale to use
 * <li>value: current color value
 * </ul>
 * <p>
 * @version $Name:  $ ($Revision: adfrt/faces/adf-faces-impl/src/main/java/oracle/adfinternal/view/faces/renderkit/core/pages/ColorPickerJSP.java#0 $) $Date: 10-nov-2005.19:03:33 $
 * @author The Oracle ADF Faces Team
 */
class ColorPickerJSP
{
  static public void service(FacesContext context)
    throws IOException
  {
    Map requestParams = context.getExternalContext().getRequestParameterMap();
    
    AdfRenderingContext arc = AdfRenderingContext.getCurrentInstance();
    CoreDocument doc = new CoreDocument();
    context.getViewRoot().getChildren().add(doc);

    doc.setTitle(arc.getTranslatedString("af_selectInputColor.PICKER_TITLE"));
    
    CoreForm form = new CoreForm();
    form.setId("d");
    doc.getChildren().add(form);
    
    HtmlTableLayout tl = new HtmlTableLayout();
    form.getChildren().add(tl);
    
    HtmlRowLayout rl1 = new HtmlRowLayout();
    tl.getChildren().add(rl1);
    HtmlCellFormat cf1 = new HtmlCellFormat();
    rl1.getChildren().add(cf1);
    cf1.setHalign("center");
    
    CoreSelectInputColor sic = new CoreSelectInputColor();
    cf1.getChildren().add(sic);
    sic.setId("c");
    sic.setOnkeypress("if(_getKC(event)==13){selectColor();return false}");
    Converter converter = _getConverter(requestParams);
    Object value = 
      converter.getAsObject(context, sic, (String)requestParams.get("value"));
    sic.setConverter(converter);
    sic.setValue(value);
    sic.setChooseId("choose");
    sic.setLabel(arc.getTranslatedString("af_selectInputColor.PICKER_PROMPT"));

    HtmlRowLayout rl2 = new HtmlRowLayout();
    tl.getChildren().add(rl2);
    CoreObjectSpacer spacer = new CoreObjectSpacer();
    rl2.getChildren().add(spacer);
    spacer.setHeight("8");
    
    HtmlRowLayout rl3 = new HtmlRowLayout();
    tl.getChildren().add(rl3);
    HtmlCellFormat cf3 = new HtmlCellFormat();
    rl3.getChildren().add(cf3);
    cf3.setHalign("center");
    
    CoreChooseColor choose = new CoreChooseColor();
    cf3.getChildren().add(choose);
    choose.setId("choose");
    choose.setWidth(7);
    
    HtmlRowLayout rl4 = new HtmlRowLayout();
    tl.getChildren().add(rl4);
    CoreObjectSpacer spacer2 = new CoreObjectSpacer();
    rl4.getChildren().add(spacer2);
    spacer2.setHeight("8");
    
    HtmlRowLayout rl5 = new HtmlRowLayout();
    tl.getChildren().add(rl5);
    HtmlCellFormat cf5 = new HtmlCellFormat();
    rl5.getChildren().add(cf5);
    cf5.setHalign("end");
    CorePanelButtonBar bar = new CorePanelButtonBar();
    cf5.getChildren().add(bar);


    CoreGoButton cancel = 
       JspUtils.createGoButton(arc, "af_selectInputColor.CANCEL");
    bar.getChildren().add(cancel);
    cancel.setOnclick("doCancel()");

    CoreGoButton select = 
       JspUtils.createGoButton(arc, "af_selectInputColor.APPLY");
    bar.getChildren().add(select);
    select.setOnclick("return selectColor()");

    HtmlScript script = new HtmlScript();
    script.setText(_SCRIPT);
    doc.getChildren().add(script);
  }

  static private Converter _getConverter(Map requestParams)
  {
    String pattern = (String) requestParams.get("pattern");
    boolean allowsTransparent =
      "true".equals(requestParams.get("allowsTransparent"));
    ColorConverter converter = new ColorConverter();
    converter.setPatterns(pattern.split(" "));
    if (allowsTransparent)
      converter.setTransparentAllowed(true);
    return converter;
  }

  static private final String _SCRIPT =

  "function doCancel()"+
  "{" +
    "top.returnValue = (void 0);"+
    "top.close();"+
    "return false;"+
  "}"+

  "function selectColor()"+
  "{"+
    "var colorField = document.forms.d.c;" +
    "var format = _getColorFieldFormat(colorField);" +
    "top.returnValue = format.getAsObject(colorField.value);" +
    "top.isApplicable = true;" +
    "top._unloadADFDialog(window.event);" +
    "top.close();" +  
    "return false;"+
  "}";
}
