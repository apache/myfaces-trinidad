/*
 * Copyright  2005,2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.renderkit.core.xhtml;
import java.io.IOException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import java.util.Set;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.validator.Validator;

import org.apache.myfaces.adf.logging.ADFLogger;
import org.apache.myfaces.adf.component.UIXEditableValue;
import org.apache.myfaces.adf.convert.ClientConverter;
import org.apache.myfaces.adf.util.MessageFactory;
import org.apache.myfaces.adf.validator.ClientValidator;

import org.apache.myfaces.adfinternal.convert.InternalClientConverter;
import org.apache.myfaces.adfinternal.renderkit.AdfRenderingContext;
import org.apache.myfaces.adfinternal.share.data.ServletRequestParameters;
import org.apache.myfaces.adfinternal.share.util.FastMessageFormat;
import org.apache.myfaces.adfinternal.util.IntegerUtils;
import org.apache.myfaces.adfinternal.util.MessageUtils;
import org.apache.myfaces.adfinternal.validator.InternalClientValidator;

/**
 *@todo - this needs to be moved to the renderkit package
 */
public class FormData
{

  public FormData(String name)
  {
    _formName = name;
  }

  public int getInputTextCount()
  {
    return _inputTextCount;
  }

  public void incrementInputTextCount()
  {
    _inputTextCount++;
  }

  public String getDefaultCommandId()
  {
    return _defaultCommandId;
  }


  public String getFormName()
  {
    return _formName;
  }

  public boolean hasImmediateComponent()
  {
    return _hasImmediateComponent;
  }

  public void setDefaultCommandId(String defaultCommandId)
  {
    _defaultCommandId = defaultCommandId;
  }

  public String getName()
  {
    return _formName;
  }
  public void addLabel(
    String targetId,
    String label
    )
  {

    if (targetId == null || label == null)
      return;

    Map labelMap = getLabelMap(true);

    labelMap.put(targetId, label);
  }



  public Map getLabelMap(
    boolean  createIfNecessary
    )
  {
    if ((_labelMap == null) && createIfNecessary)
    {
      _labelMap = new HashMap(31);
    }

    return _labelMap;
  }



  public Map getPatternMap(
    boolean createIfNecessary
    )
  {
    if ((_patternMap == null) && createIfNecessary)
    {
      _patternMap = new HashMap();
    }

    return _patternMap;
  }

  public void addPattern(
    String targetId,
    String pattern
    )
  {

    if (targetId == null || pattern == null)
      return;

    Map patternMap = getPatternMap(true);

    patternMap.put(targetId, pattern);
  }


  /**
   *
   * @todo get rid of servletRequestParameters reference
   */
  public void addNeededValue(String name)
  {
    if (name == null)
      throw new IllegalArgumentException();

    if ( ServletRequestParameters.HAS_COMPOUND_NAME.equals(name) )
    {
      _useCompoundNames = true;
      return;
    }

    List neededValues = getNeededValues(true);
    neededValues.add(name);
  }


  public void addRenderedValue(String name)
  {
    Set renderedSet = getRenderedValues(true);
    renderedSet.add(name);
  }

  /**
   * @todo get rid of needing this somehow?
   */
  public boolean useCompoundNames()
  {
    return _useCompoundNames;
  }

  /**
   * @param call a function call.
   * "eval(call)" will be called on the client when resetting.
   */
  public void addResetCall(
    String           call
    )
  {

    if (call != null)
    {
      List resetCalls = getResetCalls(true);

      // add the call
      resetCalls.add(call);
    }
  }


  public List getResetCalls(
    boolean          createIfNecessary
    )
  {

    if (_resetCallsList == null && createIfNecessary)
    {
      _resetCallsList = new ArrayList();
    }

    return _resetCallsList;
  }


  public List getClientDependencies(
    boolean          createIfNecessary
    )
  {
    if ((_clientDependencies == null) && createIfNecessary)
    {
      _clientDependencies = new ArrayList(10);
    }

    return _clientDependencies;
  }

  public List getNeededValues(
    boolean createIfNecessary
    )
  {
    if ((_neededValues == null) && createIfNecessary)
    {
      _neededValues = new ArrayList(10);
    }

    return _neededValues;
  }


  public Set getRenderedValues(
    boolean createIfNecessary
    )
  {
    if ((_renderedSet == null) && createIfNecessary)
    {
      _renderedSet =  new HashSet(23);
    }

    return _renderedSet;
  }


  public Iterator getValidationIterator()
  {
    Map validationMap = _getValidationMap(false);
    if ( validationMap == null)
      return null;

    return validationMap.keySet().iterator();
  }


  public Iterator getErrorFormatIterator()
  {
    Map errorFormatMap = _getErrorFormatMap(false);
    if ( errorFormatMap == null)
      return null;

    return errorFormatMap.keySet().iterator();
  }

  public List getFormValidatorsInfo(
    boolean createIfNecessary
    )
  {
    // create the validators if they don't already exist
    if ((_formValidatorsInfo == null) && createIfNecessary)
    {
      _formValidatorsInfo = new ArrayList();
    }

    return _formValidatorsInfo;
  }


  public List getValidatedInputList(
    boolean createIfNecessary
    )
  {
    if ((_validatedInputList == null) && createIfNecessary)
    {
      _validatedInputList = new ArrayList();
    }

    return _validatedInputList;
  }

  /**
   * @todo - adding required, converter, validators should be done separately
   * and this method should be killed.
   */
  public void addOnSubmitConverterValidators(
    UIComponent      component,
    Converter        converter,
    Iterator         validators,
    String           clientId,
    boolean          immediate,
    boolean          required,
    String           requiredMessageKey
    ) throws IOException
  {
    if (clientId == null)
    {
      _LOG.warning("Cannot add client side converter & validators as the node name is null");
      return;
    }

    if (immediate)
      _hasImmediateComponent = true;

    FormData.ConvertValidate convertValidateInfo = null;

    // required identifies that required='true' has been set and that a validation
    // error should be displayed when no value is entered in the input field
    if (required)
    {
      convertValidateInfo = _getNewConvertValidate(clientId);

      convertValidateInfo.required = true;

      String reqMsgDetail = _getRequiredDetailMessage(component,
                                                      requiredMessageKey);

      // get the format index of this error format in the registered formats
      Integer formatIndex = _addErrorFormat(reqMsgDetail);
      convertValidateInfo.requiredFormatIndex = formatIndex;

      _addValidatedInput(clientId);
    }

    if (converter != null && converter instanceof ClientConverter)
    {
      if (convertValidateInfo == null)
        convertValidateInfo = _getNewConvertValidate(clientId);

      _addOnSubmitConverter( component,
                            ((ClientConverter) converter),
                            convertValidateInfo,
                            clientId);
    }


    if (validators == null)
    {
      //=-=AEW This seems to be OK right now
      //_LOG.warning("Null validators iterator for {0}", component);
      ;
    }
    else
    {
      while (validators.hasNext())
      {
        Validator validator = (Validator) validators.next();

        if (validator instanceof ClientValidator)
        {
          if (convertValidateInfo == null)
            convertValidateInfo = _getNewConvertValidate(clientId);

          _addOnSubmitValidator( component,
                                 ((ClientValidator)validator),
                                 convertValidateInfo,
                                 clientId);
        }
      }
    }
  }



  //*******************************************************
  // private
  //*******************************************************

  /**
   * Adds converter info.
   */
  private void _addFormConverterInfo(
    String                    converter,
    Integer                   formatIndex,
    FormData.ConvertValidate  convertValidate
   )
  {
    if (converter != null && convertValidate != null)
    {
      if (convertValidate.converter == null)
        convertValidate.converter = new Object[2];
      else
        _LOG.warning("There is already a converter on \"" +
                     convertValidate.clientId +
                     "\". There should only be one converter per component.");


      // add the converter
      convertValidate.converter[0] = (_addValidation(converter));
      convertValidate.converter[1] = (formatIndex == null? "(void 0)" :
                                              formatIndex.toString());

    }
  }

 /**
   * Adds a form-level validator.
   */
  private void _addFormValidatorInfo(
    String                    validator,
    Integer                   formatIndex,
    FormData.ConvertValidate  convertValidate
   )
  {
    if (validator != null && convertValidate != null)
    {
      if (convertValidate.validators == null)
        convertValidate.validators = new ArrayList();

      // add the validator
      convertValidate.validators.add(_addValidation(validator));
      convertValidate.validators.add(formatIndex);
    }
  }

  // =-= bts make package private
  /**
   * @todo escape script???
   * @todo get rid of the colorpicker hack!
   */
  private void _addOnSubmitConverter(
    UIComponent               component,
    ClientConverter           submitConverter,
    FormData.ConvertValidate  convertValidate,
    String                    clientId
    ) throws IOException
  {

    if (component == null)
    {
      // HACK HACK - this is needed for colorPicker!
      component = new org.apache.myfaces.adf.component.UIXInput();
      component.setId(clientId);

    }

    // Should we remove getClientConversionFormat altogether???
    // For validation it may apply, but for conversion will you ever
    // have 2 different converter constructors?
    Integer formatIndex = null;

    FacesContext fcontext = FacesContext.getCurrentInstance();
    if (submitConverter instanceof InternalClientConverter)
    {
      InternalClientConverter internalConverter =
                                     (InternalClientConverter)submitConverter;
      // Write the client dependencies for the onSubmitValidator
      String clientLibrary = internalConverter.getLibKey(fcontext, component);

      _writeDependencies(fcontext, clientLibrary);


      // register the validation format string on the form
      String conversionFormat =
             internalConverter.getClientConversionFormat(fcontext, component);

      // get the format index of this error format in the registered formats
      formatIndex = _addErrorFormat(conversionFormat);
    }

    Object clientDependency = submitConverter.getClientScript(fcontext,
                                                                component);
    if (clientDependency != null)
    {
      List clientDependencies = getClientDependencies(true);
      clientDependencies.add(clientDependency);
    }

    String converter = (String)submitConverter.getClientConversion(fcontext,
                                                                   component);

    if (converter != null)
    {

      _addFormConverterInfo( converter, formatIndex, convertValidate);
      _addValidatedInput(clientId);
    }
  }


  /**
   * @todo Is there a way to remove the hack we have introduced, if the
   * component is null. This happens with composites.
   * @todo =-= bts make package private
   */
  private void _addOnSubmitValidator(
    UIComponent               component,
    ClientValidator           submitValidator,
    FormData.ConvertValidate  convertValidate,
    String                    clientId
    ) throws IOException
  {
    FacesContext fContext = FacesContext.getCurrentInstance();

    // Should we remove getClientValidationFormat altogether???
    // It would be unusual that it's much of an optimization
    Integer formatIndex = null;

    if (submitValidator instanceof InternalClientValidator)
    {
      InternalClientValidator internalValidator =
                                     (InternalClientValidator)submitValidator;

      // Write the client dependencies for the onSubmitValidator
      String clientLibrary = internalValidator.getLibKey(fContext, component);

      _writeDependencies(fContext, clientLibrary);



      // register the validation format string on the form
      String validatorFormat = internalValidator.getClientValidationFormat(
                                                  fContext,
                                                  component);

      // get the format index of this error format in the registered formats
      formatIndex = _addErrorFormat( validatorFormat);
    }
    else
    {
      Object clientDependency = submitValidator.getClientScript(fContext,
                                                                  component);
      if (clientDependency != null)
      {
        List clientDependencies = getClientDependencies(true);
        clientDependencies.add(clientDependency);
      }
    }

    String validator = (String)submitValidator.getClientValidation(
                                                     fContext,
                                                     component);

    if (validator != null)
    {
      _addFormValidatorInfo(validator, formatIndex, convertValidate);
      _addValidatedInput( clientId);
    }
  }


  /**
   * Returns the index of the validation in the list of validations, adding
   * the validation String if it doesn't already exist.
   */
  private Integer _addValidation(
    String  validation
    )
  {
    //TODO - not checking for null so map always getting created
    Map validationMap = _getValidationMap(true);

    Integer validationIndex = (Integer)validationMap.get(validation);

    if (validationIndex == null)
    {
      // the new element was added to the end of our vector
      validationIndex = IntegerUtils.getInteger(validationMap.size());

      // add the new element to our map of strings to indices
      validationMap.put(validation, validationIndex);
    }

    // return the index of this format
    return validationIndex;
  }

  // =-= bts make package private
  private Integer _addErrorFormat(
    String errorFormat
    )
  {
    if (errorFormat != null)
    {
      Map errorFormatMap = _getErrorFormatMap(true);

      Integer errorFormatIndex = (Integer)errorFormatMap.get(errorFormat);

      if (errorFormatIndex == null)
      {
        // the new element was added to the end of our vector
        errorFormatIndex = IntegerUtils.getInteger(errorFormatMap.size());
        // add the new element to our map of strings to indices
        errorFormatMap.put(errorFormat, errorFormatIndex);
      }

      // return the index of this format
      return errorFormatIndex;
    }

    return null;
  }

  /**
   * @todo should we get rid of this ConvertValidate object somehow? It's
   * used in the formRenderer.
   */
  private ConvertValidate _getNewConvertValidate(
    String           clientId
    )
  {
      // create
      ConvertValidate convertValidateInfo = new ConvertValidate();

      // set name
      convertValidateInfo.clientId = clientId;

      // add to list
      List convertValidateList = getFormValidatorsInfo(true);
      convertValidateList.add(convertValidateInfo);

      return convertValidateInfo;
  }



  /**
   * Add a UINode onto the list of UINodes that need to be validated.
   */
  // =-= bts make package private
  private void _addValidatedInput(
    String           clientId
    )
  {

    if (clientId != null)
    {
      getValidatedInputList(true).add(clientId);
    }
  }



  private Map _getValidationMap(
    boolean          createIfNecessary
    )
  {
    if ((_validationMap == null) && createIfNecessary)
    {
      _validationMap = new LinkedHashMap(31);
    }

    return _validationMap;
  }

  private Map _getErrorFormatMap(
    boolean          createIfNecessary
    )
  {
    if ((_errorFormatMap == null) && createIfNecessary)
    {
      _errorFormatMap = new LinkedHashMap(31);
    }

    return _errorFormatMap;
  }


  //*******************************************************
  // static private
  //*******************************************************
  static private String _getRequiredDetailMessage(
     UIComponent      component,
     String           requiredMessageKey)
  {
    if (component == null)
    {
      return null;
    }

    FacesContext fContext = FacesContext.getCurrentInstance();

    String detail = null;
    // it gets replaced in javaScript
    String label = "{0}";

    Object params[] = {label};

    if (component instanceof UIXEditableValue)
    {
      String requiredMessageDetail =
      ((UIXEditableValue) component).getRequiredMessageDetail();
      if (requiredMessageDetail != null)
      {
        FastMessageFormat format =
        new FastMessageFormat(requiredMessageDetail);
        detail = format.format(params);
      }

    }

    if (detail == null)
    {
      if (requiredMessageKey == null)
        requiredMessageKey = UIXEditableValue.REQUIRED_MESSAGE_ID;
      detail = MessageFactory.getMessage(fContext,
                                         requiredMessageKey,
                                         params).getDetail();
    }

    return MessageUtils.createErrorAlertMessage(fContext, label, detail);
  }

 /**
   * Opportunity for the ClientConverter or Validator to write any of its dependencies
   * to the output.  For HTML, this will typically be imports of
   * JavaScript libraries.
   */
  static private void _writeDependencies(
    FacesContext context,
    String      libReference
    ) throws IOException
  {
    String contentType = context.getResponseWriter().getContentType();
    if ("text/html".equals(contentType) ||
        "application/xhtml+xml".equals(contentType) ||
        (null == contentType))
    {
      XhtmlUtils.addLib(context,
                        AdfRenderingContext.getCurrentInstance(),
                        libReference);
    }
  }

  private int _inputTextCount = 0;
  private String _defaultCommandId = null;
  private boolean _hasImmediateComponent = false;

  // map of unique validation string to index in map
  // used so that each converter and validator constructor
  // is only written out once
  private Map _validationMap = null;

  // map of unique error string to index in map
  // used so that each error string is only written out once
  private Map _errorFormatMap = null;

  // List of ConvertValidate objects
  private List _formValidatorsInfo;

  // javascript needed for client validations
  private List _clientDependencies;


  // List of id's of input controls that need to get validated
  private List _validatedInputList;


  private boolean _useCompoundNames = false;

  // List of empty hidden fields that will be filled during event generation
  private List _neededValues;


  private Set _renderedSet;

  // maps labels to id's
  // need a label map because the label is not always an attribute
  // on the component
  private Map _labelMap = null;

  // List of reset calls
  private List _resetCallsList = null;

  // maps patterns to id's
  private Map _patternMap;

  private String _formName = null;

 public static final class ConvertValidate
  {
    public String    clientId;
    public boolean   required = false;
    public Integer   requiredFormatIndex;
    public ArrayList validators;
    public Object[]  converter;
  }


  static private final ADFLogger _LOG = ADFLogger.createADFLogger(FormData.class);
}
