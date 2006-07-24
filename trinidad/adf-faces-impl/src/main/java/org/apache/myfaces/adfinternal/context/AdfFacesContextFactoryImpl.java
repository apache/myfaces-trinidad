/*
 * Copyright  2004-2006 The Apache Software Foundation.
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
package org.apache.myfaces.adfinternal.context;

import java.util.List;

import javax.servlet.ServletContext;

import org.apache.myfaces.adf.context.AdfFacesContext;
import org.apache.myfaces.adf.context.AdfFacesContextFactory;
import org.apache.myfaces.adf.context.PageFlowScopeProvider;
import org.apache.myfaces.adf.context.PageResolver;

import org.apache.myfaces.adf.util.ClassLoaderUtils;

import org.apache.myfaces.adfinternal.webapp.ConfigParser;

/**
 * @author The Oracle ADF Faces Team
 */
public class AdfFacesContextFactoryImpl extends AdfFacesContextFactory
{
  public AdfFacesContextFactoryImpl()
  {
  }

  public AdfFacesContext createContext(Object context,
                                       Object request)
  {
    AdfFacesContextImpl impl =  new AdfFacesContextImpl(_getBean(context));
    impl.init(request);
    impl.__setPageResolver(_pageResolver);
    impl.__setPageFlowScopeProvider(_pageFlowScopeProvider);
    return impl;
  }

  private AdfFacesContextBean _getBean(Object context)
  {
    if (_bean == null)
    {
      synchronized (this)
      {
        if (context instanceof ServletContext)
        {
          _bean = ConfigParser.parseConfigFile((ServletContext) context);
        }
        else
        {
          _bean = new AdfFacesContextBean();
        }

        // And let's load the PageResolver and PageFlowScopeProvider
        // while we're in here.
        {
          List<PageResolver> list = ClassLoaderUtils.getServices(_PAGE_RESOLVER_URL);
          _pageResolver = list.isEmpty() ? null : list.get(0);
        }
        if (_pageResolver == null)
        {
          _pageResolver = PageResolverDefaultImpl.sharedInstance();
        }

        {
          List<PageFlowScopeProvider> list = 
            ClassLoaderUtils.getServices(_PAGE_FLOW_SCOPE_PROVIDER_URL);
          _pageFlowScopeProvider = list.isEmpty() ? null : list.get(0);
        }
        if (_pageFlowScopeProvider == null)
        {
          _pageFlowScopeProvider = PageFlowScopeProviderImpl.sharedInstance();
        }
      }
    }

    return _bean;
  }

  private AdfFacesContextBean _bean;
  private PageResolver        _pageResolver;
  private PageFlowScopeProvider _pageFlowScopeProvider;

  static private final String _PAGE_RESOLVER_URL =
    "org.apache.myfaces.adf.context.PageResolver";
  static private final String _PAGE_FLOW_SCOPE_PROVIDER_URL =
    "org.apache.myfaces.adf.context.PageFlowScopeProvider";
}
