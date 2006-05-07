/*
* Copyright 2006 The Apache Software Foundation.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.apache.myfaces.adfbuild.plugin.mock.maker;

import java.io.IOException;
import java.io.InputStream;

import java.util.Properties;

public class RealConfiguration implements Configuration
{
    public static final String DEFAULT_CONFIGURATION_FILENAME = "mockmaker.cfg";

    private Properties myProperties;

    public RealConfiguration( String configFileName )
    {
        super();
        myProperties = new Properties();
        InputStream configPropertyFile = ClassLoader.getSystemResourceAsStream( configFileName );
        if ( configPropertyFile != null )
        {
            try
            {
                myProperties.load( configPropertyFile );
            }
            catch ( IOException ex )
            {
                throw new RuntimeException( "Could not read property file: " + ex );
            }
        }
    }

    public String addExpectedValuesFormat()
    {
        return stringProperty( "addExpectedValuesFormat", "addExpected{0}Values" );
    }

    private boolean isTrue( String value )
    {
        return value != null && value.toLowerCase().equals( "true" );
    }

    private boolean isTrueProperty( String propertyName )
    {
        return isTrue( myProperties.getProperty( propertyName ) );
    }

    public boolean keepUsingLastReturnValue()
    {
        return isTrueProperty( "keepUsingLastReturnValue" );
    }

    public String setActualReturnValueFormat()
    {
        return stringProperty( "setActualReturnValueFormat", "setup{0}" );
    }

    public String setExpectedCallsFormat()
    {
        return stringProperty( "setExpectedCallsFormat", "setExpected{0}Calls" );
    }

    private String stringProperty( String propertyName, String defaultValue )
    {
        String result = myProperties.getProperty( propertyName );
        if ( result == null )
        {
            return defaultValue;
        }
        return result;
    }

    public String setActualReturnExceptionFormat()
    {
        return stringProperty( "setActualReturnExceptionFormat", "setupException{0}" );
    }

    public String packageNameFormat()
    {
        return stringProperty( "packageNameFormat", "{0}" );
    }

    public String classNameFormat()
    {
        return stringProperty( "classNameFormat", "Mock{0}" );
    }

    public void set(String propertyName, String newValue)
    {
        myProperties.setProperty(propertyName, newValue);
    }
}
