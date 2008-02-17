<%@ page session="false" contentType="text/html;charset=utf-8"%>
<%@ taglib uri="http://java.sun.com/jsf/html" prefix="h"%>
<%@ taglib uri="http://java.sun.com/jsf/core" prefix="f"%>

<html>

<head>
    <title>TableFormLayout Component for Trinidad</title>
</head>

<body>

<f:view>
    <h:form>

        <h:panelGrid>

            <h:outputText value="TableFormLayout Component for Trinidad" />
            <h:panelGrid style="padding-left:25px">
                <h:outputLink value="faces/example0.jspx">
                    <f:verbatim>panelFormLayout - example 0 - form without tableFormLayout</f:verbatim>
                </h:outputLink>            
                <h:outputLink value="faces/example1.jspx">
                    <f:verbatim>tableFormLayout - example 1 - common form fixed width</f:verbatim>
                </h:outputLink>
                <h:outputLink value="faces/example11.jspx">
                    <f:verbatim>tableFormLayout - example 1.1 - common form fixed width absolute columns</f:verbatim>
                </h:outputLink>
                
                <h:outputLink value="faces/example12.jspx">
                    <f:verbatim>tableFormLayout - example 1.2 - common form relative width</f:verbatim>
                </h:outputLink>
                
                <h:outputLink value="faces/example2.jspx">
                    <f:verbatim>tableFormLayout - example 2</f:verbatim>
                </h:outputLink>                                
                <h:outputLink value="faces/example3.jspx">
                    <f:verbatim>tableFormLayout - example 3</f:verbatim>
                </h:outputLink>                                
                <h:outputLink value="faces/example4.jspx">
                    <f:verbatim>tableFormLayout - example 4</f:verbatim>
                </h:outputLink>                                
                
            </h:panelGrid>

        </h:panelGrid>

    </h:form>
</f:view>
</body>
</html>
