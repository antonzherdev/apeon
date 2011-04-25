<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ taglib prefix='c' uri='http://java.sun.com/jstl/core_rt' %>
<%@ page import="org.springframework.security.web.authentication.AbstractAuthenticationProcessingFilter" %>
<%@ page import="org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter" %>
<%@ page import="org.springframework.security.core.AuthenticationException" %>
<html>
<head>
    <title>Аутентификация</title>
    <link rel="stylesheet" type="text/css" href="./VAADIN/themes/comtec/styles.css">
    <link rel="icon" type="image/vnd.microsoft.icon" href="./VAADIN/themes/comtec/favicon.ico" />
</head>
<body>
<div class="v-app">
    <table>
        <tr>
            <td valign="top" width="80">
                <img src="./VAADIN/themes/comtec/comtec64.png" alt="comtec logo" width="64" height="64">
            </td>
            <td>
                <h1>Аутентификация</h1>
                <c:if test="${not empty param.login_error}">
                    <font color="red">
                        Your login attempt was not successful, try again.<br/><br/>
                        Reason: <c:out value="${SPRING_SECURITY_LAST_EXCEPTION.message}"/>.
                    </font>
                </c:if>
                <div class="v-form-layout">
                    <form name="f" action="<c:url value='j_spring_security_check'/>" method="POST">
                        <table class="v-formlayout-margin-top v-formlayout-margin-right v-formlayout-margin-bottom v-formlayout-margin-left v-formlayout-spacing">
                            <tr class="v-formlayout-row"><td class="v-formlayout-captioncell">Имя пользователя:</td><td class="v-formlayout-contentcell"><input type='text' name='j_username' value='<c:if test="${not empty param.login_error}"><c:out value="${SPRING_SECURITY_LAST_USERNAME}"/></c:if>'/></td></tr>
                            <tr class="v-formlayout-row"><td class="v-formlayout-captioncell">Пароль:</td><td class="v-formlayout-contentcell"><input type='password' name='j_password'></td></tr>
                            <tr><td></td><td class="v-formlayout-contentcell" align="right">
                                <div class="v-checkbox">
                                    <input type="checkbox" name="_spring_security_remember_me" id="remember">
                                    <label for="remember">Запомнить</label>
                                </div>
                            </td></tr>
                            <tr><td></td><td align="right">
                                <input name="OK" type="submit" value="Вход" style="margin-right : 16px;">
                            </td></tr>
                        </table>
                    </form>
                </div>
            </td>
        </tr>
    </table>

</div>
</body>

</html>
