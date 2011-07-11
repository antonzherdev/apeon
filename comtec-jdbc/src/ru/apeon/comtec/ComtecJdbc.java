package ru.apeon.comtec;

import com.sybase.jdbc3.jdbc.SybDataSource;

import javax.sql.DataSource;
import java.io.PrintWriter;
import java.io.Serializable;
import java.sql.*;
import java.util.Properties;

/**
 * @author Anton Zherdev
 */

public class ComtecJdbc implements Driver, /*Referenceable, */Serializable, DataSource {
    SybDataSource ds = new SybDataSource();
    static {
        ComtecJdbc driverInst = new ComtecJdbc();
        try {
            DriverManager.registerDriver(driverInst);
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }
    public ComtecJdbc() {
    }

    public boolean acceptsURL(String url) throws SQLException {
        return url.startsWith("jdbc:comtec:");
    }

    public Connection connect(String url, Properties info) throws SQLException {
        return prep(ds.connect(corr(url), info));
    }

    private String corr(String url) {
        return url.replace(":comtec:", ":sybase:");
    }

    public DriverPropertyInfo[] getPropertyInfo(String url, Properties info) throws SQLException {
        return ds.getPropertyInfo(corr(url), info);
    }

    public int getMajorVersion() {
        return ds.getMajorVersion();
    }

    public int getMinorVersion() {
        return ds.getMinorVersion();
    }

    public boolean jdbcCompliant() {
        return ds.jdbcCompliant();
    }

    private Connection prep(Connection connection) throws SQLException {
        Statement statement = connection.createStatement();
        statement.execute("SET TEMPORARY OPTION CONNECTION_AUTHENTICATION='Company=Comtec Ltd;Application=Comtec for Business;Signature=000fa55157edb8e14d818eb4fe3db41447146f1571g50d0fe1fd35884b6336b6950a87abd70376da7e6'");
        statement.close();
        return connection;
    }

   /* public Reference getReference() throws NamingException {
        return ds.getReference();
    }*/

    public Connection getConnection() throws SQLException {
        return prep(ds.getConnection());
    }

    public Connection getConnection(String username, String password) throws SQLException {
        return prep(ds.getConnection(username, password));
    }

    public PrintWriter getLogWriter() throws SQLException {
        return ds.getLogWriter();
    }

    public void setLogWriter(PrintWriter out) throws SQLException {
        ds.setLogWriter(out);
    }

    public void setLoginTimeout(int seconds) throws SQLException {
        ds.setLoginTimeout(seconds);
    }

    public int getLoginTimeout() throws SQLException {
        return ds.getLoginTimeout();
    }

    public <T> T unwrap(Class<T> iface) throws SQLException {
        return null;
    }

    public boolean isWrapperFor(Class<?> iface) throws SQLException {
        return false;
    }
}
