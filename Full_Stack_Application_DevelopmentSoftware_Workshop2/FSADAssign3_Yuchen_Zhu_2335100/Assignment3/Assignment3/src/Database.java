import java.sql.*;

public class Database {

    //This method executes a query and returns the number of albums for the artist with name artistName
    public int getTitles(String artistName) {
        int titleNum = 0;
        try {
            Class.forName("org.postgresql.Driver");
            Connection con = DriverManager.getConnection(Credentials.URL, Credentials.USERNAME, Credentials.PASSWORD);
            String sql = ("SELECT COUNT(title)\n" +
                    "FROM album\n" +
                    "INNER JOIN artist\n" +
                    "ON album.artistid=artist.artistid\n" +
                    "WHERE name = ?;");
            PreparedStatement pstmt = con.prepareStatement(sql);
            pstmt.clearParameters();
            pstmt.setString(1, artistName);
            ResultSet rs = pstmt.executeQuery();
            while (rs.next()) {
                titleNum = rs.getInt("count");
            }
            pstmt.close();
            con.close();
            rs.close();
        } catch (Exception e) {
            System.out.println(e);
        }
        //Implement this method
        //Prevent SQL injections!
        return titleNum;
    }

    // This method establishes a DB connection & returns a boolean status
    public boolean establishDBConnection() {
        try {
            Class.forName("org.postgresql.Driver");
            Connection con = DriverManager.getConnection(Credentials.URL, Credentials.USERNAME, Credentials.PASSWORD);
            return con.isValid(5);
        } catch (SQLException e) {
            e.printStackTrace();
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
        //Implement this method
        //5 sec timeout
        return false;
    }
}