--  Implementation of gnatcoll-sql-exec_private for Oracle.
--  This isn't in GNATCOLL.SQL.Oracle so that GNATCOLL can have the same API
--  no matter whether Oracle is installed on the machine or not

private package GNATCOLL.SQL.Oracle.Builder is

   function Has_Oracle_Support return Boolean;
   --  Whether Oracle is supported

   function Build_Connection
     (Descr : access Oracle_Description'Class) return Database_Connection;

   function Server_Version (Self : Database_Connection) return String;

   function Client_Version return String;

end GNATCOLL.SQL.Oracle.Builder;
