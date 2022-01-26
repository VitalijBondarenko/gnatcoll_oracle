--  This package instantiates the GNATCOLL.SQL hierarchy for the Oracle DBMS

with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;
with GNATCOLL.Strings;  use GNATCOLL.Strings;

package GNATCOLL.SQL.Oracle is

   type Oracle_Description (<>) is
     new Database_Description_Record with private;
   type Oracle_Description_Access is access all Oracle_Description'Class;

   overriding function Build_Connection
     (Self : access Oracle_Description) return Database_Connection;

   type SSL_Mode is (SSL_Disable, SSL_Require);
   --  Whether to use SSL to connect to the server. This might not be
   --  applicable to all backends (for instance it doesn't apply to sqlite),
   --  and even if the backend supports SSL, some of the modes might not exist.
   --  SSL_Disable => require a non-SSL connection
   --  SSL_Require => require a SSL connection

   function Setup
     (Database      : String;
      User          : String := "";
      Password      : String := "";
      Host          : String := "";
      Port          : Integer := -1;
      SSL           : SSL_Mode := SSL_Disable;
      Cache_Support : Boolean := True;
      Errors        : access Error_Reporter'Class := null)
      return Database_Description;
   --  Return a database connection for Oracle.
   --  If Oracle was not detected at installation time, this function will
   --  return null.
   --  Errors (if specified) will be used to report errors and warnings to the
   --  application. Errors is never freed.

   function Get_Connection_Descriptor
     (Description : access Database_Description_Record'Class) return String;
   --  Create a connection descriptor from the database description

   function Get_Current_Schema (Database : Database_Connection) return String;

   function Set_Current_Schema
     (Database : Database_Connection;
      Schema   : String) return Boolean;

   function Server_Version (Self : Database_Connection) return String;

   function Client_Version return String;

   ----------------------------------------------------------------------------
   -- Oracle specific parameters support --------------------------------------
   ----------------------------------------------------------------------------

   --  function As_Date (Value : Ada.Calendar.Time) return SQL_Parameter;

   ----------------------------------------------------------------------------
   -- 'tnsnames.ora' file functions -------------------------------------------
   ----------------------------------------------------------------------------

   package Names_Vector is
     new Ada.Containers.Vectors (Natural, GNATCOLL.Strings.XString);

   subtype Names_List is Names_Vector.Vector;

   Empty_Names_List : constant Names_List;

   function Find_Tnsnames_File return String;
   --  Returns the tnsnames.ora file as an absolute path name.
   --  This tnsnames.ora file is a configuration file that contains net
   --  service names mapped to connect descriptors for the local naming method,
   --  or net service names mapped to listener protocol addresses.
   --
   --  The order checking the tnsnames.ora file is as follows:
   --
   --  1. The directory specified by the TNS_ADMIN environment variable. If the
   --     file is not found in the directory specified, then it is assumed that
   --     the file does not exist.
   --
   --  2. If the TNS_ADMIN environment variable is not set, then Oracle Net
   --     will check the ORACLE_HOME/network/admin directory.
   --
   --  Note:
   --  On Microsoft Windows, the TNS_ADMIN environment variable is used if it
   --  is set in the environment of the process. If the TNS_ADMIN environment
   --  variable is not defined in the environment, or the process is a service
   --  which does not have an environment, then Microsoft Windows scans the
   --  registry for a TNS_ADMIN parameter.
   --
   --  For UNIX users, the order in which Oracle searches locations for Oracle
   --  Net files like sqlnet.ora and tnsnames.ora are in this order:
   --  1. $HOME for hidden files only (i.e., .sqlnet.ora and .tnsnames.ora)
   --  2. $TNS_ADMIN
   --  3. $HOME
   --  4. /etc or /var/opt/oracle (depends on platform)
   --  5. $ORACLE_HOME/network/admin
   --
   --  For Windows users, the search order is a bit different.
   --  It goes like this:
   --  1. Current path (associated with the running client application)
   --  2. Environment variable TNS_ADMIN defined for the session
   --  3. Environment variable TNS_ADMIN defined for the system
   --  4. Windows Registry Key TNS_ADMIN
   --  5. %ORACLE_HOME%\network\admin
   --
   --  Returns the tnsnames.ora file as an absolute path name. If tnsnames.ora
   --  file not found, Oracle_Find_Tnsnames_File returns an empty string.

   function Get_Service_Names (Tnsfile : String) return Names_List;
   --  Returns the list containing a service names defined in the Tnsfile.

private

   type Oracle_Description is new Database_Description_Record with record
      Host     : GNATCOLL.Strings.XString;
      Dbname   : GNATCOLL.Strings.XString;
      User     : GNATCOLL.Strings.XString;
      Password : GNATCOLL.Strings.XString;
      Port     : Integer := -1;
      SSL      : SSL_Mode := SSL_Disable;
   end record;

   Empty_Names_List : constant Names_List := Names_Vector.Empty_Vector;

end GNATCOLL.SQL.Oracle;
