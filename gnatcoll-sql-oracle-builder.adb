with Ada.Calendar;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.Calendar;
with Interfaces.C.Strings;      use Interfaces.C.Strings;

--  with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.SQL.Exec_Private; use GNATCOLL.SQL.Exec_Private;

with GNATCOLL.SQL.Oracle.Gnade; use GNATCOLL.SQL.Oracle.Gnade;

package body GNATCOLL.SQL.Oracle.Builder is
--     Me : constant Trace_Handle := Create ("SQL.ORACLE");

   function Convert is new Ada.Unchecked_Conversion
     (Oracle_Statement, DBMS_Stmt);

   function Convert is new Ada.Unchecked_Conversion
     (DBMS_Stmt, Oracle_Statement);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Oracle_Statement_Record, Oracle_Statement);

   ----------------------------------------------------------------------------
   -- Connection --------------------------------------------------------------
   ----------------------------------------------------------------------------

   type Oracle_Connection_Record is
     new GNATCOLL.SQL.Exec.Database_Connection_Record with
      record
         Oracle       : Database_Access;
         Connected_On : Ada.Calendar.Time := GNAT.Calendar.No_Time;
      end record;
   type Oracle_Connection is access all Oracle_Connection_Record'Class;

   overriding procedure Close
     (Connection : access Oracle_Connection_Record);

   overriding function Parameter_String
     (Self       : Oracle_Connection_Record;
      Index      : Positive;
      Type_Descr : String) return String with Inline;

   overriding function Can_Alter_Table_Constraints
     (Self : access Oracle_Connection_Record) return Boolean;

   overriding function Has_Pragmas
     (Self : access Oracle_Connection_Record) return Boolean;

   overriding function Connect_And_Execute
     (Connection : access Oracle_Connection_Record;
      Is_Select  : Boolean;
      Direct     : Boolean;
      Query      : String         := "";
      Stmt       : DBMS_Stmt      := No_DBMS_Stmt;
      Params     : SQL_Parameters := No_Parameters)
      return Abstract_Cursor_Access;

   overriding function Connected_On
     (Connection : access Oracle_Connection_Record) return Ada.Calendar.Time;

   overriding function Connect_And_Prepare
     (Connection : access Oracle_Connection_Record;
      Query      : String;
      Name       : String;
      Direct     : Boolean) return DBMS_Stmt;

   overriding function Execute
     (Connection : access Oracle_Connection_Record;
      Prepared   : DBMS_Stmt;
      Is_Select  : Boolean;
      Direct     : Boolean;
      Params     : SQL_Parameters := No_Parameters)
      return Abstract_Cursor_Access;

   overriding procedure Force_Connect
     (Connection : access Oracle_Connection_Record);

   overriding procedure Force_Disconnect
     (Connection : access Oracle_Connection_Record);

   overriding function Insert_And_Get_PK
     (Connection : access Oracle_Connection_Record;
      Query      : String;
      Params     : SQL_Parameters := No_Parameters;
      PK         : SQL_Field_Integer) return Integer;

   overriding function Insert_And_Get_PK
     (Connection : access Oracle_Connection_Record;
      Stmt       : Prepared_Statement'Class;
      Params     : SQL_Parameters := No_Parameters;
      PK         : SQL_Field_Integer) return Integer;

   overriding function Field_Type_Autoincrement
     (Self : Oracle_Connection_Record) return String;

   overriding function Field_Type_Money
     (Self : Oracle_Connection_Record) return String;

   overriding function Error
     (Connection : access Oracle_Connection_Record) return String;

   overriding procedure Foreach_Table
     (Connection : access Oracle_Connection_Record;
      Callback   : access procedure
        (Name, Description : String; Kind : Relation_Kind));

   overriding procedure Foreach_Field
     (Connection : access Oracle_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Name           : String;
         Typ            : String;
         Index          : Natural;
         Description    : String;
         Default_Value  : String;
         Is_Primary_Key : Boolean;
         Not_Null       : Boolean));

   overriding procedure Foreach_Foreign_Key
     (Connection : access Oracle_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Index             : Positive;
         Local_Attribute   : Integer;
         Foreign_Table     : String;
         Foreign_Attribute : Integer));

   overriding procedure Finalize
     (Connection : access Oracle_Connection_Record;
      Prepared   : DBMS_Stmt);

   overriding function Is_Prepared_On_Server_Supported
     (Connection : access Oracle_Connection_Record) return Boolean;

   ----------------------------------------------------------------------------
   --  Cursor -----------------------------------------------------------------
   ----------------------------------------------------------------------------

   type Oracle_Cursor is new DBMS_Forward_Cursor with record
      Connection     : access Oracle_Connection_Record'Class;
      Stmt           : Oracle.Gnade.Oracle_Statement;

      Free_Stmt      : Boolean := True;
      --  Whether the statement needs to be finalized. This will be false for
      --  a statement prepared explicitly by the user on the server. In this
      --  case, the statement will be reset instead.

      Processed_Rows : Natural := 0;
      Has_Row        : Boolean := False;
   end record;
   type Oracle_Cursor_Access is access all Oracle_Cursor'Class;

   overriding function Current (Self : Oracle_Cursor) return Positive;

   overriding function Error_Msg (Self : Oracle_Cursor) return String;

   overriding function Status (Self : Oracle_Cursor) return String;

   overriding function Is_Success (Self : Oracle_Cursor) return Boolean;

   overriding procedure Finalize (Self : in out Oracle_Cursor);

   overriding function Processed_Rows (Self : Oracle_Cursor) return Natural;

   overriding function C_Value
     (Self  : Oracle_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return chars_ptr;

   overriding function Value
     (Self  : Oracle_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return String;

   overriding function Is_Null
     (Self  : Oracle_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return Boolean;

   overriding function Last_Id
     (Self       : Oracle_Cursor;
      Connection : access Database_Connection_Record'Class;
      Field      : SQL_Field_Integer) return Integer;

   overriding function Field_Count
     (Self : Oracle_Cursor) return GNATCOLL.SQL.Exec.Field_Index;

   overriding function Field_Name
     (Self  : Oracle_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return String;

   overriding function Has_Row (Self : Oracle_Cursor) return Boolean;

   overriding procedure Next (Self : in out Oracle_Cursor);

   overriding function Boolean_Value
     (Self : Oracle_Cursor; Field : Field_Index) return Boolean;

   overriding function Time_Value
     (Self : Oracle_Cursor; Field : Field_Index) return Ada.Calendar.Time;

   ----------------------------------------------------------------------------
   -- Connection implementation -----------------------------------------------
   ----------------------------------------------------------------------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Oracle.Gnade.Database_Record, Oracle.Gnade.Database_Access);

   ----------------------
   -- Build_Connection --
   ----------------------

   function Build_Connection
     (Descr : access Oracle_Description'Class) return Database_Connection
   is
      Conn : constant Oracle_Connection := new Oracle_Connection_Record
        (Descr, Always_Use_Transactions => False);
   begin
      Conn.Oracle := new Oracle.Gnade.Database_Record
        (Oracle_Description_Access (Conn.Descr));
      Conn.Connected_On := Ada.Calendar.Clock;
      return Database_Connection (Conn);
   end Build_Connection;

   -----------
   -- Error --
   -----------

   overriding function Error
     (Connection : access Oracle_Connection_Record) return String is
   begin
      if Connection.Oracle = null then
         return "No connection to database";
      else
         return Oracle.Gnade.Error_Msg (Connection.Oracle.all);
      end if;
   end Error;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (Connection : access Oracle_Connection_Record) is
   begin
      if Connection /= null then
         Unchecked_Free (Connection.Oracle);
         Connection.Oracle := null;
      end if;
   end Close;

   -------------------
   -- Force_Connect --
   -------------------

   overriding procedure Force_Connect
     (Connection : access Oracle_Connection_Record)
   is
      Descr : constant Oracle_Description_Access :=
        Oracle_Description_Access (Get_Description (Connection));
   begin
      if Connection.Oracle = null then
         Print_Warning
           (Connection,
            "Connecting to the database " & Get_Connection_Descriptor (Descr));
         Connection.Oracle := new Oracle.Gnade.Database_Record (Descr);
         Connection.Connected_On := Ada.Calendar.Clock;
      else
         Print_Warning
           (Connection,
            "Reconnecting to the database "
            & Get_Connection_Descriptor (Descr));
         Oracle.Gnade.Reset (Connection.Oracle.all);
         Connection.Connected_On := Ada.Calendar.Clock;
      end if;
   end Force_Connect;

   ----------------------
   -- Force_Disconnect --
   ----------------------

   overriding procedure Force_Disconnect
     (Connection : access Oracle_Connection_Record)
   is
   begin
      if Connection.Oracle = null then
         Print_Warning
           (Connection, "Can't disconnect null connection");
         return;
      end if;

      Oracle.Gnade.Logoff (Connection.Oracle.all);
   end Force_Disconnect;

   ------------------
   -- Connected_On --
   ------------------

   overriding function Connected_On
     (Connection : access Oracle_Connection_Record) return Ada.Calendar.Time
   is
   begin
      return Connection.Connected_On;
   end Connected_On;

   -------------------------
   -- Connect_And_Prepare --
   -------------------------

   overriding function Connect_And_Prepare
     (Connection : access Oracle_Connection_Record;
      Query      : String;
      Name       : String;
      Direct     : Boolean) return DBMS_Stmt
   is
      pragma Unreferenced (Name, Direct);
      Stmt : Oracle_Statement;
   begin
      if Connection.Oracle = null or else not Connection.Oracle.Is_Connected
      then
         Connection.Force_Connect;
      end if;

      if not Connection.Oracle.Is_Connected then
         return No_DBMS_Stmt;
      end if;

      if Query = "" then
         return No_DBMS_Stmt;
      end if;

      if Equal (S1 => Query, S2 => "BEGIN", Case_Sensitive => False)
      then
         return Convert
           (Oracle.Gnade.Prepare (Connection.Oracle.all, "COMMIT"));
      end if;

      Stmt := Oracle.Gnade.Prepare (Connection.Oracle.all, Query);
      return Convert (Stmt);
   end Connect_And_Prepare;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Connection : access Oracle_Connection_Record;
      Prepared   : DBMS_Stmt;
      Is_Select  : Boolean;
      Direct     : Boolean;
      Params     : SQL_Parameters := No_Parameters)
      return Abstract_Cursor_Access
   is
      pragma Unreferenced (Direct);
      Stmt : constant Oracle_Statement := Convert (Prepared);
      Res  : Oracle_Cursor_Access;
   begin
      Oracle.Gnade.Execute (Stmt, Params);
      Res            := new Oracle_Cursor;
      Res.Connection := Oracle_Connection (Connection);
      Res.Stmt       := Stmt;
      Next (Res.all);  --  Read first row

      if not Is_Select then
         Res.Processed_Rows := Oracle.Gnade.Processed_Rows (Stmt);
      end if;

      return Abstract_Cursor_Access (Res);
   end Execute;

   -----------------------
   -- Insert_And_Get_PK --
   -----------------------

   overriding function Insert_And_Get_PK
     (Connection : access Oracle_Connection_Record;
      Query      : String;
      Params     : SQL_Parameters := No_Parameters;
      PK         : SQL_Field_Integer) return Integer
   is
   begin
      return -1;
   end Insert_And_Get_PK;

   -----------------------
   -- Insert_And_Get_PK --
   -----------------------

   overriding function Insert_And_Get_PK
     (Connection : access Oracle_Connection_Record;
      Stmt       : Prepared_Statement'Class;
      Params     : SQL_Parameters := No_Parameters;
      PK         : SQL_Field_Integer) return Integer
   is
      Str : constant String := To_String (Connection, Stmt);
   begin
   --  We cannot use the prepared statement here, since we need to modify
   --  it on the fly to add a " RETURNING " suffix
      return Insert_And_Get_PK (Connection, Str, Params, PK);
   end Insert_And_Get_PK;

   -------------------------
   -- Connect_And_Execute --
   -------------------------

   overriding function Connect_And_Execute
     (Connection : access Oracle_Connection_Record;
      Is_Select  : Boolean;
      Direct     : Boolean;
      Query      : String         := "";
      Stmt       : DBMS_Stmt      := No_DBMS_Stmt;
      Params     : SQL_Parameters := No_Parameters)
      return Abstract_Cursor_Access
   is
      Res    : Abstract_Cursor_Access := null;
      P_Stmt : DBMS_Stmt := Stmt;
   begin
      if Stmt = No_DBMS_Stmt then
         P_Stmt := Connect_And_Prepare (Connection, Query, "", Direct);
      else
         P_Stmt := Stmt;
      end if;

      if P_Stmt /= No_DBMS_Stmt then
         Res := Execute
           (Connection => Connection,
            Prepared   => P_Stmt,
            Is_Select  => Is_Select,
            Direct     => Direct,
            Params     => Params);

         if Res /= null then
            if Res.all in Oracle_Cursor'Class then
               Oracle_Cursor_Access (Res).Free_Stmt := Stmt = No_DBMS_Stmt;
            end if;
         elsif Stmt = No_DBMS_Stmt then
            Finalize (Connection, P_Stmt);
         end if;
      end if;

      return Res;
   end Connect_And_Execute;

   -------------------
   -- Foreach_Table --
   -------------------

   overriding procedure Foreach_Table
     (Connection : access Oracle_Connection_Record;
      Callback   : access procedure
        (Name, Description : String; Kind : Relation_Kind))
   is
      R : Forward_Cursor;
   begin
      R.Fetch
        (Connection,
         "SELECT OBJECT_NAME, OBJECT_TYPE FROM USER_OBJECTS "
         & "WHERE OBJECT_TYPE IN ('TABLE', 'VIEW')");

      while R.Has_Row loop
         Callback
           (Name        => R.Value (0),
            Description => "",
            Kind        =>
              (if R.Value (1) = "TABLE" then Kind_Table else Kind_View));
      end loop;
   end Foreach_Table;

   -------------------
   -- Foreach_Field --
   -------------------

   overriding procedure Foreach_Field
     (Connection : access Oracle_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Name           : String;
         Typ            : String;
         Index          : Natural;
         Description    : String;
         Default_Value  : String;
         Is_Primary_Key : Boolean;
         Not_Null       : Boolean))
   is
      R : Forward_Cursor;
   begin
      R.Fetch
        (Connection,
         "SELECT tcol.TABLE_NAME, tcol.COLUMN_NAME, tcol.DATA_TYPE, "
         & "     tcol.COLUMN_ID, tcol.NULLABLE, tcol.DATA_DEFAULT, "
         & "     cc.CONSTRAINT_TYPE "
         & "FROM "
         & "   USER_TAB_COLUMNS tcol, "
         & "   (SELECT tc.TABLE_NAME, tc.COLUMN_NAME, cons.CONSTRAINT_TYPE "
         & "       FROM USER_CONS_COLUMNS tcons, USER_CONSTRAINTS cons "
         & "          WHERE "
         & "             tcons.CONSTRAINT_NAME = cons.CONSTRAINT_NAME "
         & "             AND cons.CONSTRAINT_TYPE = 'P') cc "
         & "WHERE "
         & "   tcol.TABLE_NAME = UPPER(" & Table_Name & ") "
         & "   AND tcol.TABLE_NAME = cc.TABLE_NAME(+) "
         & "   AND tcol.COLUMN_NAME = cc.COLUMN_NAME(+)");

      while R.Has_Row loop
         Callback
           (Name           => R.Value (1),
            Typ            => R.Value (2),
            Index          => Natural'Value (R.Value (3)),
            Description    => "",
            Default_Value  => R.Value (5),
            Is_Primary_Key => R.Value (6) = "P",
            Not_Null       => R.Value (4) = "N");
         R.Next;
      end loop;
   end Foreach_Field;

   -------------------------
   -- Foreach_Foreign_Key --
   -------------------------

   procedure Foreach_Foreign_Key
     (Connection : access Oracle_Connection_Record;
      Table_Name : String;
      Callback   : access procedure
        (Index             : Positive;
         Local_Attribute   : Integer;
         Foreign_Table     : String;
         Foreign_Attribute : Integer))
   is
   begin
      null;
   end Foreach_Foreign_Key;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Connection : access Oracle_Connection_Record;
      Prepared   : DBMS_Stmt)
   is
      pragma Unreferenced (Connection);
      Stmt : Oracle_Statement := Convert (Prepared);
   begin
      if Stmt /= null then
         Oracle.Gnade.Finalize (Stmt);
         Unchecked_Free (Stmt);
      end if;
   end Finalize;

   ------------------------------
   -- Field_Type_Autoincrement --
   ------------------------------

   overriding function Field_Type_Autoincrement
     (Self : Oracle_Connection_Record) return String
   is
      pragma Unreferenced (Self);
   begin
      return "NUMBER";
   end Field_Type_Autoincrement;

   ------------------------------
   -- Field_Type_Money --
   ------------------------------

   overriding function Field_Type_Money
     (Self : Oracle_Connection_Record) return String
   is
      pragma Unreferenced (Self);
   begin
      return "NUMBER (" & K_Digits'Img & "," & K_Decimals'Img & ")";
   end Field_Type_Money;

   ------------------------
   -- Has_Oracle_Support --
   ------------------------

   function Has_Oracle_Support return Boolean is
   begin
      return True;
   end Has_Oracle_Support;

   ----------------------
   -- Parameter_String --
   ----------------------

   overriding function Parameter_String
     (Self       : Oracle_Connection_Record;
      Index      : Positive;
      Type_Descr : String) return String
   is
      pragma Unreferenced (Self, Type_Descr);
   begin
      return ':' & Image (Index, 0);
   end Parameter_String;

   ---------------------------------
   -- Can_Alter_Table_Constraints --
   ---------------------------------

   overriding function Can_Alter_Table_Constraints
     (Self : access Oracle_Connection_Record) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return True;
   end Can_Alter_Table_Constraints;

   -----------------
   -- Has_Pragmas --
   -----------------

   overriding function Has_Pragmas
     (Self : access Oracle_Connection_Record) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Has_Pragmas;

   --------------------
   -- Server_Version --
   --------------------

   function Server_Version (Self : Database_Connection) return String is
      Ora_Conn : constant Oracle_Connection := Oracle_Connection (Self);
   begin
      if Ora_Conn.Oracle = null then
         return "No connection to database";
      else
         return Oracle.Gnade.Server_Version (Ora_Conn.Oracle.all);
      end if;
   end Server_Version;

   -------------------------------------
   -- Is_Prepared_On_Server_Supported --
   -------------------------------------

   overriding function Is_Prepared_On_Server_Supported
     (Connection : access Oracle_Connection_Record) return Boolean
   is
      pragma Unreferenced (Connection);
   begin
      return False;
   end Is_Prepared_On_Server_Supported;

   ----------------------------------------------------------------------------
   --  Cursor implementation --------------------------------------------------
   ----------------------------------------------------------------------------

   -------------
   -- Current --
   -------------

   overriding function Current (Self : Oracle_Cursor) return Positive is
   begin
      return Self.Processed_Rows;
   end Current;

   ---------------
   -- Error_Msg --
   ---------------

   overriding function Error_Msg (Self : Oracle_Cursor) return String is
   begin
      if Self.Connection = null or else Self.Connection.Oracle = null then
         return "No connection to database";
      else
         return Oracle.Gnade.Error_Msg (Self.Stmt);
      end if;
   end Error_Msg;

   ------------
   -- Status --
   ------------

   overriding function Status (Self : Oracle_Cursor) return String is
   begin
      return Oracle.Gnade.Error_Msg (Self.Stmt);
   end Status;

   ----------------
   -- Is_Success --
   ----------------

   overriding function Is_Success (Self : Oracle_Cursor) return Boolean is
   begin
      return Oracle.Gnade.Is_Success (Self.Stmt);
   end Is_Success;

   --------------------
   -- Processed_Rows --
   --------------------

   overriding function Processed_Rows (Self : Oracle_Cursor) return Natural is
   begin
      return Self.Processed_Rows;
   end Processed_Rows;

   -------------
   -- C_Value --
   -------------

   overriding function C_Value
     (Self  : Oracle_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return chars_ptr
   is
   begin
      return Oracle.Gnade.C_Value (Self.Stmt, Positive (Field + 1));
   end C_Value;

   -----------
   -- Value --
   -----------

   overriding function Value
     (Self  : Oracle_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return String
   is
   begin
      return Oracle.Gnade.Value (Self.Stmt, Positive (Field + 1));
   end Value;

   -------------
   -- Is_Null --
   -------------

   overriding function Is_Null
     (Self  : Oracle_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return Boolean
   is
   begin
      return Oracle.Gnade.Is_Null (Self.Stmt, Positive (Field + 1));
   end Is_Null;

   -------------
   -- Last_Id --
   -------------

   overriding function Last_Id
     (Self       : Oracle_Cursor;
      Connection : access Database_Connection_Record'Class;
      Field      : SQL_Field_Integer) return Integer
   is
      pragma Unreferenced (Self, Connection, Field);
   begin
      --  Automatically incremented column (IDENTITY) is available on
      --  the Oracle 12.1 and above.
      return -1;
   end Last_Id;

   -----------------
   -- Field_Count --
   -----------------

   overriding function Field_Count
     (Self : Oracle_Cursor) return GNATCOLL.SQL.Exec.Field_Index
   is
   begin
      return GNATCOLL.SQL.Exec.Field_Index
        (Oracle.Gnade.Field_Count (Self.Stmt));
   end Field_Count;

   ----------------
   -- Field_Name --
   ----------------

   overriding function Field_Name
     (Self  : Oracle_Cursor;
      Field : GNATCOLL.SQL.Exec.Field_Index) return String
   is
   begin
      return Oracle.Gnade.Field_Name (Self.Stmt, Positive (Field + 1));
   end Field_Name;

   -------------
   -- Has_Row --
   -------------

   overriding function Has_Row (Self : Oracle_Cursor) return Boolean is
   begin
      return Self.Has_Row;
   end Has_Row;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Self : in out Oracle_Cursor) is
   begin
      Self.Has_Row := Oracle.Gnade.Fetch (Self.Stmt);

      if Self.Has_Row then
         Self.Processed_Rows := Self.Processed_Rows + 1;
      end if;
   end Next;

   -------------------
   -- Boolean_Value --
   -------------------

   overriding function Boolean_Value
     (Self : Oracle_Cursor; Field : Field_Index) return Boolean
   is
   begin
      --  Field type must be integer
      return Self.Value (Field) /= "0";
   end Boolean_Value;

   ----------------
   -- Time_Value --
   ----------------

   overriding function Time_Value
     (Self : Oracle_Cursor; Field : Field_Index) return Ada.Calendar.Time
   is
   begin
      return Oracle.Gnade.Time_Value (Self.Stmt, Positive (Field + 1));
   end Time_Value;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Oracle_Cursor) is
   begin
      if Self.Stmt /= null then
         if Self.Free_Stmt then
            Finalize (Self.Stmt);
         end if;
      end if;
   end Finalize;

   ----------------------------------------------------------------------------
   -- Other functions ---------------------------------------------------------
   ----------------------------------------------------------------------------

   --------------------
   -- Client_Version --
   --------------------

   function Client_Version return String is
   begin
      return Oracle.Gnade.Client_Version;
   end Client_Version;

end GNATCOLL.SQL.Oracle.Builder;
