--  It contains the low-level binding to the Oracle library, and is not
--  recommended for end users (the higher-level API in GNATCOLL.SQL is easier
--  to use, more portable across DBMS and provides type-safety)

with Ada.Finalization; use Ada.Finalization;
with Interfaces.C.Strings;
with Ada.Calendar;
with Ada.Streams;

with OCI;              use OCI;

package GNATCOLL.SQL.Oracle.Gnade is

   Environment_Creation_Error  : exception;
   Error_Handle_Creation_Error : exception;

   Invalid_Handle : exception;
   Lib_Error      : exception;
   Null_Value     : exception;

   ----------------------------------------------------------------------------
   -- Connection --------------------------------------------------------------
   ----------------------------------------------------------------------------

   type Database_Record (Descr : Oracle_Description_Access) is
     tagged limited private;
   type Database_Access is access all Database_Record;

   procedure Logoff (Database : in out Database_Record'Class);

   function Is_Connected (Database : Database_Record'Class) return Boolean;

   procedure Reset (Database : in out Database_Record'Class);

   function Error_Msg (Database : Database_Record'Class) return String;

   function Make_Connection_Descriptor
     (Host         : XString;
      Port         : Integer;
      Service_Name : XString;
      SSL          : SSL_Mode) return String;

   function Make_Connection_String
     (Server   : String;
      User     : String;
      Password : String) return String;

   function Client_Version_Number return Integer;
   function Client_Version return String;

   function Server_Version_Number
     (Database : Database_Record'Class) return Integer;
   function Server_Version (Database : Database_Record'Class) return String;

   ----------------------------------------------------------------------------
   -- Statement ---------------------------------------------------------------
   ----------------------------------------------------------------------------

   type Column_Info_Record is limited private;
   type Column_Info_Access is access all Column_Info_Record;

   type Defined_Column is limited private;

   type Oracle_Statement_Record is limited private;
   type Oracle_Statement is access all Oracle_Statement_Record;

   function Is_Success (Result : Oracle_Statement) return Boolean;

   function Error_Msg (Result : Oracle_Statement) return String;

   function C_Value
     (Result   : Oracle_Statement;
      Position : Positive) return Interfaces.C.Strings.chars_ptr;

   function Value
     (Result   : Oracle_Statement;
      Position : Positive) return String;

   function Time_Value
     (Result   : Oracle_Statement;
      Position : Positive) return Ada.Calendar.Time;

   function Prepare
     (Database : Database_Record'Class;
      SQL_Code : String) return Oracle_Statement;

   procedure Execute
     (Statement : Oracle_Statement;
      Params    : SQL_Parameters := No_Parameters);

   function Fetch (Statement : Oracle_Statement) return Boolean;

   function Processed_Rows (Statement : Oracle_Statement) return Natural;

   function Field_Count (Statement : Oracle_Statement) return Natural;

   function Field_Name
     (Statement : Oracle_Statement;
      Position  : Positive) return String;

   function Is_Null
     (Statement : Oracle_Statement;
      Position  : Positive) return Boolean;

   procedure Finalize (Statement : access Oracle_Statement_Record);

   ----------------------------------------------------------------------------
   -- LOB support -------------------------------------------------------------
   ----------------------------------------------------------------------------

   type Lob_Type is (Binary_Lob, Char_Lob, NChar_Lob, File_Lob);

   subtype Lob_Length is Ub8;

   type Lob_Locator_Record is limited private;
   type Lob_Locator is access all Lob_Locator_Record;

   function Lob_Create
     (Database : Database_Record'Class;
      Kind     : Lob_Type) return Lob_Locator;

   procedure Lob_Free (Lob : in out Lob_Locator);

   function Lob_Get_Length (Lob : Lob_Locator) return Lob_Length;

   procedure Lob_Trim (Lob : Lob_Locator; New_Length : Lob_Length);

   function Lob_Value
     (Result   : Oracle_Statement;
      Position : Positive) return Lob_Locator;

   function Lob_To_C_String
     (Lob : Lob_Locator) return Interfaces.C.Strings.chars_ptr;
   function Lob_To_String (Lob : Lob_Locator) return String;
   --  Returns string value of the character LOB.
   --  For the binary LOB returns a string in format "[BLOB] N bytes", where
   --  'N' is the length of the BLOB in bytes.

   procedure Lob_To_File (Lob : Lob_Locator; Filename : String);

   procedure Lob_From_File (Lob : Lob_Locator; Filename : String);

   procedure Lob_Read
     (Lob    : Lob_Locator;
      Offset : Ada.Streams.Stream_Element_Offset;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   procedure Lob_Write
     (Lob    : Lob_Locator;
      Offset : Ada.Streams.Stream_Element_Offset;
      Item   : Ada.Streams.Stream_Element_Array);

   procedure Lob_Set_Index (Lob : Lob_Locator; To : Lob_Length);

   function Lob_Get_Index (Lob : Lob_Locator) return Lob_Length;

private

   type OCI_Environment is record
      Env : OCI.OCIEnv := Null_OCIEnv;
      Err : OCI.OCIError := Null_OCIError;
   end record;

   Null_OCI_Environment : constant OCI_Environment :=
     (Env => Null_OCIEnv, Err => Null_OCIError);

   ----------------------------------------------------------------------------
   -- Connection --------------------------------------------------------------
   ----------------------------------------------------------------------------

   type OCI_Connection is record
      Service     : OCISvcCtx := Null_OCISvcCtx;
      Server      : OCIServer := Null_OCIServer;
      Session     : OCISession := Null_OCISession;
      Environment : OCI_Environment := Null_OCI_Environment;
   end record;

   Null_OCI_Connection : constant OCI_Connection :=
     (Service     => Null_OCISvcCtx,
      Server      => Null_OCIServer,
      Session     => Null_OCISession,
      Environment => Null_OCI_Environment);

   procedure Finalize (Connect : in out OCI_Connection);

   function Connect
     (Dblink   : String;
      User     : String;
      Password : String) return OCI_Connection;

   type Database_Record (Descr : Oracle_Description_Access) is
     new Limited_Controlled with record
      Connection : OCI_Connection := Connect
        (Get_Connection_Descriptor (Descr),
         Descr.User.To_String,
         Descr.Password.To_String);
   end record;

   overriding procedure Finalize (Object : in out Database_Record);

   ----------------------------------------------------------------------------
   -- DateTime support --------------------------------------------------------
   ----------------------------------------------------------------------------

   Init_OCIDate : constant OCIDate :=
     (OCIDateYYYY => 0, -- gregorian year; range is -4712 <= year <= 9999
      OCIDateMM   => 1, -- month; range is 1 <= month < 12
      OCIDateDD   => 1, -- day; range is 1 <= day <= 31
      OCIDateTime =>
        (OCITimeHH => 0, -- hours; range is 0 <= hours <=23
         OCITimeMI => 0, -- minutes; range is 0 <= minutes <= 59
         OCITimeSS => 0  -- seconds; range is 0 <= seconds <= 59
        )
     );

   ----------------------------------------------------------------------------
   -- Statement ---------------------------------------------------------------
   ----------------------------------------------------------------------------

   type Statement_Type is
     (Stmt_Unknown,
      Stmt_Select,
      Stmt_Update,
      Stmt_Delete,
      Stmt_Insert,
      Stmt_Create,
      Stmt_Drop,
      Stmt_Alter,
      Stmt_Begin,
      Stmt_Declare,
      Stmt_Call,
      Stmt_Merge,
      Stmt_Rollback,
      Stmt_Commit);

   for Statement_Type use
     (Stmt_Unknown  => OCI_STMT_UNKNOWN,
      Stmt_Select   => OCI_STMT_SELECT,
      Stmt_Update   => OCI_STMT_UPDATE,
      Stmt_Delete   => OCI_STMT_DELETE,
      Stmt_Insert   => OCI_STMT_INSERT,
      Stmt_Create   => OCI_STMT_CREATE,
      Stmt_Drop     => OCI_STMT_DROP,
      Stmt_Alter    => OCI_STMT_ALTER,
      Stmt_Begin    => OCI_STMT_BEGIN,
      Stmt_Declare  => OCI_STMT_DECLARE,
      Stmt_Call     => OCI_STMT_CALL,
      Stmt_Merge    => OCI_STMT_MERGE,
      Stmt_Rollback => OCI_STMT_ROLLBACK,
      Stmt_Commit   => OCI_STMT_COMMIT);

   for Statement_Type'Size use Ub2'Size;

   type OCI_Statement_Record is record
      Stmt      : OCIStmt;
      Connect   : OCI_Connection;
      Executed  : Boolean := False;
      Described : Boolean := False;
   end record;
   type OCI_Statement is access all OCI_Statement_Record;

   procedure Finalize (Statement : access OCI_Statement_Record);

   type OCI_Ada_Data_Type is
     (OCI_ADT_None,
      OCI_ADT_String,
      OCI_ADT_Integer,
      OCI_ADT_Float,
      OCI_ADT_OCINumber,
      OCI_ADT_OCIDate,
      OCI_ADT_Time,
      OCI_ADT_Bin_LOB,
      OCI_ADT_Char_LOB,
      OCI_ADT_Bin_File,
      OCI_ADT_Char_File);

   type Bound_Value is limited record
      Value_String      : Interfaces.C.Strings.char_array_access := null;
      Value_String_Size : aliased Interfaces.C.size_t := 0;
      Value_OCINumber   : aliased OCINumber;
      Value_OCIDate     : aliased OCIDate := Init_OCIDate;
      Value_OCIDateTime : aliased OCIDateTime := Null_OCIDateTime;
      Value_Lob         : aliased OCILobLocator := Null_OCILobLocator;
      Value_Null        : Integer := 0;
      Indicator         : aliased Sb2 := Null_Indicator;
      Bind              : aliased OCIBind := Null_OCIBind;
   end record;
   type Bound_Value_Access is access all Bound_Value;

   type Bound_Value_Array is array (Positive range <>) of Bound_Value;
   type Bound_Value_Array_Access is access Bound_Value_Array;

   type Column_Info_Record is limited record
      Data_Type     : OCI_Ada_Data_Type := OCI_ADT_None;
      SQL_Type_Code : Ub2 := 0;
      Data_Size     : Natural := 0;
      Precision     : Integer := 0;
      Scale         : Integer := 0;
      Type_Name     : XString := Null_XString;
      Name          : XString := Null_XString;
   end record;

   type Defined_Column is limited record
      Column_Info       : Column_Info_Access := null;
      Value_String      : Interfaces.C.Strings.char_array_access := null;
      Value_String_Size : Interfaces.C.size_t := 0;
      Value_Integer     : aliased Long_Long_Integer;
      Value_Float       : aliased Long_Float;
      Value_OCINumber   : aliased OCINumber;
      Value_OCIDate     : aliased OCIDate;
      Value_OCIDateTime : aliased OCIDateTime := Null_OCIDateTime;
      Value_Lob         : Lob_Locator := null;
      Indicator         : aliased Sb2 := Null_Indicator;
      Define            : aliased OCIDefine := Null_OCIDefine;
   end record;

   type Defined_Column_Array is array (Positive range <>) of Defined_Column;
   type Defined_Column_Array_Access is access Defined_Column_Array;

   type Oracle_Statement_Record is record
      Statement    : OCI_Statement := null;
      Is_Select    : Boolean := False;
      Column_Count : Natural := 0;
      Columns      : Defined_Column_Array_Access := null;
      Binds        : Bound_Value_Array_Access := null;
      Status       : Integer := Integer (OCI_SUCCESS);
      Status_Msg   : String_Access := null;
   end record;

   ----------------------------------------------------------------------------
   -- LOB support -------------------------------------------------------------
   ----------------------------------------------------------------------------

   type Lob_Locator_Record is limited record
      Kind        : Lob_Type := Binary_Lob;
      OCI_Locator : OCILobLocator := Null_OCILobLocator;
      Connect     : OCI_Connection;
      Position    : Ub8 := 1;
   end record;

   function Lob_Create
     (Connect : OCI_Connection;
      Kind    : Lob_Type) return Lob_Locator;

end GNATCOLL.SQL.Oracle.Gnade;
