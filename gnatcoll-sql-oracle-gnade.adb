with System;
with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting;
with Ada.Streams.Stream_IO;
with GNAT.Formatted_String;
with GNAT.Calendar;

package body GNATCOLL.SQL.Oracle.Gnade is

   Oracle_Default_Port : constant Integer := 1521;

   Create_Mode : constant Ub4 := OCI_THREADED + OCI_OBJECT;

   protected Global_Env is
      procedure Set_Env (Value : OCI_Environment);
      function Get_Env return OCI_Environment;
   private
      Env : OCI_Environment;
   end Global_Env;

   function Alloc_Handle (Parent : OCIEnv; Htype : Ub4) return OCIHandle;

   procedure Free_Handle (H : in out OCIHandle; HType : Ub4);

   function Alloc_Descriptor (Parent : OCIEnv; Htype : Ub4) return OCIHandle;

   function Get_Attr
     (H     : OCIHandle;
      HType : Ub4;
      Attr  : Ub4) return String;

   generic
      type Result_Type is private;
   function Get_Attr_G
     (H     : OCIHandle;
      HType : Ub4;
      Attr  : Ub4) return Result_Type;
   pragma Inline (Get_Attr_G);

   procedure Set_Attr
     (H     : OCIHandle;
      HType : Ub4;
      Attr  : Ub4;
      Value : String);

   procedure Set_Attr
     (H     : OCIHandle;
      HType : Ub4;
      Attr  : Ub4;
      Value : OCIHandle);

   procedure Check_Error
     (Code        : SWord;
      Handle      : OCIError;
      Htype       : Integer;
      Raise_Error : Boolean);

   procedure Check_Error
     (Code            : SWord;
      Raise_Exception : Boolean := True);

   procedure Ignore_Error (Code : in SWord);

--     function Last_Error_Code return Integer;

   function Last_Error_Message (Record_No : Integer := 1) return String;

   procedure Print_Error (Msg : String);

   function Create_Env return OCI_Environment;

   function Get_Environment return OCI_Environment;

   function Get_Environment_Handle return OCIEnv;

   function Get_Error_Handle return OCIError;

   ----------------
   -- Global_Env --
   ----------------

   protected body Global_Env is

      -------------
      -- Set_Env --
      -------------

      procedure Set_Env (Value : OCI_Environment) is
      begin
         if Env = Null_OCI_Environment then
            Env := Value;
         end if;
      end Set_Env;

      -------------
      -- Get_Env --
      -------------

      function Get_Env return OCI_Environment is
      begin
         return Env;
      end Get_Env;
   end Global_Env;

   ----------------------
   -- Alloc_Descriptor --
   ----------------------

   function Alloc_Descriptor (Parent : OCIEnv; Htype : Ub4) return OCIHandle is
      Result : aliased OCIHandle := Null_OCIHandle;
   begin
      if OCIDescriptorAlloc
           (Parenth => Parent,
            Descpp  => Result'Access,
            Htype   => Htype) /= OCI_SUCCESS
      then
         raise Invalid_Handle;
      end if;

      return Result;
   end Alloc_Descriptor;

   ------------------
   -- Alloc_Handle --
   ------------------

   function Alloc_Handle (Parent : OCIEnv; Htype : Ub4) return OCIHandle is
      Result : aliased OCIHandle := Null_OCIHandle;
   begin
      if OCIHandleAlloc
           (Parenth => OCIHandle (Parent),
            Hndlpp  => Result'Access,
            Htype   => Htype) /= OCI_SUCCESS
      then
         raise Invalid_Handle;
      end if;

      return Result;
   end Alloc_Handle;

   -----------------
   -- Free_Handle --
   -----------------

   procedure Free_Handle (H : in out OCIHandle; HType : Ub4) is
      use System;
      Rc : SWord;
   begin
      if H = Null_OCIHandle then
         return;
      end if;

      Rc := OCIHandleFree (Hndlp => H, Htype => HType);
      H  := Null_OCIHandle;
      Check_Error (Code => Rc, Raise_Exception => False);
   end Free_Handle;

   --------------
   -- Get_Attr --
   --------------

   function Get_Attr
     (H     : OCIHandle;
      HType : Ub4;
      Attr  : Ub4) return String
   is
      Buff  : aliased Interfaces.C.Strings.chars_ptr;
      Rsize : aliased Ub4;
   begin
      Check_Error
        (OCIAttrGet
           (Trgthndlp  => H,
            Trghndltyp => HType,
            Attributep => Buff'Access,
            Sizep      => Rsize'Access,
            Attrtype   => Attr,
            Errhp      => Get_Error_Handle));

      if Interfaces.C.Strings."=" (Buff, Interfaces.C.Strings.Null_Ptr) then
         return "";
      else
         return Interfaces.C.To_Ada (Interfaces.C.Strings.Value (Buff), True);
      end if;
   end Get_Attr;

   ----------------
   -- Get_Attr_G --
   ----------------

   function Get_Attr_G
     (H     : OCIHandle;
      HType : Ub4;
      Attr  : Ub4) return Result_Type
   is
      Result : aliased Result_Type;
      Size   : aliased Ub4;
   begin
      Check_Error
        (OCIAttrGet
           (Trgthndlp  => H,
            Trghndltyp => HType,
            Attributep => Result'Address,
            Sizep      => Size'Unchecked_Access,
            Attrtype   => Attr,
            Errhp      => Get_Error_Handle));
      return Result;
   end Get_Attr_G;

   ----------------
   -- Get_Attr_* --
   ----------------

--     function Get_Attr_Ub1 is new Get_Attr_G (Ub1);
   function Get_Attr_Ub2 is new Get_Attr_G (Ub2);
   function Get_Attr_Ub4 is new Get_Attr_G (Ub4);
   function Get_Attr_Sb1 is new Get_Attr_G (Sb1);
   function Get_Attr_Sb2 is new Get_Attr_G (Sb2);

   --------------
   -- Set_Attr --
   --------------

   procedure Set_Attr
     (H     : OCIHandle;
      HType : Ub4;
      Attr  : Ub4;
      Value : String) is
   begin
      Check_Error
        (OCIAttrSet
           (Trgthndlp  => H,
            Trghndltyp => HType,
            Attributep => Interfaces.C.To_C (Value),
            Size       => Value'Length,
            Attrtype   => Attr,
            Errhp      => Get_Error_Handle));
   end Set_Attr;

   --------------
   -- Set_Attr --
   --------------

   procedure Set_Attr
     (H     : OCIHandle;
      HType : Ub4;
      Attr  : Ub4;
      Value : OCIHandle) is
   begin
      Check_Error
        (OCIAttrSet
           (Trgthndlp  => H,
            Trghndltyp => HType,
            Attributep => Value,
            Size       => 0,
            Attrtype   => Attr,
            Errhp      => Get_Error_Handle));
   end Set_Attr;

   -----------------
   -- Check_Error --
   -----------------

   procedure Check_Error
     (Code        : SWord;
      Handle      : OCIError;
      Htype       : Integer;
      Raise_Error : Boolean)
   is
      use Ada.Exceptions;
      Bufp     : aliased Interfaces.C.char_array :=
        (0 .. 2047 => Interfaces.C.nul);
      Errcodep : aliased Sb4 := 0;
      Rc       : SWord;

      procedure Error (Except : Exception_Id; Msg : String);

      procedure Error (Except : Exception_Id; Msg : String) is
      begin
         if Raise_Error then
            Raise_Exception (Except, Msg);
         else
            Print_Error (Msg);
         end if;
      end Error;

   begin
      case Code is
         when OCI_ERROR
            | OCI_NO_DATA
            | OCI_SUCCESS_WITH_INFO =>
            Rc := OCIErrorGet
              (Hndlp    => Handle,
               Errcodep => Errcodep'Access,
               Bufp     => Interfaces.C.Strings.To_Chars_Ptr
                 (Bufp'Unchecked_Access),
               Bufsiz   => Bufp'Length - 1,
               Htype    => Ub4 (Htype));

            if Rc = OCI_SUCCESS then
               Error (Lib_Error'Identity, Interfaces.C.To_Ada (Bufp));
            else
               Error
                 (Lib_Error'Identity,
                  "Error code:" & SWord'Image (Code) & ASCII.LF
                  & "Return error code:" & SWord'Image (Rc) & ASCII.LF
                  & "Output error code:" & Sb4'Image (Errcodep) & ASCII.LF
                  & "Message:" & Interfaces.C.To_Ada (Bufp));
            end if;

         when OCI_INVALID_HANDLE    =>
            Error (Invalid_Handle'Identity, "Invalid handle");

         when OCI_SUCCESS           =>
            null;

         when others                =>
            Error
              (Constraint_Error'Identity, "Error code" & SWord'Image (Code));
      end case;
   end Check_Error;

   -----------------
   -- Check_Error --
   -----------------

   procedure Check_Error
     (Code            : SWord;
      Raise_Exception : Boolean := True) is
   begin
      Check_Error (Code, Get_Error_Handle, OCI_HTYPE_ERROR, Raise_Exception);
   end Check_Error;

   ------------------
   -- Ignore_Error --
   ------------------

   procedure Ignore_Error (Code : in SWord) is
      pragma Unreferenced (Code);
   begin
      null;
   end Ignore_Error;

--     ---------------------
--     -- Last_Error_Code --
--     ---------------------
--
--     function Last_Error_Code return Integer is
--        Errcodep : aliased Sb4 := 0;
--        Rc       : SWord;
--        pragma Unreferenced (Rc);
--     begin
--        Rc := OCIErrorGet
--          (Hndlp    => Get_Error_Handle,
--           Recordno => 1,
--           Sqlstate => Empty_Undefined,
--           Errcodep => Errcodep'Access,
--           Bufp     => Interfaces.C.Strings.Null_Ptr,
--           Bufsiz   => 0,
--           Htype    => OCI_HTYPE_ERROR);
--        return Integer (Errcodep);
--     end Last_Error_Code;

   ------------------------
   -- Last_Error_Message --
   ------------------------

   function Last_Error_Message (Record_No : Integer := 1) return String is
      Errcodep : aliased Sb4 := 0;
      Bufp     : aliased Interfaces.C.char_array :=
        (0 .. 4095 => Interfaces.C.nul);
   begin
      Ignore_Error
        (OCIErrorGet
           (Hndlp    => Get_Error_Handle,
            Recordno => Ub4 (Record_No),
            Sqlstate => Empty_Undefined,
            Errcodep => Errcodep'Access,
            Bufp     => Interfaces.C.Strings.To_Chars_Ptr
              (Bufp'Unchecked_Access),
            Bufsiz   => Bufp'Length - 1,
            Htype    => OCI_HTYPE_ERROR));
      return Interfaces.C.To_Ada (Bufp);
   end Last_Error_Message;

   -----------------
   -- Print_Error --
   -----------------

   procedure Print_Error (Msg : String) is
      use Ada.Text_IO;
   begin
      Put_Line (Current_Error, Msg);
   end Print_Error;

   ----------------
   -- Create_Env --
   ----------------

   function Create_Env return OCI_Environment is
      Env : aliased OCIEnv := Null_OCIEnv;
      Err : aliased OCIHandle := Null_OCIHandle;
      Rc  : SWord;
   begin
      Rc := OCIEnvNlsCreate
        (Envh      => Env'Access,
         Mode      => Create_Mode,
         Ctxp      => Null_OCIHandle,
         Malocfp   => Null_OCIHandle,
         Ralocfp   => Null_OCIHandle,
         Mfreefp   => Null_OCIHandle,
         Xtramemsz => 0,
         Usrmempp  => Null_OCIHandle,
         Charset   => OCI_UTF8ID,
         Ncharset  => OCI_UTF8ID);

      if Rc /= OCI_SUCCESS then
         raise Environment_Creation_Error;
      end if;

      Rc := OCIHandleAlloc
        (Parenth => OCIHandle (Env),
         Hndlpp  => Err'Access,
         Htype   => OCI_HTYPE_ERROR);

      if Rc /= OCI_SUCCESS then
         raise Error_Handle_Creation_Error;
      end if;

      return (Env, OCIError (Err));
   end Create_Env;

   ---------------------
   -- Get_Environment --
   ---------------------

   function Get_Environment return OCI_Environment is
   begin
      if Global_Env.Get_Env = Null_OCI_Environment then
         Global_Env.Set_Env (Create_Env);
      end if;

      return Global_Env.Get_Env;
   end Get_Environment;

   ----------------------------
   -- Get_Environment_Handle --
   ----------------------------

   function Get_Environment_Handle return OCIEnv is
   begin
      return Get_Environment.Env;
   end Get_Environment_Handle;

   ----------------------
   -- Get_Error_Handle --
   ----------------------

   function Get_Error_Handle return OCIError is
   begin
      return Get_Environment.Err;
   end Get_Error_Handle;

   ----------------------------------------------------------------------------
   -- Connection imlementation ------------------------------------------------
   ----------------------------------------------------------------------------

   -------------
   -- Connect --
   -------------

   function Connect
     (Dblink   : String;
      User     : String;
      Password : String) return OCI_Connection
   is
      Result : OCI_Connection;
   begin
      Result.Environment := Get_Environment;

      --  allocate server handle

      Result.Server := OCIServer
        (Alloc_Handle (Get_Environment_Handle, OCI_HTYPE_SERVER));

      --  attach server

      Check_Error
        (OCIServerAttach
           (Result.Server,
            Get_Error_Handle,
            Interfaces.C.To_C (Dblink),
            Dblink'Length));

      --  allocate service handle

      Result.Service := OCISvcCtx
        (Alloc_Handle (Get_Environment_Handle, OCI_HTYPE_SVCCTX));

      --  set server attribute

      Set_Attr (OCIHandle (Result.Service),
                OCI_HTYPE_SVCCTX,
                OCI_ATTR_SERVER,
                OCIHandle (Result.Server));

      --  allocate session handle

      Result.Session := OCISession
        (Alloc_Handle (Get_Environment_Handle, OCI_HTYPE_SESSION));

      --  set login attributes

      Set_Attr (OCIHandle (Result.Session),
                OCI_HTYPE_SESSION,
                OCI_ATTR_USERNAME,
                User);
      Set_Attr (OCIHandle (Result.Session),
                OCI_HTYPE_SESSION,
                OCI_ATTR_PASSWORD,
                Password);

      --  set session attribute

      Set_Attr (OCIHandle (Result.Service),
                OCI_HTYPE_SVCCTX,
                OCI_ATTR_SESSION,
                OCIHandle (Result.Session));

      --  start session

      Check_Error
        (OCISessionBegin
           (Svchp => Result.Service,
            Errhp => Get_Error_Handle,
            Usrhp => Result.Session,
            Credt => OCI_CRED_RDBMS,
            Mode  => OCI_DEFAULT));
      return Result;
   end Connect;

   ------------
   -- Logoff --
   ------------

   procedure Logoff (Database : in out Database_Record'Class) is
   begin
      Finalize (Database.Connection);
   end Logoff;

   ------------------
   -- Is_Connected --
   ------------------

   function Is_Connected (Database : Database_Record'Class) return Boolean is
   begin
      return Get_Attr_Ub4
        (OCIHandle (Database.Connection.Server),
         OCI_HTYPE_SERVER,
         OCI_ATTR_SERVER_STATUS) = OCI_SERVER_NORMAL;
   end Is_Connected;

   -----------
   -- Reset --
   -----------

   procedure Reset (Database : in out Database_Record'Class) is
   begin
      null;
   end Reset;

   ---------------
   -- Error_Msg --
   ---------------

   function Error_Msg (Database : Database_Record'Class) return String is
      pragma Unreferenced (Database);
   begin
      return Last_Error_Message;
   end Error_Msg;

   --------------------------------
   -- Make_Connection_Descriptor --
   --------------------------------

   function Make_Connection_Descriptor
     (Host         : XString;
      Port         : Integer;
      Service_Name : XString;
      SSL          : SSL_Mode) return String
   is
      Connection_Descriptor_Template : constant String :=
        "(DESCRIPTION=" &
        "(ADDRESS=(PROTOCOL=_N_)(HOST=_H_)(PORT=_P_))" &
        "(CONNECT_DATA=(SERVICE_NAME=_S_)))";

      C : XString := To_XString (Connection_Descriptor_Template);
      I : Integer;
   begin
      if Is_Empty (Service_Name) then
         return "";
      end if;

      if Is_Empty (Host) then
         return To_String (Service_Name);
      end if;

      --  Protocol

      I := Find (C, "_N_");

      case SSL is
         when SSL_Disable =>
            Replace_Slice (C, I, I + 2, To_XString ("TCP"));
         when SSL_Require =>
            Replace_Slice (C, I, I + 2, To_XString ("TCPS"));
      end case;

      --  Host

      I := Find (C, "_H_");
      Replace_Slice (C, I, I + 2, Host);

      --  Port

      I := Find (C, "_P_");

      if Port = -1 then
         Replace (C, I, I + 2, Oracle_Default_Port'Img);
      else
         Replace (C, I, I + 2, Port'Img);
      end if;

      --  Service Name

      I := Find (C, "_S_");
      Replace_Slice (C, I, I + 2, Service_Name);

      return To_String (C);
   end Make_Connection_Descriptor;

   ----------------------------
   -- Make_Connection_String --
   ----------------------------

   function Make_Connection_String
     (Server   : String;
      User     : String;
      Password : String) return String
   is
   begin
      return User & "/" & Password & "@" & Server;
   end Make_Connection_String;

   ---------------------------
   -- Client_Version_Number --
   ---------------------------

   function Client_Version_Number return Integer is
      Major_Version   : SWord;
      Minor_Version   : SWord;
      Update_Num      : SWord;
      Patch_Num       : SWord;
      Port_Update_Num : SWord;

      Major    : Interfaces.Unsigned_32;
      Minor    : Interfaces.Unsigned_32;
      Update   : Interfaces.Unsigned_32;
      Port_Rel : Interfaces.Unsigned_32;
      Port_Upd : Interfaces.Unsigned_32;
   begin
      OCIClientVersion
        (Major_Version, Minor_Version, Update_Num, Patch_Num, Port_Update_Num);
      Major :=
        Interfaces.Shift_Left (Interfaces.Unsigned_32 (Major_Version), 24) and
        Interfaces.Unsigned_32 (16#FF000000#);
      Minor :=
        Interfaces.Shift_Left (Interfaces.Unsigned_32 (Minor_Version), 20) and
        Interfaces.Unsigned_32 (16#00F00000#);
      Update :=
        Interfaces.Shift_Left (Interfaces.Unsigned_32 (Update_Num), 12) and
        Interfaces.Unsigned_32 (16#000FF000#);
      Port_Rel :=
        Interfaces.Shift_Left (Interfaces.Unsigned_32 (Patch_Num), 8) and
        Interfaces.Unsigned_32 (16#00000F00#);
      Port_Upd :=
        Interfaces.Shift_Left (Interfaces.Unsigned_32 (Port_Update_Num), 0) and
        Interfaces.Unsigned_32 (16#000000FF#);
      return Integer (Major + Minor + Update + Port_Rel + Port_Upd);
   end Client_Version_Number;

   --------------------
   -- Client_Version --
   --------------------

   function Client_Version return String is
      Major_Version   : SWord;
      Minor_Version   : SWord;
      Update_Num      : SWord;
      Patch_Num       : SWord;
      Port_Update_Num : SWord;
      use GNAT.Formatted_String;
   begin
      OCIClientVersion
        (Major_Version, Minor_Version, Update_Num, Patch_Num, Port_Update_Num);
      return -(+"%d.%d.%d.%d.%d"
               & Integer (Major_Version)
               & Integer (Minor_Version)
               & Integer (Update_Num)
               & Integer (Patch_Num)
               & Integer (Port_Update_Num));
   end Client_Version;

   ---------------------------
   -- Server_Version_Number --
   ---------------------------

   function Server_Version_Number
     (Database : Database_Record'Class) return Integer
   is
      Buff : aliased Text := (0 .. 15 => Interfaces.C.nul);
      Ver  : aliased Ub4 := 0;
   begin
      Check_Error
        (OCIServerRelease
           (Hndlp    => OCIHandle (Database.Connection.Service),
            Errhp    => Get_Error_Handle,
            Bufp     => Interfaces.C.Strings.To_Chars_Ptr
              (Buff'Unchecked_Access),
            Bufsz    => Buff'Length - 1,
            Hndltype => OCI_HTYPE_SVCCTX,
            Version  => Ver'Access));
      return Integer (Ver);
   end Server_Version_Number;

   --------------------
   -- Server_Version --
   --------------------

   function Server_Version (Database : Database_Record'Class) return String is
      Buff : aliased Text := (0 .. 511 => Interfaces.C.nul);
   begin
      Check_Error
        (OCIServerVersion
           (Hndlp    => OCIHandle (Database.Connection.Service),
            Errhp    => Get_Error_Handle,
            Bufp     => Interfaces.C.Strings.To_Chars_Ptr
              (Buff'Unchecked_Access),
            Bufsz    => Buff'Length - 1,
            Hndltype => OCI_HTYPE_SVCCTX));
      return Interfaces.C.To_Ada (Buff);
   end Server_Version;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Connect : in out OCI_Connection) is
   begin
      if Connect.Service /= Null_OCISvcCtx then
         if Connect.Session /= Null_OCISession then
            Ignore_Error
              (OCISessionEnd
                 (Svchp => Connect.Service,
                  Errhp => Get_Error_Handle,
                  Usrhp => Connect.Session,
                  Mode  => OCI_DEFAULT));
            Free_Handle (OCIHandle (Connect.Session), OCI_HTYPE_SESSION);
            Free_Handle (OCIHandle (Connect.Service), OCI_HTYPE_SVCCTX);
         else
            Ignore_Error (OCILogoff (Connect.Service, Get_Error_Handle));
         end if;

         if Connect.Server /= Null_OCIServer then
            Ignore_Error (OCIServerDetach (Connect.Server, Get_Error_Handle));
            Free_Handle (OCIHandle (Connect.Server), OCI_HTYPE_SERVER);
         end if;

         Connect := Null_OCI_Connection;
      end if;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Database_Record) is
   begin
      if Object.Connection.Service /= Null_OCISvcCtx then
         Logoff (Object);
      end if;
   end Finalize;

   ----------------------------------------------------------------------------
   -- Number support ----------------------------------------------------------
   ----------------------------------------------------------------------------

   function To_Number (From : Long_Long_Integer) return OCINumber;

   function To_Number (From : Long_Float) return OCINumber;

   function To_C_String
     (From : OCINumber; Format : String := "TM9")
      return Interfaces.C.Strings.chars_ptr;

   --  function To_String
   --    (From : OCINumber; Format : String := "TM9") return String;

   ----------------------------------------------------------------------------
   -- DateTime support --------------------------------------------------------
   ----------------------------------------------------------------------------

   --  Oracle can handle date ranges from January 1, 4712 BCE through
   --  December 31, 9999 CE (Common Era, or "AD"). Unless BCE ("BC" in the
   --  format mask) is specifically used, CE date entries are the default.

   --  Internal oracle date representation.

   --  SQLT_DAT
   --  DATE is a data type used to store date and time values in a 7-byte
   --  structure. The internal format is the following one:
   --     1. century + 100
   --     2. year in the century + 100
   --     3. month
   --     4. day
   --     5. hour + 1
   --     6. minute + 1
   --     7. second + 1

   --  SQLT_TIMESTAMP
   --  SQLT_TIMESTAMP_LTZ
   --  TIMESTAMP is an extension of the Date datatype that can store date and
   --  time data (including fractional seconds). The size is 7 or 11 bytes,
   --  depending on the precision.
   --  The internal format is the following one:
   --     1. century + 100
   --     2. year in the century + 100
   --     3. month
   --     4. day
   --     5. hour + 1
   --     6. minute + 1
   --     7. second + 1
   --     8-11. the next 4 bytes gives the nanoseconds in the second

   --  SQLT_TIMESTAMP_TZ
   --  In case of a TIMESTAMP WITH TIME ZONE, 2 more bytes give the time zone,
   --  hour and minutes with a value between -12 and +14 for the hour part and
   --  between 0 and 59 for the minute part. The size is fixed at 13 bytes.
   --  The format of these two bytes is the following one :
   --     12. hour part of time zone + 20
   --     13. minute part of the time zone + 60

   -----------------------
   -- OCIDate functions --
   -----------------------

   function To_C_String
     (From   : OCIDate;
      Format : String := "YYYY-MM-DD HH24:MI:SS")
      return Interfaces.C.Strings.chars_ptr;

   --  function To_String
   --    (From   : OCIDate;
   --     Format : String := "YYYY-MM-DD HH24:MI:SS") return String;

   function To_Ada (DT : OCIDate) return Ada.Calendar.Time;

   function To_OCIDate (DT : Ada.Calendar.Time) return OCIDate;

   ---------------------------
   -- OCIDateTime functions --
   ---------------------------

   function To_Second_Duration
     (Fsec : Ub4) return Ada.Calendar.Formatting.Second_Duration;

   function UTC_Time_Offset_To_String
     (Offset : Ada.Calendar.Time_Zones.Time_Offset) return String;
   --  Returns a string representation of time zone displacement is the
   --  difference (in hours and minutes) between local time and UTC
   --  (Coordinated Universal Time—formerly Greenwich Mean Time) in the
   --  format "[+|-][HH:MM]". For example, "-08:00".

   function To_Ada (DT : OCIDateTime) return Ada.Calendar.Time;

   function To_OCIDateTime (DT : Ada.Calendar.Time) return OCIDateTime;

   function To_C_String
     (From   : OCIDateTime;
      Format : String := "YYYY-MM-DD HH24:MI:SS TZH:TZM")
      return Interfaces.C.Strings.chars_ptr;

   --  function To_String
   --    (From   : OCIDateTime;
   --     Format : String := "YYYY-MM-DD HH24:MI:SS TZH:TZM") return String;

   function Init_OCIDateTime return OCIDateTime;
   --  Creates OCIDateTime descriptor

   ----------------------------------------------------------------------------
   -- Statement implementation ------------------------------------------------
   ----------------------------------------------------------------------------

   Dos_Fix_CR : constant Ada.Strings.Maps.Character_Mapping :=
     Ada.Strings.Maps.To_Mapping ("" & ASCII.CR, " ");

   subtype SQL_Type is OCI.Ub2;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (OCI_Statement_Record, OCI_Statement);

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Bound_Value_Array, Bound_Value_Array_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Defined_Column_Array, Defined_Column_Array_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Interfaces.C.char_array, Interfaces.C.Strings.char_array_access);

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Lob_Locator_Record, Lob_Locator);

   function Type_Of_Statement (Stmt : OCI_Statement) return Statement_Type;

   function Number_Of_Columns (Stmt : OCI_Statement) return Natural;

   function Get_Column_Info
     (Stmt : OCI_Statement;
      Index : Positive) return Column_Info_Access;

   function Column_Ada_Type (Col_Type : SQL_Type) return OCI_Ada_Data_Type;

   function Execute_Internal (Stmt : OCI_Statement) return Boolean;

   ----------------
   -- Is_Success --
   ----------------

   function Is_Success (Result : Oracle_Statement) return Boolean is
   begin
      return Result.Status = OCI_SUCCESS;
   end Is_Success;

   ---------------
   -- Error_Msg --
   ---------------

   function Error_Msg (Result : Oracle_Statement) return String is
   begin
      return Result.Status_Msg.all;
   end Error_Msg;

   -------------
   -- C_Value --
   -------------

   function C_Value
     (Result   : Oracle_Statement;
      Position : Positive) return Interfaces.C.Strings.chars_ptr
   is
   begin
      if Result.Columns (Position).Indicator /= Null_Indicator then
         case Result.Columns (Position).Column_Info.Data_Type is
            when OCI_ADT_String    =>
               return Interfaces.C.Strings.To_Chars_Ptr
                 (Result.Columns (Position).Value_String);

            when OCI_ADT_Integer   =>
               return Interfaces.C.Strings.New_String
                 (Result.Columns (Position).Value_Integer'Img);

            when OCI_ADT_Float     =>
               return Interfaces.C.Strings.New_String
                 (Result.Columns (Position).Value_Float'Img);

            when OCI_ADT_OCINumber =>
               return To_C_String (Result.Columns (Position).Value_OCINumber);

            when OCI_ADT_OCIDate   =>
               return To_C_String (Result.Columns (Position).Value_OCIDate);

            when OCI_ADT_Time      =>
               return To_C_String
                 (Result.Columns (Position).Value_OCIDateTime);

            when OCI_ADT_Char_LOB
               | OCI_ADT_Char_File =>
               return Lob_To_C_String (Result.Columns (Position).Value_Lob);

            when OCI_ADT_Bin_LOB
               | OCI_ADT_Bin_File  =>
               return Interfaces.C.Strings.New_String
                 ("[BLOB]" & Lob_Length'Image
                    (Lob_Get_Length (Result.Columns (Position).Value_Lob))
                  & " bytes");

            when others            =>
               null;
         end case;
      end if;

      return Interfaces.C.Strings.Null_Ptr;
   end C_Value;

   -----------
   -- Value --
   -----------

   function Value
     (Result   : Oracle_Statement;
      Position : Positive) return String
   is
      Str : constant Interfaces.C.Strings.chars_ptr :=
        C_Value (Result, Position);
   begin
      if Str = Null_Ptr then
         return "";
      end if;

      return Interfaces.C.Strings.Value (Str);
   end Value;

   ----------------
   -- Time_Value --
   ----------------

   function Time_Value
     (Result   : Oracle_Statement;
      Position : Positive) return Ada.Calendar.Time
   is
   begin
      case Result.Columns (Position).Column_Info.Data_Type is
         when OCI_ADT_OCIDate =>
            return To_Ada (Result.Columns (Position).Value_OCIDate);
         when OCI_ADT_Time    =>
            return To_Ada (Result.Columns (Position).Value_OCIDateTime);
         when others          =>
            return GNAT.Calendar.No_Time;
      end case;
   end Time_Value;

   -----------------------
   -- Type_Of_Statement --
   -----------------------

   function Type_Of_Statement (Stmt : OCI_Statement) return Statement_Type is
      function To_Statement_Type is
        new Ada.Unchecked_Conversion (Ub2, Statement_Type);
   begin
      return To_Statement_Type
        (Get_Attr_Ub2
           (OCIHandle (Stmt.Stmt), OCI_HTYPE_STMT, OCI_ATTR_STMT_TYPE));
   end Type_Of_Statement;

   -----------------------
   -- Number_Of_Columns --
   -----------------------

   function Number_Of_Columns (Stmt : OCI_Statement) return Natural is
   begin
      return Natural
        (Get_Attr_Ub4
           (OCIHandle (Stmt.Stmt), OCI_HTYPE_STMT, OCI_ATTR_PARAM_COUNT));
   end Number_Of_Columns;

   ---------------------
   -- Get_Column_Info --
   ---------------------

   function Get_Column_Info
     (Stmt : OCI_Statement;
      Index : Positive) return Column_Info_Access
   is
      Param  : aliased OCIParam;
      Result : constant Column_Info_Access := new Column_Info_Record;
   begin
      Check_Error
        (OCIParamGet
           (Hndlp   => OCIHandle (Stmt.Stmt),
            Htype   => OCI_HTYPE_STMT,
            Errhp   => Get_Error_Handle,
            Parmdpp => Param'Access,
            Pos     => Ub4 (Index)));

      if Param = Null_OCIParam then
         return Result;
      end if;

      Result.SQL_Type_Code := SQL_Type
        (Get_Attr_Ub2
           (OCIHandle (Param), OCI_DTYPE_PARAM, OCI_ATTR_DATA_TYPE));
      Result.Data_Type := Column_Ada_Type (Result.SQL_Type_Code);
      Result.Data_Size := Natural
        (Get_Attr_Ub2
           (OCIHandle (Param), OCI_DTYPE_PARAM, OCI_ATTR_DATA_SIZE));
      Result.Precision := Integer
        (Get_Attr_Sb2
           (OCIHandle (Param), OCI_DTYPE_PARAM, OCI_ATTR_PRECISION));
      Result.Scale := Integer
        (Get_Attr_Sb1
           (OCIHandle (Param), OCI_DTYPE_PARAM, OCI_ATTR_SCALE));
      Result.Type_Name := To_XString
        (Get_Attr (OCIHandle (Param), OCI_DTYPE_PARAM, OCI_ATTR_TYPE_NAME));
      Result.Name := To_XString
        (Get_Attr (OCIHandle (Param), OCI_DTYPE_PARAM, OCI_ATTR_NAME));
      return Result;
   end Get_Column_Info;

   ----------------------
   -- Execute_Internal --
   ----------------------

   function Execute_Internal (Stmt : OCI_Statement) return Boolean is
      Not_Select      : constant Boolean :=
        Type_Of_Statement (Stmt) /= Stmt_Select;
      Raise_Exception : Boolean := True;
      Rc              : SWord;
   begin
      Rc := OCIStmtExecute
        (Svchp    => Stmt.Connect.Service,
         Stmtp    => Stmt.Stmt,
         Errhp    => Get_Error_Handle,
         Iters    => Boolean'Pos (Not_Select),
         Rowoff   => 0,
         Snap_In  => Null_OCIHandle,
         Snap_Out => Null_OCIHandle,
         Mode     => OCI_DEFAULT);

      if Raise_Exception then
         Check_Error (Rc, True);
         Raise_Exception := False;
      else
         if Rc = OCI_SUCCESS or else Rc = OCI_SUCCESS_WITH_INFO then
            Stmt.Executed := True;
            Stmt.Described := True;
         else
            Raise_Exception := True;
         end if;
      end if;

      return not Raise_Exception;
   end Execute_Internal;

   -------------
   -- Prepare --
   -------------

   function Prepare
     (Database : Database_Record'Class;
      SQL_Code : String) return Oracle_Statement
   is
      H      : aliased OCIStmt;
      Rc     : SWord;
      P_Stmt : constant Oracle_Statement := new Oracle_Statement_Record;
   begin
      P_Stmt.Statement := new OCI_Statement_Record;
      Rc := OCIStmtPrepare2
        (Svchp    => Database.Connection.Service,
         Stmtp    => H'Access,
         Errhp    => Get_Error_Handle,
         Stmt     => Interfaces.C.To_C
           (Ada.Strings.Fixed.Translate (SQL_Code, Dos_Fix_CR)),
         Stmt_Len => SQL_Code'Length,
         Key      => Interfaces.C.Strings.Null_Ptr,
         KeyLen   => 0,
         Language => OCI_NTV_SYNTAX,
         Mode     => OCI_DEFAULT);

      if Rc /= OCI_SUCCESS then
         Ignore_Error
           (OCIStmtRelease
              (Stmtp  => H,
               Errhp  => Get_Error_Handle,
               Key    => Interfaces.C.Strings.Null_Ptr,
               KeyLen => 0,
               Mode   => OCI_DEFAULT));
         Check_Error (Code => Rc, Raise_Exception => True);
      end if;

      P_Stmt.Statement.all :=
        (Stmt      => H,
         Connect   => Database.Connection,
         Executed  => False,
         Described => False);
      P_Stmt.Is_Select := Type_Of_Statement (P_Stmt.Statement) = Stmt_Select;
      return P_Stmt;
   end Prepare;

   ---------------------
   -- Column_Ada_Type --
   ---------------------

   function Column_Ada_Type (Col_Type : SQL_Type) return OCI_Ada_Data_Type is
   begin
      case Col_Type is
         when SQLT_STR
            | SQLT_CHR
            | SQLT_AFC
            | SQLT_AVC
            | SQLT_VST
            | SQLT_VCS           =>
            return OCI_ADT_String;

         when SQLT_INT           =>
            return OCI_ADT_Integer;

         when SQLT_FLT           =>
            return OCI_ADT_Float;

         when SQLT_NUM           =>
            return OCI_ADT_OCINumber;

         when SQLT_DAT           =>
            return OCI_ADT_OCIDate;

         when SQLT_TIME
            | SQLT_TIME_TZ
            | SQLT_TIMESTAMP
            | SQLT_TIMESTAMP_TZ
            | SQLT_TIMESTAMP_LTZ =>
            return OCI_ADT_Time;

         when SQLT_BLOB          =>
            return OCI_ADT_Bin_LOB;

         when SQLT_CLOB          =>
            return OCI_ADT_Char_LOB;

         when SQLT_BFILEE        =>
            return OCI_ADT_Bin_File;

         when SQLT_CFILEE        =>
            return OCI_ADT_Char_File;

         when others             =>
            return OCI_ADT_None;
      end case;
   end Column_Ada_Type;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Statement : Oracle_Statement;
      Params    : SQL_Parameters := No_Parameters)
   is
      Col_Count : Natural;
   begin
      if Params /= No_Parameters then

         if Statement.Binds /= null then
            Free (Statement.Binds);
         end if;

         Statement.Binds := new Bound_Value_Array (1 .. Params'Length);

         for I in Params'Range loop
            if Params (I) = Null_Parameter then
               Statement.Binds (I).Indicator := Null_Indicator;
               Check_Error
                 (OCIBindByPos
                    (Stmtp    => Statement.Statement.Stmt,
                     Bindpp   => Statement.Binds (I).Bind'Access,
                     Errhp    => Get_Error_Handle,
                     Position => Ub4 (I),
                     Valuep   => Statement.Binds (I).Value_Null'Address,
                     Value_Sz => Statement.Binds (I).Value_Null'Size /
                         System.Storage_Unit,
                     Dty      => SQLT_INT,
                     Indp     => Statement.Binds (I).Indicator'Access));

            elsif Params (I).Get in SQL_Parameter_Text'Class then
               declare
                  P : constant access SQL_Parameter_Text :=
                    SQL_Parameter_Text (Params (I).Get.Element.all)'Access;
                  S : constant String := P.To_String;
               begin
                  Statement.Binds (I).Value_String :=
                    new Interfaces.C.char_array'
                      (To_C (Item => S, Append_Nul => True));
                  Statement.Binds (I).Value_String_Size :=
                    (size_t (S'Length) + 1) * 2;
                  Statement.Binds (I).Indicator := Not_Null_Indicator;
                  Check_Error
                    (OCIBindByPos
                       (Stmtp    => Statement.Statement.Stmt,
                        Bindpp   => Statement.Binds (I).Bind'Access,
                        Errhp    => Get_Error_Handle,
                        Position => Ub4 (I),
                        Valuep   => DVoid
                          (Statement.Binds (I).Value_String (0)'Address),
                        Value_Sz =>
                          Sb4 (Statement.Binds (I).Value_String_Size),
                        Dty      => SQLT_STR,
                        Indp     => Statement.Binds (I).Indicator'Access));
               end;

            elsif Params (I).Get in SQL_Parameter_Integer'Class then
               declare
                  P : constant access SQL_Parameter_Integer :=
                    SQL_Parameter_Integer (Params (I).Get.Element.all)'Access;
               begin
                  Statement.Binds (I).Value_OCINumber :=
                    To_Number (Long_Long_Integer (P.Val));
                  Statement.Binds (I).Indicator := Not_Null_Indicator;
                  Check_Error
                    (OCIBindByPos
                       (Stmtp    => Statement.Statement.Stmt,
                        Bindpp   => Statement.Binds (I).Bind'Access,
                        Errhp    => Get_Error_Handle,
                        Position => Ub4 (I),
                        Valuep   =>
                          Statement.Binds (I).Value_OCINumber'Address,
                        Value_Sz => Statement.Binds (I).Value_OCINumber'Size /
                            System.Storage_Unit,
                        Dty      => SQLT_VNU,
                        Indp     => Statement.Binds (I).Indicator'Access));
               end;

            elsif Params (I).Get in SQL_Parameter_Bigint'Class then
               declare
                  P : constant access SQL_Parameter_Bigint :=
                    SQL_Parameter_Bigint (Params (I).Get.Element.all)'Access;
               begin
                  Statement.Binds (I).Value_OCINumber := To_Number (P.Val);
                  Statement.Binds (I).Indicator := Not_Null_Indicator;
                  Check_Error
                    (OCIBindByPos
                       (Stmtp    => Statement.Statement.Stmt,
                        Bindpp   => Statement.Binds (I).Bind'Access,
                        Errhp    => Get_Error_Handle,
                        Position => Ub4 (I),
                        Valuep   =>
                          Statement.Binds (I).Value_OCINumber'Address,
                        Value_Sz => Statement.Binds (I).Value_OCINumber'Size /
                            System.Storage_Unit,
                        Dty      => SQLT_VNU,
                        Indp     => Statement.Binds (I).Indicator'Access));
               end;

            elsif Params (I).Get in SQL_Parameter_Float'Class then
               declare
                  P : constant access SQL_Parameter_Float :=
                    SQL_Parameter_Float (Params (I).Get.Element.all)'Access;
               begin
                  Statement.Binds (I).Value_OCINumber :=
                    To_Number (Long_Float (P.Val));
                  Statement.Binds (I).Indicator := Not_Null_Indicator;
                  Check_Error
                    (OCIBindByPos
                       (Stmtp    => Statement.Statement.Stmt,
                        Bindpp   => Statement.Binds (I).Bind'Access,
                        Errhp    => Get_Error_Handle,
                        Position => Ub4 (I),
                        Valuep   =>
                          Statement.Binds (I).Value_OCINumber'Address,
                        Value_Sz => Statement.Binds (I).Value_OCINumber'Size /
                            System.Storage_Unit,
                        Dty      => SQLT_VNU,
                        Indp     => Statement.Binds (I).Indicator'Access));
               end;

            elsif Params (I).Get in SQL_Parameter_Long_Float'Class then
               declare
                  P : constant access SQL_Parameter_Long_Float :=
                    SQL_Parameter_Long_Float
                      (Params (I).Get.Element.all)'Access;
               begin
                  Statement.Binds (I).Value_OCINumber := To_Number (P.Val);
                  Statement.Binds (I).Indicator := Not_Null_Indicator;
                  Check_Error
                    (OCIBindByPos
                       (Stmtp    => Statement.Statement.Stmt,
                        Bindpp   => Statement.Binds (I).Bind'Access,
                        Errhp    => Get_Error_Handle,
                        Position => Ub4 (I),
                        Valuep   =>
                          Statement.Binds (I).Value_OCINumber'Address,
                        Value_Sz => Statement.Binds (I).Value_OCINumber'Size /
                            System.Storage_Unit,
                        Dty      => SQLT_VNU,
                        Indp     => Statement.Binds (I).Indicator'Access));
               end;

            elsif Params (I).Get in SQL_Parameter_Money'Class then
               declare
                  P : constant access SQL_Parameter_Money :=
                    SQL_Parameter_Money
                      (Params (I).Get.Element.all)'Access;
               begin
                  Statement.Binds (I).Value_OCINumber :=
                    To_Number (Long_Float (P.Val));
                  Statement.Binds (I).Indicator := Not_Null_Indicator;
                  Check_Error
                    (OCIBindByPos
                       (Stmtp    => Statement.Statement.Stmt,
                        Bindpp   => Statement.Binds (I).Bind'Access,
                        Errhp    => Get_Error_Handle,
                        Position => Ub4 (I),
                        Valuep   =>
                          Statement.Binds (I).Value_OCINumber'Address,
                        Value_Sz => Statement.Binds (I).Value_OCINumber'Size /
                            System.Storage_Unit,
                        Dty      => SQLT_VNU,
                        Indp     => Statement.Binds (I).Indicator'Access));
               end;

            elsif Params (I).Get in SQL_Parameter_Date'Class then
               declare
                  P : constant access SQL_Parameter_Date :=
                    SQL_Parameter_Date (Params (I).Get.Element.all)'Access;
               begin
                  Statement.Binds (I).Value_OCIDate := To_OCIDate (P.Val);
                  Statement.Binds (I).Indicator := Not_Null_Indicator;
                  Check_Error
                    (OCIBindByPos
                       (Stmtp    => Statement.Statement.Stmt,
                        Bindpp   => Statement.Binds (I).Bind'Access,
                        Errhp    => Get_Error_Handle,
                        Position => Ub4 (I),
                        Valuep   => Statement.Binds (I).Value_OCIDate'Address,
                        Value_Sz => Statement.Binds (I).Value_OCIDate'Size /
                            System.Storage_Unit,
                        Dty      => SQLT_ODT,
                        Indp     => Statement.Binds (I).Indicator'Access));
               end;

            elsif Params (I).Get in SQL_Parameter_Time'Class then
               declare
                  P : constant access SQL_Parameter_Time :=
                    SQL_Parameter_Time (Params (I).Get.Element.all)'Access;
               begin
                  Statement.Binds (I).Value_OCIDateTime :=
                    To_OCIDateTime (P.Val);
                  Statement.Binds (I).Indicator := Not_Null_Indicator;
                  Check_Error
                    (OCIBindByPos
                       (Stmtp    => Statement.Statement.Stmt,
                        Bindpp   => Statement.Binds (I).Bind'Access,
                        Errhp    => Get_Error_Handle,
                        Position => Ub4 (I),
                        Valuep   =>
                          Statement.Binds (I).Value_OCIDateTime'Address,
                        Value_Sz =>
                          Statement.Binds (I).Value_OCIDateTime'Size /
                            System.Storage_Unit,
                        Dty      => SQLT_TIMESTAMP_TZ,
                        Indp     => Statement.Binds (I).Indicator'Access));
               end;

            --  elsif Params (I).Get in SQL_Parameter_BLOB'Class then
            --     declare
            --        P : constant access SQL_Parameter_BLOB :=
            --          SQL_Parameter_BLOB (Params (I).Get.Element.all)'Access;
            --     begin
            --        Statement.Binds (I).Value_Lob := P.Val.OCI_Locator;
            --        Statement.Binds (I).Indicator := Not_Null_Indicator;
            --        Check_Error
            --          (OCIBindByPos
            --             (Stmtp    => Statement.Statement.Stmt,
            --              Bindpp   => Statement.Binds (I).Bind'Access,
            --              Errhp    => Get_Error_Handle,
            --              Position => Ub4 (I),
            --              Valuep   => Statement.Binds (I).Value_Lob'Address,
            --              Value_Sz => -1,
            --              Dty      => SQLT_BLOB,
            --              Indp     => Statement.Binds (I).Indicator'Access));
            --     end;

            end if;
         end loop;
      end if;

      if not Execute_Internal (Statement.Statement) then
         return;
      end if;

      if not Statement.Is_Select then
         return;
      end if;

      Col_Count := Number_Of_Columns (Statement.Statement);

      if Statement.Columns /= null then
         Free (Statement.Columns);
      end if;

      if Statement.Columns = null and then Col_Count > 0 then
         Statement.Columns := new Defined_Column_Array (1 .. Col_Count);
      else
         return;
      end if;

      for I in 1 .. Col_Count loop
         --  get column info

         Statement.Columns (I).Column_Info := Get_Column_Info
           (Stmt  => Statement.Statement,
            Index => I);

         --  define value

         case Statement.Columns (I).Column_Info.Data_Type is
            when OCI_ADT_String =>
               Statement.Columns (I).Value_String_Size := size_t
                 (Statement.Columns (I).Column_Info.Data_Size + 1) * 2;

               if Statement.Columns (I).Value_String /= null then
                  Free (Statement.Columns (I).Value_String);
               end if;

               if Statement.Columns (I).Value_String = null then
                  Statement.Columns (I).Value_String :=
                    new Interfaces.C.char_array
                      (0 .. Statement.Columns (I).Value_String_Size);
                  Statement.Columns (I).Value_String.all :=
                    (0 .. Statement.Columns (I).Value_String_Size => nul);
               end if;

               Check_Error
                 (OCIDefineByPos
                    (Stmtp    => Statement.Statement.Stmt,
                     Defnpp   => Statement.Columns (I).Define'Access,
                     Errhp    => Get_Error_Handle,
                     Position => Ub4 (I),
                     Value    =>
                        Statement.Columns (I).Value_String (0)'Address,
                     Value_Sz => Sb4 (Statement.Columns (I).Value_String_Size),
                     Dty      => SQLT_STR,
                     Indp     => Statement.Columns (I).Indicator'Access));

            when OCI_ADT_Integer =>
               Check_Error
                 (OCIDefineByPos
                    (Stmtp    => Statement.Statement.Stmt,
                     Defnpp   => Statement.Columns (I).Define'Access,
                     Errhp    => Get_Error_Handle,
                     Position => Ub4 (I),
                     Value    => Statement.Columns (I).Value_Integer'Address,
                     Value_Sz => Statement.Columns (I).Value_Integer'Size /
                         System.Storage_Unit,
                     Dty      => SQLT_INT,
                     Indp     => Statement.Columns (I).Indicator'Access));

            when OCI_ADT_Float =>
               Check_Error
                 (OCIDefineByPos
                    (Stmtp    => Statement.Statement.Stmt,
                     Defnpp   => Statement.Columns (I).Define'Access,
                     Errhp    => Get_Error_Handle,
                     Position => Ub4 (I),
                     Value    => Statement.Columns (I).Value_Float'Address,
                     Value_Sz => Statement.Columns (I).Value_Float'Size /
                         System.Storage_Unit,
                     Dty      => SQLT_FLT,
                     Indp     => Statement.Columns (I).Indicator'Access));

            when OCI_ADT_OCINumber =>
               Check_Error
                 (OCIDefineByPos
                    (Stmtp    => Statement.Statement.Stmt,
                     Defnpp   => Statement.Columns (I).Define'Access,
                     Errhp    => Get_Error_Handle,
                     Position => Ub4 (I),
                     Value    => Statement.Columns (I).Value_OCINumber'Address,
                     Value_Sz => Statement.Columns (I).Value_OCINumber'Size /
                         System.Storage_Unit,
                     Dty      => SQLT_VNU,
                     Indp     => Statement.Columns (I).Indicator'Access));

            when OCI_ADT_OCIDate =>
               Check_Error
                 (OCIDefineByPos
                    (Stmtp    => Statement.Statement.Stmt,
                     Defnpp   => Statement.Columns (I).Define'Access,
                     Errhp    => Get_Error_Handle,
                     Position => Ub4 (I),
                     Value    => Statement.Columns (I).Value_OCIDate'Address,
                     Value_Sz => Statement.Columns (I).Value_OCIDate'Size /
                         System.Storage_Unit,
                     Dty      => SQLT_ODT,
                     Indp     => Statement.Columns (I).Indicator'Access));

            when OCI_ADT_Time =>
               Statement.Columns (I).Value_OCIDateTime := Init_OCIDateTime;
               Check_Error
                 (OCIDefineByPos
                    (Stmtp    => Statement.Statement.Stmt,
                     Defnpp   => Statement.Columns (I).Define'Access,
                     Errhp    => Get_Error_Handle,
                     Position => Ub4 (I),
                     Value    =>
                        Statement.Columns (I).Value_OCIDateTime'Address,
                     Value_Sz => Statement.Columns (I).Value_OCIDateTime'Size /
                         System.Storage_Unit,
                     Dty      => SQLT_TIMESTAMP_TZ,
                     Indp     => Statement.Columns (I).Indicator'Access));

            when OCI_ADT_Bin_LOB .. OCI_ADT_Char_File =>
               case Statement.Columns (I).Column_Info.SQL_Type_Code is
                  when SQLT_BFILEE | SQLT_CFILEE =>
                     Statement.Columns (I).Value_Lob :=
                       Lob_Create (Statement.Statement.Connect, File_Lob);
                  when SQLT_BLOB                 =>
                     Statement.Columns (I).Value_Lob :=
                       Lob_Create (Statement.Statement.Connect, Binary_Lob);
                  when SQLT_CLOB                  =>
                     Statement.Columns (I).Value_Lob :=
                       Lob_Create (Statement.Statement.Connect, Char_Lob);
                  when others                     =>
                     Statement.Columns (I).Value_Lob := null;
               end case;

               if Statement.Columns (I).Value_Lob /= null then
                  Check_Error
                    (OCIDefineByPos
                       (Stmtp    => Statement.Statement.Stmt,
                        Defnpp   => Statement.Columns (I).Define'Access,
                        Errhp    => Get_Error_Handle,
                        Position => Ub4 (I),
                        Value    =>
                          Statement.Columns (I).Value_Lob.OCI_Locator'Address,
                        Value_Sz => -1,
                        Dty      =>
                          Statement.Columns (I).Column_Info.SQL_Type_Code,
                        Indp     => Statement.Columns (I).Indicator'Access));
               end if;

            when others =>
               null;
         end case;

         Statement.Column_Count := I;
      end loop;
   end Execute;

   -----------
   -- Fetch --
   -----------

   function Fetch (Statement : Oracle_Statement) return Boolean is
      Rc : SWord;
   begin
      if Statement.Is_Select then
         Rc := OCIStmtFetch2
           (Stmtp       => Statement.Statement.Stmt,
            Errhp       => Get_Error_Handle,
            Nrows       => 1,
            Orientation => OCI_FETCH_NEXT,
            FetchOffset => 0,
            Mode        => OCI_DEFAULT);

         case Rc is
            when OCI_NO_DATA           => return False;
            when OCI_SUCCESS
               | OCI_SUCCESS_WITH_INFO => return True;
            when others                => Check_Error (Rc);
         end case;

         --  We should not be there.

         raise Program_Error;
      else
         return False;
      end if;
   end Fetch;

   --------------------
   -- Processed_Rows --
   --------------------

   function Processed_Rows (Statement : Oracle_Statement) return Natural is
   begin
      return Natural
        (Get_Attr_Ub4
           (OCIHandle (Statement.Statement.Stmt),
            OCI_HTYPE_STMT,
            OCI_ATTR_ROW_COUNT));
   end Processed_Rows;

   -----------------
   -- Field_Count --
   -----------------

   function Field_Count (Statement : Oracle_Statement) return Natural is
   begin
      return Statement.Column_Count;
   end Field_Count;

   ----------------
   -- Field_Name --
   ----------------

   function Field_Name
     (Statement : Oracle_Statement;
      Position  : Positive) return String
   is
   begin
      return Statement.Columns (Position).Column_Info.Name.To_String;
   end Field_Name;

   -------------
   -- Is_Null --
   -------------

   function Is_Null
     (Statement : Oracle_Statement;
      Position  : Positive) return Boolean
   is
   begin
      return Statement.Columns (Position).Indicator = Null_Indicator;
   end Is_Null;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Statement : access Oracle_Statement_Record) is
   begin
      if Statement.Statement /= null then
         Finalize (Statement.Statement);
         Free (Statement.Statement);
         Free (Statement.Columns);
         Free (Statement.Binds);
         Free (Statement.Status_Msg);
      end if;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Statement : access OCI_Statement_Record) is
   begin
      if Statement.Stmt /= Null_OCIStmt then
         Check_Error
           (OCIStmtRelease
              (Stmtp  => Statement.Stmt,
               Errhp  => Get_Error_Handle));
         Statement.Stmt := Null_OCIStmt;
         Statement.Connect := Null_OCI_Connection;
      end if;
   end Finalize;

   ----------------------------------------------------------------------------
   -- LOB support -------------------------------------------------------------
   ----------------------------------------------------------------------------

   Empty_OCI_Locator : constant OCILobLocator := Null_OCILobLocator;

   type4alloc : constant array (Lob_Type) of Ub4 :=
     (Binary_Lob => OCI_DTYPE_LOB,
      Char_Lob   => OCI_DTYPE_LOB,
      NChar_Lob  => OCI_DTYPE_LOB,
      File_Lob   => OCI_DTYPE_FILE);

   ----------------
   -- Lob_Create --
   ----------------

   function Lob_Create
     (Connect : OCI_Connection;
      Kind    : Lob_Type) return Lob_Locator
   is
      Object : constant Lob_Locator := new Lob_Locator_Record;
   begin
      Object.Connect := Connect;
      Object.Kind := Kind;
      Object.OCI_Locator := OCILobLocator
        (Alloc_Descriptor
           (Parent => Get_Environment_Handle,
            Htype  => type4alloc (Kind)));
      return Object;
   end Lob_Create;

   ----------------
   -- Lob_Create --
   ----------------

   function Lob_Create
     (Database : Database_Record'Class;
      Kind     : Lob_Type) return Lob_Locator
   is
   begin
      return Lob_Create (Database.Connection, Kind);
   end Lob_Create;

   --------------
   -- Lob_Free --
   --------------

   procedure Lob_Free (Lob : in out Lob_Locator) is
   begin
      if Lob.OCI_Locator /= Empty_OCI_Locator then
         Check_Error
           (OCIDescriptorFree
              (Descp => OCIHandle (Lob.OCI_Locator),
               Dtype => type4alloc (Lob.Kind)));
         Free (Lob);
         Lob := null;
      end if;
   end Lob_Free;

   --------------------
   -- Lob_Get_Length --
   --------------------

   function Lob_Get_Length (Lob : Lob_Locator) return Lob_Length is
      Result : aliased Ub8 := 0;
   begin
      Check_Error
        (OCILobGetLength2
           (Svchp => Lob.Connect.Service,
            Errhp => Get_Error_Handle,
            Locp  => Lob.OCI_Locator,
            Lenp  => Result'Unchecked_Access));
      return Lob_Length (Result);
   end Lob_Get_Length;

   --------------
   -- Lob_Trim --
   --------------

   procedure Lob_Trim (Lob : Lob_Locator; New_Length : Lob_Length) is
   begin
      Check_Error
        (OCILobTrim2
           (Svchp  => Lob.Connect.Service,
            Errhp  => Get_Error_Handle,
            Locp   => Lob.OCI_Locator,
            Newlen => New_Length));
   end Lob_Trim;

   ---------------
   -- Lob_Value --
   ---------------

   function Lob_Value
     (Result   : Oracle_Statement;
      Position : Positive) return Lob_Locator
   is
   begin
      return Result.Columns (Position).Value_Lob;
   end Lob_Value;

   ---------------------
   -- Lob_To_C_String --
   ---------------------

   function Lob_To_C_String
     (Lob : Lob_Locator) return Interfaces.C.Strings.chars_ptr
   is
      Len       : constant Lob_Length := Lob_Get_Length (Lob);
      Max_Chars : constant Ub8 := Ub8 (Positive'Last);
      Buffer    : aliased Text :=
        (0 .. size_t (Ub8'Min (Max_Chars, Len) - 1) => nul);
      Offset    : constant Ub8 := 1;
      B_Amt     : aliased Ub8 := 0;
      C_Amt     : aliased Ub8 := Max_Chars;
   begin
      Check_Error
        (OCILobRead2
           (Svchp     => Lob.Connect.Service,
            Errhp     => Get_Error_Handle,
            Locp      => Lob.OCI_Locator,
            Byte_Amtp => B_Amt'Unchecked_Access,
            Char_Amtp => C_Amt'Unchecked_Access,
            Offset    => Offset,
            Bufp      => Buffer (0)'Address,
            Bufl      => Buffer'Length,
            Piece     => OCI_ONE_PIECE,
            Ctxp      => Null_OCIHandle,
            Cbfp      => Null_OCIHandle,
            Csid      => 0,
            Csfrm     => SQLCS_IMPLICIT));

      if C_Amt > 0 then
         return Interfaces.C.Strings.To_Chars_Ptr
           (Buffer'Unchecked_Access, False);
      elsif B_Amt > 0 then
         return Interfaces.C.Strings.New_String
           ("[BLOB]" & Lob_Length'Image (Lob_Get_Length (Lob)) & " bytes");
      end if;

      return Interfaces.C.Strings.Null_Ptr;
   end Lob_To_C_String;

   -------------------
   -- Lob_To_String --
   -------------------

   function Lob_To_String (Lob : Lob_Locator) return String is
      Str : constant Interfaces.C.Strings.chars_ptr := Lob_To_C_String (Lob);
   begin
      if Str = Null_Ptr then
         return "";
      end if;

      return Interfaces.C.Strings.Value (Str);
   end Lob_To_String;

   -----------------
   -- Lob_To_File --
   -----------------

   procedure Lob_To_File (Lob : Lob_Locator; Filename : String) is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;
      Buffer_Size : constant Stream_Element_Offset := 65536;
      Buffer      : aliased Stream_Element_Array (1 .. Buffer_Size);
      Data_File   : File_Type;
      Lob_Size    : Lob_Length;
      Offset      : Ub8 := 1;
      B_Amt       : aliased Ub8 := Ub8 (Buffer_Size);
      C_Amt       : aliased Ub8 := 0;
   begin
      Create (File => Data_File,
              Mode => Out_File,
              Name => Filename,
              Form => "");
      Lob_Size := Lob_Get_Length (Lob);

      while Offset < Lob_Size loop
         Check_Error
           (OCILobRead2
              (Svchp     => Lob.Connect.Service,
               Errhp     => Get_Error_Handle,
               Locp      => Lob.OCI_Locator,
               Byte_Amtp => B_Amt'Unchecked_Access,
               Char_Amtp => C_Amt'Unchecked_Access,
               Offset    => Offset,
               Bufp      => Buffer (1)'Address,
               Bufl      => Buffer'Length,
               Piece     => OCI_ONE_PIECE,
               Ctxp      => Null_OCIHandle,
               Cbfp      => Null_OCIHandle,
               Csid      => 0,
               Csfrm     => SQLCS_IMPLICIT));

         if B_Amt > 0 then
            Offset := Offset + B_Amt;
            Write (File => Data_File,
                   Item => Buffer (1 .. Stream_Element_Offset (B_Amt)));
         end if;
      end loop;

      Close (Data_File);

   exception
      when others => Close (Data_File);
   end Lob_To_File;

   -------------------
   -- Lob_From_File --
   -------------------

   procedure Lob_From_File (Lob : Lob_Locator; Filename : String) is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;
      Buffer_Size : constant Stream_Element_Offset := 65536;
      Buffer      : aliased Stream_Element_Array (1 .. Buffer_Size);
      Last_El     : Stream_Element_Offset;
      Data_File   : File_Type;
      Offset      : Ub8 := 1;
      B_Amt       : aliased Ub8 := Ub8 (Buffer_Size);
      C_Amt       : aliased Ub8 := 0;
   begin
      Open (File => Data_File,
            Mode => In_File,
            Name => Filename,
            Form => "");
      Check_Error
        (OCILobOpen
           (Svchp => Lob.Connect.Service,
            Errhp => Get_Error_Handle,
            Locp  => Lob.OCI_Locator,
            Mode  => OCI_LOB_WRITEONLY));

      while not End_Of_File (Data_File) loop
         Read
           (File => Data_File,
            Item => Buffer,
            Last => Last_El);
         B_Amt := Ub8 (Last_El);
         Check_Error
           (OCILobWrite2
              (Svchp     => Lob.Connect.Service,
               Errhp     => Get_Error_Handle,
               Locp      => Lob.OCI_Locator,
               Byte_Amtp => B_Amt'Unchecked_Access,
               Char_Amtp => C_Amt'Unchecked_Access,
               Offset    => Offset,
               Bufp      => Buffer (1)'Address,
               Buflen    => Buffer'Length,
               Piece     => OCI_ONE_PIECE,
               Ctxp      => Null_OCIHandle,
               Cbfp      => Null_OCIHandle,
               Csid      => 0,
               Csfrm     => SQLCS_IMPLICIT));
            Offset := Offset + B_Amt;
      end loop;

      Check_Error
        (OCILobClose
           (Svchp => Lob.Connect.Service,
            Errhp => Get_Error_Handle,
            Locp  => Lob.OCI_Locator));
      Close (Data_File);

   exception
      when others =>
         Ignore_Error
           (OCILobClose
              (Svchp => Lob.Connect.Service,
               Errhp => Get_Error_Handle,
               Locp  => Lob.OCI_Locator));
         Close (Data_File);
   end Lob_From_File;

   --------------
   -- Lob_Read --
   --------------

   procedure Lob_Read
     (Lob    : Lob_Locator;
      Offset : Ada.Streams.Stream_Element_Offset;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      use Ada.Streams;
      B_Amt : aliased Ub8 := Ub8 (Item'Length);
      C_Amt : aliased Ub8 := 0;
   begin
      Lob.Position := Ub8 (Offset);
      Check_Error
        (OCILobRead2
           (Svchp     => Lob.Connect.Service,
            Errhp     => Get_Error_Handle,
            Locp      => Lob.OCI_Locator,
            Byte_Amtp => B_Amt'Unchecked_Access,
            Char_Amtp => C_Amt'Unchecked_Access,
            Offset    => Lob.Position,
            Bufp      => Item (1)'Address,
            Bufl      => Item'Length,
            Piece     => OCI_ONE_PIECE,
            Ctxp      => Null_OCIHandle,
            Cbfp      => Null_OCIHandle,
            Csid      => 0,
            Csfrm     => SQLCS_IMPLICIT));
      Last := Item'First + Stream_Element_Offset (B_Amt) - 1;
      Lob.Position := Lob.Position + B_Amt;
   end Lob_Read;

   ---------------
   -- Lob_Write --
   ---------------

   procedure Lob_Write
     (Lob    : Lob_Locator;
      Offset : Ada.Streams.Stream_Element_Offset;
      Item   : Ada.Streams.Stream_Element_Array)
   is
      B_Amt : aliased Ub8 := Ub8 (Item'Length);
      C_Amt : aliased Ub8 := 0;
   begin
      Lob.Position := Ub8 (Offset);
      Check_Error
        (OCILobWrite2
           (Svchp     => Lob.Connect.Service,
            Errhp     => Get_Error_Handle,
            Locp      => Lob.OCI_Locator,
            Byte_Amtp => B_Amt'Unchecked_Access,
            Char_Amtp => C_Amt'Unchecked_Access,
            Offset    => Lob.Position,
            Bufp      => Item (1)'Address,
            Buflen    => Item'Length,
            Piece     => OCI_ONE_PIECE,
            Ctxp      => Null_OCIHandle,
            Cbfp      => Null_OCIHandle,
            Csid      => 0,
            Csfrm     => SQLCS_IMPLICIT));
      Lob.Position := Lob.Position + B_Amt;
   end Lob_Write;

   -------------------
   -- Lob_Set_Index --
   -------------------

   procedure Lob_Set_Index (Lob : Lob_Locator; To : Lob_Length) is
   begin
      Lob.Position := To;
   end Lob_Set_Index;

   -------------------
   -- Lob_Get_Index --
   -------------------

   function Lob_Get_Index (Lob : Lob_Locator) return Lob_Length is
   begin
      return Lob.Position;
   end Lob_Get_Index;

   ----------------------------------------------------------------------------
   -- Number support ----------------------------------------------------------
   ----------------------------------------------------------------------------

   ---------------
   -- To_Number --
   ---------------

   function To_Number (From : Long_Long_Integer) return OCINumber is
      Result : aliased OCINumber;
   begin
      Check_Error
        (OCINumberFromInt
           (Err         => Get_Error_Handle,
            Inum        => From'Address,
            Inum_Length => From'Size / System.Storage_Unit,
            Inum_S_Flag => OCI_NUMBER_SIGNED,
            Number      => Result'Access));
      return Result;
   end To_Number;

   ---------------
   -- To_Number --
   ---------------

   function To_Number (From : Long_Float) return OCINumber is
      Result : aliased OCINumber;
   begin
      Check_Error
        (OCINumberFromReal
           (Err         => Get_Error_Handle,
            Rnum        => From'Address,
            Rnum_Length => From'Size / System.Storage_Unit,
            Number      => Result'Access));
      return Result;
   end To_Number;

   -----------------
   -- To_C_String --
   -----------------

   function To_C_String
     (From : OCINumber; Format : String := "TM9")
      return Interfaces.C.Strings.chars_ptr
   is
      Buff : aliased Text := (0 .. 127 => Interfaces.C.nul);
      Len  : aliased Ub4 := Buff'Length;
      NLS  : constant String := "NLS_NUMERIC_CHARACTERS='. '";
   begin
      Check_Error
        (OCINumberToText
           (Err        => Get_Error_Handle,
            Date       => From,
            Fmt        => Interfaces.C.To_C (Format),
            Fmt_Length => Format'Length,
            Nls_Name   => Interfaces.C.To_C (NLS),
            Nls_Length => NLS'Length,
            Buf_Size   => Len'Access,
            Buf        => Interfaces.C.Strings.To_Chars_Ptr
              (Buff'Unchecked_Access)));
      return Interfaces.C.Strings.To_Chars_Ptr (Buff'Unchecked_Access, False);
   end To_C_String;

   --  ---------------
   --  -- To_String --
   --  ---------------
   --
   --  function To_String
   --    (From : OCINumber; Format : String := "TM9") return String
   --  is
   --     Str : constant Interfaces.C.Strings.chars_ptr :=
   --       To_C_String (From, Format);
   --  begin
   --     if Str = Null_Ptr then
   --        return "";
   --     end if;
   --
   --     return Interfaces.C.Strings.Value (Str);
   --  end To_String;

   ----------------------------------------------------------------------------
   -- DateTime support --------------------------------------------------------
   ----------------------------------------------------------------------------

   ------------
   -- To_Ada --
   ------------

   function To_Ada (DT : OCIDate) return Ada.Calendar.Time is
   begin
      return Ada.Calendar.Time_Of
        (Year    => Integer (DT.OCIDateYYYY),
         Month   => Integer (DT.OCIDateMM),
         Day     => Integer (DT.OCIDateDD),
         Seconds => Duration
           (Natural (DT.OCIDateTime.OCITimeSS)
            + 60 * (Natural (DT.OCIDateTime.OCITimeMI)
              + 60 * Natural (DT.OCIDateTime.OCITimeHH))));
   end To_Ada;

   ----------------
   -- To_OCIDate --
   ----------------

   function To_OCIDate (DT : Ada.Calendar.Time) return OCIDate is
      use Ada.Calendar;
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Day_Sec : Day_Duration;
      Secs    : Natural;
   begin
      Split (DT, Year, Month, Day, Day_Sec);
      Secs := (if Day_Sec = 0.0 then 0 else Natural (Day_Sec - 0.5));
      return (OCIDateYYYY => Sb2 (Year),
              OCIDateMM   => Ub1 (Month),
              OCIDateDD   => Ub1 (Day),
              OCIDateTime =>
                (OCITimeHH => Ub1 (Secs / 3_600),
                 OCITimeMI => Ub1 (Secs / 60),
                 OCITimeSS => Ub1 (Secs mod 60)));
   end To_OCIDate;

   -----------------
   -- To_C_String --
   -----------------

   function To_C_String
     (From   : OCIDate;
      Format : String := "YYYY-MM-DD HH24:MI:SS")
      return Interfaces.C.Strings.chars_ptr
   is
      Buff : aliased Text := (0 .. Format'Length + 64 => Interfaces.C.nul);
      Len  : aliased Ub4 := Buff'Length - 1;
   begin
      Check_Error
        (OCIDateToText
           (Err         => Get_Error_Handle,
            Date        => From,
            Fmt         => Interfaces.C.To_C (Format),
            Fmt_Length  => Format'Length,
            Lang_Name   => Interfaces.C.Strings.Null_Ptr,
            Lang_Length => 0,
            Buf_Size    => Len'Access,
            Buf         => Interfaces.C.Strings.To_Chars_Ptr
              (Buff'Unchecked_Access)));
      return Interfaces.C.Strings.To_Chars_Ptr (Buff'Unchecked_Access, False);
   end To_C_String;

   --  ---------------
   --  -- To_String --
   --  ---------------
   --
   --  function To_String
   --    (From   : OCIDate;
   --     Format : String := "YYYY-MM-DD HH24:MI:SS") return String
   --  is
   --     Str : constant Interfaces.C.Strings.chars_ptr :=
   --       To_C_String (From, Format);
   --  begin
   --     if Str = Null_Ptr then
   --        return "";
   --     end if;
   --
   --     return Interfaces.C.Strings.Value (Str);
   --  end To_String;

   -------------------------------
   -- UTC_Time_Offset_To_String --
   -------------------------------

   function UTC_Time_Offset_To_String
     (Offset : Ada.Calendar.Time_Zones.Time_Offset) return String
   is
      use Ada.Calendar.Time_Zones;
      To_Char : constant array (0 .. 9) of Character := "0123456789";
      Result  : String := "_00:00";
      Hour    : constant Integer := Integer (Offset / 60);
      Minute  : constant Integer := Integer (Offset mod 60);
   begin
      Result (Result'First) := (if Offset < 0 then '-' else '+');

      --  Hour processing, positions 2 and 3

      Result (2) := To_Char (Hour / 10);
      Result (3) := To_Char (Hour mod 10);

      --  Minute processing, positions 5 and 6

      Result (5) := To_Char (Minute / 10);
      Result (6) := To_Char (Minute mod 10);

      return Result;
   end UTC_Time_Offset_To_String;

   ------------------------
   -- To_Second_Duration --
   ------------------------

   function To_Second_Duration
     (Fsec : Ub4) return Ada.Calendar.Formatting.Second_Duration
   is
      To_Char : constant array (0 .. 9) of Character := "0123456789";
      Result  : String := "000000000";
   begin
      for I in 1 .. 8 loop
         Result (I) := To_Char (Integer (Fsec / 10 ** (9 - I)));
      end loop;

      Result (9) := To_Char (Integer (Fsec mod 10));
      return Ada.Calendar.Formatting.Second_Duration'Value ("0." & Result);
   end To_Second_Duration;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (DT : OCIDateTime) return Ada.Calendar.Time is
      YYYY : aliased Sb2;
      MM   : aliased Ub1;
      DD   : aliased Ub1;
      HH24 : aliased Ub1;
      MI   : aliased Ub1;
      SS   : aliased Ub1;
      FS   : aliased Ub4;
      TZH  : aliased Sb1;
      TZM  : aliased Sb1;
   begin
      Check_Error
        (OCIDateTimeGetDate
           (Hndl     => Get_Environment_Handle,
            Err      => Get_Error_Handle,
            Datetime => DT,
            Year     => YYYY'Access,
            Month    => MM'Access,
            Day      => DD'Access));

      Check_Error
        (OCIDateTimeGetTime
           (Hndl     => Get_Environment_Handle,
            Err      => Get_Error_Handle,
            Datetime => DT,
            Hour     => HH24'Access,
            Min      => MI'Access,
            Sec      => SS'Access,
            Fsec     => FS'Access));

      Check_Error
        (OCIDateTimeGetTimeZoneOffset
           (Hndl     => Get_Environment_Handle,
            Err      => Get_Error_Handle,
            Datetime => DT,
            Hour     => TZH'Access,
            Min      => TZM'Access));

      return Ada.Calendar.Formatting.Time_Of
        (Year        => Ada.Calendar.Year_Number (YYYY),
         Month       => Ada.Calendar.Month_Number (MM),
         Day         => Ada.Calendar.Day_Number (DD),
         Hour        => Ada.Calendar.Formatting.Hour_Number (HH24),
         Minute      => Ada.Calendar.Formatting.Minute_Number (MI),
         Second      => Ada.Calendar.Formatting.Second_Number (SS),
         Sub_Second  => To_Second_Duration (FS),
         Leap_Second => False,
         Time_Zone   => Ada.Calendar.Time_Zones.Time_Offset
           ((if TZH < 0 then -1 else 1) * (abs (TZH) * 60 + TZM)));
   end To_Ada;

   --------------------
   -- To_OCIDateTime --
   --------------------

   function To_OCIDateTime (DT : Ada.Calendar.Time) return OCIDateTime is
      use Ada.Calendar;
      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Day_Secs   : Duration;
      Hour       : Integer;
      Minute     : Integer;
      Second     : Integer;
      Sub_Second : Day_Duration;
      Res        : aliased OCIDateTime := Init_OCIDateTime;
      TZ         : constant Time_Zones.Time_Offset :=
        Time_Zones.UTC_Time_Offset (DT);
      TZ_String  : constant String := UTC_Time_Offset_To_String (TZ);
      Secs       : Natural;
   begin
      Ada.Calendar.Split (DT, Year, Month, Day, Day_Secs);
      Secs       := (if Day_Secs = 0.0 then 0 else Natural (Day_Secs - 0.5));
      Sub_Second := Day_Secs - Day_Duration (Secs);
      Hour       := Secs / 3_600;
      Secs       := Secs mod 3_600;
      Minute     := Secs / 60;
      Second     := Secs mod 60;
      Check_Error
        (OCIDateTimeConstruct
           (Hndl            => Get_Environment_Handle,
            Err             => Get_Error_Handle,
            Datetime        => Res'Access,
            Year            => Sb2 (Year),
            Month           => Ub1 (Month),
            Day             => Ub1 (Day),
            Hour            => Ub1 (Hour),
            Min             => Ub1 (Minute),
            Sec             => Ub1 (Second),
            Fsec            => Ub4 (Sub_Second),
            Timezone        => Interfaces.C.To_C (TZ_String, False),
            Timezone_Length => TZ_String'Length));
      return Res;
   end To_OCIDateTime;

   -----------------
   -- To_C_String --
   -----------------

   function To_C_String
     (From   : OCIDateTime;
      Format : String := "YYYY-MM-DD HH24:MI:SS TZH:TZM")
      return Interfaces.C.Strings.chars_ptr
   is
      Buff : aliased Text := (0 .. Format'Length + 64 => Interfaces.C.nul);
      Len  : aliased Ub4 := Buff'Length - 1;
   begin
      Check_Error
        (OCIDateTimeToText
           (Hndl         => Get_Environment_Handle,
            Err          => Get_Error_Handle,
            Date         => From,
            Fmt          => Interfaces.C.To_C (Format),
            Fmt_length   => Format'Length,
            Fsprec       => 9,
            Lang_Name    => Interfaces.C.Strings.Null_Ptr,
            Lang_Lenngth => 0,
            Buf_Size     => Len'Access,
            Buf          => Interfaces.C.Strings.To_Chars_Ptr
              (Buff'Unchecked_Access)));
      return Interfaces.C.Strings.To_Chars_Ptr (Buff'Unchecked_Access, False);
   end To_C_String;

   --  ---------------
   --  -- To_String --
   --  ---------------
   --
   --  function To_String
   --    (From   : OCIDateTime;
   --     Format : String := "YYYY-MM-DD HH24:MI:SS TZH:TZM") return String
   --  is
   --     Str : constant Interfaces.C.Strings.chars_ptr :=
   --       To_C_String (From, Format);
   --  begin
   --     if Str = Null_Ptr then
   --        return "";
   --     end if;
   --
   --     return Interfaces.C.Strings.Value (Str);
   --  end To_String;

   ----------------------
   -- Init_OCIDateTime --
   ----------------------

   function Init_OCIDateTime return OCIDateTime is
   begin
      return OCIDateTime
        (Alloc_Descriptor (Get_Environment_Handle, OCI_DTYPE_TIMESTAMP_TZ));
   end Init_OCIDateTime;

end GNATCOLL.SQL.Oracle.Gnade;
