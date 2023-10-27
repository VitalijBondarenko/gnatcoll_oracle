with GNAT.Regpat; use GNAT.Regpat;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.SQL.Oracle.Builder;
with GNATCOLL.SQL.Oracle.Gnade;

package body GNATCOLL.SQL.Oracle is

   -----------
   -- Setup --
   -----------

   function Setup
     (Database      : String;
      User          : String := "";
      Password      : String := "";
      Host          : String := "";
      Port          : Integer := -1;
      SSL           : SSL_Mode := SSL_Disable;
      Cache_Support : Boolean := True;
      Errors        : access Error_Reporter'Class := null)
      return Database_Description
   is
      Result : Oracle_Description_Access;
   begin
      if not Oracle.Builder.Has_Oracle_Support then
         return null;
      end if;

      Result := new Oracle_Description
        (Caching => Cache_Support, Errors => Errors);
      Result.User := To_XString (User);
      Result.Password := To_XString (Password);
      Result.Host := To_XString (Host);
      Result.Dbname := To_XString (Database);
      Result.Port := Port;
      Result.SSL := SSL;

      return Database_Description (Result);
   end Setup;

   ----------------------
   -- Build_Connection --
   ----------------------

   overriding function Build_Connection
     (Self : access Oracle_Description) return Database_Connection
   is
      Result : Database_Connection;
   begin
      Result := Oracle.Builder.Build_Connection (Self);
      Reset_Connection (Result);
      return Result;
   end Build_Connection;

   ---------------------------
   -- Get_Connection_String --
   ---------------------------

   function Get_Connection_Descriptor
     (Description : access Database_Description_Record'Class) return String
   is
      Descr : constant Oracle_Description_Access :=
        Oracle_Description_Access (Description);
   begin
      return Oracle.Gnade.Make_Connection_Descriptor
        (Host         => Descr.Host,
         Port         => Descr.Port,
         Service_Name => Descr.Dbname,
         SSL          => Descr.SSL);
   end Get_Connection_Descriptor;

   ------------------------
   -- Get_Current_Schema --
   ------------------------

   function Get_Current_Schema (Database : Database_Connection) return String
   is
      R : Forward_Cursor;
   begin
      R.Fetch
        (Connection => Database,
         Query      =>
           "SELECT SYS_CONTEXT('USERENV','CURRENT_SCHEMA') FROM DUAL",
         Params     => No_Parameters);
      return R.Value (0);
   end Get_Current_Schema;

   ------------------------
   -- Set_Current_Schema --
   ------------------------

   function Set_Current_Schema
     (Database : Database_Connection;
      Schema   : String) return Boolean
   is
   begin
      if Schema /= "" then
         Database.Execute
           (Query  =>
              "ALTER SESSION SET CURRENT_SCHEMA = " & Schema,
            Params => No_Parameters);
      end if;

      return True;
   exception
      when others => return False;
   end Set_Current_Schema;

   --------------------
   -- Server_Version --
   --------------------

   function Server_Version (Self : Database_Connection) return String is
   begin
      return Builder.Server_Version (Self);
   end Server_Version;

   --------------------
   -- Client_Version --
   --------------------

   function Client_Version return String is
   begin
      return Builder.Client_Version;
   end Client_Version;

   --  -------------
   --  -- As_Date --
   --  -------------
   --
   --  function As_Date (Value : Ada.Calendar.Time) return SQL_Parameter is
   --     R : SQL_Parameter;
   --     P : SQL_Parameter_Date;
   --  begin
   --     P.Val := Value;
   --     R.Set (P);
   --     return R;
   --  end As_Date;

   ------------------------
   -- Find_Tnsnames_File --
   ------------------------

   function Find_Tnsnames_File return String is separate;

   -----------------------
   -- Get_Service_Names --
   -----------------------

   function Get_Service_Names (Tnsfile : String) return Names_List
   is
      Matcher : constant Pattern_Matcher :=
        Compile ("^([^#()\W ][a-zA-Z0-9.]*(?:[.][a-zA-Z0-9]*\s?=)?)");
      Matches : Match_Array (0 .. 0);
      NL      : Names_List;
      F       : File_Type;
      Buf     : String (1 .. 1024);
      L       : Natural;
   begin
      Open (F, In_File, Tnsfile);

      while not End_Of_File (F) loop
         Get_Line (F, Buf, L);
         Match (Matcher, Buf (1 .. L), Matches);

         if Matches (0) /= No_Match then
            Names_Vector.Append
              (NL, To_XString (Buf (Matches (0).First .. Matches (0).Last)));
         end if;
      end loop;

      Close (F);
      return NL;

   exception
      when others =>
         if Is_Open (F) then
            Close (F);
         end if;

         return Empty_Names_List;
   end Get_Service_Names;

end GNATCOLL.SQL.Oracle;
