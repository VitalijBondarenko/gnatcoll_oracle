with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Registry;             use GNAT.Registry;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Environment_Variables; use Ada.Environment_Variables;

separate (GNATCOLL.SQL.Oracle)

------------------------
-- Find_Tnsnames_File --
------------------------

function Find_Tnsnames_File return String is

   function Get_Oracle_Home_Key return HKEY;

   function Get_Oracle_Home return String;

   -------------------------
   -- Get_Oracle_Home_Key --
   -------------------------

   function Get_Oracle_Home_Key return HKEY is
      Oracle_Key            : HKEY;
      Oracle9_All_Homes_Key : HKEY;
      Oracle_Home_Key       : HKEY;
      Oracle10_Home_Key     : HKEY;

      procedure Subkey_Action
        (Index    : Positive;
         Key      : HKEY;
         Key_Name : String;
         Quit     : in out Boolean);

      -------------------
      -- Subkey_Action --
      -------------------

      procedure Subkey_Action
        (Index    : Positive;
         Key      : HKEY;
         Key_Name : String;
         Quit     : in out Boolean)
      is
         pragma Unreferenced (Index);
      begin
         if Key_Name (Key_Name'First .. Key_Name'First + 3) = "KEY_" then
            Oracle10_Home_Key := Open_Key (Key, "");
            Quit := True;
         end if;
      end Subkey_Action;

      procedure For_Each_Key is new For_Every_Key (Action => Subkey_Action);

      --  Get_Oracle_Home_Key begin
   begin
      Oracle_Key :=
        Open_Key (HKEY_LOCAL_MACHINE, "Software\Oracle", Read_Only);

      --  Oracle version up to 9
      if Key_Exists (Oracle_Key, "ALL_HOMES") then
         Oracle9_All_Homes_Key := Open_Key (Oracle_Key, "ALL_HOMES");
         Oracle_Home_Key := Open_Key
           (Oracle_Key,
            "HOME" & Query_Value (Oracle9_All_Homes_Key, "LAST_HOME"));
         Close_Key (Oracle9_All_Homes_Key);
      end if;

      --  Oracle version 10 and above
      For_Each_Key (Oracle_Key, False);

      if Key_Exists (Oracle10_Home_Key, "") then
         Oracle_Home_Key := Open_Key (Oracle10_Home_Key, "");
         Close_Key (Oracle10_Home_Key);
      end if;

      Close_Key (Oracle_Key);
      return Oracle_Home_Key;
   end Get_Oracle_Home_Key;

   ---------------------
   -- Get_Oracle_Home --
   ---------------------

   function Get_Oracle_Home return String is
   begin
      if Exists ("ORACLE_HOME") then
         return Value ("ORACLE_HOME");
      else
         return Query_Value (Get_Oracle_Home_Key, "ORACLE_HOME");
      end if;

   exception
      when others => return "";
   end Get_Oracle_Home;

   Tns_File_Name : constant String := "tnsnames.ora";
   Ora_Key       : HKEY;

   --  Find_Tnsnames_File begin
begin
   --  1. Current path (associated with the running client application)
   declare
      S : constant String :=
        Normalize_Pathname (Dir_Name (Command_Name))
        & GNAT.OS_Lib.Directory_Separator
        & Tns_File_Name;
   begin
      if GNAT.OS_Lib.Is_Regular_File (S) then
         return S;
      end if;
   end;

   --  2. Environment variable TNS_ADMIN defined for the session
   --  3. Environment variable TNS_ADMIN defined for the system
   if Exists ("TNS_ADMIN") then
      declare
         S : constant String :=
           Value ("TNS_ADMIN")
           & GNAT.OS_Lib.Directory_Separator
           & Tns_File_Name;
      begin
         if GNAT.OS_Lib.Is_Regular_File (S) then
            return S;
         end if;
      end;
   end if;

   --  4. Windows Registry Key TNS_ADMIN
   Ora_Key := Get_Oracle_Home_Key;

   if Key_Exists (Ora_Key, "TNS_ADMIN") then
      declare
         S : constant String :=
           Query_Value (Ora_Key, "TNS_ADMIN")
           & GNAT.OS_Lib.Directory_Separator
           & Tns_File_Name;
      begin
         if GNAT.OS_Lib.Is_Regular_File (S) then
            return S;
         end if;
      end;
   end if;

   --  5. %ORACLE_HOME%\network\admin
   if Get_Oracle_Home /= "" then
      declare
         S : constant String :=
           Get_Oracle_Home
           & GNAT.OS_Lib.Directory_Separator
           & "network"
           & GNAT.OS_Lib.Directory_Separator
           & "admin"
           & GNAT.OS_Lib.Directory_Separator
           & Tns_File_Name;
      begin
         if GNAT.OS_Lib.Is_Regular_File (S) then
            return S;
         end if;
      end;
   end if;

   return "";

exception
   when others => return "";
end Find_Tnsnames_File;
