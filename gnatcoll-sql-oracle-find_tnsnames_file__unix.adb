with Ada.Environment_Variables; use Ada.Environment_Variables;
with GNAT.OS_Lib;

separate (GNATCOLL.SQL.Oracle)

------------------------
-- Find_Tnsnames_File --
------------------------

function Find_Tnsnames_File return String is
   Tns_File_Name : constant String := "tnsnames.ora";
begin
   --  1. $HOME for hidden files only (i.e., .sqlnet.ora and .tnsnames.ora)
   if Exists ("HOME") then
      declare
         S : constant String :=
           Value ("HOME")
           & GNAT.OS_Lib.Directory_Separator
           & "." & Tns_File_Name;
      begin
         if GNAT.OS_Lib.Is_Regular_File (S) then
            return S;
         end if;
      end;
   end if;

   --  2. $TNS_ADMIN
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

   --  3. $HOME
   if Exists ("HOME") then
      declare
         S : constant String :=
           Value ("HOME")
           & GNAT.OS_Lib.Directory_Separator
           & Tns_File_Name;
      begin
         if GNAT.OS_Lib.Is_Regular_File (S) then
            return S;
         end if;
      end;
   end if;

   --  4. /etc or /var/opt/oracle (depends on platform)
   declare
      S_Unix : constant String :=
        GNAT.OS_Lib.Directory_Separator
        & "etc"
        & GNAT.OS_Lib.Directory_Separator
        & Tns_File_Name;
      S_Solaris : constant String :=
        GNAT.OS_Lib.Directory_Separator
        & "var"
        & GNAT.OS_Lib.Directory_Separator
        & "opt"
        & GNAT.OS_Lib.Directory_Separator
        & "oracle"
        & GNAT.OS_Lib.Directory_Separator
        & Tns_File_Name;
   begin
      if GNAT.OS_Lib.Is_Regular_File (S_Unix) then
         return S_Unix;
      end if;

      if GNAT.OS_Lib.Is_Regular_File (S_Solaris) then
         return S_Solaris;
      end if;
   end;

   --  5. $ORACLE_HOME/network/admin
   if Exists ("ORACLE_HOME") then
      declare
         S : constant String :=
           Value ("ORACLE_HOME")
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
end Find_Tnsnames_File;
