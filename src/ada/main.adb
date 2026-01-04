-- SPDX-License-Identifier: AGPL-3.0-or-later
-- main.adb - Entry point for nerdsafe-restart TUI
--
-- Usage:
--   nerdsafe-tui              Run interactive TUI
--   nerdsafe-tui --detect     Show detected resources and exit
--   nerdsafe-tui --quick      Run quick validation and exit
--   nerdsafe-tui --standard   Run standard validation and exit
--   nerdsafe-tui --thorough   Run thorough validation and exit
--   nerdsafe-tui --paranoid   Run paranoid validation and exit

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;
with Nerdsafe_TUI;        use Nerdsafe_TUI;

procedure Main is
   Limits  : Resource_Limits;
   Result  : Exit_Code;
begin
   --  Auto-detect system resources
   Auto_Detect_Resources (Limits);

   --  Check for command-line arguments
   if Argument_Count = 0 then
      --  No arguments: run interactive TUI
      Run_TUI;

   elsif Argument_Count = 1 then
      declare
         Arg : constant String := Argument (1);
      begin
         if Arg = "--help" or Arg = "-h" then
            Put_Line ("nerdsafe-tui - Pre-reboot Shell Configuration Validator");
            Put_Line ("");
            Put_Line ("Usage:");
            Put_Line ("  nerdsafe-tui              Run interactive TUI");
            Put_Line ("  nerdsafe-tui --detect     Show detected resources");
            Put_Line ("  nerdsafe-tui --quick      Run quick validation");
            Put_Line ("  nerdsafe-tui --standard   Run standard validation");
            Put_Line ("  nerdsafe-tui --thorough   Run thorough validation");
            Put_Line ("  nerdsafe-tui --paranoid   Run paranoid validation");
            Put_Line ("  nerdsafe-tui --version    Show version");
            Put_Line ("");
            Put_Line ("Exit codes:");
            Put_Line ("  0  - All validations passed");
            Put_Line ("  1  - Syntax errors found");
            Put_Line ("  2  - Timeout exceeded");
            Put_Line ("  3  - ShellCheck warnings");
            Put_Line ("  4  - Missing critical files");
            Put_Line ("  5  - Permission errors");
            Put_Line ("  10 - Container build failed");
            Put_Line ("  11 - Resource detection failed");

         elsif Arg = "--version" or Arg = "-v" then
            Put_Line ("nerdsafe-tui version 0.1.0");
            Put_Line ("License: AGPL-3.0-or-later");

         elsif Arg = "--detect" then
            Put_Line ("System Resources Detected:");
            Put_Line ("  Total Memory:     " & Format_Memory (Get_Total_Memory_KB));
            Put_Line ("  Available Memory: " & Format_Memory (Get_Available_Memory_KB));
            Put_Line ("  CPU Count:        " & Positive'Image (Get_CPU_Count));
            Put_Line ("  Swap Total:       " & Format_Memory (Get_Swap_Total_KB));
            Put_Line ("");
            Put_Line ("Container Limits (calculated):");
            Put_Line ("  Memory Limit:     " & Positive'Image (Limits.Memory_MB) & " MB");
            Put_Line ("  CPU Limit:        " & Positive'Image (Limits.CPU_Count));
            Put_Line ("  PID Limit:        " & Positive'Image (Limits.PID_Limit));
            Put_Line ("  Timeout:          " & Positive'Image (Limits.Timeout_Sec) & "s");
            Put_Line ("");
            Put_Line ("Container Runtime: " & Get_Container_Command);
            if Is_Container_Available then
               Put_Line ("  Status: Available");
            else
               Put_Line ("  Status: NOT FOUND");
            end if;

         elsif Arg = "--quick" then
            Put_Line ("Running quick validation...");
            Result := Run_Validation (Quick, Limits);
            Display_Result (Result, 5.0);
            Set_Exit_Status (Exit_Code'Pos (Result));

         elsif Arg = "--standard" then
            Put_Line ("Running standard validation...");
            Result := Run_Validation (Standard, Limits);
            Display_Result (Result, 30.0);
            Set_Exit_Status (Exit_Code'Pos (Result));

         elsif Arg = "--thorough" then
            Put_Line ("Running thorough validation...");
            Result := Run_Validation (Thorough, Limits);
            Display_Result (Result, 120.0);
            Set_Exit_Status (Exit_Code'Pos (Result));

         elsif Arg = "--paranoid" then
            Put_Line ("Running paranoid validation...");
            Result := Run_Validation (Paranoid, Limits);
            Display_Result (Result, 300.0);
            Set_Exit_Status (Exit_Code'Pos (Result));

         else
            Put_Line ("Unknown argument: " & Arg);
            Put_Line ("Use --help for usage information.");
            Set_Exit_Status (1);
         end if;
      end;

   else
      Put_Line ("Too many arguments.");
      Put_Line ("Use --help for usage information.");
      Set_Exit_Status (1);
   end if;

end Main;
