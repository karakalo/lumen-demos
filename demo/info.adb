
-- Print various bits of OpenGL information, retrieved from the GPU.

with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Command_Line;

with Lumen.GL;
with Lumen.Window;
use Lumen;

procedure Info is

   ---------------------------------------------------------------------------

   Win   : Lumen.Window.Window_Handle;
   Index : Natural := 0;

   ---------------------------------------------------------------------------

begin  -- Info

   -- Create Lumen window, accepting most defaults
   Lumen.Window.Create (Win, Name => "Tutorial 02");

   -- Fetch and print the info
   Put_Line ("OpenGL version:           " & GL.Get_String (GL.GL_VERSION));
   Put_Line ("Renderer:                 " & GL.Get_String (GL.GL_RENDERER));
   Put_Line ("Vendor:                   " & GL.Get_String (GL.GL_VENDOR));
   Put_Line ("Shader language version:  " & GL.Get_String (GL.GL_SHADING_LANGUAGE_VERSION));

   if Ada.Command_Line.Argument_Count > 0 then
      Put_Line ("Extensions:");

      loop
         declare
            Name : String := GL.Get_String (GL.GL_EXTENSIONS, Index);
         begin
            exit when Name = "";
            Put_Line ("   " & Name);
            Index := Index + 1;
         end;
      end loop;
   end if;

   ---------------------------------------------------------------------------

end Info;
