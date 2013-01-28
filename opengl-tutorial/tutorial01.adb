
-- Tutorial 01 from opengl-tutorial.org, done using Lumen
-- Purpose is to create an "OpenGL window".  Draws nothing.

with Lumen.Events;
with Lumen.Window;
use Lumen;

procedure Tutorial01 is

   Win        : Lumen.Window.Window_Handle;
   Terminated : Boolean := False;

   procedure Key_Press
     (Category  : Events.Key_Category;
      Symbol    : Events.Key_Symbol;
      Modifiers : Events.Modifier_Set) is
   begin
      Terminated := Lumen.Events.To_Character (Symbol) = ASCII.ESC;
   end Key_Press;

begin  -- Tutorial01

   -- Create Lumen window, accepting most defaults
   Lumen.Window.Create (Win, Name => "Tutorial 01");
   Win.Key_Press := Key_Press'Unrestricted_Access;

   -- Loop until user hits the Escape or clicks the window's Close button
   while Lumen.Window.Process_Events (Win) loop

      -- Exit when they destroy the window, or when they hit any key
      exit when Terminated;

      Lumen.Window.Swap (Win);  -- not necessary since we drew nothing
   end loop;

end Tutorial01;
