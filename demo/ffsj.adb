
-- Prototype code for joystick demo: Simple joystick tracker with no bells and
-- whistles; most output goes to stdout.  Uses fixed-function OpenGL pipeline,
-- rather than the shader-based 3.3 pipeline used by the otherwise identical
-- program simple_joy.

-- WMR 13 Feb 2013


-- Environment
with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Float_Text_IO;
with Ada.Text_IO;

with Lumen.Events;
with Lumen.GL;
with Lumen.GLU;
with Lumen.Joystick;
with Lumen.Shader;
with Lumen.Window;

use Lumen;

-- The simple joys are the best, even with the fixed-function pipeline
procedure FFSJ is

   ---------------------------------------------------------------------------

   Program_Exit : exception;

   ---------------------------------------------------------------------------

   -- Keystrokes we care about
   Escape   : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.ESC));
   Letter_q : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.LC_Q));
   Space    : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.Space));

   ---------------------------------------------------------------------------

   -- Globals, yuck
   Win                : Window.Window_Handle;
   Wide               : Natural := 400;
   High               : Natural := 400;
   Terminated         : Boolean := False;
   Stick              : Joystick.Handle;
   Have_JS            : Boolean := False;
   Calibrating        : Boolean := True;
   Moved              : Boolean := False;
   Stdout_Tracking    : Boolean := False;
   Min_Axis           : Integer;
   Max_Axis           : Integer;
   Vertex_Array_ID    : GL.UInt;
   Vertex_Buffer_ID   : GL.UInt;
   Element_Array_ID   : GL.UInt;
   Cross_ID           : GL.UInt;
   Cross_Elts         : GL.UInt;
   Vertex_Shader_ID   : GL.UInt;
   Fragment_Shader_ID : GL.UInt;
   Compiled           : Boolean;
   Program_ID         : GL.UInt;
   Color_Loc          : GL.Int;
   Xlat_Loc           : GL.Int;
   Joy_X              : Float := 0.0;
   Joy_Y              : Float := 0.0;

   ---------------------------------------------------------------------------

   -- Adjust joystick axis value to -1.0 .. 1.0
   function Scale_Axis (Value : Integer) return Float is

      Factor : constant Float := Float (Max_Axis) - Float (Min_Axis);

   begin  -- Scale_Axis
      return Float (Value) / Factor * 2.0;
   end Scale_Axis;

   ---------------------------------------------------------------------------

   -- Set or reset the window view parameters
   procedure Set_View (W, H : in Natural) is

      Aspect : GL.Double;

   begin  -- Set_View

      -- Viewport dimensions
      GL.Viewport (0, 0, GL.SizeI (W), GL.SizeI (H));

      -- Set up the projection matrix based on the window's shape--wider than
      -- high, or higher than wide
      GL.Matrix_Mode (GL.GL_PROJECTION);
      GL.Load_Identity;

      if W <= H then
         Aspect := GL.Double (H) / GL.Double (W);
         GLU.Ortho_2D (-2.0, 2.0, -2.0 * Aspect, 2.0 * Aspect);
      else
         Aspect := GL.Double (W) / GL.Double (H);
         GLU.Ortho_2D (-2.0 * Aspect, 2.0 * Aspect, -2.0, 2.0);
      end if;
   end Set_View;

   ---------------------------------------------------------------------------

   -- Re-draw the scene
   procedure Draw is
   begin  -- Draw

      -- Clear the screen
      GL.Clear (GL.GL_COLOR_BUFFER_BIT);

      -- Draw the tracking cross
      GL.Color (Float (0.0), 0.0, 0.0, 1.0);  -- black
      GL.Begin_Primitive (GL.GL_LINES);
      GL.Vertex (Joy_X - 0.1, Joy_Y);
      GL.Vertex (Joy_X + 0.1, Joy_Y);
      GL.End_Primitive;

      GL.Begin_Primitive (GL.GL_LINES);
      GL.Vertex (Joy_X, Joy_Y - 0.1);
      GL.Vertex (Joy_X, Joy_Y + 0.1);
      GL.End_Primitive;

      -- Swap in the backbuffer so we can see what we just drew
      Lumen.Window.Swap (Win);
   end Draw;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for Key_Press events
   procedure Key_Press
     (Category  : Events.Key_Category;
      Symbol    : Events.Key_Symbol;
      Modifiers : Events.Modifier_Set) is
   begin  -- Key_Press
      if Symbol = Escape or Symbol = Letter_q then
         Terminated := True;
      end if;
   end Key_Press;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for Exposed events
   procedure Expose_Handler
     (Top    : Integer;
      Left   : Integer;
      Height : Natural;
      Width  : Natural) is
   begin  -- Expose_Handler
      Draw;
   end Expose_Handler;

   ---------------------------------------------------------------------------

   -- Simple event handler routine for Resized events
   procedure Resize_Handler
     (Height : Integer;
      Width  : Integer) is
   begin  -- Resize_Handler
      Wide := Width;
      High := Height;
      Set_View (Wide, High);
      Draw;
   end Resize_Handler;

   ---------------------------------------------------------------------------

   -- Initialize graphics
   procedure Init is
   begin  -- Init

      -- Create Lumen window, accepting most defaults
      Lumen.Window.Create (Win, Name => "Fixed-Function Simple Joystick Tracker", Width => Wide, Height => High);
      Win.Key_Press := Key_Press'Unrestricted_Access;
      Win.Exposed   := Expose_Handler'Unrestricted_Access;
      Win.Resize    := Resize_Handler'Unrestricted_Access;

      -- Orange background
      GL.Clear_Color (1.0, 0.8, 0.4, 1.0);

   end Init;

   ---------------------------------------------------------------------------

begin  -- FFSJ

   -- Handle command line
   declare
      use Ada.Command_Line;
   begin
      for Index in 1 .. Argument_Count loop
         declare
            Arg : String := Argument (Index);
         begin
            -- Separate options from the pathname
            if Arg (Arg'First) = '-' then
               if Arg'Length > 1 and then Arg (Arg'First + 1) = 't' then
                  -- Turn on stdout tracking
                  Stdout_Tracking := True;
               end if;
            else
               begin
                  Lumen.Joystick.Open (Stick, Path => Arg);
                  Ada.Text_IO.Put_Line ("Found joystick " & Joystick.Name (Stick) & " at " & Arg);
                  Have_JS := True;
               exception
                  when Joystick.Open_Failed =>
                     Have_JS := False;
               end;
            end if;
         end;
      end loop;
   end;

   -- Try opening default joystick if no usable pathname given
   if not Have_JS then
      begin
         Lumen.Joystick.Open (Stick);
         Ada.Text_IO.Put_Line ("Found joystick " & Joystick.Name (Stick) & " at " & Joystick.Default_Pathname);
         Have_JS := True;

      exception
         when Joystick.Open_Failed =>
            Ada.Text_IO.Put_Line ("You don't seem to have a joystick.  Too bad!");
            Have_JS := False;
      end;
   end if;

   -- Report what we found, and calibrate it
   if Have_JS then
      declare
         use Ada.Text_IO;
      begin
         Put_Line ("Axes           " & Natural'Image (Joystick.Axes (Stick)));
         Put_Line ("Buttons        " & Natural'Image (Joystick.Buttons (Stick)));

         New_Line;
         Put_Line ("Now calibrate your joystick:  move an axis through its entire range ...");
         Put_Line ("When done, click a joystick button.");

         Min_Axis := Integer'Last;
         Max_Axis := Integer'First;
         while Calibrating loop
            while Joystick.Pending (Stick) > 0 loop
               declare
                  use Joystick;

                  Joy : constant Joystick_Event_Data := Next_Event (Stick);
               begin
                  case Joy.Which is
                     when Joystick_Button_Press | Joystick_Button_Release =>
                        Calibrating := False;

                     when Joystick_Axis_Change =>
                        Moved := True;
                        if Joy.Axis_Value < Min_Axis then
                           Min_Axis := Joy.Axis_Value;
                        elsif Joy.Axis_Value > Max_Axis then
                           Max_Axis := Joy.Axis_Value;
                        end if;
                  end case;
               end;
            end loop;
         end loop;

         if Moved then
            Put_Line ("I saw values in the range " & Integer'Image (Min_Axis) & " .. " & Integer'Image (Max_Axis));
            Put_Line ("And away we go!");
         else
            Put_Line ("I didn't see any axis move ... maybe try that again.");
            raise Program_Exit;
         end if;
      end;
   end if;

   -- Initialize Lumen and OpenGL
   Init;

   -- Loop until user hits the Escape (done by the Key_Press routine above) or
   -- clicks the window's Close button (done by Process_Events)
   while Lumen.Window.Process_Events (Win) loop

      -- Exit when they hit the Esc key
      exit when Terminated;

      -- Make something appear
      Draw;

      -- Process joystick events if there are any pending
      if Have_JS then
         while Joystick.Pending (Stick) > 0 loop
            declare
               use Ada.Text_IO;
               use Joystick;

               Joy : constant Joystick_Event_Data := Next_Event (Stick);
            begin
               case Joy.Which is
                  when Joystick_Button_Press =>
                     Put_Line ("Button" & Positive'Image (Joy.Number) & " press");

                  when Joystick_Button_Release =>
                     Put_Line ("Button" & Positive'Image (Joy.Number) & " release");

                  when Joystick_Axis_Change =>
                     if Stdout_Tracking then
                        Put ("Axis" & Positive'Image (Joy.Number) & " --> " & Integer'Image (Joy.Axis_Value) & "  (");
                        Ada.Float_Text_IO.Put (Scale_Axis (Joy.Axis_Value), Fore => 0, Aft => 2, Exp => 0);
                        Put (")");
                        New_Line;
                     end if;

                     -- Treat axis 1 as X, axis 2 as Y
                     if Joy.Number = 1 then
                        Joy_X := Scale_Axis (Joy.Axis_Value);
                     elsif Joy.Number = 2 then
                        Joy_Y :=  -Scale_Axis (Joy.Axis_Value);  -- invert Y, just cuz
                     end if;
               end case;
            end;
         end loop;
      end if;

   end loop;

exception
   when Program_Exit =>
      null;
end FFSJ;
