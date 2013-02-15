
-- Prototype code for joystick demo: Simple joystick tracker with no bells and
-- whistles; most output goes to stdout

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

-- The simple joys are the best
procedure Simple_Joy is

   ---------------------------------------------------------------------------

   Program_Exit : exception;

   ---------------------------------------------------------------------------

   -- Shader source stuff
   type Supported_Shader_Version is ( Shaders_1_20, Shaders_3_3 );

   Vertex_Shader_Pathname_1_20   : constant String := "data/js.vertex.shader.1.20";
   Fragment_Shader_Pathname_1_20 : constant String := "data/js.fragment.shader.1.20";
   Vertex_Shader_Pathname_3_3    : constant String := "data/js.vertex.shader.3.3";
   Fragment_Shader_Pathname_3_3  : constant String := "data/js.fragment.shader.3.3";

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
   Program_ID         : GL.UInt;
   Color_Loc          : GL.Int;
   Xlat_Loc           : GL.Int;
   Joy_X              : Float := 0.0;
   Joy_Y              : Float := 0.0;

   ---------------------------------------------------------------------------

   -- The data to define our background and the tracking cross
   type Vertex_Array_Type is array (Integer range <>) of Float;
   Background : Vertex_Array_Type :=
      (
       -1.0, -1.0,
        1.0, -1.0,
       -1.0,  1.0,

        1.0,  1.0,
       -1.0,  1.0,
        1.0, -1.0
      );
   Background_Size : constant GL.SizeI := Background'Length * (Float'Size / 8);
   Cross : Vertex_Array_Type :=
      (
        0.0,  0.1,
        0.0, -0.1,

       -0.1,  0.0,
        0.1,  0.0
      );
   Cross_Size : constant GL.SizeI := Cross'Length * (Float'Size / 8);

   type Indices is array (Integer range <>) of Natural;
   Quad : Indices := (0, 1, 2, 3, 4, 5);
   Quad_Size : constant GL.SizeI := Quad'Length * (Integer'Size / 8);
   Lines : Indices := (0, 1, 2, 3);
   Lines_Size : constant GL.SizeI := Lines'Length * (Integer'Size / 8);

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
   end Set_View;

   ---------------------------------------------------------------------------

   -- Re-draw the scene
   procedure Draw is
   begin  -- Draw

      -- Clear the screen
      GL.Clear (GL.GL_COLOR_BUFFER_BIT);

      -- Activate our shaders
      GL.Use_Program (Program_ID);

      -- The attribute here (0, output of the fragment shader) is the vertex position (X,Y)
      GL.Enable_Vertex_Attrib_Array (0);
      GL.Bind_Buffer (GL.GL_ARRAY_BUFFER, Vertex_Buffer_ID);
      GL.Vertex_Attrib_Pointer
         (
          0,                  -- attribute 0. No particular reason for 0, but must match the layout in the shader
          2,                  -- 2D vertices
          GL.GL_FLOAT,        -- type
          GL.GL_FALSE,        -- normalized?
          0,                  -- stride
          GL.Null_Pointer     -- array buffer offset
         );

      -- Draw the quad
      GL.Uniform (Color_Loc, 1.0, 0.8, 0.4);  -- a nice light orange background
      GL.Uniform (Xlat_Loc, 0.0, 0.0);
      GL.Bind_Buffer (GL.GL_ELEMENT_ARRAY_BUFFER, Element_Array_ID);
      GL.Draw_Elements (GL.GL_TRIANGLES, 6, GL.GL_UNSIGNED_INT, GL.Null_Pointer);

      -- Draw the tracking cross
      GL.Bind_Buffer (GL.GL_ARRAY_BUFFER, Cross_ID);
      GL.Vertex_Attrib_Pointer
         (
          0,                  -- attribute 0. No particular reason for 0, but must match the layout in the shader
          2,                  -- 2D vertices
          GL.GL_FLOAT,        -- type
          GL.GL_FALSE,        -- normalized?
          0,                  -- stride
          GL.Null_Pointer     -- array buffer offset
         );
      GL.Uniform (Color_Loc, 0.0, 0.0, 0.0);  -- tracking cross is black
      GL.Uniform (Xlat_Loc, Joy_X, Joy_Y);
      GL.Bind_Buffer (GL.GL_ELEMENT_ARRAY_BUFFER, Cross_Elts);
      GL.Draw_Elements (GL.GL_LINES, 4, GL.GL_UNSIGNED_INT, GL.Null_Pointer);

      GL.Disable_Vertex_Attrib_Array (0);

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

      Compiled       : Boolean := False;
      Picked_Shaders : Supported_Shader_Version := Shaders_1_20;

   begin  -- Init

      -- Create Lumen window, accepting most defaults
      Lumen.Window.Create (Win, Name => "Simple Joystick Tracker", Width => Wide, Height => High);
      Win.Key_Press := Key_Press'Unrestricted_Access;
      Win.Exposed   := Expose_Handler'Unrestricted_Access;
      Win.Resize    := Resize_Handler'Unrestricted_Access;

      -- Black background
      GL.Clear_Color (0.0, 0.0, 0.0, 0.0);

      GL.Gen_Vertex_Arrays (1, Vertex_Array_ID'Address);
      GL.Bind_Vertex_Array (Vertex_Array_ID);

      -- Pick the correct shader based on supported version
      begin
         if Float'Value (GL.Get_String (GL.GL_SHADING_LANGUAGE_VERSION)) >= 3.3 then
            Picked_Shaders := Shaders_3_3;
            Ada.Text_IO.Put_Line ("Using 3.3 shader source");
         else
            Picked_Shaders := Shaders_1_20;
            Ada.Text_IO.Put_Line ("Using 1.20 shader source");
         end if;
      exception
         when others =>
            Ada.Text_IO.Put_Line ("Cannot recognize shader version """ & GL.Get_String (GL.GL_SHADING_LANGUAGE_VERSION) &
                                     """--using 1.20");
            Picked_Shaders := Shaders_1_20;  -- lower common denominator?
      end;

      -- Create and compile our GLSL program from the shaders
      if Picked_Shaders = Shaders_1_20 then
         Shader.From_File (GL.GL_VERTEX_SHADER, Vertex_Shader_Pathname_1_20, Vertex_Shader_ID, Compiled);
      else
         Shader.From_File (GL.GL_VERTEX_SHADER, Vertex_Shader_Pathname_3_3, Vertex_Shader_ID, Compiled);
      end if;
      if not Compiled then
         Ada.Text_IO.Put_Line ("Vertex shader failed to compile.  Error:");
         Ada.Text_IO.Put_Line (Shader.Get_Info_Log (Vertex_Shader_ID));
      end if;
      if Picked_Shaders = Shaders_1_20 then
         Shader.From_File (GL.GL_FRAGMENT_SHADER, Fragment_Shader_Pathname_3_3, Fragment_Shader_ID, Compiled);
      else
         Shader.From_File (GL.GL_FRAGMENT_SHADER, Fragment_Shader_Pathname_3_3, Fragment_Shader_ID, Compiled);
      end if;
      if not Compiled then
         Ada.Text_IO.Put_Line ("Fragment shader failed to compile.  Error:");
         Ada.Text_IO.Put_Line (Shader.Get_Info_Log (Fragment_Shader_ID));
      end if;

      -- Link the program
      Program_ID := GL.Create_Program;
      GL.Attach_Shader (Program_ID, Vertex_Shader_ID);
      GL.Attach_Shader (Program_ID, Fragment_Shader_ID);
      GL.Link_Program (Program_ID);

      -- Find the shader input params' locations
      Color_Loc := GL.Get_Uniform_Location (Program_ID, "color");
      Xlat_Loc := GL.Get_Uniform_Location (Program_ID, "xlat");

      -- Done with these now
      GL.Delete_Shader (Vertex_Shader_ID);
      GL.Delete_Shader (Fragment_Shader_ID);

      -- Generate 1 buffer, put the resulting identifier in Vertex_Buffer_ID
      GL.Gen_Buffers (1, Vertex_Buffer_ID'Address);

      -- The following commands will talk about our 'Vertex_Buffer_ID' buffer
      GL.Bind_Buffer (GL.GL_ARRAY_BUFFER, Vertex_Buffer_ID);

      -- Give our vertices to OpenGL.
      GL.Buffer_Data (GL.GL_ARRAY_BUFFER, Background_Size, Background'Address, GL.GL_STATIC_DRAW);

      -- Generate 1 buffer, put the resulting identifier in Element_Array_ID
      GL.Gen_Buffers (1, Element_Array_ID'Address);

      -- The following commands will talk about our 'ELement_Array_ID' buffer
      GL.Bind_Buffer (GL.GL_ELEMENT_ARRAY_BUFFER, Element_Array_ID);

      -- Give our element indices to OpenGL.
      GL.Buffer_Data (GL.GL_ELEMENT_ARRAY_BUFFER, Quad_Size, Quad'Address, GL.GL_STATIC_DRAW);

      -- Now set up to draw the tracking cross, same as above with different data
      GL.Gen_Buffers (1, Cross_ID'Address);
      GL.Bind_Buffer (GL.GL_ARRAY_BUFFER, Cross_ID);
      GL.Buffer_Data (GL.GL_ARRAY_BUFFER, Cross_Size, Cross'Address, GL.GL_STATIC_DRAW);

      GL.Gen_Buffers (1, Cross_Elts'Address);
      GL.Bind_Buffer (GL.GL_ELEMENT_ARRAY_BUFFER, Cross_Elts);
      GL.Buffer_Data (GL.GL_ELEMENT_ARRAY_BUFFER, Lines_Size, Lines'Address, GL.GL_STATIC_DRAW);
   end Init;

   ---------------------------------------------------------------------------

begin  -- Simple_Joy

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

         Hit_Char  : Character;
         Have_Char : Boolean := False;
      begin
         Put_Line ("Axes           " & Natural'Image (Joystick.Axes (Stick)));
         Put_Line ("Buttons        " & Natural'Image (Joystick.Buttons (Stick)));

         New_Line;
         Put_Line ("Now calibrate your joystick:  move an axis through its entire range ...");
         Put_Line ("When done, press Enter or click a joystick button.");

         Min_Axis := Integer'Last;
         Max_Axis := Integer'First;
         while Calibrating loop

            Get_Immediate (Hit_Char, Have_Char);
            Calibrating := not Have_Char;

            while Joystick.Pending (Stick) > 0 loop
               declare
                  use Joystick;

                  Joy       : constant Joystick_Event_Data := Next_Event (Stick);
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
            Put_Line ("I didn't see any axis move ... maybe quit and try that again.");
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
end Simple_Joy;
