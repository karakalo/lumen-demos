
-- Simple Lumen demo/test program to allow easy testing of visual attributes

with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Text_IO;

with Lumen.Window;
with Lumen.Events;
with Lumen.Font.Txf;
with Lumen.GL;
with Lumen.GLU;

use Lumen;

procedure Attribs is

   ---------------------------------------------------------------------------

   -- A font to fall back on
   Default_Font_Path : constant String := "fsb.txf";

   -- Keystrokes we care about
   Escape   : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.ESC));
   Space    : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.Space));
   Letter_q : constant Events.Key_Symbol := Events.Key_Symbol (Character'Pos (Ada.Characters.Latin_1.LC_Q));

   ---------------------------------------------------------------------------

   Win     : Lumen.Window.Window_Handle;
   Tx_Font : Font.Txf.Handle;
   Wide    : Natural := 400;
   High    : Natural := 400;
   Object  : GL.UInt;
   Attrs   : Window.Context_Attributes := Window.Default_Context_Attributes;
   Direct  : Boolean := True;  -- want direct rendering by default

   ---------------------------------------------------------------------------

   Terminated  : Boolean := False;
   Font_Loaded : Boolean := False;

   Program_Error : exception;

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

   -- Draw our scene
   procedure Draw is

      ------------------------------------------------------------------------

      MW    : Natural;
      MA    : Integer;
      MD    : Integer;
      Scale : Float;

      ------------------------------------------------------------------------

      procedure Draw_Msg (Msg : in String;
                          Y   : in Float) is
      begin  -- Draw_Msg
         GL.Push_Matrix;
         Font.Txf.Get_String_Metrics (Tx_Font, Msg, MW, MA, MD);
         GL.Translate (-(Scale * Float (MW)) / 2.0, Y, 0.0);  -- center each string
         GL.Scale (Scale, Scale, Scale);
         Font.Txf.Render (Tx_Font, Msg);
         GL.Pop_Matrix;
      end Draw_Msg;

      ------------------------------------------------------------------------

   begin  -- Draw

      -- Draw the white square
      GL.Clear_Color (Float (0.35), 0.3, 0.0, 1.0);
      GL.Clear (GL.GL_COLOR_BUFFER_BIT);

      GL.Disable (GL.GL_TEXTURE_2D);
      GL.Disable (GL.GL_BLEND);
      GL.Disable (GL.GL_ALPHA_TEST);

      -- Draw the text messages
      GL.Push_Matrix;
      GL.Enable (GL.GL_TEXTURE_2D);
      GL.Enable (GL.GL_ALPHA_TEST);
      GL.Alpha_Func (GL.GL_GEQUAL, 0.0625);
      GL.Enable (GL.GL_BLEND);
      GL.Blend_Func (GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);
      GL.Enable (GL.GL_POLYGON_OFFSET_FILL);
      GL.Polygon_Offset (0.0, -3.0);
      GL.Color (Float (1.0), 1.0, 1.0);
      Font.Txf.Get_String_Metrics (Tx_Font, "OpenGL", MW, MA, MD);
      Scale := 1.5 / Float (MW);  -- draw all messages at the same size
      Draw_Msg ("RGB size    " & Integer'Image (Attrs.Red_Size),     1.5);  -- values just hand-jobbed, but could be calculated
      Draw_Msg ("Alpha size  " & Integer'Image (Attrs.Alpha_Size),   1.0);
      Draw_Msg ("Stencil size" & Integer'Image (Attrs.Stencil_Size), 0.5);
      Draw_Msg ("Depth size  " & Integer'Image (Attrs.Depth_Size),   0.0);
      if Direct then
         Draw_Msg ("Direct rendering",  -0.5);
      else
         Draw_Msg ("Indirect rendering",  -0.5);
      end if;
      Draw_Msg ("These values work!",  -1.5);
      GL.Pop_Matrix;

      -- Now show it
      GL.Flush;
      Window.Swap (Win);

   end Draw;

   ---------------------------------------------------------------------------

   procedure Key_Press
     (Category  : Events.Key_Category;
      Symbol    : Events.Key_Symbol;
      Modifiers : Events.Modifier_Set) is
   begin
      if Symbol = Escape or Symbol = Letter_q then
         Terminated := True;
      end if;
   end Key_Press;

   ---------------------------------------------------------------------------

   procedure Window_Resize
     (Height : Integer;
      Width  : Integer) is
   begin
      Wide := Width;
      High := Height;
      Set_View (Width,Height);
      Draw;
   end Window_Resize;

   ---------------------------------------------------------------------------

begin  -- Attribs

   -- If command-line arguments were given then process them
   for Index in 1 .. Ada.Command_Line.Argument_Count loop

      declare
         Arg : String := Ada.Command_Line.Argument (Index);
      begin
         case Arg (Arg'First) is

            when 'a' =>
               Attrs.Alpha_Size := Integer'Value (Arg (Arg'First + 1 .. Arg'Last));

            when 'c' =>
               Attrs.Red_Size   := Integer'Value (Arg (Arg'First + 1 .. Arg'Last));
               Attrs.Blue_Size  := Integer'Value (Arg (Arg'First + 1 .. Arg'Last));
               Attrs.Green_Size := Integer'Value (Arg (Arg'First + 1 .. Arg'Last));

            when 'd' =>
               Attrs.Depth_Size := Integer'Value (Arg (Arg'First + 1 .. Arg'Last));

            when 'f' =>
               declare
                  Font_Path : constant String := Arg (Arg'First + 1 .. Arg'Last);
               begin
                  Font.Txf.Load (Tx_Font, Font_Path);
                  Font_Loaded := True;

               exception
                  when others =>
                     raise Program_Error with "cannot find font file """ & Font_Path & """";
               end;

            when 'n' =>
               Direct := False;

            when 's' =>
               Attrs.Stencil_Size := Integer'Value (Arg (Arg'First + 1 .. Arg'Last));

            when others =>
               null;

         end case;
      end;

   end loop;

   -- If no font was given on the command line, use the default
   if not Font_Loaded then
      begin
         -- Check in CWD
         Font.Txf.Load (Tx_Font, Default_Font_Path);
      exception
         when others =>
            begin
               -- Check in standard data subdir
               Font.Txf.Load (Tx_Font, "data/" & Default_Font_Path);
            exception
               when others =>
                  begin
                     -- Check in standard data subdir, if idiot, er, user has cd'd into bin
                     Font.Txf.Load (Tx_Font, "../data/" & Default_Font_Path);
                  exception
                     when others =>
                        raise Program_Error with "cannot find default font file """ & Default_Font_Path & """";
                  end;
            end;
      end;
   end if;

   -- Report what we're about to do
   declare
      use Ada.Text_IO;
   begin
      New_Line;
      Put_Line ("About to create a window using the following attribuse:");
      Put_Line ("   RGB size    " & Integer'Image (Attrs.Red_Size));
      Put_Line ("   Alpha size  " & Integer'Image (Attrs.Alpha_Size));
      Put_Line ("   Stencil size" & Integer'Image (Attrs.Stencil_Size));
      PuT_Line ("   Depth size  " & Integer'Image (Attrs.Depth_Size));
      if Direct then
         Put_Line ("   Direct rendering");
      else
         Put_Line ("   Indirect rendering");
      end if;
   end;

   -- Create Lumen window, accepting most defaults; turn double buffering off
   -- for simplicity
   Window.Create (Win,
                  Name       => "Visual Attribute Checker",
                  Width      => Wide,
                  Height     => High,
                  Direct     => Direct,
                  Attributes => Attrs);
   Win.Key_Press := Key_Press'Unrestricted_Access;
   Win.Resize    := Window_Resize'Unrestricted_Access;

   -- Set up the viewport and scene parameters
   Set_View (Wide, High);
   Object := Font.Txf.Establish_Texture (Tx_Font, 0, True);
   Draw;

   -- Enter the event loop
   while Lumen.Window.Process_Events (Win) loop
      exit when Terminated;
      Draw;
   end loop;

end Attribs;
