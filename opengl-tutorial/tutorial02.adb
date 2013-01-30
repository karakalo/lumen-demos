
-- Tutorial 02 from opengl-tutorial.org, done using Lumen
-- Purpose is to create a red triangle, using vertex array objects and shaders

with Ada.Text_IO;

with Lumen.Events;
with Lumen.GL;
with Lumen.Shader;
with Lumen.Window;
use Lumen;

procedure Tutorial02 is

   ---------------------------------------------------------------------------

   use Ada.Text_IO;
   use type GL.Bitfield;

   ---------------------------------------------------------------------------

   Vertex_Shader_Pathname   : constant String := "data/tutorial02.vertex.shader";
   Fragment_Shader_Pathname : constant String := "data/tutorial02.fragment.shader";

   ---------------------------------------------------------------------------

   Win                : Lumen.Window.Window_Handle;
   Terminated         : Boolean := False;
   Vertex_Array_ID    : GL.UInt;
   Vertex_Buffer_ID   : GL.UInt;
   Vertex_Shader_ID   : GL.UInt;
   Fragment_Shader_ID : GL.UInt;
   Compiled           : Boolean;
   Program_ID         : GL.UInt;

   ---------------------------------------------------------------------------

   type Vertex_Array_Type is array (Integer range 1 .. 9) of Float;
   Vertex_Buffer_Data : Vertex_Array_Type :=
      (
       -1.0, -1.0, 0.0,
        1.0, -1.0, 0.0,
        0.0,  1.0, 0.0
      );
   Vertex_Buffer_Data_Size : constant GL.SizeI := Vertex_Buffer_Data'Length * (Float'Size / 8);

   ---------------------------------------------------------------------------

   procedure Key_Press
     (Category  : Events.Key_Category;
      Symbol    : Events.Key_Symbol;
      Modifiers : Events.Modifier_Set) is
   begin
      Terminated := Lumen.Events.To_Character (Symbol) = ASCII.ESC;
   end Key_Press;

   ---------------------------------------------------------------------------

begin  -- Tutorial02

   -- Create Lumen window, accepting most defaults
   Lumen.Window.Create (Win, Name => "Tutorial 02");
   Win.Key_Press := Key_Press'Unrestricted_Access;

   -- Dark blue background
   GL.Clear_Color (0.0, 0.0, 0.4, 0.0);

   GL.Gen_Vertex_Arrays (1, Vertex_Array_ID'Address);
   GL.Bind_Vertex_Array (Vertex_Array_ID);

   -- Create and compile our GLSL program from the shaders
   Put_Line ("Compiling vertex shader " & Vertex_Shader_Pathname);
   Shader.From_File (GL.GL_VERTEX_SHADER, Vertex_Shader_Pathname, Vertex_Shader_ID, Compiled);
   if Compiled then
      Put_Line ("Vertex shader compiled successfully");
   else
      Put_Line ("Vertex shader failed to compile.  Error:");
      Put_Line (Shader.Get_Info_Log (Vertex_Shader_ID));
   end if;
   Put_Line ("Compiling fragment shader " & Fragment_Shader_Pathname);
   Shader.From_File (GL.GL_FRAGMENT_SHADER, "data/tutorial02.fragment.shader", Fragment_Shader_ID, Compiled);
   if Compiled then
      Put_Line ("Fragment shader compiled successfully");
   else
      Put_Line ("Fragment shader failed to compile.  Error:");
      Put_Line (Shader.Get_Info_Log (Fragment_Shader_ID));
   end if;

   -- Link the program
   Put_Line ("Linking program");
   Program_ID := GL.Create_Program;
   GL.Attach_Shader (Program_ID, Vertex_Shader_ID);
   GL.Attach_Shader (Program_ID, Fragment_Shader_ID);
   GL.Link_Program (Program_ID);

   -- Done with these now
   GL.Delete_Shader (Vertex_Shader_ID);
   GL.Delete_Shader (Fragment_Shader_ID);

   -- Generate 1 buffer, put the resulting identifier in Vertex_Buffer_ID
   GL.Gen_Buffers (1, Vertex_Buffer_ID'Address);

   -- The following commands will talk about our 'Vertex_Buffer_ID' buffer
   GL.Bind_Buffer (GL.GL_ARRAY_BUFFER, Vertex_Buffer_ID);

   -- Give our vertices to OpenGL.
   GL.Buffer_Data (GL.GL_ARRAY_BUFFER, Vertex_Buffer_Data_Size, Vertex_Buffer_Data'Address, GL.GL_STATIC_DRAW);

   -- Loop until user hits the Escape (done by the Key_Press routine above) or
   -- clicks the window's Close button (done by Process_Events)
   while Lumen.Window.Process_Events (Win) loop

      -- Exit when they hit the Esc key
      exit when Terminated;

      -- Clear the screen
      GL.Clear (GL.GL_COLOR_BUFFER_BIT or GL.GL_DEPTH_BUFFER_BIT);

      -- Activate our shaders
      GL.Use_Program (Program_ID);

      -- The attribute here (0, output of the fragment shader) is the vertex position (X,Y,Z)
      GL.Enable_Vertex_Attrib_Array (0);
      GL.Bind_Buffer (GL.GL_ARRAY_BUFFER, Vertex_Buffer_ID);
      GL.Vertex_Attrib_Pointer
         (
          0,                  -- attribute 0. No particular reason for 0, but must match the layout in the shader
          3,                  -- size
          GL.GL_FLOAT,        -- type
          GL.GL_FALSE,        -- normalized?
          0,                  -- stride
          GL.Null_Pointer     -- array buffer offset
         );

      -- Draw the triangle !
      GL.Draw_Arrays (GL.GL_TRIANGLES, 0, 3); -- 3 indices starting at 0 -> 1 triangle

      GL.Disable_Vertex_Attrib_Array (0);

      -- Swap in the backbuffer so we can see what we just drew
      Lumen.Window.Swap (Win);
   end loop;

end Tutorial02;
