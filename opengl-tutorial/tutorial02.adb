
-- Tutorial 02 from opengl-tutorial.org, done using Lumen
-- Purpose is to create a red triangle, using vertex array objects and shaders

with Ada.Text_IO;

with Lumen.Events;
with Lumen.GL;
with Lumen.Window;
use Lumen;

procedure Tutorial02 is

   ---------------------------------------------------------------------------

   Win              : Lumen.Window.Window_Handle;
   Terminated       : Boolean := False;
   Vertex_Array_ID  : GL.UInt;
   Vertex_Buffer_ID : GL.UInt;

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

   -- Ensure loading of newer (GL 1.5) functions
   if not Lumen.GL.Load_GL_1_5 then
      Ada.Text_IO.Put_Line ("Missing OpenGL 1.5 functions");
   end if;

   -- Create Lumen window, accepting most defaults
   Lumen.Window.Create (Win, Name => "Tutorial 02");
   Win.Key_Press := Key_Press'Unrestricted_Access;

   -- Dark blue background
   GL.Clear_Color (0.0, 0.0, 0.4, 0.0);

   GL.Gen_Vertex_Arrays (1, Vertex_Array_ID'Address);
   GL.Bind_Vertex_Array (Vertex_Array_ID);

   -- Generate 1 buffer, put the resulting identifier in vertexbuffer
   GL.Gen_Buffers (1, Vertex_Buffer_ID'Address);

   -- The following commands will talk about our 'vertexbuffer' buffer
   GL.Bind_Buffer (GL.GL_ARRAY_BUFFER, Vertex_Buffer_ID);

   -- Give our vertices to OpenGL.
   GL.Buffer_Data (GL.GL_ARRAY_BUFFER, Vertex_Buffer_Data_Size, Vertex_Buffer_Data'Address, GL.GL_STATIC_DRAW);

   -- Loop until user hits the Escape or clicks the window's Close button
   while Lumen.Window.Process_Events (Win) loop

      -- Exit when they destroy the window, or when they hit any key
      exit when Terminated;

      -- Clear the screen
      GL.Clear (GL.GL_COLOR_BUFFER_BIT);

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

      Lumen.Window.Swap (Win);  -- not necessary since we drew nothing
   end loop;

end Tutorial02;
