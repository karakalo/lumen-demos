---------------------------------------------------------------------------
--
--  Demonstrates a simple fire-like particle effect using Lumen
--
---------------------------------------------------------------------------

---------------------------------------------------------------------------
--  Copyright (c) 2011, David Bouchain <david@bouchain.de>
--
--  Permission to use, copy, modify, and/or distribute this software
--  for any purpose with or without fee is hereby granted, provided
--  that the above copyright notice and this permission notice appear
--  in all copies.
--
--  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
--  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
--  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
--  AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
--  CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
--  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
--  NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
--  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

with Ada.Numerics.Elementary_Functions;
with Ada.Real_Time;
with Ada.Numerics.Float_Random;

with Lumen.Window;
with Lumen.Events;
with Lumen.GL;

procedure Fire is

   ------------------------------------------------------------------------
   --
   --  These are the system parameters.
   --
   --  The maximum age of a particle:
   Particle_Lifetime    : constant	:= 2.0;
   --
   --  The number of particles in the particle system:
   Particle_Count       : constant	:= 1000;
   --
   --  The amount of scatter around the emitter when spawning:
   Particle_Spread      : constant      := 0.4;
   --
   --  The amount upward acceleration:
   Particle_Lift	: constant	:= 0.5;
   --
   --  The red component of the particle color:
   Particle_Red         : constant      := 1.0;
   --
   --  The green component of the particle color:
   Particle_Green       : constant      := 0.25;
   --
   --  The blue component of the particle color:
   Particle_Blue        : constant      := 0.1;

   ------------------------------------------------------------------------
   --
   --  A very simple 2D vector implementation. This is just to make
   --  our life a bit easier downstairs.
   --
   type Vector is record
      X, Y : Float := 0.0;
   end record;

   function "+" (Left, Right : Vector) return Vector is
   begin
      return Vector'(Left.X + Right.X, Left.Y + Right.Y);
   end "+";

   function "-" (Left, Right : Vector) return Vector is
   begin
      return Vector'(Left.X - Right.X, Left.Y - Right.Y);
   end "-";

   function "*" (S : Float; V : Vector) return Vector is
   begin
      return Vector'(S * V.X, S * V.Y);
   end "*";

   ------------------------------------------------------------------------
   --
   --  A particle has a position R, a velocity V, and an age in
   --  seconds.
   --
   type Particle is record
      R, V	: Vector;
      Age	: Duration;
   end record;
   --
   --  The particle system has a fixed number of particles.
   --
   Particles : array (1 .. Particle_Count) of Particle;

   ------------------------------------------------------------------------
   --
   --  The random number generator is needed for the system noise,
   --  which is added to the particles velocity on respawn.
   --
   RNG : Ada.Numerics.Float_Random.Generator;

   ------------------------------------------------------------------------
   --
   --  This sets the age of the particles so that the global particle
   --  lifetime is uniformly filled. The spawning interval is
   --  therefore Particle_Lifetime / Particle_Count.
   --
   --  This also jitters the particles' velocities.
   --
   procedure Initialize_Particles is
      use Ada.Numerics.Float_Random;
      Spawn_Period : Duration := Particle_Lifetime / Duration (Particle_Count);
   begin
      for I in 1 .. Particle_Count loop
         Particles (I).Age := Duration (I) * Spawn_Period;
         Particles (I).R.Y := -0.5;
         Particles (I).V := Vector'(Particle_Spread * Random (RNG)
   				      - Particle_Spread / 2.0,
                                    Particle_Spread * Random (RNG)
   				      - Particle_Spread / 2.0);
      end loop;
   end Initialize_Particles;

   ------------------------------------------------------------------------
   --
   --  If a particle has reached the maximum lifetime, it is reset,
   --  similarly to the Initialize_Particles procedure
   --  above. Otherwise the simple difference equation for all
   --  particles is computed.
   --
   --  The reset is broken when the delta time is larger than the
   --  particle lifetime. But if that is ever the case, that
   --  particular machine shouldn't even run OpenGL.
   --
   procedure Update_Particles (DT : Duration) is
      use Ada.Numerics.Float_Random;
   begin
      for I in 1 .. Particle_Count loop
         Particles (I).Age := Particles (I).Age + DT;
         if Particles (I).Age >= Particle_Lifetime then
            Particles (I).Age := Particles (I).Age - Particle_Lifetime;
            Particles (I).R := Vector'(0.0, -0.5);
            Particles (I).V := Vector'(Particle_Spread * Random (RNG)
					 - Particle_Spread / 2.0,
                                       Particle_Spread * Random (RNG)
					 - Particle_Spread / 2.0);
         else
	    Particles (I).V.Y := Particles (I).V.Y
	      + Float (DT) * Particle_Lift;
            Particles (I).R := Particles (I).R
	      + Float (DT) * Particles (I).V;
         end if;
      end loop;
   end Update_Particles;

   ------------------------------------------------------------------------
   --
   --  This draws all particles as simple points. The alpha value of a
   --  particle is computed based on its age.
   --
   procedure Render_Particles is
      use Lumen.GL;
      Alpha : Float;
   begin
      for I in 1 .. Particle_Count loop
         Begin_Primitive (GL_POINTS);
         Alpha := Float ((Particle_Lifetime
			    - Particles (I).Age) / Particle_Lifetime);
         Color (Particle_Red, Particle_Green, Particle_Blue,
	 	0.25 * Alpha);
         Vertex (Particles (I).R.X, Particles (I).R.Y);
         End_Primitive;
      end loop;
   end Render_Particles;

   ------------------------------------------------------------------------
   --
   --  This is all that is needed to run a Lumen GL window.
   --
   Win        : Lumen.Window.Window_Handle;
   Terminated : Boolean := False;

   ------------------------------------------------------------------------
   --
   --  This is for real-time rendering.
   --
   Start_Time	: Ada.Real_Time.Time    := Ada.Real_Time.Clock;
   End_Time	: Ada.Real_Time.Time;
   Delta_Time	: Duration;

   ------------------------------------------------------------------------
   --
   --  This is for computing the frame rate every second.
   --
   FPS_Base_Time	: Ada.Real_Time.Time	:= Start_Time;
   FPS_Counter		: Integer		:= 0;

   ------------------------------------------------------------------------
   --
   --  Callback for the key-pressed event
   --
   procedure Key_Press (Category  : in Lumen.Events.Key_Category;
                        Symbol    : in Lumen.Events.Key_Symbol;
                        Modifiers : in Lumen.Events.Modifier_Set) is
   begin
      Terminated := True;
   end Key_Press;

begin

   ------------------------------------------------------------------------
   --
   --  Creates the window using reasonable defaults and creates and
   --  activates the OpenGL context.
   --
   Lumen.Window.Create (Win, Name => "FPS: 0");
   Win.Key_Press := Key_Press'Unrestricted_Access;

   Initialize_Particles;

   declare
      use Lumen;
      Texture : aliased GL.Uint;
      Texture_Data : aliased
	array (1 .. 32, 1 .. 32, 1 .. 4) of GL.UByte
	:= (others => (others => (others => 16#FF#)));
   begin
      ---------------------------------------------------------------------
      --
      --  We need large point size for the point sprites.
      --
      GL.Point_Size (32.0);

      ---------------------------------------------------------------------
      --
      --  This creates a 32 by 32 texture data. The color is white (as
      --  initialized above) and the alpha value is the distance of
      --  the texel to the center normalized by the half the point
      --  size.
      --
      for X in 1 .. 32 loop
	 for Y in 1 .. 32 loop
	    declare
	       use Ada.Numerics.Elementary_Functions;
	       X_Dist : Float := abs (Float (X - 17) - 0.5) / 16.0;
	       Y_Dist : Float := abs (Float (Y - 17) - 0.5) / 16.0;
	       Alpha : Float := 1.0 - Sqrt (X_Dist**2.0 + Y_Dist**2.0);
	    begin
	       if Alpha < 0.0 then
		  Alpha := 0.0;
	       end if;
	       Texture_Data (X, Y, 4) := GL.UByte (Alpha * 255.0);
	    end;
	 end loop;
      end loop;

      ---------------------------------------------------------------------
      --
      --  This enables additive blending. It is essential to use
      --  additive blending for the effect to work.
      --
      GL.Enable (GL.GL_BLEND);
      GL.Blend_Func (GL.GL_SRC_ALPHA, GL.GL_ONE);

      ---------------------------------------------------------------------
      --
      --  This creates the point sprite texture.
      --
      GL.Enable (GL.GL_TEXTURE_2D);
      GL.Gen_Textures (1, Texture'Address);
      GL.Bind_Texture (GL.GL_TEXTURE_2D, Texture);

      GL.Tex_Parameter (GL.GL_TEXTURE_2D,
		       GL.GL_TEXTURE_MAG_FILTER,
		       GL.GL_NEAREST);

      GL.Tex_Parameter (GL.GL_TEXTURE_2D,
		       GL.GL_TEXTURE_MIN_FILTER,
		       GL.GL_NEAREST);

      GL.Tex_Image (GL.GL_TEXTURE_2D,
		   0,
		   GL.GL_RGBA,
		   32, 32,
		   0,
		   GL.GL_RGBA, GL.GL_UNSIGNED_BYTE,
		   Texture_Data'Address);

      ---------------------------------------------------------------------
      --
      --  This enables the actual
      GL.Enable (GL.GL_POINT_SPRITE);
      GL.Tex_Env (GL.GL_POINT_SPRITE, GL.GL_COORD_REPLACE, 1);
   end;

   ------------------------------------------------------------------------
   --
   --  This is the main application loop.
   --
   while Lumen.Window.Process_Events (Win) loop
      exit when Terminated;

      declare
	 use Ada.Real_Time;
      begin

         ------------------------------------------------------------------
         --
         --  This computes the delta time, i.e. the time the last frame
         --  took to complete.
         --
	 End_Time := Clock;
	 Delta_Time := To_Duration (End_Time - Start_Time);
	 Start_Time := End_Time;

         ------------------------------------------------------------------
         --
	 --  This counts how many frames are rendered in one
	 --  second. When one second is over the frames-per-second
	 --  value is displayed as the window name (usually in the
	 --  title bar) and the counter is reset.
         --
	 if To_Duration (Start_Time - FPS_Base_Time) >= 1.0 then
	    Lumen.Window.Set_Names (Win, Name => "FPS: " & Integer'Image (FPS_Counter));
	    FPS_Base_Time := Start_Time;
	    FPS_Counter := 0;
	 else
	    FPS_Counter := FPS_Counter + 1;
	 end if;
      end;

      ---------------------------------------------------------------------
      --
      --  This is our 'physics update'.
      --
      Update_Particles (Delta_Time);

      ---------------------------------------------------------------------
      --
      --  This renders all particles. Depth testing is disabled so we
      --  don't need to clear the depth buffer.
      --
      declare
         use Lumen;
      begin
         GL.Clear_Color (0.0, 0.0, 0.0, 1.0);
         GL.Clear (GL.GL_COLOR_BUFFER_BIT);
         Render_Particles;
         GL.Flush;
      end;

      ---------------------------------------------------------------------
      --
      --  This swaps the double buffer and is usually the last thing
      --  in an animation loop.
      --
      Lumen.Window.Swap (Win);
   end loop;
end Fire;
