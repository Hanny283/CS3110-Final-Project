(* Interface for Tool types and module signatures *)

type settings = Settings.settings
type building_settings = Settings.building_settings
type road_settings = Settings.road_settings
type intersection_settings = Settings.intersection_settings
type direction = North | South | East | West
type tool = INTERSECTION | BUILDING | ROAD

module type TOOL = sig
  type t = tool

  (* takes in cr which is the space where the drawing happens, x and y which are
  both ints and represent the center of the drawing, the angle which is an int
  and is how much to rotate the object, and a settings argument which is 
  just the settings of what you're drawing. All arguments are determined by 
  either the object you're drawing or where you click. Draws the object of
  your choice*)
  val draw :
    cr:Cairo.context -> x:int -> y:int -> angle:float -> settings -> unit

  (* takes in cr which is the space where the drawing happens, x and y which are
  both ints and represent the center of the drawing, the angle which is an int
  and is how much to rotate the object, and a settings argument which is 
  just the settings of what you're drawing. All arguments are determined by 
  either the object you're drawing or where you click. Highlights the object*)
  val draw_selection :
    cr:Cairo.context -> x:int -> y:int -> angle:float -> settings -> unit

  (* takes in cr which is of type Cairo.context  and is the space where the 
  drawing happens, x and y which are both ints and represent the center of the 
  drawing, the angle which is an int and is how much to rotate the object, 
  and a settings argument which is just the settings of what you're drawing. 
  All arguments are determined by either the object you're drawing or 
  where you click. Draws the rotate button which you select move and press
  an object. *)
  val draw_rotate_button :
    cr:Cairo.context -> x:int -> y:int -> angle:float -> settings -> unit

  (* takes in x and y which are both ints and represent the center of the 
  drawing, a settings argument which is just the settings of what you're 
  drawing, and px and py which are floats and represent the location
  of which the mouse makes a press at. This functino returns a bool which 
  is true if the point is inside the bounds and false otherwise. *)
  val point_inside : x:int -> y:int -> px:float -> py:float -> settings -> bool

  (* takes in x and y which are both ints and represent the center of the 
  drawing, a settings argument which is just the settings of what you're 
  drawing, px and py which are floats and represent the location
  of which the mouse makes a press at, and an angle which is a float and 
  represents the angle at which you're rotating the object. 
  This functino returns a bool which is true if the mouse clicks on the rotate 
  button false otherwise. *)
  val point_on_rotate_button :
    x:int -> y:int -> angle:float -> px:float -> py:float -> settings -> bool

  (* Takes in cx and cy which are floats and represent the center of the object,
  mx and my which are floats and represent the location of the mouse. 
  Calculates rotation angle from center (cx, cy) based on mouse position 
  (mx, my) *)
  val calculate_rotation : cx:float -> cy:float -> mx:float -> my:float -> float

  (* returns the current settings for this tool *)
  val get_settings : unit -> settings

  (* This takes in an argument of type settings that contains the new settings 
  you would like and updates the settings for this tool with the new ones *)
  val set_settings : settings -> unit

  (* returns the tool type *)
  val get_tool : unit -> tool

  (* returns tool name for display in toolbar *)
  val get_name : unit -> string
end
