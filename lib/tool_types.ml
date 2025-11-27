(* Define Tool types and Modules *)
open Settings

type direction = North | South | East | West

type tool = INTERSECTION | BUILDING | ROAD

module type TOOL = sig
  type t = tool

  (* Draw the tool on a Cairo context at given coordinates with optional rotation *)
  val draw : Cairo.context -> x:int -> y:int -> angle:float -> settings -> unit

  (* Draw selection highlight (yellow border) around the tool *)
  val draw_selection :
    Cairo.context -> x:int -> y:int -> angle:float -> settings -> unit

  (* Draw rotate button (circular arrow icon) on the right side of the tool *)
  val draw_rotate_button :
    Cairo.context -> x:int -> y:int -> angle:float -> settings -> unit

  (* Erase the tool from a Cairo context at given coordinates *)
  val erase : Cairo.context -> x:int -> y:int -> settings -> unit

  (* Check if a point (px, py) is inside the tool's bounds *)
  val point_inside : x:int -> y:int -> px:float -> py:float -> settings -> bool

  (* Check if a point (px, py) is on the rotate button *)
  val point_on_rotate_button :
    x:int -> y:int -> angle:float -> px:float -> py:float -> settings -> bool

  (* Calculate rotation angle from center (cx, cy) based on mouse position (mx, my) *)
  val calculate_rotation : cx:float -> cy:float -> mx:float -> my:float -> float

  (* Get the current settings for this tool *)
  val get_settings : unit -> settings

  (* Update the settings for this tool *)
  val set_settings : settings -> unit

  (* Get the tool type *)
  val get_tool : unit -> tool

  (* Get the tool name for display in toolbar *)
  val get_name : unit -> string
end

module MakeTool (T : TOOL) = struct
  let draw = T.draw
  let draw_selection = T.draw_selection
  let draw_rotate_button = T.draw_rotate_button
  let erase = T.erase
  let point_inside = T.point_inside
  let point_on_rotate_button = T.point_on_rotate_button
  let calculate_rotation = T.calculate_rotation
  let get_settings = T.get_settings
  let set_settings = T.set_settings
  let get_tool = T.get_tool
  let get_name = T.get_name
end
