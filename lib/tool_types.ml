type settings = Settings.settings
type building_settings = Settings.building_settings
type road_settings = Settings.road_settings
type intersection_settings = Settings.intersection_settings
type direction = North | South | East | West
type tool = INTERSECTION | BUILDING | ROAD

module type TOOL = sig
  type t = tool

  val draw : Cairo.context -> x:int -> y:int -> angle:float -> settings -> unit

  val draw_selection :
    Cairo.context -> x:int -> y:int -> angle:float -> settings -> unit

  val draw_rotate_button :
    Cairo.context -> x:int -> y:int -> angle:float -> settings -> unit

  val erase : Cairo.context -> x:int -> y:int -> settings -> unit
  val point_inside : x:int -> y:int -> px:float -> py:float -> settings -> bool

  val point_on_rotate_button :
    x:int -> y:int -> angle:float -> px:float -> py:float -> settings -> bool

  val calculate_rotation : cx:float -> cy:float -> mx:float -> my:float -> float
  val get_settings : unit -> settings
  val set_settings : settings -> unit
  val get_tool : unit -> tool
  val get_name : unit -> string
end
