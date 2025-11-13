(* Define Tool types and Modules *)

type direction = 
type building_type = 
| HOUSE of { time_in = 5; time_out = 9}
| OFFICE of { time_in = 9; time_out = 5}
| SCHOOL of { time_in = 7; time_out = 4}
| HOSPITAL of { }

type tool = INTERSECION | BUILDING of building_type  | ROAD


type building_settings = { mutable rate_of_traffic : int; building : building_type }

type road_settings = { mutable speed_limit : int } 


type intersection_settings = {num_stop : int}

module type TOOL = sig 

    type t = tool 

    val draw 

    val erase

    val settings : settings

    val get_settings : settings -> settings

    val set_settings : settings -> settings

    val get_tool : tool -> tool

end 



module MakeTool (T : TOOL) = struct

    let draw () = T.draw ()

    let erase () = T.erase ()

    let settings () = T.settings ()

    let get_settings () = T.get_settings ()

    let set_settings () = T.set_settings ()

    let get_tool () = T.get_tool ()
end 