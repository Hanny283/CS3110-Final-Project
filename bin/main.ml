open Cs3110_final_project.Tool_types
open Cs3110_final_project.Road
open Cs3110_final_project.Intersection
open Cs3110_final_project.Building

type drawn_object = {
  tool_type : tool;
  x : int;
  y : int;
  angle : float;
  ungrouped : bool;
}

(**This is just the length of the road*)
let road_length = 120.0

(**This is the width of the road*)
let road_width = 30.0

let building_width = 50.0

(** This is the maximum threshold for how far away you're allowed to click and
    still have a snap occured*)
let snap_threshold = 55.0

(**requires two points that represent the x and y distance of an object and
   returns the difference in distance between two objects. This is mainly used
   for snapping logic and to check if two objects are connected*)
let distance (x1, y1) (x2, y2) =
  let dx = x1 -. x2 in
  let dy = y1 -. y2 in
  sqrt ((dx *. dx) +. (dy *. dy))

(** Takes in an object(Which we use for roads only) and adds and substracts the
    distance of half the round to the roads x and y coordinates to return a
    tuple that contains the coordinates for both ends of the road. This is used
    for the snapping logic as well as the snapping check between roads and int-
    sections*)
let road_endpoints (obj : drawn_object) =
  let fx = float_of_int obj.x in
  let fy = float_of_int obj.y in
  let half_length = road_length /. 2.0 in
  let dx = half_length *. cos obj.angle in
  let dy = half_length *. sin obj.angle in
  ((fx -. dx, fy -. dy), (fx +. dx, fy +. dy))

(**Takes in an object(which we only use for rodas) and returns a list x1;y1. x1
   is a tuple (x,y) that represents the coordinates of the right side of the
   road and y2 is a tuple (x,y) that represents the coordinates of the left side
   of the road. This is used for snapping buildings onto roads as well as
   checking for connections when moving groups *)
let road_side_points (obj : drawn_object) =
  let fx = float_of_int obj.x in
  let fy = float_of_int obj.y in
  let normal_x = -.sin obj.angle in
  let normal_y = cos obj.angle in
  let offset = (road_width /. 2.0) +. (building_width /. 2.0) +. 5.0 in
  let sx1 = fx +. (offset *. normal_x) in
  let sy1 = fy +. (offset *. normal_y) in
  let sx2 = fx -. (offset *. normal_x) in
  let sy2 = fy -. (offset *. normal_y) in
  [ (sx1, sy1); (sx2, sy2) ]

(**a type that is used for determing how to treat the either newly snapped item
   or already existing. This is used to determine how to snap correctly, and
   move groruped items.*)
type snap_target_kind =
  | Target_road_end
  | Target_intersection_center
  | Target_road_side

type snap_target = { tx : float; ty : float; tkind : snap_target_kind }

(**Takes in a list of objects and returns a triple (x,y,z)

   where x is a list of snap_targets which tell us where snaps have happened and
   with what other object to the long left side and long right side of the road,

   where y is a list of snap_targets which tell us where snaps have happened and
   with what other object to intersections.

   where z is a list of snap_targets which tell us where snaps have happened and
   with what other object to the short left side and short right side of the
   road *)
let collect_snap_targets (objs : drawn_object list) :
    snap_target list * snap_target list * snap_target list =
  let road_targets = ref [] in
  let intersection_targets = ref [] in
  let road_side_targets = ref [] in
  List.iter
    (fun obj ->
      match obj.tool_type with
      | ROAD ->
          let (sx, sy), (ex, ey) = road_endpoints obj in
          road_targets :=
            { tx = sx; ty = sy; tkind = Target_road_end }
            :: { tx = ex; ty = ey; tkind = Target_road_end }
            :: !road_targets;
          let side_points = road_side_points obj in
          List.iter
            (fun (px, py) ->
              road_side_targets :=
                { tx = px; ty = py; tkind = Target_road_side }
                :: !road_side_targets)
            side_points
      | INTERSECTION ->
          let fx = float_of_int obj.x in
          let fy = float_of_int obj.y in
          intersection_targets :=
            { tx = fx; ty = fy; tkind = Target_intersection_center }
            :: !intersection_targets
      | BUILDING -> ())
    objs;
  (!road_targets, !intersection_targets, !road_side_targets)

(** takes in a list of already drawn objects thave have already been draw if
    any, a tool_type representing the object that is currently being drawn, an x
    and y coordinate of where the drawing is happening (provided by tool_type)
    and returns a triple(x,y,angle) representing where the new snap, if any, is
    and at what angle*)
let snap_new_object ~(others : drawn_object list) ~(tool_type : tool) ~(x : int)
    ~(y : int) ~(angle : float) : int * int * float =
  let fx = float_of_int x in
  let fy = float_of_int y in
  let road_targets, intersection_targets, road_side_targets =
    collect_snap_targets others
  in
  match tool_type with
  | ROAD -> (
      let obj = { tool_type = ROAD; x; y; angle; ungrouped = false } in
      let (sx, sy), (ex, ey) = road_endpoints obj in
      let best = ref (None : (float * (float * float) * snap_target) option) in
      let all_targets = road_targets @ intersection_targets in
      List.iter
        (fun t ->
          let d1 = distance (sx, sy) (t.tx, t.ty) in
          let d2 = distance (ex, ey) (t.tx, t.ty) in
          let cand_d, cand_pt =
            if d1 < d2 then (d1, (sx, sy)) else (d2, (ex, ey))
          in
          match !best with
          | None -> best := Some (cand_d, cand_pt, t)
          | Some (best_d, _, _) ->
              if cand_d < best_d then best := Some (cand_d, cand_pt, t))
        all_targets;
      match !best with
      | Some (best_d, (px, py), tgt) when best_d <= snap_threshold ->
          let dx = tgt.tx -. px in
          let dy = tgt.ty -. py in
          (int_of_float (fx +. dx), int_of_float (fy +. dy), angle)
      | _ -> (x, y, angle))
  | INTERSECTION -> (
      let best = ref (None : (float * snap_target) option) in
      let all_targets = road_targets @ intersection_targets in
      List.iter
        (fun t ->
          let d = distance (fx, fy) (t.tx, t.ty) in
          match !best with
          | None -> best := Some (d, t)
          | Some (best_d, _) -> if d < best_d then best := Some (d, t))
        all_targets;
      match !best with
      | Some (best_d, tgt) when best_d <= snap_threshold ->
          (int_of_float tgt.tx, int_of_float tgt.ty, angle)
      | _ -> (x, y, angle))
  | BUILDING -> (
      let best = ref (None : (float * snap_target) option) in
      List.iter
        (fun t ->
          match t.tkind with
          | Target_road_side -> (
              let d = distance (fx, fy) (t.tx, t.ty) in
              match !best with
              | None -> best := Some (d, t)
              | Some (best_d, _) -> if d < best_d then best := Some (d, t))
          | _ -> ())
        road_side_targets;
      match !best with
      | Some (best_d, tgt) when best_d <= snap_threshold ->
          (int_of_float tgt.tx, int_of_float tgt.ty, angle)
      | _ -> (x, y, angle))

(** Takes in a list of objects as well as two ints i and j which represent the
    index of two objets within the list and we check if they are grouped. You
    are considered grouped if you are snapped to another object. Returns a bool
    true if grouped false otherwise *)
let is_connected (objs : drawn_object list) (i : int) (j : int) : bool =
  if i = j then false
  else
    let oi = List.nth objs i in
    let oj = List.nth objs j in
    if oi.ungrouped || oj.ungrouped then false
    else
      match (oi.tool_type, oj.tool_type) with
      | ROAD, ROAD ->
          let s1, e1 = road_endpoints oi in
          let s2, e2 = road_endpoints oj in
          let dists =
            [ distance s1 s2; distance s1 e2; distance e1 s2; distance e1 e2 ]
          in
          List.exists (fun d -> d <= snap_threshold) dists
      | ROAD, INTERSECTION ->
          let s1, e1 = road_endpoints oi in
          let cx = float_of_int oj.x in
          let cy = float_of_int oj.y in
          min (distance s1 (cx, cy)) (distance e1 (cx, cy)) <= snap_threshold
      | INTERSECTION, ROAD ->
          let cx = float_of_int oi.x in
          let cy = float_of_int oi.y in
          let s2, e2 = road_endpoints oj in
          min (distance (cx, cy) s2) (distance (cx, cy) e2) <= snap_threshold
      | ROAD, BUILDING ->
          let side_points = road_side_points oi in
          let cx = float_of_int oj.x in
          let cy = float_of_int oj.y in
          List.exists
            (fun (px, py) -> distance (px, py) (cx, cy) <= snap_threshold)
            side_points
      | BUILDING, ROAD ->
          let side_points = road_side_points oj in
          let cx = float_of_int oi.x in
          let cy = float_of_int oi.y in
          List.exists
            (fun (px, py) -> distance (px, py) (cx, cy) <= snap_threshold)
            side_points
      | INTERSECTION, INTERSECTION ->
          let cx1 = float_of_int oi.x in
          let cy1 = float_of_int oi.y in
          let cx2 = float_of_int oj.x in
          let cy2 = float_of_int oj.y in
          distance (cx1, cy1) (cx2, cy2) <= 1.0
      | BUILDING, BUILDING -> false
      | INTERSECTION, BUILDING -> false
      | BUILDING, INTERSECTION -> false

let () =
  (* Initialize GTK *)
  let _ = GMain.init () in

  (* Create the main window *)
  let window =
    GWindow.window ~title:"CS3110 Final Project - Drawing Application"
      ~resizable:true ()
  in
  window#set_default_size ~width:800 ~height:600;

  (*Create a vertical box to organize widgets *)
  let vbox = GPack.vbox ~packing:window#add () in

  (* Toolbar / tools row *)
  let toolbar = GPack.hbox ~packing:(vbox#pack ~expand:false) () in
  toolbar#misc#set_size_request ~height:45 ();
  let _label = GMisc.label ~text:"Drawing Tools" ~packing:toolbar#add () in

  (*represents the current selection that the user has*)
  let current_tool = ref None in

  (*List of objects that are currently drawn*)
  let objects = ref [] in

  (* Currently selected object index can be None*)
  let selected_object = ref None in

  (* list of indicies. where the indicies is the index of objects that is grouped
  so the currently selected object 
  (selected object index is also within this list since its in the group) *)
  let selected_group = ref [] in

  (*List of indicies where the indicies represent the group number that 
  the object is in. -1 if the object is ungrouped. A connection with obj does 
  exist meaning object[i] group number is group_ids[i].*)
  let group_ids = ref [||] in

  (*Hashtable where the key is a number in group_id and the value is the 
  objects that belong to that group*)
  let groups_table : (int, int list) Hashtbl.t = Hashtbl.create 32 in

  (*List of all the objects that are not within a group*)
  let ungrouped_list = ref [] in

  (*reconstructs ungrouped_list, groups_table, and group_ids whenever a 
  new object is drawn or an object is ungrouped from its current group. makes
  use of DFS to construct and make the groups*)
  let recompute_groups () =
    let objs = !objects in
    let n = List.length objs in
    let visited = Array.make n false in
    let groupID_table = Array.make n (-1) in
    Hashtbl.reset groups_table;
    let ungrouped_acc = ref [] in
    let rec dfs i groupID =
      visited.(i) <- true;
      groupID_table.(i) <- groupID;
      let members =
        match Hashtbl.find_opt groups_table groupID with
        | Some lst -> i :: lst
        | None -> [ i ]
      in
      Hashtbl.replace groups_table groupID members;
      for j = 0 to n - 1 do
        if (not visited.(j)) && is_connected objs i j then dfs j groupID
      done
    in
    for i = 0 to n - 1 do
      let obj = List.nth objs i in
      if obj.ungrouped then ungrouped_acc := i :: !ungrouped_acc
      else if not visited.(i) then dfs i i
    done;
    group_ids := groupID_table;
    ungrouped_list := List.rev !ungrouped_acc
  in

  (* Get the group (list of indices) for a given object index.
     If the object is ungrouped or grouping info is missing, returns [idx]. *)
  let get_group_for_index idx =
    let objs = !objects in
    let len = Array.length !group_ids in
    if idx < 0 || idx >= len then [ idx ]
    else
      let obj = List.nth objs idx in
      if obj.ungrouped then [ idx ]
      else
        let gid = !group_ids.(idx) in
        match Hashtbl.find_opt groups_table gid with
        | Some members -> members
        | None -> [ idx ]
  in

  let is_moving = ref false in

  let is_deleting = ref false in

  let is_rotating = ref false in

  let is_ungrouping = ref false in

  let drag_offset = ref None in

  let button_pressed = ref false in

  let add_button label callback =
    let b = GButton.button ~label ~packing:toolbar#add () in
    ignore (b#connect#clicked ~callback);
    b
  in

  (*Road button*)
  let _ =
    add_button "Road" (fun () ->
        current_tool := Some ROAD;
        is_deleting := false;
        is_moving := false;
        is_ungrouping := false)
  in

  (*Intersection button*)
  let _ =
    add_button "Intersection" (fun () ->
        current_tool := Some INTERSECTION;
        is_deleting := false;
        is_moving := false;
        is_ungrouping := false)
  in

  (*Building button*)
  let _ =
    add_button "Building" (fun () ->
        current_tool := Some BUILDING;
        is_deleting := false;
        is_moving := false;
        is_ungrouping := false)
  in

  (* Move button *)
  let _ =
    add_button "Move" (fun () ->
        current_tool := None;
        is_moving := true;
        is_deleting := false;
        is_ungrouping := false)
  in

  (* Ungroup button *)
  let _ =
    add_button "Ungroup" (fun () ->
        current_tool := None;
        is_moving := false;
        is_deleting := false;
        is_ungrouping := true)
  in

  (* Create the drawing area *)
  let drawing_area =
    GMisc.drawing_area ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  drawing_area#misc#set_size_request ~width:800 ~height:550 ();

  (* Set up the drawing context *)
  let surface = ref None in

  (* Initialize the surface for drawing *)
  let init_surface width height =
    let s = Cairo.Image.(create ARGB32 ~w:width ~h:height) in
    let cr = Cairo.create s in
    Cairo.set_source_rgb cr 1.0 1.0 1.0;
    Cairo.paint cr;
    surface := Some s;
    s
  in

  (* Helper function to redraw all objects *)
  let redraw_all () =
    match !surface with
    | Some s ->
        let cr = Cairo.create s in
        Cairo.set_source_rgb cr 1.0 1.0 1.0;
        Cairo.paint cr;

        (* Draw all objects *)
        List.iter
          (fun obj ->
            match obj.tool_type with
            | ROAD ->
                Road.draw cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                  (Road.get_settings ())
            | INTERSECTION ->
                Intersection.draw cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                  (Intersection.get_settings ())
            | BUILDING ->
                Building.draw cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                  (Building.get_settings ()))
          !objects;

        (match !selected_group with
        | [] -> ()
        | group_indices ->
            List.iter
              (fun idx ->
                try
                  let obj = List.nth !objects idx in
                  match obj.tool_type with
                  | ROAD ->
                      Road.draw_selection cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                        (Road.get_settings ())
                  | INTERSECTION ->
                      Intersection.draw_selection cr ~x:obj.x ~y:obj.y
                        ~angle:obj.angle
                        (Intersection.get_settings ())
                  | BUILDING ->
                      Building.draw_selection cr ~x:obj.x ~y:obj.y
                        ~angle:obj.angle (Building.get_settings ())
                with _ -> ())
              group_indices);

        (match !selected_object with
        | Some idx -> (
            try
              let obj = List.nth !objects idx in
              match obj.tool_type with
              | ROAD ->
                  Road.draw_rotate_button cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                    (Road.get_settings ())
              | INTERSECTION ->
                  Intersection.draw_rotate_button cr ~x:obj.x ~y:obj.y
                    ~angle:obj.angle
                    (Intersection.get_settings ())
              | BUILDING ->
                  Building.draw_rotate_button cr ~x:obj.x ~y:obj.y
                    ~angle:obj.angle (Building.get_settings ())
            with _ -> ())
        | None -> ());
        drawing_area#misc#queue_draw ()
    | None -> ()
  in

  (* Delete button *)
  let _ =
    add_button "Delete" (fun () ->
        current_tool := None;
        is_deleting := true;
        is_moving := false;
        is_ungrouping := false;
        selected_object := None;
        selected_group := [])
  in
  (* Draw handler - called when the widget needs to be redrawn *)
  let draw_callback cr =
    match !surface with
    | Some s ->
        Cairo.set_source_surface cr s ~x:0.0 ~y:0.0;
        Cairo.paint cr;
        true
    | None -> false
  in

  (* Configure event - called when the window is resized *)
  let configure_callback _ =
    let alloc = drawing_area#misc#allocation in
    let _ = init_surface alloc.Gtk.width alloc.Gtk.height in
    true
  in

  (* Mouse button press handler - handle drawing, selection, rotation, delete, ungroup *)
  let button_press_callback ev =
    let button = GdkEvent.Button.button ev in
    if button = 1 then (
      button_pressed := true;
      let x_f = GdkEvent.Button.x ev in
      let y_f = GdkEvent.Button.y ev in
      let x = int_of_float x_f in
      let y = int_of_float y_f in

      match !current_tool with
      | Some tool_type ->
          let angle = 0.0 in
          let snapped_x, snapped_y, snapped_angle =
            snap_new_object ~others:!objects ~tool_type ~x ~y ~angle
          in
          (match tool_type with
          | ROAD ->
              objects :=
                {
                  tool_type = ROAD;
                  x = snapped_x;
                  y = snapped_y;
                  angle = snapped_angle;
                  ungrouped = false;
                }
                :: !objects
          | INTERSECTION ->
              objects :=
                {
                  tool_type = INTERSECTION;
                  x = snapped_x;
                  y = snapped_y;
                  angle = snapped_angle;
                  ungrouped = false;
                }
                :: !objects
          | BUILDING ->
              objects :=
                {
                  tool_type = BUILDING;
                  x = snapped_x;
                  y = snapped_y;
                  angle = snapped_angle;
                  ungrouped = false;
                }
                :: !objects);
          recompute_groups ();
          selected_object := None;
          selected_group := [];
          redraw_all ();
          true
      | None ->
          if !is_deleting then (
            let clicked_object = ref None in
            List.iteri
              (fun idx obj ->
                if !clicked_object = None then
                  match obj.tool_type with
                  | ROAD ->
                      if
                        Road.point_inside ~x:obj.x ~y:obj.y ~px:x_f ~py:y_f
                          (Road.get_settings ())
                      then clicked_object := Some idx
                  | INTERSECTION ->
                      if
                        Intersection.point_inside ~x:obj.x ~y:obj.y ~px:x_f
                          ~py:y_f
                          (Intersection.get_settings ())
                      then clicked_object := Some idx
                  | BUILDING ->
                      if
                        Building.point_inside ~x:obj.x ~y:obj.y ~px:x_f ~py:y_f
                          (Building.get_settings ())
                      then clicked_object := Some idx)
              !objects;
            (match !clicked_object with
            | Some idx ->
                let objs = !objects in
                let group = get_group_for_index idx in
                let obj = List.nth objs idx in
                let to_delete =
                  if List.length group > 1 && not obj.ungrouped then group
                  else [ idx ]
                in
                objects :=
                  List.filteri (fun i _ -> not (List.mem i to_delete)) !objects;
                recompute_groups ();
                selected_object := None;
                selected_group := [];
                redraw_all ()
            | None -> ());
            true)
          else if !is_ungrouping then (
            let clicked_object = ref None in
            List.iteri
              (fun idx obj ->
                if !clicked_object = None then
                  match obj.tool_type with
                  | ROAD ->
                      if
                        Road.point_inside ~x:obj.x ~y:obj.y ~px:x_f ~py:y_f
                          (Road.get_settings ())
                      then clicked_object := Some idx
                  | INTERSECTION ->
                      if
                        Intersection.point_inside ~x:obj.x ~y:obj.y ~px:x_f
                          ~py:y_f
                          (Intersection.get_settings ())
                      then clicked_object := Some idx
                  | BUILDING ->
                      if
                        Building.point_inside ~x:obj.x ~y:obj.y ~px:x_f ~py:y_f
                          (Building.get_settings ())
                      then clicked_object := Some idx)
              !objects;
            (match !clicked_object with
            | Some idx ->
                objects :=
                  List.mapi
                    (fun i o ->
                      if i = idx then { o with ungrouped = true } else o)
                    !objects;
                recompute_groups ();
                selected_object := Some idx;
                selected_group := [ idx ];
                redraw_all ()
            | None -> ());
            true)
          else if !is_moving then (
            let clicked_object = ref None in
            let clicked_rotate_button = ref false in

            List.iteri
              (fun idx obj ->
                if !clicked_object = None then (
                  (if !selected_object = Some idx then
                     match obj.tool_type with
                     | ROAD ->
                         if
                           Road.point_on_rotate_button ~x:obj.x ~y:obj.y
                             ~angle:obj.angle ~px:x_f ~py:y_f
                             (Road.get_settings ())
                         then (
                           clicked_object := Some idx;
                           clicked_rotate_button := true;
                           is_rotating := true)
                     | INTERSECTION ->
                         if
                           Intersection.point_on_rotate_button ~x:obj.x ~y:obj.y
                             ~angle:obj.angle ~px:x_f ~py:y_f
                             (Intersection.get_settings ())
                         then (
                           clicked_object := Some idx;
                           clicked_rotate_button := true;
                           is_rotating := true)
                     | BUILDING ->
                         if
                           Building.point_on_rotate_button ~x:obj.x ~y:obj.y
                             ~angle:obj.angle ~px:x_f ~py:y_f
                             (Building.get_settings ())
                         then (
                           clicked_object := Some idx;
                           clicked_rotate_button := true;
                           is_rotating := true));

                  if !clicked_object = None then
                    match obj.tool_type with
                    | ROAD ->
                        if
                          Road.point_inside ~x:obj.x ~y:obj.y ~px:x_f ~py:y_f
                            (Road.get_settings ())
                        then (
                          clicked_object := Some idx;
                          clicked_rotate_button := false;
                          is_rotating := false;
                          let fx = float_of_int obj.x in
                          let fy = float_of_int obj.y in
                          drag_offset := Some (x_f -. fx, y_f -. fy))
                    | INTERSECTION ->
                        if
                          Intersection.point_inside ~x:obj.x ~y:obj.y ~px:x_f
                            ~py:y_f
                            (Intersection.get_settings ())
                        then (
                          clicked_object := Some idx;
                          clicked_rotate_button := false;
                          is_rotating := false;
                          let fx = float_of_int obj.x in
                          let fy = float_of_int obj.y in
                          drag_offset := Some (x_f -. fx, y_f -. fy))
                    | BUILDING ->
                        if
                          Building.point_inside ~x:obj.x ~y:obj.y ~px:x_f
                            ~py:y_f (Building.get_settings ())
                        then (
                          clicked_object := Some idx;
                          clicked_rotate_button := false;
                          is_rotating := false;
                          let fx = float_of_int obj.x in
                          let fy = float_of_int obj.y in
                          drag_offset := Some (x_f -. fx, y_f -. fy))))
              !objects;

            (match !clicked_object with
            | Some idx ->
                selected_object := Some idx;
                selected_group := get_group_for_index idx
            | None ->
                selected_object := None;
                selected_group := [];
                is_rotating := false;
                drag_offset := None);

            redraw_all ();
            true)
          else false)
    else false
  in

  (* Mouse motion handler - handle dragging and rotation *)
  let motion_callback ev =
    if !button_pressed && !selected_object <> None then
      let x = GdkEvent.Motion.x ev in
      let y = GdkEvent.Motion.y ev in

      let selected_idx =
        match !selected_object with Some idx -> idx | None -> assert false
      in

      try
        let objs = !objects in
        let obj = List.nth objs selected_idx in
        let group = !selected_group in

        (if !is_rotating then
           let cx = float_of_int obj.x in
           let cy = float_of_int obj.y in
           let new_angle =
             match obj.tool_type with
             | ROAD -> Road.calculate_rotation ~cx ~cy ~mx:x ~my:y
             | INTERSECTION ->
                 Intersection.calculate_rotation ~cx ~cy ~mx:x ~my:y
             | BUILDING -> Building.calculate_rotation ~cx ~cy ~mx:x ~my:y
           in

           objects :=
             List.mapi
               (fun i o ->
                 if i = selected_idx then { o with angle = new_angle } else o)
               objs
         else
           match !drag_offset with
           | Some (offset_x, offset_y) ->
               let new_x = int_of_float (x -. offset_x) in
               let new_y = int_of_float (y -. offset_y) in
               let dx = new_x - obj.x in
               let dy = new_y - obj.y in
               objects :=
                 List.mapi
                   (fun i o ->
                     if
                       (group <> [] && List.mem i group)
                       || (group = [] && i = selected_idx)
                     then { o with x = o.x + dx; y = o.y + dy }
                     else o)
                   objs
           | None -> ());

        redraw_all ();
        true
      with _ -> false
    else false
  in

  (* Mouse button release handler *)
  let button_release_callback _ev =
    button_pressed := false;
    is_rotating := false;
    (match !selected_object with
    | Some idx when !is_moving -> (
        let objs = !objects in
        try
          let obj = List.nth objs idx in
          let others =
            List.mapi (fun i o -> (i, o)) objs
            |> List.filter (fun (i, _) -> i <> idx)
            |> List.map snd
          in
          let snapped_x, snapped_y, snapped_angle =
            snap_new_object ~others ~tool_type:obj.tool_type ~x:obj.x ~y:obj.y
              ~angle:obj.angle
          in
          let dx = snapped_x - obj.x in
          let dy = snapped_y - obj.y in
          let group = !selected_group in
          objects :=
            List.mapi
              (fun i o ->
                if group <> [] && List.mem i group then
                  if i = idx then
                    {
                      o with
                      x = snapped_x;
                      y = snapped_y;
                      angle = snapped_angle;
                      ungrouped = false;
                    }
                  else { o with x = o.x + dx; y = o.y + dy }
                else if group = [] && i = idx then
                  {
                    o with
                    x = snapped_x;
                    y = snapped_y;
                    angle = snapped_angle;
                    ungrouped = false;
                  }
                else o)
              objs;

          recompute_groups ();
          selected_group := get_group_for_index idx;
          redraw_all ()
        with _ -> ())
    | _ -> ());
    drag_offset := None;
    false
  in

  (* Connect signals *)
  let _ = drawing_area#misc#connect#draw ~callback:draw_callback in
  let _ = drawing_area#event#connect#configure ~callback:configure_callback in
  let _ =
    drawing_area#event#connect#button_press ~callback:button_press_callback
  in
  let _ =
    drawing_area#event#connect#button_release ~callback:button_release_callback
  in
  let _ = drawing_area#event#connect#motion_notify ~callback:motion_callback in

  (* Set event mask to receive mouse events *)
  drawing_area#event#add
    [ `BUTTON_PRESS; `BUTTON_RELEASE; `POINTER_MOTION; `BUTTON1_MOTION ];

  (* Connect window destroy signal to quit the application *)
  let _ = window#connect#destroy ~callback:GMain.quit in

  (* Show all widgets *)
  window#show ();

  (* Start the GTK main loop *)
  GMain.main ()
