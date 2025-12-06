open Cs3110_final_project.Tool_types
open Cs3110_final_project.Road
open Cs3110_final_project.Intersection
open Cs3110_final_project.Building
open Cs3110_final_project.Settings

(* Type to represent a drawn object *)
type drawn_object = {
  tool_type : tool;
  x : int;
  y : int;
  angle : float;
  settings : settings; (* per-object settings *)
  ungrouped : bool;
}

let road_length = 120.0
let road_width = 30.0
let building_width = 50.0

(* Max distance in pixels to snap. *)
let snap_threshold = 25.0

let distance (x1, y1) (x2, y2) =
  let dx = x1 -. x2 in
  let dy = y1 -. y2 in
  sqrt ((dx *. dx) +. (dy *. dy))

(* Compute the two endpoints of a road, based on its center and angle. *)
let road_endpoints (obj : drawn_object) =
  let fx = float_of_int obj.x in
  let fy = float_of_int obj.y in
  let half_len = road_length /. 2.0 in
  let dx = half_len *. cos obj.angle in
  let dy = half_len *. sin obj.angle in
  ((fx -. dx, fy -. dy), (fx +. dx, fy +. dy))

(* Approximate attachment points on each side of the road where buildings can snap. *)
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

type snap_target_kind =
  | Target_road_end
  | Target_intersection_center
  | Target_road_side

type snap_target = { tx : float; ty : float; tkind : snap_target_kind }

(* Collect all potential snap targets from existing objects. *)
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
          (* Side points for building attachment *)
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

(* Compute a snapped position for a new or moved object. *)
let snap_new_object ~(others : drawn_object list) ~(tool_type : tool) ~(x : int)
    ~(y : int) ~(angle : float) : int * int * float =
  let fx = float_of_int x in
  let fy = float_of_int y in
  let road_targets, intersection_targets, road_side_targets =
    collect_snap_targets others
  in
  match tool_type with
  | ROAD -> (
      let obj =
        {
          tool_type = ROAD;
          x;
          y;
          angle;
          settings = Road.get_settings ();
          ungrouped = false;
        }
      in
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

(* Create a right-click context menu with nested submenus *)
let make_settings_menu_for_road () =
  let menu = GMenu.menu () in

  (* Speed submenu *)
  let speed_menu = GMenu.menu () in
  let speed_item =
    GMenu.menu_item ~label:"Speed Limit" ~packing:menu#append ()
  in
  speed_item#set_submenu speed_menu;
  List.iter
    (fun spd ->
      let item =
        GMenu.menu_item
          ~label:(string_of_int spd ^ " mph")
          ~packing:speed_menu#append ()
      in
      ignore
        (item#connect#activate ~callback:(fun () ->
             Road.set_settings
               (RoadSettings
                  {
                    speed_limit = spd;
                    num_lanes =
                      (match Road.get_settings () with
                      | RoadSettings r -> r.num_lanes
                      | _ -> 2);
                    max_capacity =
                      (match Road.get_settings () with
                      | RoadSettings r -> r.max_capacity
                      | _ -> 10);
                  }))))
    [ 25; 35; 50; 65 ];

  (* Lane submenu *)
  let lane_menu = GMenu.menu () in
  let lane_item =
    GMenu.menu_item ~label:"Number of Lanes" ~packing:menu#append ()
  in
  lane_item#set_submenu lane_menu;
  List.iter
    (fun ln ->
      let item =
        GMenu.menu_item
          ~label:(string_of_int ln ^ " lanes")
          ~packing:lane_menu#append ()
      in
      ignore
        (item#connect#activate ~callback:(fun () ->
             Road.set_settings
               (RoadSettings
                  {
                    speed_limit =
                      (match Road.get_settings () with
                      | RoadSettings r -> r.speed_limit
                      | _ -> 35);
                    num_lanes = ln;
                    max_capacity =
                      (match Road.get_settings () with
                      | RoadSettings r -> r.max_capacity
                      | _ -> 10);
                  }))))
    [ 1; 2; 3; 4 ];

  (* Capacity submenu *)
  let cap_menu = GMenu.menu () in
  let cap_item =
    GMenu.menu_item ~label:"Road Capacity" ~packing:menu#append ()
  in
  cap_item#set_submenu cap_menu;
  List.iter
    (fun cap ->
      let item =
        GMenu.menu_item
          ~label:(string_of_int cap ^ " cars")
          ~packing:cap_menu#append ()
      in
      ignore
        (item#connect#activate ~callback:(fun () ->
             Road.set_settings
               (RoadSettings
                  {
                    speed_limit =
                      (match Road.get_settings () with
                      | RoadSettings r -> r.speed_limit
                      | _ -> 35);
                    num_lanes =
                      (match Road.get_settings () with
                      | RoadSettings r -> r.num_lanes
                      | _ -> 2);
                    max_capacity = cap;
                  }))))
    [ 5; 10; 20; 50 ];

  menu

let make_settings_menu_for_intersection () =
  let menu = GMenu.menu () in

  (* Stop type submenu *)
  let stop_menu = GMenu.menu () in
  let stop_item = GMenu.menu_item ~label:"Stop Type" ~packing:menu#append () in
  stop_item#set_submenu stop_menu;

  let mk_stop_item label stops =
    let item = GMenu.menu_item ~label ~packing:stop_menu#append () in
    ignore
      (item#connect#activate ~callback:(fun () ->
           Intersection.set_settings
             (IntersectionSettings
                {
                  num_stops = stops;
                  has_traffic_light =
                    (match Intersection.get_settings () with
                    | IntersectionSettings s -> s.has_traffic_light
                    | _ -> false);
                  stop_duration =
                    (match Intersection.get_settings () with
                    | IntersectionSettings s -> s.stop_duration
                    | _ -> 3.0);
                })))
  in
  mk_stop_item "2-way stop" 2;
  mk_stop_item "4-way stop" 4;

  (* Traffic light toggle *)
  let light_item =
    GMenu.menu_item ~label:"Toggle Traffic Light" ~packing:menu#append ()
  in
  ignore
    (light_item#connect#activate ~callback:(fun () ->
         let cur =
           match Intersection.get_settings () with
           | IntersectionSettings s -> s
           | _ ->
               { num_stops = 4; has_traffic_light = false; stop_duration = 3.0 }
         in
         Intersection.set_settings
           (IntersectionSettings
              { cur with has_traffic_light = not cur.has_traffic_light })));

  menu

let make_settings_menu_for_building () =
  let menu = GMenu.menu () in
  let item10 = GMenu.menu_item ~label:"Rate 10" ~packing:menu#append () in
  let item20 = GMenu.menu_item ~label:"Rate 20" ~packing:menu#append () in
  let item40 = GMenu.menu_item ~label:"Rate 40" ~packing:menu#append () in

  let set r =
    Building.set_settings (BuildingSettings { rate_of_traffic = r })
  in
  ignore (item10#connect#activate ~callback:(fun () -> set 10));
  ignore (item20#connect#activate ~callback:(fun () -> set 20));
  ignore (item40#connect#activate ~callback:(fun () -> set 40));

  menu

(* ---------- Grouping helpers (based on snapping) ---------- *)

(* Check if two objects are "connected" via snapping, ignoring ungrouped ones. *)
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

  (* Create a vertical box to organize widgets *)
  let vbox = GPack.vbox ~packing:window#add () in

  (* Toolbar / tools row *)
  let toolbar = GPack.hbox ~packing:(vbox#pack ~expand:false) () in
  toolbar#misc#set_size_request ~height:45 ();
  let _label = GMisc.label ~text:"Drawing Tools" ~packing:toolbar#add () in

  let current_tool = ref None in
  let objects : drawn_object list ref = ref [] in
  let selected_object : int option ref = ref None in
  (* Currently selected object index *)
  let selected_group : int list ref = ref [] in
  (* Indices of the currently selected group *)

  (* Grouping data:
     - group_ids.(i) = group id (an int) for object index i, or -1 if ungrouped
     - groups_tbl : group id -> list of indices in that group
     - ungrouped_list : list of indices of objects with ungrouped = true *)
  let group_ids : int array ref = ref [||] in
  let groups_tbl : (int, int list) Hashtbl.t = Hashtbl.create 32 in
  let ungrouped_list : int list ref = ref [] in

  (* Recompute all groups + ungrouped indices from !objects using DFS and is_connected. *)
  let recompute_groups () =
    let objs = !objects in
    let n = List.length objs in
    let visited = Array.make n false in
    let groupID_table = Array.make n (-1) in
    Hashtbl.reset groups_tbl;
    let ungrouped_acc = ref [] in
    let rec dfs i groupID =
      visited.(i) <- true;
      groupID_table.(i) <- groupID;
      let members =
        match Hashtbl.find_opt groups_tbl groupID with
        | Some lst -> i :: lst
        | None -> [ i ]
      in
      Hashtbl.replace groups_tbl groupID members;
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

  (* Get the group (list of indices) for a given object index. *)
  let get_group_for_index idx =
    let objs = !objects in
    let len = Array.length !group_ids in
    if idx < 0 || idx >= len then [ idx ]
    else
      let obj = List.nth objs idx in
      if obj.ungrouped then [ idx ]
      else
        let gid = !group_ids.(idx) in
        match Hashtbl.find_opt groups_tbl gid with
        | Some members -> members
        | None -> [ idx ]
  in

  let is_moving = ref false in
  let is_deleting = ref false in
  let is_rotating = ref false in
  let is_ungrouping = ref false in
  let drag_offset : (float * float) option ref = ref None in
  let button_pressed = ref false in

  let add_button label callback =
    let b = GButton.button ~label ~packing:toolbar#add () in
    ignore (b#connect#clicked ~callback);
    b
  in

  let road_btn =
    add_button "Road" (fun () ->
        current_tool := Some ROAD;
        is_deleting := false;
        is_moving := false;
        is_ungrouping := false)
  in

  ignore
    (road_btn#event#connect#button_press ~callback:(fun ev ->
         if GdkEvent.Button.button ev = 3 then (
           let menu = make_settings_menu_for_road () in
           menu#popup ~button:3 ~time:(GdkEvent.Button.time ev);
           true)
         else false));

  let int_btn =
    add_button "Intersection" (fun () ->
        current_tool := Some INTERSECTION;
        is_deleting := false;
        is_moving := false;
        is_ungrouping := false)
  in
  int_btn#event#add [ `BUTTON_PRESS ];

  ignore
    (int_btn#event#connect#button_press ~callback:(fun ev ->
         if GdkEvent.Button.button ev = 3 then (
           let menu = make_settings_menu_for_intersection () in
           menu#popup ~button:3 ~time:(GdkEvent.Button.time ev);
           true)
         else false));

  let bldg_btn =
    add_button "Building" (fun () ->
        current_tool := Some BUILDING;
        is_deleting := false;
        is_moving := false;
        is_ungrouping := false)
  in
  bldg_btn#event#add [ `BUTTON_PRESS ];

  ignore
    (bldg_btn#event#connect#button_press ~callback:(fun ev ->
         if GdkEvent.Button.button ev = 3 then (
           let menu = make_settings_menu_for_building () in
           menu#popup ~button:3 ~time:(GdkEvent.Button.time ev);
           true)
         else false));

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

  (* Drawing area *)
  let drawing_area =
    GMisc.drawing_area ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  drawing_area#misc#set_size_request ~width:800 ~height:550 ();

  let surface : Cairo.Surface.t option ref = ref None in

  let init_surface width height =
    let s = Cairo.Image.(create ARGB32 ~w:width ~h:height) in
    let cr = Cairo.create s in
    Cairo.set_source_rgb cr 1.0 1.0 1.0;
    Cairo.paint cr;
    surface := Some s;
    s
  in

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
                Road.draw ~cr ~x:obj.x ~y:obj.y ~angle:obj.angle obj.settings
            | INTERSECTION ->
                Intersection.draw ~cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                  obj.settings
            | BUILDING ->
                Building.draw ~cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                  obj.settings)
          !objects;

        (* Draw selection highlights for whole group *)
        (match !selected_group with
        | [] -> ()
        | group_indices ->
            List.iter
              (fun idx ->
                try
                  let obj = List.nth !objects idx in
                  match obj.tool_type with
                  | ROAD ->
                      Road.draw_selection ~cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                        obj.settings
                  | INTERSECTION ->
                      Intersection.draw_selection ~cr ~x:obj.x ~y:obj.y
                        ~angle:obj.angle obj.settings
                  | BUILDING ->
                      Building.draw_selection ~cr ~x:obj.x ~y:obj.y
                        ~angle:obj.angle obj.settings
                with _ -> ())
              group_indices);

        (* Draw rotate button only for explicitly selected object *)
        (match !selected_object with
        | Some idx -> (
            try
              let obj = List.nth !objects idx in
              match obj.tool_type with
              | ROAD ->
                  Road.draw_selection ~cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                    obj.settings;
                  Road.draw_rotate_button ~cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                    obj.settings
              | INTERSECTION ->
                  Intersection.draw_selection ~cr ~x:obj.x ~y:obj.y
                    ~angle:obj.angle obj.settings;
                  Intersection.draw_rotate_button ~cr ~x:obj.x ~y:obj.y
                    ~angle:obj.angle obj.settings
              | BUILDING ->
                  Building.draw_selection ~cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                    obj.settings;
                  Building.draw_rotate_button ~cr ~x:obj.x ~y:obj.y
                    ~angle:obj.angle obj.settings
            with _ -> ())
        | None -> ());

        drawing_area#misc#queue_draw ()
    | None -> ()
  in

  (* Draw handler *)
  let draw_callback cr =
    match !surface with
    | Some s ->
        Cairo.set_source_surface cr s ~x:0.0 ~y:0.0;
        Cairo.paint cr;
        true
    | None -> false
  in

  (* Resize handler *)
  let configure_callback _ =
    let alloc = drawing_area#misc#allocation in
    ignore (init_surface alloc.Gtk.width alloc.Gtk.height);
    redraw_all ();
    true
  in

  (* Mouse press handler *)
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
          (* Drawing mode - place new object *)
          let angle = 0.0 in
          let snapped_x, snapped_y, snapped_angle =
            snap_new_object ~others:!objects ~tool_type ~x ~y ~angle
          in
          objects :=
            {
              tool_type;
              x = snapped_x;
              y = snapped_y;
              angle = snapped_angle;
              settings =
                (match tool_type with
                | ROAD -> Road.get_settings ()
                | INTERSECTION -> Intersection.get_settings ()
                | BUILDING -> Building.get_settings ());
              ungrouped = false;
            }
            :: !objects;
          recompute_groups ();
          selected_object := None;
          selected_group := [];
          redraw_all ();
          true
      | None ->
          (* DELETE MODE *)
          if !is_deleting then (
            let clicked_object = ref None in

            List.iteri
              (fun idx obj ->
                if !clicked_object = None then
                  match obj.tool_type with
                  | ROAD ->
                      if
                        Road.point_inside ~x:obj.x ~y:obj.y ~px:x_f ~py:y_f
                          obj.settings
                      then clicked_object := Some idx
                  | INTERSECTION ->
                      if
                        Intersection.point_inside ~x:obj.x ~y:obj.y ~px:x_f
                          ~py:y_f obj.settings
                      then clicked_object := Some idx
                  | BUILDING ->
                      if
                        Building.point_inside ~x:obj.x ~y:obj.y ~px:x_f ~py:y_f
                          obj.settings
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
                  List.filteri (fun i _ -> not (List.mem i to_delete)) objs;
                recompute_groups ();
                selected_object := None;
                selected_group := [];
                redraw_all ()
            | None -> ());
            true (* MOVE MODE *))
          else if !is_moving then (
            let clicked_object = ref None in

            List.iteri
              (fun idx obj ->
                if !clicked_object = None then (
                  (* Check rotate button for selected object *)
                  (if !selected_object = Some idx then
                     match obj.tool_type with
                     | ROAD ->
                         if
                           Road.point_on_rotate_button ~x:obj.x ~y:obj.y
                             ~angle:obj.angle ~px:x_f ~py:y_f obj.settings
                         then (
                           clicked_object := Some idx;
                           is_rotating := true)
                     | INTERSECTION ->
                         if
                           Intersection.point_on_rotate_button ~x:obj.x ~y:obj.y
                             ~angle:obj.angle ~px:x_f ~py:y_f obj.settings
                         then (
                           clicked_object := Some idx;
                           is_rotating := true)
                     | BUILDING ->
                         if
                           Building.point_on_rotate_button ~x:obj.x ~y:obj.y
                             ~angle:obj.angle ~px:x_f ~py:y_f obj.settings
                         then (
                           clicked_object := Some idx;
                           is_rotating := true));

                  (* Hitbox check *)
                  if !clicked_object = None then
                    match obj.tool_type with
                    | ROAD ->
                        if
                          Road.point_inside ~x:obj.x ~y:obj.y ~px:x_f ~py:y_f
                            obj.settings
                        then (
                          clicked_object := Some idx;
                          is_rotating := false;
                          drag_offset :=
                            Some
                              ( x_f -. float_of_int obj.x,
                                y_f -. float_of_int obj.y ))
                    | INTERSECTION ->
                        if
                          Intersection.point_inside ~x:obj.x ~y:obj.y ~px:x_f
                            ~py:y_f obj.settings
                        then (
                          clicked_object := Some idx;
                          is_rotating := false;
                          drag_offset :=
                            Some
                              ( x_f -. float_of_int obj.x,
                                y_f -. float_of_int obj.y ))
                    | BUILDING ->
                        if
                          Building.point_inside ~x:obj.x ~y:obj.y ~px:x_f
                            ~py:y_f obj.settings
                        then (
                          clicked_object := Some idx;
                          is_rotating := false;
                          drag_offset :=
                            Some
                              ( x_f -. float_of_int obj.x,
                                y_f -. float_of_int obj.y ))))
              !objects;

            (match !clicked_object with
            | Some idx ->
                let obj = List.nth !objects idx in
                if obj.ungrouped then (
                  selected_object := Some idx;
                  selected_group := [ idx ])
                else (
                  selected_object := Some idx;
                  selected_group := get_group_for_index idx);
                redraw_all ()
            | None ->
                selected_object := None;
                selected_group := [];
                is_rotating := false;
                drag_offset := None);
            true)
          else false)
    else false
  in

  (* Mouse motion handler *)
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
           (* Rotating: calculate angle from center to mouse position *)
           let cx = float_of_int obj.x in
           let cy = float_of_int obj.y in
           let new_angle =
             match obj.tool_type with
             | ROAD -> Road.calculate_rotation ~cx ~cy ~mx:x ~my:y
             | INTERSECTION ->
                 Intersection.calculate_rotation ~cx ~cy ~mx:x ~my:y
             | BUILDING -> Building.calculate_rotation ~cx ~cy ~mx:x ~my:y
           in
           (* Only rotate the selected object, not the whole group *)
           objects :=
             List.mapi
               (fun i o ->
                 if i = selected_idx then { o with angle = new_angle } else o)
               objs
         else
           (* Dragging: move object (and its group) by mouse offset *)
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

  (* Mouse release handler *)
  let button_release_callback _ev =
    button_pressed := false;
    is_rotating := false;
    (match !selected_object with
    | Some idx when !is_moving && not !is_rotating -> (
        (* Snap AFTER releasing a drag — do NOT modify settings *)
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
                      (* allow regrouping after a move/snap *)
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
          (* After snapping, recompute grouping and update the selected_group *)
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

  (* Enable mouse events *)
  drawing_area#event#add
    [ `BUTTON_PRESS; `BUTTON_RELEASE; `POINTER_MOTION; `BUTTON1_MOTION ];

  (* Quit on window close *)
  let _ = window#connect#destroy ~callback:GMain.quit in

  window#show ();
  GMain.main ()
