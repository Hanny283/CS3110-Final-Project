open Cs3110_final_project.Tool_types
open Cs3110_final_project.Road
open Cs3110_final_project.Intersection
open Cs3110_final_project.Building

(* Type to represent a drawn object *)
type drawn_object = {
  tool_type : tool;
  x : int;
  y : int;
  angle : float;
  ungrouped : bool; (* true = treat individually even if snapped *)
}

(* Constants for snapping logic *)
let road_length = 120.0
let road_width = 30.0
let building_width = 50.0

(* Max distance in pixels to snap. *)
let snap_threshold = 55.0

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
  let objects = ref [] in
  (* List of drawn objects *)
  let selected_object = ref None in
  (* Currently selected object index *)
  let selected_group = ref [] in
  (* Indices of the currently selected group *)

  (* Grouping data:
     - group_ids.(i) = group id (an int) for object index i, or -1 if ungrouped
     - groups_tbl : group id -> list of indices in that group
     - ungrouped_list : list of indices of objects with ungrouped = true *)
  let group_ids = ref [||] in
  let groups_tbl : (int, int list) Hashtbl.t = Hashtbl.create 32 in
  let ungrouped_list = ref [] in

  (* Recompute all groups + ungrouped indices from !objects using DFS and is_connected.
     This replaces the old BFS-based compute_group. *)
  let recompute_groups () =
    let objs = !objects in
    let n = List.length objs in
    let visited = Array.make n false in
    let gids = Array.make n (-1) in
    Hashtbl.reset groups_tbl;
    let ungrouped_acc = ref [] in
    let rec dfs i gid =
      visited.(i) <- true;
      gids.(i) <- gid;
      let members =
        match Hashtbl.find_opt groups_tbl gid with
        | Some lst -> i :: lst
        | None -> [ i ]
      in
      Hashtbl.replace groups_tbl gid members;
      for j = 0 to n - 1 do
        if (not visited.(j)) && is_connected objs i j then dfs j gid
      done
    in
    for i = 0 to n - 1 do
      let obj = List.nth objs i in
      if obj.ungrouped then ungrouped_acc := i :: !ungrouped_acc
      else if not visited.(i) then dfs i i
    done;
    group_ids := gids;
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
        match Hashtbl.find_opt groups_tbl gid with
        | Some members -> members
        | None -> [ idx ]
  in

  let is_moving = ref false in
  (* Whether we're in move mode *)
  let is_deleting = ref false in
  (* Whether we're in delete mode *)
  let is_rotating = ref false in
  (* Whether we're rotating an object *)
  let is_ungrouping = ref false in
  (* Whether we're in ungroup mode *)
  let drag_offset = ref None in
  (* Offset from object center when dragging *)
  let button_pressed = ref false in
  (* Whether mouse button is currently pressed *)
  let add_button label callback =
    let b = GButton.button ~label ~packing:toolbar#add () in
    ignore (b#connect#clicked ~callback);
    b
  in

  let _ =
    add_button "Road" (fun () ->
        current_tool := Some ROAD;
        is_deleting := false;
        is_moving := false;
        is_ungrouping := false)
  in

  let _ =
    add_button "Intersection" (fun () ->
        current_tool := Some INTERSECTION;
        is_deleting := false;
        is_moving := false;
        is_ungrouping := false)
  in

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
    (* Fill with white background *)
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

        (* Draw rotate button only for the explicitly selected object *)
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
          (* Drawing mode - place new object *)
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
          (* Recompute grouping after adding a new object *)
          recompute_groups ();
          (* Clear selection when drawing new objects *)
          selected_object := None;
          selected_group := [];
          redraw_all ();
          true
      | None ->
          if
            (* Delete mode - delete clicked object or whole group *)
            !is_deleting
          then (
            let clicked_object = ref None in
            (* Check each object (from front to back) *)
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
            (* Delete the clicked object or its group if found *)
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
                (* Recompute grouping after delete *)
                recompute_groups ();
                selected_object := None;
                selected_group := [];
                redraw_all ()
            | None -> ());
            true)
          else if
            (* Ungroup mode - ungroup clicked object only *)
            !is_ungrouping
          then (
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
                (* Mark this object as ungrouped so it moves/rotates/deletes independently *)
                objects :=
                  List.mapi
                    (fun i o ->
                      if i = idx then { o with ungrouped = true } else o)
                    !objects;
                (* Recompute grouping after ungrouping *)
                recompute_groups ();
                selected_object := Some idx;
                selected_group := [ idx ];
                redraw_all ()
            | None -> ());
            true)
          else if !is_moving then (
            let clicked_object = ref None in
            let clicked_rotate_button = ref false in

            (* Check each object (from front to back) *)
            List.iteri
              (fun idx obj ->
                if !clicked_object = None then (
                  (* Check if clicked on rotate button *)
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

                  (* Check if clicked on object itself *)
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

            (* Update selection + group *)
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
          else
            (* No mode active - do nothing *)
            false)
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

  (* Set event mask to receive mouse events *)
  drawing_area#event#add
    [ `BUTTON_PRESS; `BUTTON_RELEASE; `POINTER_MOTION; `BUTTON1_MOTION ];

  (* Connect window destroy signal to quit the application *)
  let _ = window#connect#destroy ~callback:GMain.quit in

  (* Show all widgets *)
  window#show ();

  (* Start the GTK main loop *)
  GMain.main ()
