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
  settings : settings; (* NEW: per-object settings *)
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
        { tool_type = ROAD; x; y; angle; settings = Road.get_settings () }
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
  let selected_object = ref None in
  let is_moving = ref false in
  let is_deleting = ref false in
  let is_rotating = ref false in
  let drag_offset = ref None in
  let button_pressed = ref false in

  let add_button label callback =
    let b = GButton.button ~label ~packing:toolbar#add () in
    ignore (b#connect#clicked ~callback);
    b
  in

  let _ =
    add_button "Road" (fun () ->
        current_tool := Some ROAD;
        is_deleting := false;
        is_moving := false)
  in

  let _ =
    add_button "Intersection" (fun () ->
        current_tool := Some INTERSECTION;
        is_deleting := false;
        is_moving := false)
  in

  let _ =
    add_button "Building" (fun () ->
        current_tool := Some BUILDING;
        is_deleting := false;
        is_moving := false)
  in

  (* Move button *)
  let _ =
    add_button "Move" (fun () ->
        current_tool := None;
        is_moving := true;
        is_deleting := false;
        selected_object := None)
  in

  (* Drawing area *)
  let drawing_area =
    GMisc.drawing_area ~packing:(vbox#pack ~expand:true ~fill:true) ()
  in
  drawing_area#misc#set_size_request ~width:800 ~height:550 ();

  let surface = ref None in

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

        List.iter
          (fun obj ->
            match obj.tool_type with
            | ROAD ->
                Road.draw cr ~x:obj.x ~y:obj.y ~angle:obj.angle obj.settings
            | INTERSECTION ->
                Intersection.draw cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                  obj.settings
            | BUILDING ->
                Building.draw cr ~x:obj.x ~y:obj.y ~angle:obj.angle obj.settings)
          !objects;

        (* Selection highlight + rotate button *)
        (match !selected_object with
        | Some idx -> (
            try
              let obj = List.nth !objects idx in
              match obj.tool_type with
              | ROAD ->
                  Road.draw_selection cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                    obj.settings;
                  Road.draw_rotate_button cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                    obj.settings
              | INTERSECTION ->
                  Intersection.draw_selection cr ~x:obj.x ~y:obj.y
                    ~angle:obj.angle obj.settings;
                  Intersection.draw_rotate_button cr ~x:obj.x ~y:obj.y
                    ~angle:obj.angle obj.settings
              | BUILDING ->
                  Building.draw_selection cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                    obj.settings;
                  Building.draw_rotate_button cr ~x:obj.x ~y:obj.y
                    ~angle:obj.angle obj.settings
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
        selected_object := None)
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
          let angle = 0.0 in
          let snapped_x, snapped_y, snapped_angle =
            snap_new_object ~others:!objects ~tool_type ~x ~y ~angle
          in

          let settings =
            match tool_type with
            | ROAD -> Road.get_settings ()
            | INTERSECTION -> Intersection.get_settings ()
            | BUILDING -> Building.get_settings ()
          in

          objects :=
            {
              tool_type;
              x = snapped_x;
              y = snapped_y;
              angle = snapped_angle;
              settings;
            }
            :: !objects;

          selected_object := None;
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
                objects := List.filteri (fun i _ -> i <> idx) !objects;
                redraw_all ()
            | None -> ());

            true)
          else if !is_moving then (
            let clicked_object = ref None in

            List.iteri
              (fun idx obj ->
                if !clicked_object = None then (
                  (* Check rotate button first *)
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

                  (* If not rotate button, check object hitbox *)
                  if !clicked_object = None then
                    match obj.tool_type with
                    | ROAD ->
                        if
                          Road.point_inside ~x:obj.x ~y:obj.y ~px:x_f ~py:y_f
                            obj.settings
                        then (
                          clicked_object := Some idx;
                          is_rotating := false;
                          let fx = float_of_int obj.x in
                          let fy = float_of_int obj.y in
                          drag_offset := Some (x_f -. fx, y_f -. fy))
                    | INTERSECTION ->
                        if
                          Intersection.point_inside ~x:obj.x ~y:obj.y ~px:x_f
                            ~py:y_f obj.settings
                        then (
                          clicked_object := Some idx;
                          is_rotating := false;
                          let fx = float_of_int obj.x in
                          let fy = float_of_int obj.y in
                          drag_offset := Some (x_f -. fx, y_f -. fy))
                    | BUILDING ->
                        if
                          Building.point_inside ~x:obj.x ~y:obj.y ~px:x_f
                            ~py:y_f obj.settings
                        then (
                          clicked_object := Some idx;
                          is_rotating := false;
                          let fx = float_of_int obj.x in
                          let fy = float_of_int obj.y in
                          drag_offset := Some (x_f -. fx, y_f -. fy))))
              !objects;

            selected_object := !clicked_object;
            redraw_all ();
            true)
          else false)
    else false
  in

  (* Mouse motion handler *)
  let motion_callback ev =
    if !button_pressed && !selected_object <> None then (
      let x = GdkEvent.Motion.x ev in
      let y = GdkEvent.Motion.y ev in

      let selected_idx =
        match !selected_object with Some idx -> idx | None -> assert false
      in

      let obj = List.nth !objects selected_idx in
      let objects_list = !objects in

      (if !is_rotating then
         let cx = float_of_int obj.x in
         let cy = float_of_int obj.y in

         let new_angle =
           match obj.tool_type with
           | ROAD -> Road.calculate_rotation ~cx ~cy ~mx:x ~my:y
           | INTERSECTION -> Intersection.calculate_rotation ~cx ~cy ~mx:x ~my:y
           | BUILDING -> Building.calculate_rotation ~cx ~cy ~mx:x ~my:y
         in

         objects :=
           List.mapi
             (fun i o ->
               if i = selected_idx then { o with angle = new_angle } else o)
             objects_list
       else
         match !drag_offset with
         | Some (off_x, off_y) ->
             let new_x = int_of_float (x -. off_x) in
             let new_y = int_of_float (y -. off_y) in
             objects :=
               List.mapi
                 (fun i o ->
                   if i = selected_idx then { o with x = new_x; y = new_y }
                   else o)
                 objects_list
         | None -> ());

      redraw_all ();
      true)
    else false
  in

  (* Mouse release handler *)
  let button_release_callback _ev =
    button_pressed := false;

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
          let sx, sy, sa =
            snap_new_object ~others ~tool_type:obj.tool_type ~x:obj.x ~y:obj.y
              ~angle:obj.angle
          in
          objects :=
            List.mapi
              (fun i o ->
                if i = idx then { o with x = sx; y = sy; angle = sa } else o)
              objs;
          redraw_all ()
        with _ -> ())
    | _ -> ());

    is_rotating := false;
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
