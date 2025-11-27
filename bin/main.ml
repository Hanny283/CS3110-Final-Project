open Cs3110_final_project.Tool_types
open Cs3110_final_project.Road
open Cs3110_final_project.Intersection
open Cs3110_final_project.Building

(* Type to represent a drawn object *)
type drawn_object = { tool_type : tool; x : int; y : int; angle : float }

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
  let is_moving = ref false in
  (* Whether we're in move mode *)
  let is_deleting = ref false in
  (* Whether we're in delete mode *)
  let is_rotating = ref false in
  (* Whether we're rotating an object *)
  let drag_offset = ref None in
  (* Offset from object center when dragging *)
  let button_pressed = ref false in
  (* Whether mouse button is currently pressed *)
  let add_button label callback =
    let b = GButton.button ~label ~packing:toolbar#add () in
    ignore (b#connect#clicked ~callback);
    b
  in

  let _ = add_button "Road" (fun () ->
      current_tool := Some ROAD;
      is_deleting := false;
      is_moving := false) in

  let _ =
    add_button "Intersection" (fun () ->
        current_tool := Some INTERSECTION;
        is_deleting := false;
        is_moving := false)
  in

  let _ =
    add_button "Building" (fun () ->
        current_tool := Some (BUILDING (HOUSE { time_in = 8; time_out = 18 }));
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
        List.iter
          (fun obj ->
            match obj.tool_type with
            | ROAD ->
                Road.draw cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                  (Road.get_settings ())
            | INTERSECTION ->
                Intersection.draw cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                  (Intersection.get_settings ())
            | BUILDING _ ->
                Building.draw cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                  (Building.get_settings ()))
          !objects;
        (* Draw selection highlights and rotate buttons *)
        (match !selected_object with
        | Some idx -> (
            try
              let obj = List.nth !objects idx in
              match obj.tool_type with
              | ROAD ->
                  Road.draw_selection cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                    (Road.get_settings ());
                  Road.draw_rotate_button cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                    (Road.get_settings ())
              | INTERSECTION ->
                  Intersection.draw_selection cr ~x:obj.x ~y:obj.y
                    ~angle:obj.angle (Intersection.get_settings ());
                  Intersection.draw_rotate_button cr ~x:obj.x ~y:obj.y
                    ~angle:obj.angle (Intersection.get_settings ())
              | BUILDING _ ->
                  Building.draw_selection cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                    (Building.get_settings ());
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
        selected_object := None)
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

  (* Mouse button press handler - handle drawing, selection, and rotation *)
  let button_press_callback ev =
    let button = GdkEvent.Button.button ev in
    if button = 1 then (
      button_pressed := true;
      let x_f = GdkEvent.Button.x ev in
      let y_f = GdkEvent.Button.y ev in
      let x = int_of_float x_f in
      let y = int_of_float y_f in

      match !current_tool with
      | Some tool_type -> (
          (* Drawing mode - place new object *)
          (match tool_type with
          | ROAD ->
              objects := { tool_type = ROAD; x; y; angle = 0.0 } :: !objects
          | INTERSECTION ->
              objects :=
                { tool_type = INTERSECTION; x; y; angle = 0.0 } :: !objects
          | BUILDING _ ->
              objects := { tool_type; x; y; angle = 0.0 } :: !objects);
          (* Clear selection when drawing new objects *)
          selected_object := None;
          redraw_all ();
          true)
      | None -> (
          (* Delete mode - delete clicked object *)
          if !is_deleting then (
            let clicked_object = ref None in
            (* Check each object (from front to back) *)
            List.iteri
              (fun idx obj ->
                if !clicked_object = None then (
                  match obj.tool_type with
                  | ROAD ->
                      if Road.point_inside ~x:obj.x ~y:obj.y ~px:x_f ~py:y_f
                           (Road.get_settings ())
                      then clicked_object := Some idx
                  | INTERSECTION ->
                      if Intersection.point_inside ~x:obj.x ~y:obj.y ~px:x_f
                           ~py:y_f (Intersection.get_settings ())
                      then clicked_object := Some idx
                  | BUILDING _ ->
                      if Building.point_inside ~x:obj.x ~y:obj.y ~px:x_f ~py:y_f
                           (Building.get_settings ())
                      then clicked_object := Some idx))
              !objects;
            (* Delete the clicked object if found *)
            (match !clicked_object with
            | Some idx -> (
                objects := List.filteri (fun i _ -> i <> idx) !objects;
                redraw_all ())
            | None -> ());
            true)
          (* Selection/move mode - check for clicks on objects *)
          else if !is_moving then (
          let clicked_object = ref None in
          let clicked_rotate_button = ref false in

          (* Check each object (from front to back) *)
          List.iteri
            (fun idx obj ->
              if !clicked_object = None then (
                (* Check if clicked on rotate button *)
                if !selected_object = Some idx then (
                  match obj.tool_type with
                  | ROAD ->
                      if Road.point_on_rotate_button ~x:obj.x ~y:obj.y
                           ~angle:obj.angle ~px:x_f ~py:y_f
                           (Road.get_settings ())
                      then (
                        clicked_object := Some idx;
                        clicked_rotate_button := true;
                        is_rotating := true)
                  | INTERSECTION ->
                      if Intersection.point_on_rotate_button ~x:obj.x ~y:obj.y
                           ~angle:obj.angle ~px:x_f ~py:y_f
                           (Intersection.get_settings ())
                      then (
                        clicked_object := Some idx;
                        clicked_rotate_button := true;
                        is_rotating := true)
                  | BUILDING _ ->
                      if Building.point_on_rotate_button ~x:obj.x ~y:obj.y
                           ~angle:obj.angle ~px:x_f ~py:y_f
                           (Building.get_settings ())
                      then (
                        clicked_object := Some idx;
                        clicked_rotate_button := true;
                        is_rotating := true));

                (* Check if clicked on object itself *)
                if !clicked_object = None then (
                  match obj.tool_type with
                  | ROAD ->
                      if Road.point_inside ~x:obj.x ~y:obj.y ~px:x_f ~py:y_f
                           (Road.get_settings ())
                      then (
                        clicked_object := Some idx;
                        clicked_rotate_button := false;
                        is_rotating := false;
                        let fx = float_of_int obj.x in
                        let fy = float_of_int obj.y in
                        drag_offset := Some (x_f -. fx, y_f -. fy))
                  | INTERSECTION ->
                      if Intersection.point_inside ~x:obj.x ~y:obj.y ~px:x_f
                           ~py:y_f (Intersection.get_settings ())
                      then (
                        clicked_object := Some idx;
                        clicked_rotate_button := false;
                        is_rotating := false;
                        let fx = float_of_int obj.x in
                        let fy = float_of_int obj.y in
                        drag_offset := Some (x_f -. fx, y_f -. fy))
                  | BUILDING _ ->
                      if Building.point_inside ~x:obj.x ~y:obj.y ~px:x_f ~py:y_f
                           (Building.get_settings ())
                      then (
                        clicked_object := Some idx;
                        clicked_rotate_button := false;
                        is_rotating := false;
                        let fx = float_of_int obj.x in
                        let fy = float_of_int obj.y in
                        drag_offset := Some (x_f -. fx, y_f -. fy)))))
            !objects;

          (* Update selection *)
          if !clicked_object <> None then
            selected_object := !clicked_object
          else (
            (* Clicked on empty space - deselect *)
            selected_object := None;
            is_rotating := false;
            drag_offset := None);

          redraw_all ();
          true)
          else (
            (* No mode active - do nothing *)
            false)))
    else false
  in

  (* Mouse motion handler - handle dragging and rotation *)
  let motion_callback ev =
    if !button_pressed && !selected_object <> None then (
      let x = GdkEvent.Motion.x ev in
      let y = GdkEvent.Motion.y ev in

      let selected_idx =
        match !selected_object with
        | Some idx -> idx
        | None -> assert false
      in

      try
        let obj = List.nth !objects selected_idx in
        let objects_list = !objects in

        if !is_rotating then (
          (* Rotating: calculate angle from center to mouse position *)
          let cx = float_of_int obj.x in
          let cy = float_of_int obj.y in
          let new_angle =
            match obj.tool_type with
            | ROAD -> Road.calculate_rotation ~cx ~cy ~mx:x ~my:y
            | INTERSECTION ->
                Intersection.calculate_rotation ~cx ~cy ~mx:x ~my:y
            | BUILDING _ ->
                Building.calculate_rotation ~cx ~cy ~mx:x ~my:y
          in
          objects :=
            List.mapi
              (fun i o -> if i = selected_idx then { o with angle = new_angle } else o)
              objects_list)
        else (
          (* Dragging: move object by mouse offset *)
          match !drag_offset with
          | Some (offset_x, offset_y) ->
              let new_x = int_of_float (x -. offset_x) in
              let new_y = int_of_float (y -. offset_y) in
              objects :=
                List.mapi
                  (fun i o ->
                    if i = selected_idx then { o with x = new_x; y = new_y }
                    else o)
                  objects_list
          | None -> ());

        redraw_all ();
        true
      with _ -> false)
    else false
  in

  (* Mouse button release handler *)
  let button_release_callback _ev =
    button_pressed := false;
    drag_offset := None;
    is_rotating := false;
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
    [
      `BUTTON_PRESS;
      `BUTTON_RELEASE;
      `POINTER_MOTION;
      `BUTTON1_MOTION;
    ];

  (* Connect window destroy signal to quit the application *)
  let _ = window#connect#destroy ~callback:GMain.quit in

  (* Show all widgets *)
  window#show ();

  (* Start the GTK main loop *)
  GMain.main ()
