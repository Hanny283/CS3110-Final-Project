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
  settings : settings;
}

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

  (* Helper function to create a tool button with settings dropdown *)
  let add_tool_with_settings tool_name tool_type settings_editor =
    (* Create a horizontal box for the tool button and settings button *)
    let tool_container = GPack.hbox ~packing:toolbar#add () in
    
    (* Tool button *)
    let tool_button = GButton.button ~label:tool_name ~packing:tool_container#add () in
    ignore (tool_button#connect#clicked ~callback:(fun () ->
        current_tool := Some tool_type;
        is_deleting := false;
        is_moving := false));
    
    (* Settings button (gear icon) *)
    let settings_button = GButton.button ~label:"⚙" ~packing:tool_container#add () in
    settings_button#misc#set_size_request ~width:30 ~height:30 ();
    ignore (settings_button#connect#clicked ~callback:(fun () ->
        settings_editor ()));
    
    tool_container
  in

  (* Settings editor for Road *)
  let edit_road_settings () =
    let dialog = GWindow.dialog ~title:"Road Settings" ~modal:true () in
    let vbox_dialog = dialog#vbox in
    
    (* Speed Limit *)
    let speed_hbox = GPack.hbox ~packing:vbox_dialog#pack () in
    let _ = GMisc.label ~text:"Speed Limit (mph): " ~packing:speed_hbox#add () in
    let speed_entry = GEdit.entry ~packing:speed_hbox#add () in
    let current_road_settings = match Road.get_settings () with
      | RoadSettings s -> s
      | _ -> { speed_limit = 35; num_lanes = 2; max_capacity = 100 }
    in
    speed_entry#set_text (string_of_int current_road_settings.speed_limit);
    
    (* Number of Lanes *)
    let lanes_hbox = GPack.hbox ~packing:vbox_dialog#pack () in
    let _ = GMisc.label ~text:"Number of Lanes: " ~packing:lanes_hbox#add () in
    let lanes_entry = GEdit.entry ~packing:lanes_hbox#add () in
    lanes_entry#set_text (string_of_int current_road_settings.num_lanes);
    
    (* Max Capacity *)
    let capacity_hbox = GPack.hbox ~packing:vbox_dialog#pack () in
    let _ = GMisc.label ~text:"Max Capacity: " ~packing:capacity_hbox#add () in
    let capacity_entry = GEdit.entry ~packing:capacity_hbox#add () in
    capacity_entry#set_text (string_of_int current_road_settings.max_capacity);
    
    (* Buttons *)
    dialog#add_button_stock `OK `OK;
    dialog#add_button_stock `CANCEL `CANCEL;
    
    ignore (dialog#connect#response ~callback:(fun resp ->
        if resp = `OK then (
          try
            let speed = int_of_string speed_entry#text in
            let lanes = int_of_string lanes_entry#text in
            let capacity = int_of_string capacity_entry#text in
            if lanes > 0 && capacity > 0 && speed > 0 then (
              Road.set_settings (RoadSettings {
                speed_limit = speed;
                num_lanes = lanes;
                max_capacity = capacity;
              });
              dialog#destroy ()
            ) else (
              let error_dialog = GWindow.message_dialog
                ~message:"Invalid values! All values must be positive integers."
                ~message_type:`ERROR
                ~buttons:GWindow.Buttons.ok
                () in
              ignore (error_dialog#connect#response ~callback:(fun _ -> error_dialog#destroy ()));
              error_dialog#show ()
            )
          with
          | Failure _ ->
              let error_dialog = GWindow.message_dialog
                ~message:"Invalid input! Please enter valid integers."
                ~message_type:`ERROR
                ~buttons:GWindow.Buttons.ok
                () in
              ignore (error_dialog#connect#response ~callback:(fun _ -> error_dialog#destroy ()));
              error_dialog#show ()
        ) else
          dialog#destroy ()));
    
    dialog#show ()
  in

  (* Settings editor for Intersection *)
  let edit_intersection_settings () =
    let dialog = GWindow.dialog ~title:"Intersection Settings" ~modal:true () in
    let vbox_dialog = dialog#vbox in
    
    let current_int_settings = match Intersection.get_settings () with
      | IntersectionSettings s -> s
      | _ -> { num_stops = 4; has_traffic_light = false; stop_duration = 3.0 }
    in
    
    (* Number of Stops (2 or 4) *)
    let stops_hbox = GPack.hbox ~packing:vbox_dialog#pack () in
    let _ = GMisc.label ~text:"Stop Type: " ~packing:stops_hbox#add () in
    let stops_combo = GEdit.combo ~popdown_strings:["2-way"; "4-way"] ~packing:stops_hbox#add () in
    stops_combo#entry#set_text (if current_int_settings.num_stops = 2 then "2-way" else "4-way");
    
    (* Traffic Light checkbox *)
    let traffic_light_check = GButton.check_button ~label:"Has Traffic Light" ~packing:vbox_dialog#pack () in
    traffic_light_check#set_active current_int_settings.has_traffic_light;
    
    (* Stop Duration *)
    let duration_hbox = GPack.hbox ~packing:vbox_dialog#pack () in
    let _ = GMisc.label ~text:"Stop Duration (seconds): " ~packing:duration_hbox#add () in
    let duration_entry = GEdit.entry ~packing:duration_hbox#add () in
    duration_entry#set_text (Printf.sprintf "%.1f" current_int_settings.stop_duration);
    
    (* Buttons *)
    dialog#add_button_stock `OK `OK;
    dialog#add_button_stock `CANCEL `CANCEL;
    
    ignore (dialog#connect#response ~callback:(fun resp ->
        if resp = `OK then (
          try
            let num_stops = if stops_combo#entry#text = "2-way" then 2 else 4 in
            let has_traffic_light = traffic_light_check#active in
            let stop_duration = float_of_string duration_entry#text in
            if stop_duration > 0.0 then (
              Intersection.set_settings (IntersectionSettings {
                num_stops = num_stops;
                has_traffic_light = has_traffic_light;
                stop_duration = stop_duration;
              });
              dialog#destroy ()
            ) else (
              let error_dialog = GWindow.message_dialog
                ~message:"Stop duration must be positive!"
                ~message_type:`ERROR
                ~buttons:GWindow.Buttons.ok
                () in
              ignore (error_dialog#connect#response ~callback:(fun _ -> error_dialog#destroy ()));
              error_dialog#show ()
            )
          with
          | Failure _ ->
              let error_dialog = GWindow.message_dialog
                ~message:"Invalid input! Please enter a valid number for stop duration."
                ~message_type:`ERROR
                ~buttons:GWindow.Buttons.ok
                () in
              ignore (error_dialog#connect#response ~callback:(fun _ -> error_dialog#destroy ()));
              error_dialog#show ()
        ) else
          dialog#destroy ()));
    
    dialog#show ()
  in

  (* Settings editor for Building *)
  let edit_building_settings () =
    let dialog = GWindow.dialog ~title:"Building Settings" ~modal:true () in
    let vbox_dialog = dialog#vbox in
    
    let current_building_settings = match Building.get_settings () with
      | BuildingSettings s -> s
      | _ -> { rate_of_traffic = 10 }
    in
    
    (* Rate of Traffic *)
    let rate_hbox = GPack.hbox ~packing:vbox_dialog#pack () in
    let _ = GMisc.label ~text:"Rate of Traffic: " ~packing:rate_hbox#add () in
    let rate_entry = GEdit.entry ~packing:rate_hbox#add () in
    rate_entry#set_text (string_of_int current_building_settings.rate_of_traffic);
    
    (* Buttons *)
    dialog#add_button_stock `OK `OK;
    dialog#add_button_stock `CANCEL `CANCEL;
    
    ignore (dialog#connect#response ~callback:(fun resp ->
        if resp = `OK then (
          try
            let rate = int_of_string rate_entry#text in
            if rate >= 0 then (
              Building.set_settings (BuildingSettings {
                rate_of_traffic = rate;
              });
              dialog#destroy ()
            ) else (
              let error_dialog = GWindow.message_dialog
                ~message:"Rate of traffic must be non-negative!"
                ~message_type:`ERROR
                ~buttons:GWindow.Buttons.ok
                () in
              ignore (error_dialog#connect#response ~callback:(fun _ -> error_dialog#destroy ()));
              error_dialog#show ()
            )
          with
          | Failure _ ->
              let error_dialog = GWindow.message_dialog
                ~message:"Invalid input! Please enter a valid integer."
                ~message_type:`ERROR
                ~buttons:GWindow.Buttons.ok
                () in
              ignore (error_dialog#connect#response ~callback:(fun _ -> error_dialog#destroy ()));
              error_dialog#show ()
        ) else
          dialog#destroy ()));
    
    dialog#show ()
  in

  (* Add tool buttons with settings *)
  let _ = add_tool_with_settings "Road" ROAD edit_road_settings in
  let _ = add_tool_with_settings "Intersection" INTERSECTION edit_intersection_settings in
  let _ = add_tool_with_settings "Building" BUILDING edit_building_settings in

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
                Road.draw cr ~x:obj.x ~y:obj.y ~angle:obj.angle obj.settings
            | INTERSECTION ->
                Intersection.draw cr ~x:obj.x ~y:obj.y ~angle:obj.angle
                  obj.settings
            | BUILDING ->
                Building.draw cr ~x:obj.x ~y:obj.y ~angle:obj.angle obj.settings)
          !objects;
        (* Draw selection highlights and rotate buttons *)
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
      | Some tool_type ->
          (* Drawing mode - place new object *)
          (match tool_type with
          | ROAD ->
              objects :=
                {
                  tool_type = ROAD;
                  x;
                  y;
                  angle = 0.0;
                  settings = Road.get_settings ();
                }
                :: !objects
          | INTERSECTION ->
              objects :=
                {
                  tool_type = INTERSECTION;
                  x;
                  y;
                  angle = 0.0;
                  settings = Intersection.get_settings ();
                }
                :: !objects
          | BUILDING ->
              objects :=
                {
                  tool_type = BUILDING;
                  x;
                  y;
                  angle = 0.0;
                  settings = Building.get_settings ();
                }
                :: !objects);
          (* Clear selection when drawing new objects *)
          selected_object := None;
          redraw_all ();
          true
      | None ->
          if
            (* Delete mode - delete clicked object *)
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
            (* Delete the clicked object if found *)
            (match !clicked_object with
            | Some idx ->
                objects := List.filteri (fun i _ -> i <> idx) !objects;
                redraw_all ()
            | None -> ());
            true (* Selection/move mode - check for clicks on objects *))
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
                             ~angle:obj.angle ~px:x_f ~py:y_f obj.settings
                         then (
                           clicked_object := Some idx;
                           clicked_rotate_button := true;
                           is_rotating := true)
                     | INTERSECTION ->
                         if
                           Intersection.point_on_rotate_button ~x:obj.x ~y:obj.y
                             ~angle:obj.angle ~px:x_f ~py:y_f obj.settings
                         then (
                           clicked_object := Some idx;
                           clicked_rotate_button := true;
                           is_rotating := true)
                     | BUILDING ->
                         if
                           Building.point_on_rotate_button ~x:obj.x ~y:obj.y
                             ~angle:obj.angle ~px:x_f ~py:y_f obj.settings
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
                            obj.settings
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
                            ~py:y_f obj.settings
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
                            ~py:y_f obj.settings
                        then (
                          clicked_object := Some idx;
                          clicked_rotate_button := false;
                          is_rotating := false;
                          let fx = float_of_int obj.x in
                          let fy = float_of_int obj.y in
                          drag_offset := Some (x_f -. fx, y_f -. fy))))
              !objects;

            (* Update selection *)
            if !clicked_object <> None then selected_object := !clicked_object
            else (
              (* Clicked on empty space - deselect *)
              selected_object := None;
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
        let obj = List.nth !objects selected_idx in
        let objects_list = !objects in

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
           objects :=
             List.mapi
               (fun i o ->
                 if i = selected_idx then { o with angle = new_angle } else o)
               objects_list
         else
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
      with _ -> false
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
    [ `BUTTON_PRESS; `BUTTON_RELEASE; `POINTER_MOTION; `BUTTON1_MOTION ];

  (* Connect window destroy signal to quit the application *)
  let _ = window#connect#destroy ~callback:GMain.quit in

  (* Show all widgets *)
  window#show ();

  (* Start the GTK main loop *)
  GMain.main ()
