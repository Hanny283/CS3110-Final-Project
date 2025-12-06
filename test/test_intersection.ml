(* Interactive test script with selection, drag, and rotate functionality *)

open Cs3110_final_project

(* State for managing intersections *)
type intersection_state = {
  x : int ref;
  y : int ref;
  angle : float ref;
  settings : Settings.settings;
}

type app_state = {
  intersections : intersection_state list ref;
  selected : int option ref; (* Index of selected intersection *)
  drag_start : (float * float) option ref;
      (* Mouse position when drag started *)
  drag_offset : (float * float) option ref;
      (* Offset from intersection center when dragging *)
  rotating : bool ref; (* Whether we're currently rotating *)
  button_pressed : bool ref; (* Whether mouse button is pressed *)
}

let () =
  (* Initialize GTK *)
  ignore (GMain.init ());

  (* Create main window *)
  let window =
    GWindow.window
      ~title:
        "Interactive Intersection Test - Click to select, Drag to move, Use \
         rotate button to rotate"
      ~width:600 ~height:500 ~position:`CENTER ()
  in

  (* Handle window close event *)
  ignore (window#connect#destroy ~callback:GMain.quit);

  (* Create a drawing area *)
  let drawing_area = GMisc.drawing_area ~packing:window#add () in
  drawing_area#misc#set_size_request ~width:600 ~height:500 ();

  (* Initialize application state *)
  let state =
    {
      intersections =
        ref
          [
            {
              x = ref 300;
              y = ref 250;
              angle = ref 0.0;
              settings = Intersection.Intersection.get_settings ();
            };
          ];
      selected = ref (Some 0);
      drag_start = ref None;
      drag_offset = ref None;
      rotating = ref false;
      button_pressed = ref false;
    }
  in

  (* Redraw function *)
  let redraw () = drawing_area#misc#queue_draw () in

  (* Connect the draw signal to render all intersections *)
  ignore
    (drawing_area#misc#connect#draw ~callback:(fun cr ->
         (* Clear background to white *)
         Cairo.set_source_rgb cr 1.0 1.0 1.0;
         Cairo.paint cr;

         (* Draw all intersections *)
         List.iteri
           (fun idx intersection ->
             (* Draw the intersection *)
             Intersection.Intersection.draw ~cr ~x:!(intersection.x)
               ~y:!(intersection.y) ~angle:!(intersection.angle)
               intersection.settings;

             (* If selected, draw selection highlight and rotate button *)
             if !(state.selected) = Some idx then begin
               Intersection.Intersection.draw_selection ~cr ~x:!(intersection.x)
                 ~y:!(intersection.y) ~angle:!(intersection.angle)
                 intersection.settings;

               Intersection.Intersection.draw_rotate_button ~cr
                 ~x:!(intersection.x) ~y:!(intersection.y)
                 ~angle:!(intersection.angle) intersection.settings
             end)
           !(state.intersections);

         (* Draw instructions *)
         Cairo.set_source_rgb cr 0.0 0.0 0.0;
         Cairo.select_font_face cr "Sans" ~slant:Cairo.Upright
           ~weight:Cairo.Normal;
         Cairo.set_font_size cr 11.0;
         Cairo.move_to cr 10.0 15.0;
         Cairo.show_text cr
           "Click intersection to select | Drag to move | Use rotate button to \
            rotate";

         true));

  (* Handle mouse button press *)
  ignore
    (drawing_area#event#connect#button_press ~callback:(fun ev ->
         let x = GdkEvent.Button.x ev in
         let y = GdkEvent.Button.y ev in
         let button = GdkEvent.Button.button ev in

         if button = 1 then begin
           (* Left mouse button *)
           state.button_pressed := true;

           (* Check each intersection (from front to back) to see if we clicked on it *)
           let clicked_intersection = ref None in
           let clicked_rotate_button = ref false in

           List.iteri
             (fun idx intersection ->
               if !clicked_intersection = None then begin
                 let settings = intersection.settings in

                 (* Check if clicked on rotate button *)
                 if !(state.selected) = Some idx then begin
                   if
                     Intersection.Intersection.point_on_rotate_button
                       ~x:!(intersection.x) ~y:!(intersection.y)
                       ~angle:!(intersection.angle) ~px:x ~py:y settings
                   then begin
                     clicked_intersection := Some idx;
                     clicked_rotate_button := true;
                     state.rotating := true
                   end
                 end;

                 (* Check if clicked on intersection itself *)
                 if !clicked_intersection = None then begin
                   if
                     Intersection.Intersection.point_inside ~x:!(intersection.x)
                       ~y:!(intersection.y) ~px:x ~py:y settings
                   then begin
                     clicked_intersection := Some idx;
                     clicked_rotate_button := false;
                     state.rotating := false;

                     (* Calculate offset from intersection center for smooth dragging *)
                     let fx = float_of_int !(intersection.x) in
                     let fy = float_of_int !(intersection.y) in
                     state.drag_offset := Some (x -. fx, y -. fy)
                   end
                 end
               end)
             !(state.intersections);

           (* Update selection *)
           if !clicked_intersection <> None then begin
             state.selected := !clicked_intersection;
             state.drag_start := Some (x, y)
           end
           else begin
             (* Clicked on empty space - deselect *)
             state.selected := None;
             state.drag_start := None;
             state.rotating := false
           end;

           redraw ();
           true
         end
         else false));

  (* Handle mouse button release *)
  ignore
    (drawing_area#event#connect#button_release ~callback:(fun _ev ->
         state.button_pressed := false;
         state.drag_start := None;
         state.drag_offset := None;
         state.rotating := false;
         false));

  (* Handle mouse motion (for dragging and rotating) *)
  ignore
    (drawing_area#event#connect#motion_notify ~callback:(fun ev ->
         if !(state.button_pressed) && !(state.selected) <> None then begin
           let x = GdkEvent.Motion.x ev in
           let y = GdkEvent.Motion.y ev in

           let selected_idx =
             match !(state.selected) with
             | Some idx -> idx
             | None -> assert false
           in

           let intersection = List.nth !(state.intersections) selected_idx in

           if !(state.rotating) then begin
             (* Rotating: calculate angle from center to mouse position *)
             let cx = float_of_int !(intersection.x) in
             let cy = float_of_int !(intersection.y) in
             let new_angle =
               Intersection.Intersection.calculate_rotation ~cx ~cy ~mx:x ~my:y
             in
             intersection.angle := new_angle
           end
           else begin
             (* Dragging: move intersection by mouse offset *)
             match !(state.drag_offset) with
             | Some (offset_x, offset_y) ->
                 intersection.x := int_of_float (x -. offset_x);
                 intersection.y := int_of_float (y -. offset_y)
             | None -> ()
           end;

           redraw ();
           true
         end
         else false));

  (* Enable motion events *)
  drawing_area#event#add
    [ `BUTTON_PRESS; `BUTTON_RELEASE; `POINTER_MOTION; `POINTER_MOTION_HINT ];

  (* Show window and start main loop *)
  window#show ();

  Printf.printf "Interactive intersection test:\n";
  Printf.printf "- Click an intersection to select it (yellow highlight)\n";
  Printf.printf "- Drag the intersection to move it\n";
  Printf.printf "- Click and drag the purple rotate button to rotate\n";
  Printf.printf "- Close the window to exit.\n";
  flush stdout;

  GMain.main ()
