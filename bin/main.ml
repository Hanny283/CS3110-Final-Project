(* main.ml *)

let () =
  (* Initialize GTK *)
  let _ = GMain.init () in

  (* Create the main window *)
  let window = GWindow.window 
    ~title:"CS3110 Final Project - Drawing Application"
    ~resizable:true () in
  window#set_default_size ~width:800 ~height:600;

  (* Create a vertical box to organize widgets *)
  let vbox = GPack.vbox ~packing:window#add () in

  (* Create a toolbar/menu area *)
  let toolbar = GPack.hbox ~packing:(vbox#pack ~expand:false) () in
  let _label = GMisc.label ~text:"Drawing Tools" ~packing:toolbar#add () in

  (* Create the drawing area *)
  let drawing_area = GMisc.drawing_area 
    ~packing:(vbox#pack ~expand:true ~fill:true) () in
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

  (* Mouse button press handler - start drawing *)
  let button_press_callback ev =
    let button = GdkEvent.Button.button ev in
    if button = 1 then begin
      match !surface with
      | Some s ->
          let cr = Cairo.create s in
          let x = GdkEvent.Button.x ev in
          let y = GdkEvent.Button.y ev in
          (* Draw a small circle where clicked *)
          Cairo.set_source_rgb cr 0.0 0.0 0.0;
          Cairo.arc cr x y ~r:5.0 ~a1:0.0 ~a2:(2.0 *. Float.pi);
          Cairo.fill cr;
          (* Queue a redraw *)
          drawing_area#misc#queue_draw ();
          true
      | None -> false
    end else
      false
  in

  (* Mouse motion handler - draw while dragging *)
  let motion_callback ev =
    let state = GdkEvent.Motion.state ev in
    if Gdk.Convert.test_modifier `BUTTON1 state then begin
      match !surface with
      | Some s ->
          let cr = Cairo.create s in
          let x = GdkEvent.Motion.x ev in
          let y = GdkEvent.Motion.y ev in
          (* Draw a small circle while dragging *)
          Cairo.set_source_rgb cr 0.0 0.0 0.0;
          Cairo.arc cr x y ~r:3.0 ~a1:0.0 ~a2:(2.0 *. Float.pi);
          Cairo.fill cr;
          (* Queue a redraw *)
          drawing_area#misc#queue_draw ();
          true
      | None -> false
    end else
      false
  in

  (* Connect signals *)
  let _ = drawing_area#misc#connect#draw ~callback:draw_callback in
  let _ = drawing_area#event#connect#configure ~callback:configure_callback in
  let _ = drawing_area#event#connect#button_press ~callback:button_press_callback in
  let _ = drawing_area#event#connect#motion_notify ~callback:motion_callback in

  (* Set event mask to receive mouse events *)
  drawing_area#event#add [
    `BUTTON_PRESS;
    `POINTER_MOTION;
    `BUTTON1_MOTION;
  ];

  (* Connect window destroy signal to quit the application *)
  let _ = window#connect#destroy ~callback:GMain.quit in

  (* Show all widgets *)
  window#show ();

  (* Start the GTK main loop *)
  GMain.main ()
