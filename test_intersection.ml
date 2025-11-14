(* Test script to visualize an intersection with default settings *)

open Cs3110_final_project

let () =
  (* Initialize GTK *)
  ignore (GMain.init ());
  
  (* Create main window *)
  let window = GWindow.window 
    ~title:"Intersection Test" 
    ~width:400 
    ~height:400 
    ~position:`CENTER () in
  
  (* Handle window close event *)
  ignore (window#connect#destroy ~callback:GMain.quit);
  
  (* Create a drawing area *)
  let drawing_area = GMisc.drawing_area 
    ~width:400 
    ~height:400 
    ~packing:window#add () in
  
  (* Connect the draw signal to render the intersection *)
  ignore (drawing_area#misc#connect#draw ~callback:(fun cr ->
    (* Clear background to white *)
    Cairo.set_source_rgb cr 1.0 1.0 1.0;
    Cairo.paint cr;
    
    (* Get default settings from Intersection module *)
    let settings = Intersection.Intersection.get_settings () in
    
    (* Draw intersection at center of window *)
    let center_x = 200 in
    let center_y = 200 in
    
    Intersection.Intersection.draw cr ~x:center_x ~y:center_y settings;
    
    (* Add a label *)
    Cairo.set_source_rgb cr 0.0 0.0 0.0;
    Cairo.select_font_face cr "Sans" Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_BOLD;
    Cairo.set_font_size cr 16.0;
    Cairo.move_to cr 10.0 30.0;
    Cairo.show_text cr "Intersection with Default Settings";
    
    (* Display settings info *)
    Cairo.set_font_size cr 12.0;
    Cairo.move_to cr 10.0 380.0;
    let settings_text = match settings with
      | Tool_types.IntersectionSettings s ->
          Printf.sprintf "Stops: %d | Traffic Light: %b" 
            s.num_stops s.has_traffic_light
      | _ -> "Unknown settings"
    in
    Cairo.show_text cr settings_text;
    
    true
  ));
  
  (* Show window and start main loop *)
  window#show ();
  
  Printf.printf "Displaying intersection with default settings...\n";
  Printf.printf "Close the window to exit.\n";
  flush stdout;
  
  GMain.main ()

