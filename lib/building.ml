(* Building module - implements TOOL signature *)
open Tool_types

module Building : TOOL = struct
  type t = tool

  (* Default building settings *)
  let current_settings =
    ref
      { rate_of_traffic = 10; building = HOUSE { time_in = 8; time_out = 18 } }

  (* Size of the building *)
  let building_width = 50.0
  let building_height = 60.0

  (* Helper to choose colors based on building type *)
  let building_colors b =
    match b with
    | HOUSE _ ->
        (* warm, residential-looking palette *)
        ( (0.9, 0.9, 0.9),
          (* body *)
          (0.6, 0.2, 0.2) )
        (* accent/roof *)
    | OFFICE _ ->
        (* cooler, glass/steel palette *)
        ( (0.8, 0.8, 0.9),
          (* body *)
          (0.2, 0.2, 0.5) )
        (* accent *)
    | SCHOOL _ ->
        (* bright, friendly palette *)
        ( (0.95, 0.95, 0.8),
          (* body *)
          (0.3, 0.5, 0.2) )
        (* accent *)
    | HOSPITAL ->
        (* clinical, clean palette *)
        ( (0.95, 0.95, 0.95),
          (* body *)
          (0.7, 0.0, 0.0) )
  (* accent / cross *)

  (* Draw simple windows for non-hospital buildings *)
  let draw_windows cr ~x ~y ~w ~h =
    let cols = 2 in
    let rows = 3 in
    let margin_x = 6.0 in
    let margin_y = 8.0 in
    let cell_w = (w -. (2.0 *. margin_x)) /. float_of_int cols in
    let cell_h = (h -. (2.0 *. margin_y)) /. float_of_int rows in

    Cairo.set_source_rgb cr 0.8 0.9 1.0;
    (* light blue windows *)
    for i = 0 to cols - 1 do
      for j = 0 to rows - 1 do
        let wx = x -. (w /. 2.0) +. margin_x +. (float_of_int i *. cell_w) in
        let wy = y -. (h /. 2.0) +. margin_y +. (float_of_int j *. cell_h) in
        Cairo.rectangle cr wx wy ~w:(cell_w -. 3.0) ~h:(cell_h -. 3.0);
        Cairo.fill cr
      done
    done

  (* Draw a red cross for hospital buildings *)
  let draw_hospital_cross cr ~x ~y ~w ~h =
    let cross_w = w /. 3.0 in
    let cross_h = h /. 3.0 in
    let thickness = cross_w /. 3.0 in

    Cairo.set_source_rgb cr 0.8 0.0 0.0;
    (* vertical bar *)
    Cairo.rectangle cr
      (x -. (thickness /. 2.0))
      (y -. (cross_h /. 2.0))
      ~w:thickness ~h:cross_h;
    Cairo.fill cr;

    (* horizontal bar *)
    Cairo.rectangle cr
      (x -. (cross_w /. 2.0))
      (y -. (thickness /. 2.0))
      ~w:cross_w ~h:thickness;
    Cairo.fill cr

  (* Draw the building on the Cairo context with rotation *)
  let draw cr ~x ~y ~angle settings =
    match settings with
    | BuildingSettings s ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let bw = building_width in
        let bh = building_height in

        let (body_r, body_g, body_b), (accent_r, accent_g, accent_b) =
          building_colors s.building
        in

        Cairo.save cr;

        (* Translate to center, rotate, then translate back *)
        Cairo.translate cr fx fy;
        Cairo.rotate cr angle;
        Cairo.translate cr (-.fx) (-.fy);

        (* Draw main building rectangle *)
        Cairo.set_source_rgb cr body_r body_g body_b;
        Cairo.rectangle cr (fx -. (bw /. 2.0)) (fy -. (bh /. 2.0)) ~w:bw ~h:bh;
        Cairo.fill cr;

        (* Draw roof/accent as a top strip *)
        Cairo.set_source_rgb cr accent_r accent_g accent_b;
        let roof_h = 10.0 in
        Cairo.rectangle cr
          (fx -. (bw /. 2.0))
          (fy -. (bh /. 2.0))
          ~w:bw ~h:roof_h;
        Cairo.fill cr;

        (* Draw details based on building type *)
        (match s.building with
        | HOSPITAL -> draw_hospital_cross cr ~x:fx ~y:fy ~w:bw ~h:bh
        | _ -> draw_windows cr ~x:fx ~y:fy ~w:bw ~h:(bh -. roof_h));

        (* Draw outline around building *)
        Cairo.set_source_rgb cr 0.0 0.0 0.0;
        Cairo.set_line_width cr 2.0;
        Cairo.rectangle cr (fx -. (bw /. 2.0)) (fy -. (bh /. 2.0)) ~w:bw ~h:bh;
        Cairo.stroke cr;

        Cairo.restore cr
    | _ -> failwith "Building.draw: expected BuildingSettings"

  (* Erase a building from the Cairo context *)
  let erase cr ~x ~y settings =
    match settings with
    | BuildingSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let padding = 10.0 in
        let bw = building_width +. (padding *. 2.0) in
        let bh = building_height +. (padding *. 2.0) in

        Cairo.set_source_rgb cr 1.0 1.0 1.0;
        Cairo.rectangle cr (fx -. (bw /. 2.0)) (fy -. (bh /. 2.0)) ~w:bw ~h:bh;
        Cairo.fill cr
    | _ -> failwith "Building.erase: expected BuildingSettings"

  (* Get current settings *)
  let get_settings () = BuildingSettings !current_settings

  (* Update settings *)
  let set_settings new_settings =
    match new_settings with
    | BuildingSettings s -> current_settings := s
    | _ -> failwith "Building.set_settings: expected BuildingSettings"

  (* Get the tool type *)
  let get_tool () =
    let s = !current_settings in
    BUILDING s.building

  (* Draw selection highlight (yellow border) around the building *)
  let draw_selection cr ~x ~y ~angle settings =
    match settings with
    | BuildingSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let bw = building_width in
        let bh = building_height in
        let padding = 5.0 in

        Cairo.save cr;
        Cairo.translate cr fx fy;
        Cairo.rotate cr angle;
        Cairo.translate cr (-.fx) (-.fy);

        Cairo.set_source_rgba cr 1.0 1.0 0.0 0.7;
        (* Yellow with transparency *)
        Cairo.set_line_width cr 3.0;
        Cairo.set_line_cap cr Cairo.ROUND;
        Cairo.set_line_join cr Cairo.JOIN_ROUND;

        Cairo.rectangle cr
          (fx -. (bw /. 2.0) -. padding)
          (fy -. (bh /. 2.0) -. padding)
          ~w:(bw +. (padding *. 2.0))
          ~h:(bh +. (padding *. 2.0));
        Cairo.stroke cr;

        Cairo.restore cr
    | _ -> failwith "Building.draw_selection: expected BuildingSettings"

  (* Draw rotate button (circular arrow icon) on the right side *)
  let draw_rotate_button cr ~x ~y ~angle settings =
    match settings with
    | BuildingSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let bw = building_width in
        let button_radius = 12.0 in
        let button_distance = (bw /. 2.0) +. 20.0 in

        let button_x = fx +. (button_distance *. cos angle) in
        let button_y = fy +. (button_distance *. sin angle) in

        Cairo.save cr;

        (* Outer circle (purple background) *)
        Cairo.set_source_rgb cr 0.5 0.2 0.8;
        Cairo.arc cr button_x button_y ~r:button_radius ~a1:0.0
          ~a2:(2.0 *. 3.14159);
        Cairo.fill cr;

        (* Inner circle (white) *)
        Cairo.set_source_rgb cr 1.0 1.0 1.0;
        Cairo.arc cr button_x button_y ~r:(button_radius -. 2.0) ~a1:0.0
          ~a2:(2.0 *. 3.14159);
        Cairo.fill cr;

        (* Circular arrow icon *)
        Cairo.set_source_rgb cr 0.5 0.2 0.8;
        Cairo.set_line_width cr 2.0;
        Cairo.set_line_cap cr Cairo.ROUND;

        Cairo.arc cr button_x button_y ~r:(button_radius -. 4.0)
          ~a1:(-0.5 *. 3.14159) ~a2:(1.5 *. 3.14159);
        Cairo.stroke cr;

        let arrow_angle = 1.5 *. 3.14159 in
        let arrow_tip_x =
          button_x +. ((button_radius -. 4.0) *. cos arrow_angle)
        in
        let arrow_tip_y =
          button_y +. ((button_radius -. 4.0) *. sin arrow_angle)
        in

        Cairo.set_line_width cr 2.5;
        Cairo.move_to cr arrow_tip_x arrow_tip_y;
        Cairo.line_to cr
          (arrow_tip_x -. (4.0 *. cos (arrow_angle -. 0.5)))
          (arrow_tip_y -. (4.0 *. sin (arrow_angle -. 0.5)));
        Cairo.stroke cr;

        Cairo.move_to cr arrow_tip_x arrow_tip_y;
        Cairo.line_to cr
          (arrow_tip_x -. (4.0 *. cos (arrow_angle +. 0.5)))
          (arrow_tip_y -. (4.0 *. sin (arrow_angle +. 0.5)));
        Cairo.stroke cr;

        Cairo.restore cr
    | _ -> failwith "Building.draw_rotate_button: expected BuildingSettings"

  (* Check if a point is inside the building bounds *)
  let point_inside ~x ~y ~px ~py settings =
    match settings with
    | BuildingSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let bw = building_width in
        let bh = building_height in
        let half_w = bw /. 2.0 in
        let half_h = bh /. 2.0 in
        px >= fx -. half_w
        && px <= fx +. half_w
        && py >= fy -. half_h
        && py <= fy +. half_h
    | _ -> false

  (* Check if a point is on the rotate button *)
  let point_on_rotate_button ~x ~y ~angle ~px ~py settings =
    match settings with
    | BuildingSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let bw = building_width in
        let button_radius = 12.0 in
        let button_distance = (bw /. 2.0) +. 20.0 in

        let button_x = fx +. (button_distance *. cos angle) in
        let button_y = fy +. (button_distance *. sin angle) in

        let dx = px -. button_x in
        let dy = py -. button_y in
        let dist = sqrt ((dx *. dx) +. (dy *. dy)) in
        dist <= button_radius
    | _ -> false

  (* Calculate rotation angle from center based on mouse position *)
  let calculate_rotation ~cx ~cy ~mx ~my =
    let dx = mx -. cx in
    let dy = my -. cy in
    atan2 dy dx

  (* Get the tool name for display *)
  let get_name () = "Building"
end
