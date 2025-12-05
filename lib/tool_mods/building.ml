open Tool_types
open Settings

module Building : TOOL = struct
  type t = tool

  (*Default building settings aka rate of traffic *)
  let current_settings = ref { rate_of_traffic = 10 }

  (* width of the building *)
  let building_width = 50.0

  (* Height of the building *)
  let building_height = 60.0

  (* Building objects for future simulations *)
  class building_object ~(x : int) ~(y : int) ~(angle : float)
    ~(settings : building_settings) =
    object
      val x = x
      val y = y
      val angle = angle
      val settings = settings
      method x = x
      method y = y
      method angle = angle
      method rate_of_traffic = settings.rate_of_traffic
      method settings = settings
    end

  (* This sets the color of the building*)
  let body_color = (0.8, 0.8, 0.8)
  let accent_color = (0.6, 0.6, 0.6)

  (**This draws the windows on the buildings*)
  let draw_windows cr ~x ~y ~w ~h =
    let cols = 2 in
    let rows = 3 in
    let margin_x = 6.0 in
    let margin_y = 8.0 in
    let cell_w = (w -. (2.0 *. margin_x)) /. float_of_int cols in
    let cell_h = (h -. (2.0 *. margin_y)) /. float_of_int rows in

    Cairo.set_source_rgb cr 0.8 0.9 1.0;
    for i = 0 to cols - 1 do
      for j = 0 to rows - 1 do
        let wx = x -. (w /. 2.0) +. margin_x +. (float_of_int i *. cell_w) in
        let wy = y -. (h /. 2.0) +. margin_y +. (float_of_int j *. cell_h) in
        Cairo.rectangle cr wx wy ~w:(cell_w -. 3.0) ~h:(cell_h -. 3.0);
        Cairo.fill cr
      done
    done

  (** This handles the logic for drawing a building when placing, moving, or
      rotating it(when its not in a group).*)
  let draw cr ~x ~y ~angle settings =
    match settings with
    | BuildingSettings s ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let bw = building_width in
        let bh = building_height in

        Cairo.save cr;

        Cairo.translate cr fx fy;
        Cairo.rotate cr angle;
        Cairo.translate cr (-.fx) (-.fy);

        let body_r, body_g, body_b = body_color in
        Cairo.set_source_rgb cr body_r body_g body_b;
        Cairo.rectangle cr (fx -. (bw /. 2.0)) (fy -. (bh /. 2.0)) ~w:bw ~h:bh;
        Cairo.fill cr;

        let accent_r, accent_g, accent_b = accent_color in
        Cairo.set_source_rgb cr accent_r accent_g accent_b;
        let roof_h = 10.0 in
        Cairo.rectangle cr
          (fx -. (bw /. 2.0))
          (fy -. (bh /. 2.0))
          ~w:bw ~h:roof_h;
        Cairo.fill cr;

        draw_windows cr ~x:fx ~y:fy ~w:bw ~h:(bh -. roof_h);

        Cairo.set_source_rgb cr 0.0 0.0 0.0;
        Cairo.set_line_width cr 2.0;
        Cairo.rectangle cr (fx -. (bw /. 2.0)) (fy -. (bh /. 2.0)) ~w:bw ~h:bh;
        Cairo.stroke cr;

        let (_ : building_object) =
          new building_object ~x ~y ~angle ~settings:s
        in

        Cairo.restore cr
    | _ -> failwith "Building.draw: expected BuildingSettings"

  (**Erase a building from the Cairo context although its never used it still
     had to be defined for the sig match if you attempt to not define it you get
     an error (i tried) *)
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

  (*current settings of a building*)
  let get_settings () = BuildingSettings !current_settings

  (** Manages the updates of the building *)
  let set_settings new_settings =
    match new_settings with
    | BuildingSettings s -> current_settings := s
    | _ -> failwith "Building.set_settings: expected BuildingSettings"

  (* Get the tool type *)
  let get_tool () = BUILDING

  (**Highlights the building when selected to move or rotate(when its not in a
     group).*)
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

  (**This draws the rotate button which you will see if you select move. its the
     purple arrow*)
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

  (**Makes sure that we are trying to building within the legal space*)
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

  (*Check if a point is on the rotate button *)
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
