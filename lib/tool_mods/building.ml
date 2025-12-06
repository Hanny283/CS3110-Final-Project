open Tool_types
open Settings

module Building : TOOL = struct
  type t = tool

  (* Default building settings *)
  let current_settings = ref { rate_of_traffic = 10 }
  let building_width = 50.0
  let building_height = 40.0

  (* Get / set settings *)
  let get_settings () = BuildingSettings !current_settings

  let set_settings = function
    | BuildingSettings s -> current_settings := s
    | _ -> failwith "Building.set_settings: expected BuildingSettings"

  let get_tool () = BUILDING
  let get_name () = "Building"

  (* Draw a simple building rectangle *)
  let draw ~cr ~x ~y ~angle settings =
    match settings with
    | BuildingSettings _s ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let w = building_width in
        let h = building_height in
        let half_w = w /. 2.0 in
        let half_h = h /. 2.0 in

        Cairo.save cr;
        Cairo.translate cr fx fy;
        Cairo.rotate cr angle;
        Cairo.translate cr (-.fx) (-.fy);

        (* body *)
        Cairo.set_source_rgb cr 0.8 0.8 0.85;
        Cairo.rectangle cr (fx -. half_w) (fy -. half_h) ~w ~h;
        Cairo.fill cr;

        (* outline *)
        Cairo.set_source_rgb cr 0.2 0.2 0.2;
        Cairo.set_line_width cr 2.0;
        Cairo.rectangle cr (fx -. half_w) (fy -. half_h) ~w ~h;
        Cairo.stroke cr;

        Cairo.restore cr
    | _ -> failwith "Building.draw: expected BuildingSettings"

  (* Selection highlight *)
  let draw_selection ~cr ~x ~y ~angle settings =
    match settings with
    | BuildingSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let w = building_width in
        let h = building_height in
        let half_w = w /. 2.0 in
        let half_h = h /. 2.0 in
        let pad = 5.0 in

        Cairo.save cr;
        Cairo.translate cr fx fy;
        Cairo.rotate cr angle;
        Cairo.translate cr (-.fx) (-.fy);

        Cairo.set_source_rgba cr 1.0 1.0 0.0 0.7;
        Cairo.set_line_width cr 3.0;
        Cairo.rectangle cr
          (fx -. half_w -. pad)
          (fy -. half_h -. pad)
          ~w:(w +. (2.0 *. pad))
          ~h:(h +. (2.0 *. pad));
        Cairo.stroke cr;

        Cairo.restore cr
    | _ -> failwith "Building.draw_selection: expected BuildingSettings"

  (* Erase by overdrawing white box *)
  let erase ~cr ~x ~y settings =
    match settings with
    | BuildingSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let w = building_width +. 20.0 in
        let h = building_height +. 20.0 in
        let half_w = w /. 2.0 in
        let half_h = h /. 2.0 in

        Cairo.set_source_rgb cr 1.0 1.0 1.0;
        Cairo.rectangle cr (fx -. half_w) (fy -. half_h) ~w ~h;
        Cairo.fill cr
    | _ -> failwith "Building.erase: expected BuildingSettings"

  (* Hitbox: axis-aligned in screen space (no rotation compensation for now) *)
  let point_inside ~x ~y ~px ~py settings =
    match settings with
    | BuildingSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let half_w = building_width /. 2.0 in
        let half_h = building_height /. 2.0 in
        px >= fx -. half_w
        && px <= fx +. half_w
        && py >= fy -. half_h
        && py <= fy +. half_h
    | _ -> false

  (* Rotate button on the right side *)
  let draw_rotate_button ~cr ~x ~y ~angle settings =
    match settings with
    | BuildingSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let r = 12.0 in
        let dist = (building_width /. 2.0) +. 20.0 in
        let bx = fx +. (dist *. cos angle) in
        let by = fy +. (dist *. sin angle) in

        Cairo.save cr;

        (* outer circle *)
        Cairo.set_source_rgb cr 0.5 0.2 0.8;
        Cairo.arc cr bx by ~r ~a1:0.0 ~a2:(2.0 *. Float.pi);
        Cairo.fill cr;

        (* inner circle *)
        Cairo.set_source_rgb cr 1.0 1.0 1.0;
        Cairo.arc cr bx by ~r:(r -. 2.0) ~a1:0.0 ~a2:(2.0 *. Float.pi);
        Cairo.fill cr;

        (* circular arrow *)
        Cairo.set_source_rgb cr 0.5 0.2 0.8;
        Cairo.set_line_width cr 2.0;
        Cairo.arc cr bx by ~r:(r -. 4.0) ~a1:(-0.5 *. Float.pi)
          ~a2:(1.5 *. Float.pi);
        Cairo.stroke cr;

        let ang = 1.5 *. Float.pi in
        let tipx = bx +. ((r -. 4.0) *. cos ang) in
        let tipy = by +. ((r -. 4.0) *. sin ang) in
        Cairo.move_to cr tipx tipy;
        Cairo.line_to cr
          (tipx -. (4.0 *. cos (ang -. 0.5)))
          (tipy -. (4.0 *. sin (ang -. 0.5)));
        Cairo.stroke cr;
        Cairo.move_to cr tipx tipy;
        Cairo.line_to cr
          (tipx -. (4.0 *. cos (ang +. 0.5)))
          (tipy -. (4.0 *. sin (ang +. 0.5)));
        Cairo.stroke cr;

        Cairo.restore cr
    | _ -> failwith "Building.draw_rotate_button: expected BuildingSettings"

  let point_on_rotate_button ~x ~y ~angle ~px ~py settings =
    match settings with
    | BuildingSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let dist = (building_width /. 2.0) +. 20.0 in
        let r = 12.0 in
        let bx = fx +. (dist *. cos angle) in
        let by = fy +. (dist *. sin angle) in
        let dx = px -. bx in
        let dy = py -. by in
        sqrt ((dx *. dx) +. (dy *. dy)) <= r
    | _ -> false

  let calculate_rotation ~cx ~cy ~mx ~my = atan2 (my -. cy) (mx -. cx)
end
