(* Road module - implements TOOL signature *)
open Tool_types
open Settings

module Road : TOOL = struct
  type t = tool

  class road_object ~(x : int) ~(y : int) ~(angle : float)
    ~(settings : road_settings) =
    object
      val x = x
      val y = y
      val angle = angle
      val settings = settings
      val mutable occupancy = 0
      method x = x
      method y = y
      method angle = angle
      method settings = settings
      method speed_limit = settings.speed_limit
      method num_lanes = settings.num_lanes
      method max_capacity = settings.max_capacity
      method occupancy = occupancy
      method reset_occupancy = occupancy <- 0

      method update_occupancy delta =
        let next = occupancy + delta in
        let bounded = max 0 (min settings.max_capacity next) in
        occupancy <- bounded
    end

  (* Default road settings *)
  let current_settings =
    ref { speed_limit = 35; num_lanes = 2; max_capacity = 100 }

  (* Width per lane instead of constant width *)
  let width_per_lane = 12.0
  let road_length = 120.0

  (* Draw a road *)
  let draw cr ~x ~y ~angle settings =
    match settings with
    | RoadSettings s ->
        let lanes = max 1 s.num_lanes in
        let road_width = width_per_lane *. float lanes in
        let half_len = road_length /. 2.0 in
        let half_wid = road_width /. 2.0 in

        let fx = float_of_int x in
        let fy = float_of_int y in

        Cairo.save cr;
        Cairo.translate cr fx fy;
        Cairo.rotate cr angle;
        Cairo.translate cr (-.fx) (-.fy);

        (* asphalt *)
        Cairo.set_source_rgb cr 0.22 0.22 0.22;
        Cairo.rectangle cr (fx -. half_len) (fy -. half_wid) ~w:road_length
          ~h:road_width;
        Cairo.fill cr;

        (* center line *)
        Cairo.set_source_rgb cr 1.0 0.9 0.2;
        Cairo.set_line_width cr 2.4;
        Cairo.set_dash cr [| 12.0; 10.0 |] ~ofs:0.0;
        Cairo.move_to cr (fx -. half_len +. 4.0) fy;
        Cairo.line_to cr (fx +. half_len -. 4.0) fy;
        Cairo.stroke cr;
        Cairo.set_dash cr [||] ~ofs:0.0;

        (* lane dividers *)
        Cairo.set_source_rgb cr 1.0 1.0 1.0;
        Cairo.set_line_width cr 1.8;

        if lanes > 1 then
          for i = 1 to lanes - 1 do
            let y_offset = (float i *. width_per_lane) -. half_wid in
            let y_line = fy +. y_offset in
            if Float.abs (y_line -. fy) > 1.0 then (
              Cairo.set_dash cr [| 10.0; 10.0 |] ~ofs:0.0;
              Cairo.move_to cr (fx -. half_len +. 4.0) y_line;
              Cairo.line_to cr (fx +. half_len -. 4.0) y_line;
              Cairo.stroke cr;
              Cairo.set_dash cr [||] ~ofs:0.0)
          done;

        (* outline *)
        Cairo.set_source_rgb cr 0.0 0.0 0.0;
        Cairo.set_line_width cr 2.0;
        Cairo.rectangle cr (fx -. half_len) (fy -. half_wid) ~w:road_length
          ~h:road_width;
        Cairo.stroke cr;

        Cairo.restore cr;

        let (_ : road_object) = new road_object ~x ~y ~angle ~settings:s in
        ()
    | _ -> failwith "Road.draw: expected RoadSettings"

  (* Erase the road *)
  let erase cr ~x ~y settings =
    match settings with
    | RoadSettings s ->
        let lanes = max 1 s.num_lanes in
        let road_width = width_per_lane *. float lanes in

        let fx = float_of_int x in
        let fy = float_of_int y in

        Cairo.set_source_rgb cr 1.0 1.0 1.0;

        Cairo.rectangle cr
          (fx -. (road_length /. 2.0) -. 10.0)
          (fy -. (road_width /. 2.0) -. 10.0)
          ~w:(road_length +. 20.0) ~h:(road_width +. 20.0);
        Cairo.fill cr
    | _ -> failwith "Road.erase: expected RoadSettings"

  (* Get current settings *)
  let get_settings () = RoadSettings !current_settings

  (* Update settings *)
  let set_settings = function
    | RoadSettings s -> current_settings := s
    | _ -> failwith "Road.set_settings: expected RoadSettings"

  (* Tool type *)
  let get_tool () = ROAD

  (* Draw selection rectangle *)
  let draw_selection cr ~x ~y ~angle settings =
    match settings with
    | RoadSettings s ->
        let lanes = max 1 s.num_lanes in
        let road_width = width_per_lane *. float lanes in

        let fx = float_of_int x in
        let fy = float_of_int y in

        let half_len = road_length /. 2.0 in
        let half_wid = road_width /. 2.0 in
        let padding = 5.0 in

        Cairo.save cr;
        Cairo.translate cr fx fy;
        Cairo.rotate cr angle;
        Cairo.translate cr (-.fx) (-.fy);

        Cairo.set_source_rgba cr 1.0 1.0 0.0 0.7;
        Cairo.set_line_width cr 3.0;

        Cairo.rectangle cr
          (fx -. half_len -. padding)
          (fy -. half_wid -. padding)
          ~w:(road_length +. (padding *. 2.0))
          ~h:(road_width +. (padding *. 2.0));
        Cairo.stroke cr;

        Cairo.restore cr
    | _ -> failwith "Road.draw_selection: expected RoadSettings"

  (* Draw rotate button *)
  let draw_rotate_button cr ~x ~y ~angle _settings =
    let fx = float_of_int x in
    let fy = float_of_int y in
    let half_len = road_length /. 2.0 in
    let button_radius = 12.0 in
    let button_dist = half_len +. 25.0 in

    let bx = fx +. (button_dist *. cos angle) in
    let by = fy +. (button_dist *. sin angle) in

    Cairo.save cr;

    (* outer purple circle *)
    Cairo.set_source_rgb cr 0.5 0.2 0.8;
    Cairo.arc cr bx by ~r:button_radius ~a1:0.0 ~a2:(2.0 *. Float.pi);
    Cairo.fill cr;

    (* inner white circle *)
    Cairo.set_source_rgb cr 1.0 1.0 1.0;
    Cairo.arc cr bx by ~r:(button_radius -. 2.0) ~a1:0.0 ~a2:(2.0 *. Float.pi);
    Cairo.fill cr;

    (* circular arrow icon *)
    Cairo.set_source_rgb cr 0.5 0.2 0.8;
    Cairo.set_line_width cr 2.0;
    Cairo.arc cr bx by ~r:(button_radius -. 4.0) ~a1:(-0.5 *. Float.pi)
      ~a2:(1.5 *. Float.pi);
    Cairo.stroke cr;

    (* arrow *)
    let ang = 1.5 *. Float.pi in
    let tipx = bx +. ((button_radius -. 4.0) *. cos ang) in
    let tipy = by +. ((button_radius -. 4.0) *. sin ang) in

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

  (* Point inside road (axis-aligned in its local coordinates) *)
  let point_inside ~x ~y ~px ~py settings =
    match settings with
    | RoadSettings s ->
        let lanes = max 1 s.num_lanes in
        let road_width = width_per_lane *. float lanes in

        let fx = float_of_int x in
        let fy = float_of_int y in

        let dx = px -. fx in
        let dy = py -. fy in

        let half_len = road_length /. 2.0 in
        let half_wid = road_width /. 2.0 in

        dx >= -.half_len && dx <= half_len && dy >= -.half_wid && dy <= half_wid
    | _ -> false

  (* Point on rotate button *)
  let point_on_rotate_button ~x ~y ~angle ~px ~py _settings =
    let fx = float_of_int x in
    let fy = float_of_int y in
    let half_len = road_length /. 2.0 in
    let button_radius = 12.0 in
    let button_dist = half_len +. 25.0 in

    let bx = fx +. (button_dist *. cos angle) in
    let by = fy +. (button_dist *. sin angle) in

    let dx = px -. bx in
    let dy = py -. by in
    sqrt ((dx *. dx) +. (dy *. dy)) <= button_radius

  (* Rotation angle from center *)
  let calculate_rotation ~cx ~cy ~mx ~my = atan2 (my -. cy) (mx -. cx)

  (* Name *)
  let get_name () = "Road"
end
