(* Intersection module - implements TOOL signature *)
open Tool_types
open Settings

[@@@warning "-34"]

module Intersection : TOOL = struct
  type t = tool

  class intersection_object ~(x : int) ~(y : int) ~(angle : float)
    ~(settings : intersection_settings) =
    object
      val x = x
      val y = y
      val angle = angle
      val settings = settings
      method x = x
      method y = y
      method angle = angle
      method settings = settings
      method num_stops = settings.num_stops
      method has_traffic_light = settings.has_traffic_light
      method stop_duration = settings.stop_duration
    end

  (* Default intersection settings *)
  let current_settings =
    ref { num_stops = 4; has_traffic_light = false; stop_duration = 3.0 }

  (* Size of the intersection *)
  let intersection_size = 60.0

  (* Draw intersection *)
  let draw ~cr ~x ~y ~angle settings =
    match settings with
    | IntersectionSettings s ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let size = intersection_size in
        let half = size /. 2.0 in

        Cairo.save cr;
        Cairo.translate cr fx fy;
        Cairo.rotate cr angle;
        Cairo.translate cr (-.fx) (-.fy);

        (* Gray intersection block *)
        Cairo.set_source_rgb cr 0.25 0.25 0.25;
        Cairo.rectangle cr (fx -. half) (fy -. half) ~w:size ~h:size;
        Cairo.fill cr;

        (* Stop sign helper *)
        let draw_stop_sign bx by =
          Cairo.set_source_rgb cr 0.9 0.1 0.1;
          Cairo.arc cr bx by ~r:8.0 ~a1:0.0 ~a2:(2.0 *. Float.pi);
          Cairo.fill cr
        in

        (* Stop signs if no light *)
        (if not s.has_traffic_light then
           match s.num_stops with
           | 2 ->
               draw_stop_sign (fx -. half -. 10.) fy;
               (* West *)
               draw_stop_sign (fx +. half +. 10.) fy (* East *)
           | 4 ->
               draw_stop_sign (fx -. half -. 10.) fy;
               draw_stop_sign (fx +. half +. 10.) fy;
               draw_stop_sign fx (fy -. half -. 10.);
               draw_stop_sign fx (fy +. half +. 10.)
           | _ -> ());

        (* Traffic light overrides stop signs *)
        if s.has_traffic_light then (
          Cairo.set_source_rgb cr 0.1 0.1 0.1;
          Cairo.rectangle cr (fx -. 8.) (fy -. 25.) ~w:16. ~h:50.;
          Cairo.fill cr;

          let draw_light i (r, g, b) =
            let ly = fy -. 15. +. (float i *. 12.) in
            Cairo.set_source_rgb cr r g b;
            Cairo.arc cr fx ly ~r:4. ~a1:0.0 ~a2:(2.0 *. Float.pi);
            Cairo.fill cr
          in

          draw_light 0 (1.0, 0.0, 0.0);
          (* red *)
          draw_light 1 (1.0, 1.0, 0.0);
          (* yellow *)
          draw_light 2 (0.0, 1.0, 0.0)
          (* green *));

        Cairo.restore cr
    | _ -> failwith "Intersection.draw: expected IntersectionSettings"

  (* Erase *)
  let erase ~cr ~x ~y settings =
    match settings with
    | IntersectionSettings _ ->
        let size = intersection_size +. 30.0 in
        let fx = float_of_int x in
        let fy = float_of_int y in
        Cairo.set_source_rgb cr 1.0 1.0 1.0;
        Cairo.rectangle cr
          (fx -. (size /. 2.))
          (fy -. (size /. 2.))
          ~w:size ~h:size;
        Cairo.fill cr
    | _ -> failwith "Intersection.erase: expected IntersectionSettings"

  let get_settings () = IntersectionSettings !current_settings

  let set_settings new_settings =
    match new_settings with
    | IntersectionSettings s -> current_settings := s
    | _ -> failwith "Intersection.set_settings: expected IntersectionSettings"

  let get_tool () = INTERSECTION

  (* Selection outline *)
  let draw_selection ~cr ~x ~y ~angle settings =
    match settings with
    | IntersectionSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let size = intersection_size in
        let pad = 5.0 in

        Cairo.save cr;
        Cairo.translate cr fx fy;
        Cairo.rotate cr angle;
        Cairo.translate cr (-.fx) (-.fy);

        Cairo.set_source_rgba cr 1.0 1.0 0.0 0.7;
        Cairo.set_line_width cr 3.0;

        Cairo.rectangle cr
          (fx -. (size /. 2.) -. pad)
          (fy -. (size /. 2.) -. pad)
          ~w:(size +. (pad *. 2.))
          ~h:(size +. (pad *. 2.));
        Cairo.stroke cr;

        Cairo.restore cr
    | _ -> failwith "Intersection.draw_selection: expected IntersectionSettings"

  (* Rotate button *)
  let draw_rotate_button ~cr ~x ~y ~angle settings =
    match settings with
    | IntersectionSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let radius = 12.0 in
        let dist = (intersection_size /. 2.) +. 20.0 in

        let bx = fx +. (dist *. cos angle) in
        let by = fy +. (dist *. sin angle) in

        Cairo.save cr;

        Cairo.set_source_rgb cr 0.5 0.2 0.8;
        Cairo.arc cr bx by ~r:radius ~a1:0.0 ~a2:(2.0 *. Float.pi);
        Cairo.fill cr;

        Cairo.set_source_rgb cr 1.0 1.0 1.0;
        Cairo.arc cr bx by ~r:(radius -. 2.) ~a1:0.0 ~a2:(2.0 *. Float.pi);
        Cairo.fill cr;

        Cairo.restore cr
    | _ ->
        failwith
          "Intersection.draw_rotate_button: expected IntersectionSettings"

  (* Hit test *)
  let point_inside ~x ~y ~px ~py settings =
    match settings with
    | IntersectionSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let half = intersection_size /. 2. in
        px >= fx -. half
        && px <= fx +. half
        && py >= fy -. half
        && py <= fy +. half
    | _ -> false

  (* Rotate button hit test *)
  let point_on_rotate_button ~x ~y ~angle ~px ~py settings =
    match settings with
    | IntersectionSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let r = 12.0 in
        let dist = (intersection_size /. 2.) +. 20.0 in
        let bx = fx +. (dist *. cos angle) in
        let by = fy +. (dist *. sin angle) in
        let dx = px -. bx and dy = py -. by in
        sqrt ((dx *. dx) +. (dy *. dy)) <= r
    | _ -> false

  let calculate_rotation ~cx ~cy ~mx ~my = atan2 (my -. cy) (mx -. cx)
  let get_name () = "Intersection"
end
