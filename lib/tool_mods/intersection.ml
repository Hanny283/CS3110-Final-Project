open Tool_types
open Settings

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

  (*Default intersection settings *)
  let current_settings =
    ref { num_stops = 4; has_traffic_light = false; stop_duration = 3.0 }

  (*Size of the intersection *)
  let intersection_size = 60.0

  (**Draw an intersection on the Cairo context with rotation *)
  let draw cr ~x ~y ~angle settings =
    match settings with
    | IntersectionSettings s ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let size = intersection_size in

        Cairo.save cr;

        Cairo.translate cr fx fy;
        Cairo.rotate cr angle;
        Cairo.translate cr (-.fx) (-.fy);

        Cairo.set_source_rgb cr 0.3 0.3 0.3;

        Cairo.rectangle cr
          (fx -. (size /. 2.0))
          (fy -. (size /. 2.0))
          ~w:size ~h:size;
        Cairo.fill cr;

        Cairo.set_source_rgb cr 1.0 1.0 1.0;

        Cairo.set_line_width cr 2.0;

        Cairo.move_to cr (fx -. (size /. 2.0)) fy;
        Cairo.line_to cr (fx +. (size /. 2.0)) fy;
        Cairo.stroke cr;

        Cairo.move_to cr fx (fy -. (size /. 2.0));
        Cairo.line_to cr fx (fy +. (size /. 2.0));
        Cairo.stroke cr;

        if s.has_traffic_light then begin
          Cairo.set_source_rgb cr 0.0 0.0 0.0;

          Cairo.rectangle cr
            (fx +. (size /. 2.0) +. 5.0)
            (fy -. 15.0) ~w:10.0 ~h:30.0;
          Cairo.fill cr;

          Cairo.set_source_rgb cr 1.0 0.0 0.0;

          Cairo.arc cr
            (fx +. (size /. 2.0) +. 10.0)
            (fy -. 9.0) ~r:3.0 ~a1:0.0 ~a2:(2.0 *. 3.14159);
          Cairo.fill cr;

          Cairo.set_source_rgb cr 1.0 1.0 0.0;

          Cairo.arc cr
            (fx +. (size /. 2.0) +. 10.0)
            fy ~r:3.0 ~a1:0.0 ~a2:(2.0 *. 3.14159);
          Cairo.fill cr;

          Cairo.set_source_rgb cr 0.0 1.0 0.0;

          Cairo.arc cr
            (fx +. (size /. 2.0) +. 10.0)
            (fy +. 9.0) ~r:3.0 ~a1:0.0 ~a2:(2.0 *. 3.14159);
          Cairo.fill cr
        end
        else begin
          Cairo.set_source_rgb cr 1.0 0.0 0.0;

          let stop_size = 8.0 in

          for i = 0 to min (s.num_stops - 1) 3 do
            let stop_angle = float_of_int i *. 3.14159 /. 2.0 in
            let stop_x = fx +. (((size /. 2.0) +. 10.0) *. cos stop_angle) in
            let stop_y = fy +. (((size /. 2.0) +. 10.0) *. sin stop_angle) in
            Cairo.rectangle cr
              (stop_x -. (stop_size /. 2.0))
              (stop_y -. (stop_size /. 2.0))
              ~w:stop_size ~h:stop_size;
            Cairo.fill cr
          done
        end;

        Cairo.set_source_rgb cr 0.0 0.0 0.0;

        Cairo.set_line_width cr 2.0;
        Cairo.rectangle cr
          (fx -. (size /. 2.0))
          (fy -. (size /. 2.0))
          ~w:size ~h:size;
        Cairo.stroke cr;

        Cairo.restore cr;

        let (_ : intersection_object) =
          new intersection_object ~x ~y ~angle ~settings:s
        in
        ()
    | _ ->
        (* Invalid settings type for intersection *)
        failwith "Intersection.draw: expected IntersectionSettings"

  (**Erase an intersection, but again not used but it had to be fined.*)
  let erase cr ~x ~y settings =
    match settings with
    | IntersectionSettings _ ->
        let size = 80.0 in

        let fx = float_of_int x in
        let fy = float_of_int y in

        Cairo.set_source_rgb cr 1.0 1.0 1.0;

        Cairo.rectangle cr
          (fx -. (size /. 2.0))
          (fy -. (size /. 2.0))
          ~w:size ~h:size;
        Cairo.fill cr
    | _ -> failwith "Intersection.erase: expected IntersectionSettings"

  (**Get current settings *)
  let get_settings () = IntersectionSettings !current_settings

  (**Update settings *)
  let set_settings new_settings =
    match new_settings with
    | IntersectionSettings s -> current_settings := s
    | _ -> failwith "Intersection.set_settings: expected IntersectionSettings"

  (**Get the tool type *)
  let get_tool () = INTERSECTION

  (**Highlight the selected intersection *)
  let draw_selection cr ~x ~y ~angle settings =
    match settings with
    | IntersectionSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let size = intersection_size in
        let padding = 5.0 in

        Cairo.save cr;
        Cairo.translate cr fx fy;
        Cairo.rotate cr angle;
        Cairo.translate cr (-.fx) (-.fy);

        Cairo.set_source_rgba cr 1.0 1.0 0.0 0.7;
        Cairo.set_line_width cr 3.0;
        Cairo.set_line_cap cr Cairo.ROUND;
        Cairo.set_line_join cr Cairo.JOIN_ROUND;
        Cairo.rectangle cr
          (fx -. (size /. 2.0) -. padding)
          (fy -. (size /. 2.0) -. padding)
          ~w:(size +. (padding *. 2.0))
          ~h:(size +. (padding *. 2.0));
        Cairo.stroke cr;

        Cairo.restore cr
    | _ -> failwith "Intersection.draw_selection: expected IntersectionSettings"

  (**Draw rotate button*)
  let draw_rotate_button cr ~x ~y ~angle settings =
    match settings with
    | IntersectionSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let size = intersection_size in
        let button_radius = 12.0 in
        let button_distance = (size /. 2.0) +. 20.0 in

        let button_x = fx +. (button_distance *. cos angle) in
        let button_y = fy +. (button_distance *. sin angle) in

        Cairo.save cr;

        Cairo.set_source_rgb cr 0.5 0.2 0.8;

        Cairo.arc cr button_x button_y ~r:button_radius ~a1:0.0
          ~a2:(2.0 *. 3.14159);
        Cairo.fill cr;

        Cairo.set_source_rgb cr 1.0 1.0 1.0;

        Cairo.arc cr button_x button_y ~r:(button_radius -. 2.0) ~a1:0.0
          ~a2:(2.0 *. 3.14159);
        Cairo.fill cr;

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
    | _ ->
        failwith
          "Intersection.draw_rotate_button: expected IntersectionSettings"

  (* Check if a point is inside the intersection bounds*)
  let point_inside ~x ~y ~px ~py settings =
    match settings with
    | IntersectionSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let size = intersection_size in
        let half_size = size /. 2.0 in
        px >= fx -. half_size
        && px <= fx +. half_size
        && py >= fy -. half_size
        && py <= fy +. half_size
    | _ -> false

  (*Check if a point is on the rotate button *)
  let point_on_rotate_button ~x ~y ~angle ~px ~py settings =
    match settings with
    | IntersectionSettings _ ->
        let fx = float_of_int x in
        let fy = float_of_int y in
        let size = intersection_size in
        let button_radius = 12.0 in
        let button_distance = (size /. 2.0) +. 20.0 in

        let button_x = fx +. (button_distance *. cos angle) in
        let button_y = fy +. (button_distance *. sin angle) in

        let dx = px -. button_x in
        let dy = py -. button_y in
        let dist = sqrt ((dx *. dx) +. (dy *. dy)) in
        dist <= button_radius
    | _ -> false

  (* Calculate rotation angle from center*)
  let calculate_rotation ~cx ~cy ~mx ~my =
    let dx = mx -. cx in
    let dy = my -. cy in
    atan2 dy dx

  (* Get the tool name for display *)
  let get_name () = "Intersection"
end
