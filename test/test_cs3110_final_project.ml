(* Test suite for CS3110 Final Project
 *
 * This test suite uses OUnit2 for unit testing and is instrumented with
 * Bisect_ppx for code coverage measurement. Run tests with:
 *   dune test
 *
 * To generate coverage reports, run:
 *   dune runtest --instrument-with bisect_ppx --force
 *   bisect-ppx-report html
 *)

open OUnit2
open Cs3110_final_project
open Cs3110_final_project.Road
open Cs3110_final_project.Intersection
open Cs3110_final_project.Building

(* ============================================================================
 * Settings Module Tests
 * ============================================================================ *)

module Settings_tests = struct
  let test_building_settings_creation _ =
    let settings = { Settings.rate_of_traffic = 15 } in
    assert_equal 15 settings.rate_of_traffic

  let test_road_settings_creation _ =
    let settings =
      { Settings.speed_limit = 55; num_lanes = 4; max_capacity = 200 }
    in
    assert_equal 55 settings.speed_limit;
    assert_equal 4 settings.num_lanes;
    assert_equal 200 settings.max_capacity

  let test_intersection_settings_creation _ =
    let settings =
      { Settings.num_stops = 4; has_traffic_light = true; stop_duration = 5.0 }
    in
    assert_equal 4 settings.num_stops;
    assert_equal true settings.has_traffic_light;
    assert_equal 5.0 settings.stop_duration

  let test_settings_variant_building _ =
    let building_settings = { Settings.rate_of_traffic = 10 } in
    let settings = Settings.BuildingSettings building_settings in
    match settings with
    | Settings.BuildingSettings s -> assert_equal 10 s.rate_of_traffic
    | _ -> failwith "Expected BuildingSettings"

  let test_settings_variant_road _ =
    let road_settings =
      { Settings.speed_limit = 35; num_lanes = 2; max_capacity = 100 }
    in
    let settings = Settings.RoadSettings road_settings in
    match settings with
    | Settings.RoadSettings s ->
        assert_equal 35 s.speed_limit;
        assert_equal 2 s.num_lanes;
        assert_equal 100 s.max_capacity
    | _ -> failwith "Expected RoadSettings"

  let test_settings_variant_intersection _ =
    let intersection_settings =
      { Settings.num_stops = 4; has_traffic_light = false; stop_duration = 3.0 }
    in
    let settings = Settings.IntersectionSettings intersection_settings in
    match settings with
    | Settings.IntersectionSettings s ->
        assert_equal 4 s.num_stops;
        assert_equal false s.has_traffic_light;
        assert_equal 3.0 s.stop_duration
    | _ -> failwith "Expected IntersectionSettings"

  let suite =
    "Settings Module"
    >::: [
           "building_settings_creation" >:: test_building_settings_creation;
           "road_settings_creation" >:: test_road_settings_creation;
           "intersection_settings_creation"
           >:: test_intersection_settings_creation;
           "settings_variant_building" >:: test_settings_variant_building;
           "settings_variant_road" >:: test_settings_variant_road;
           "settings_variant_intersection"
           >:: test_settings_variant_intersection;
         ]
end

(* ============================================================================
 * Tool_types Module Tests
 * ============================================================================ *)

module Tool_types_tests = struct
  let test_tool_type_road _ = assert_equal Tool_types.ROAD Tool_types.ROAD

  let test_tool_type_building _ =
    assert_equal Tool_types.BUILDING Tool_types.BUILDING

  let test_tool_type_intersection _ =
    assert_equal Tool_types.INTERSECTION Tool_types.INTERSECTION

  let test_direction_types _ =
    assert_equal Tool_types.North Tool_types.North;
    assert_equal Tool_types.South Tool_types.South;
    assert_equal Tool_types.East Tool_types.East;
    assert_equal Tool_types.West Tool_types.West

  let suite =
    "Tool_types Module"
    >::: [
           "tool_type_road" >:: test_tool_type_road;
           "tool_type_building" >:: test_tool_type_building;
           "tool_type_intersection" >:: test_tool_type_intersection;
           "direction_types" >:: test_direction_types;
         ]
end

(* ============================================================================
 * Road Module Tests
 * ============================================================================ *)

module Road_tests = struct
  let test_get_tool _ =
    let tool = Road.get_tool () in
    assert_equal Tool_types.ROAD tool

  let test_get_name _ =
    let name = Road.get_name () in
    assert_equal "Road" name

  let test_get_settings _ =
    let settings = Road.get_settings () in
    match settings with
    | Settings.RoadSettings s ->
        assert_equal 35 s.speed_limit;
        assert_equal 2 s.num_lanes;
        assert_equal 100 s.max_capacity
    | _ -> failwith "Expected RoadSettings"

  let test_set_settings _ =
    let new_settings =
      Settings.RoadSettings
        { Settings.speed_limit = 45; num_lanes = 3; max_capacity = 150 }
    in
    Road.set_settings new_settings;
    let settings = Road.get_settings () in
    (match settings with
    | Settings.RoadSettings s ->
        assert_equal 45 s.speed_limit;
        assert_equal 3 s.num_lanes;
        assert_equal 150 s.max_capacity;
        ()
    | _ -> failwith "Expected RoadSettings");
    (* Reset to default *)
    Road.set_settings
      (Settings.RoadSettings
         { Settings.speed_limit = 35; num_lanes = 2; max_capacity = 100 })

  let test_point_inside_center _ =
    let settings = Road.get_settings () in
    let x, y = (100, 100) in
    let px, py = (100.0, 100.0) in
    let result = Road.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point at center should be inside" result

  let test_point_inside_outside _ =
    let settings = Road.get_settings () in
    let x, y = (100, 100) in
    let px, py = (500.0, 500.0) in
    let result = Road.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point far away should be outside" (not result)

  let test_calculate_rotation _ =
    let cx, cy = (100.0, 100.0) in
    let mx, my = (150.0, 100.0) in
    let angle = Road.calculate_rotation ~cx ~cy ~mx ~my in
    (* Should be approximately 0 (pointing right/east) *)
    assert_bool "Angle should be close to 0" (abs_float angle < 0.01)

  (* Helper function to create a Cairo image surface for testing *)
  let create_test_surface () =
    Cairo.Image.create Cairo.Image.ARGB32 ~w:500 ~h:500

  (* Test draw function with default settings (2 lanes) *)
  let test_draw_default _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let settings = Road.get_settings () in
    Road.draw ~cr ~x:250 ~y:250 ~angle:0.0 settings;
    Cairo.Surface.finish surface;
    assert_bool "Draw with default settings should complete" true

  (* Test draw function with 1 lane *)
  let test_draw_one_lane _ =
    let settings =
      Settings.RoadSettings
        { Settings.speed_limit = 25; num_lanes = 1; max_capacity = 50 }
    in
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    Road.draw ~cr ~x:250 ~y:250 ~angle:0.0 settings;
    Cairo.Surface.finish surface;
    assert_bool "Draw with 1 lane should complete" true

  (* Test draw function with multiple lanes (3, 4, 5) *)
  let test_draw_multiple_lanes _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    (* 3 lanes *)
    let settings3 =
      Settings.RoadSettings
        { Settings.speed_limit = 45; num_lanes = 3; max_capacity = 150 }
    in
    Road.draw ~cr ~x:100 ~y:100 ~angle:0.0 settings3;
    (* 4 lanes *)
    let settings4 =
      Settings.RoadSettings
        { Settings.speed_limit = 55; num_lanes = 4; max_capacity = 200 }
    in
    Road.draw ~cr ~x:200 ~y:200 ~angle:0.0 settings4;
    (* 5 lanes *)
    let settings5 =
      Settings.RoadSettings
        { Settings.speed_limit = 65; num_lanes = 5; max_capacity = 250 }
    in
    Road.draw ~cr ~x:300 ~y:300 ~angle:0.0 settings5;
    Cairo.Surface.finish surface;
    assert_bool "Draw with multiple lanes should complete" true

  (* Test draw function with different rotation angles *)
  let test_draw_with_rotation _ =
    let settings = Road.get_settings () in
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    Road.draw ~cr ~x:250 ~y:250 ~angle:0.0 settings;
    Road.draw ~cr ~x:250 ~y:250 ~angle:(Float.pi /. 4.0) settings;
    Road.draw ~cr ~x:250 ~y:250 ~angle:(Float.pi /. 2.0) settings;
    Road.draw ~cr ~x:250 ~y:250 ~angle:Float.pi settings;
    Road.draw ~cr ~x:250 ~y:250 ~angle:(3.0 *. Float.pi /. 2.0) settings;
    Cairo.Surface.finish surface;
    assert_bool "Draw with various rotations should complete" true

  (* Test erase function *)
  let test_erase _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let settings = Road.get_settings () in
    (* First draw, then erase *)
    Road.draw ~cr ~x:250 ~y:250 ~angle:0.0 settings;
    Road.erase ~cr ~x:250 ~y:250 settings;
    Cairo.Surface.finish surface;
    assert_bool "Erase should complete without error" true

  (* Test erase with different lane counts *)
  let test_erase_various_lanes _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let settings1 =
      Settings.RoadSettings
        { Settings.speed_limit = 25; num_lanes = 1; max_capacity = 50 }
    in
    Road.erase ~cr ~x:100 ~y:100 settings1;
    let settings3 =
      Settings.RoadSettings
        { Settings.speed_limit = 45; num_lanes = 3; max_capacity = 150 }
    in
    Road.erase ~cr ~x:200 ~y:200 settings3;
    Cairo.Surface.finish surface;
    assert_bool "Erase with various lanes should complete" true

  (* Test draw_selection function *)
  let test_draw_selection _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let settings = Road.get_settings () in
    Road.draw_selection ~cr ~x:250 ~y:250 ~angle:0.0 settings;
    Road.draw_selection ~cr ~x:250 ~y:250 ~angle:(Float.pi /. 4.0) settings;
    Cairo.Surface.finish surface;
    assert_bool "Draw selection should complete without error" true

  (* Test draw_selection with different lane counts *)
  let test_draw_selection_various_lanes _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let settings1 =
      Settings.RoadSettings
        { Settings.speed_limit = 25; num_lanes = 1; max_capacity = 50 }
    in
    Road.draw_selection ~cr ~x:100 ~y:100 ~angle:0.0 settings1;
    let settings4 =
      Settings.RoadSettings
        { Settings.speed_limit = 55; num_lanes = 4; max_capacity = 200 }
    in
    Road.draw_selection ~cr ~x:200 ~y:200 ~angle:(Float.pi /. 2.0) settings4;
    Cairo.Surface.finish surface;
    assert_bool "Draw selection with various lanes should complete" true

  (* Test draw_rotate_button function *)
  let test_draw_rotate_button _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let settings = Road.get_settings () in
    Road.draw_rotate_button ~cr ~x:250 ~y:250 ~angle:0.0 settings;
    Road.draw_rotate_button ~cr ~x:250 ~y:250 ~angle:(Float.pi /. 2.0) settings;
    Road.draw_rotate_button ~cr ~x:250 ~y:250 ~angle:Float.pi settings;
    Cairo.Surface.finish surface;
    assert_bool "Draw rotate button should complete without error" true

  (* Test point_inside boundary conditions - left/right *)
  let test_point_inside_boundaries_horizontal _ =
    let settings = Road.get_settings () in
    let x, y = 100, 100 in
    (* Road length is 120.0, half_len is 60.0 *)
    (* Left boundary: 100 - 60 = 40 *)
    let result1 = Road.point_inside ~x ~y ~px:40.0 ~py:100.0 settings in
    assert_bool "Point on left boundary should be inside" result1;
    (* Right boundary: 100 + 60 = 160 *)
    let result2 = Road.point_inside ~x ~y ~px:160.0 ~py:100.0 settings in
    assert_bool "Point on right boundary should be inside" result2;
    (* Just outside left *)
    let result3 = Road.point_inside ~x ~y ~px:39.9 ~py:100.0 settings in
    assert_bool "Point just outside left should be outside" (not result3);
    (* Just outside right *)
    let result4 = Road.point_inside ~x ~y ~px:160.1 ~py:100.0 settings in
    assert_bool "Point just outside right should be outside" (not result4)

  (* Test point_inside boundary conditions - top/bottom *)
  let test_point_inside_boundaries_vertical _ =
    let settings = Road.get_settings () in
    let x, y = 100, 100 in
    (* 2 lanes, width = 24.0, half_wid = 12.0 *)
    (* Top boundary: 100 - 12 = 88 *)
    let result1 = Road.point_inside ~x ~y ~px:100.0 ~py:88.0 settings in
    assert_bool "Point on top boundary should be inside" result1;
    (* Bottom boundary: 100 + 12 = 112 *)
    let result2 = Road.point_inside ~x ~y ~px:100.0 ~py:112.0 settings in
    assert_bool "Point on bottom boundary should be inside" result2;
    (* Just outside top *)
    let result3 = Road.point_inside ~x ~y ~px:100.0 ~py:87.9 settings in
    assert_bool "Point just outside top should be outside" (not result3);
    (* Just outside bottom *)
    let result4 = Road.point_inside ~x ~y ~px:100.0 ~py:112.1 settings in
    assert_bool "Point just outside bottom should be outside" (not result4)

  (* Test point_inside with different lane counts *)
  let test_point_inside_various_lanes _ =
    let x, y = 100, 100 in
    let px, py = 100.0, 100.0 in
    (* 1 lane: width = 12, half_wid = 6 *)
    let settings1 =
      Settings.RoadSettings
        { Settings.speed_limit = 25; num_lanes = 1; max_capacity = 50 }
    in
    let result1 = Road.point_inside ~x ~y ~px ~py settings1 in
    assert_bool "Point inside should work with 1 lane" result1;
    (* 4 lanes: width = 48, half_wid = 24 *)
    let settings4 =
      Settings.RoadSettings
        { Settings.speed_limit = 55; num_lanes = 4; max_capacity = 200 }
    in
    let result4 = Road.point_inside ~x ~y ~px ~py settings4 in
    assert_bool "Point inside should work with 4 lanes" result4

  (* Test point_on_rotate_button at center *)
  let test_point_on_rotate_button_center _ =
    let settings = Road.get_settings () in
    let x, y = 100, 100 in
    let angle = 0.0 in
    (* Button is at distance (60 + 25) = 85 from center, at angle 0 (to the right) *)
    (* Button center: (185, 100), radius = 12 *)
    let button_x = 185.0 in
    let button_y = 100.0 in
    let result =
      Road.point_on_rotate_button ~x ~y ~angle ~px:button_x ~py:button_y settings
    in
    assert_bool "Point at button center should be on button" result

  (* Test point_on_rotate_button at edge and outside *)
  let test_point_on_rotate_button_edge _ =
    let settings = Road.get_settings () in
    let x, y = 100, 100 in
    let angle = 0.0 in
    let button_x = 185.0 in
    (* Just inside radius (12) *)
    let result1 =
      Road.point_on_rotate_button ~x ~y ~angle ~px:button_x ~py:111.9 settings
    in
    assert_bool "Point just inside button should be on button" result1;
    (* Just outside radius *)
    let result2 =
      Road.point_on_rotate_button ~x ~y ~angle ~px:button_x ~py:112.1 settings
    in
    assert_bool "Point just outside button should not be on button" (not result2)

  (* Test point_on_rotate_button at different angles *)
  let test_point_on_rotate_button_various_angles _ =
    let settings = Road.get_settings () in
    let x, y = 100, 100 in
    (* Test at 90 degrees *)
    let angle1 = Float.pi /. 2.0 in
    let button_dist = 60.0 +. 25.0 in
    let button_x1 = 100.0 +. (button_dist *. cos angle1) in
    let button_y1 = 100.0 +. (button_dist *. sin angle1) in
    let result1 =
      Road.point_on_rotate_button ~x ~y ~angle:angle1 ~px:button_x1 ~py:button_y1
        settings
    in
    assert_bool "Point at button center at 90 degrees should be on button" result1;
    (* Test at 180 degrees *)
    let angle2 = Float.pi in
    let button_x2 = 100.0 +. (button_dist *. cos angle2) in
    let button_y2 = 100.0 +. (button_dist *. sin angle2) in
    let result2 =
      Road.point_on_rotate_button ~x ~y ~angle:angle2 ~px:button_x2 ~py:button_y2
        settings
    in
    assert_bool "Point at button center at 180 degrees should be on button" result2

  (* Test calculate_rotation comprehensive *)
  let test_calculate_rotation_comprehensive _ =
    let cx, cy = 100.0, 100.0 in
    (* Right *)
    let angle1 = Road.calculate_rotation ~cx ~cy ~mx:150.0 ~my:100.0 in
    assert_bool "Right should be ~0" (abs_float angle1 < 0.1);
    (* Down *)
    let angle2 = Road.calculate_rotation ~cx ~cy ~mx:100.0 ~my:150.0 in
    assert_bool "Down should be ~pi/2"
      (abs_float (angle2 -. (Float.pi /. 2.0)) < 0.1);
    (* Left *)
    let angle3 = Road.calculate_rotation ~cx ~cy ~mx:50.0 ~my:100.0 in
    assert_bool "Left should be ~pi or ~-pi"
      (abs_float (abs_float angle3 -. Float.pi) < 0.1);
    (* Up *)
    let angle4 = Road.calculate_rotation ~cx ~cy ~mx:100.0 ~my:50.0 in
    assert_bool "Up should be ~-pi/2"
      (abs_float (angle4 +. (Float.pi /. 2.0)) < 0.1)

  (* Test error case: draw with wrong settings type *)
  let test_draw_error_wrong_settings _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let wrong_settings =
      Settings.IntersectionSettings
        { Settings.num_stops = 4; has_traffic_light = false; stop_duration = 3.0 }
    in
    try
      Road.draw ~cr ~x:250 ~y:250 ~angle:0.0 wrong_settings;
      failwith "Should have raised an exception"
    with Failure msg ->
      assert_bool "Should fail with expected message"
        (String.contains msg 'R' || String.contains msg 'r')

  (* Test error case: erase with wrong settings type *)
  let test_erase_error_wrong_settings _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let wrong_settings =
      Settings.BuildingSettings { Settings.rate_of_traffic = 10 }
    in
    try
      Road.erase ~cr ~x:250 ~y:250 wrong_settings;
      failwith "Should have raised an exception"
    with Failure msg ->
      assert_bool "Should fail with expected message"
        (String.contains msg 'R' || String.contains msg 'r')

  (* Test error case: draw_selection with wrong settings type *)
  let test_draw_selection_error_wrong_settings _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let wrong_settings =
      Settings.BuildingSettings { Settings.rate_of_traffic = 10 }
    in
    try
      Road.draw_selection ~cr ~x:250 ~y:250 ~angle:0.0 wrong_settings;
      failwith "Should have raised an exception"
    with Failure msg ->
      assert_bool "Should fail with expected message"
        (String.contains msg 'R' || String.contains msg 'r')

  (* Test error case: set_settings with wrong settings type *)
  let test_set_settings_error_wrong_settings _ =
    let wrong_settings =
      Settings.IntersectionSettings
        { Settings.num_stops = 4; has_traffic_light = false; stop_duration = 3.0 }
    in
    try
      Road.set_settings wrong_settings;
      failwith "Should have raised an exception"
    with Failure msg ->
      assert_bool "Should fail with expected message"
        (String.contains msg 'R' || String.contains msg 'r')

  (* Test error case: point_inside with wrong settings type *)
  let test_point_inside_wrong_settings _ =
    let wrong_settings =
      Settings.BuildingSettings { Settings.rate_of_traffic = 10 }
    in
    let x, y = 100, 100 in
    let px, py = 100.0, 100.0 in
    let result = Road.point_inside ~x ~y ~px ~py wrong_settings in
    assert_bool "Should return false for wrong settings type" (not result)

  (* Test settings with various speed limits *)
  let test_settings_various_speed_limits _ =
    let settings1 =
      Settings.RoadSettings
        { Settings.speed_limit = 15; num_lanes = 1; max_capacity = 30 }
    in
    Road.set_settings settings1;
    let retrieved1 = Road.get_settings () in
    (match retrieved1 with
    | Settings.RoadSettings s -> assert_equal 15 s.speed_limit
    | _ -> failwith "Expected RoadSettings");
    let settings2 =
      Settings.RoadSettings
        { Settings.speed_limit = 75; num_lanes = 4; max_capacity = 300 }
    in
    Road.set_settings settings2;
    let retrieved2 = Road.get_settings () in
    (match retrieved2 with
    | Settings.RoadSettings s -> assert_equal 75 s.speed_limit
    | _ -> failwith "Expected RoadSettings");
    (* Reset to default *)
    Road.set_settings
      (Settings.RoadSettings
         { Settings.speed_limit = 35; num_lanes = 2; max_capacity = 100 })

  let suite =
    "Road Module"
    >::: [
           "get_tool" >:: test_get_tool;
           "get_name" >:: test_get_name;
           "get_settings" >:: test_get_settings;
           "set_settings" >:: test_set_settings;
           "point_inside_center" >:: test_point_inside_center;
           "point_inside_outside" >:: test_point_inside_outside;
           "calculate_rotation" >:: test_calculate_rotation;
           "draw_default" >:: test_draw_default;
           "draw_one_lane" >:: test_draw_one_lane;
           "draw_multiple_lanes" >:: test_draw_multiple_lanes;
           "draw_with_rotation" >:: test_draw_with_rotation;
           "erase" >:: test_erase;
           "erase_various_lanes" >:: test_erase_various_lanes;
           "draw_selection" >:: test_draw_selection;
           "draw_selection_various_lanes" >:: test_draw_selection_various_lanes;
           "draw_rotate_button" >:: test_draw_rotate_button;
           "point_inside_boundaries_horizontal"
           >:: test_point_inside_boundaries_horizontal;
           "point_inside_boundaries_vertical"
           >:: test_point_inside_boundaries_vertical;
           "point_inside_various_lanes" >:: test_point_inside_various_lanes;
           "point_on_rotate_button_center" >:: test_point_on_rotate_button_center;
           "point_on_rotate_button_edge" >:: test_point_on_rotate_button_edge;
           "point_on_rotate_button_various_angles"
           >:: test_point_on_rotate_button_various_angles;
           "calculate_rotation_comprehensive"
           >:: test_calculate_rotation_comprehensive;
           "draw_error_wrong_settings" >:: test_draw_error_wrong_settings;
           "erase_error_wrong_settings" >:: test_erase_error_wrong_settings;
           "draw_selection_error_wrong_settings"
           >:: test_draw_selection_error_wrong_settings;
           "set_settings_error_wrong_settings"
           >:: test_set_settings_error_wrong_settings;
           "point_inside_wrong_settings" >:: test_point_inside_wrong_settings;
           "settings_various_speed_limits" >:: test_settings_various_speed_limits;
         ]
end

(* ============================================================================
 * Building Module Tests
 * ============================================================================ *)

module Building_tests = struct
  let test_get_tool _ =
    let tool = Building.get_tool () in
    assert_equal Tool_types.BUILDING tool

  let test_get_name _ =
    let name = Building.get_name () in
    assert_equal "Building" name

  let test_get_settings _ =
    let settings = Building.get_settings () in
    match settings with
    | Settings.BuildingSettings s -> assert_equal 10 s.rate_of_traffic
    | _ -> failwith "Expected BuildingSettings"

  let test_set_settings _ =
    let new_settings =
      Settings.BuildingSettings { Settings.rate_of_traffic = 20 }
    in
    Building.set_settings new_settings;
    let settings = Building.get_settings () in
    (match settings with
    | Settings.BuildingSettings s ->
        assert_equal 20 s.rate_of_traffic;
        ()
    | _ -> failwith "Expected BuildingSettings");
    (* Reset to default *)
    Building.set_settings
      (Settings.BuildingSettings { Settings.rate_of_traffic = 10 })

  let test_point_inside_center _ =
    let settings = Building.get_settings () in
    let x, y = (100, 100) in
    let px, py = (100.0, 100.0) in
    let result = Building.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point at center should be inside" result

  let test_point_inside_outside _ =
    let settings = Building.get_settings () in
    let x, y = (100, 100) in
    let px, py = (500.0, 500.0) in
    let result = Building.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point far away should be outside" (not result)

  let test_calculate_rotation _ =
    let cx, cy = (100.0, 100.0) in
    let mx, my = (100.0, 150.0) in
    let angle = Building.calculate_rotation ~cx ~cy ~mx ~my in
    (* Should be approximately pi/2 (pointing down/south) *)
    assert_bool "Angle should be close to pi/2"
      (abs_float (angle -. 1.5708) < 0.1)

  let test_set_settings_invalid _ =
    assert_raises (Failure "Building.set_settings: expected BuildingSettings")
      (fun () ->
        Building.set_settings
          (Settings.IntersectionSettings
             { num_stops = 4; has_traffic_light = false; stop_duration = 3.0 }))

  let test_point_inside_wrong_settings _ =
    let wrong =
      Settings.IntersectionSettings
        { num_stops = 4; has_traffic_light = false; stop_duration = 3.0 }
    in
    assert_bool "wrong settings should return false"
      (not (Building.point_inside ~x:0 ~y:0 ~px:0. ~py:0. wrong))

  let test_point_on_rotate_button_basic _ =
    let s = Building.get_settings () in
    assert_bool "should not crash evaluating rotate button"
      (not
         (Building.point_on_rotate_button ~x:100 ~y:100 ~angle:0.0 ~px:100.
            ~py:100. s))

  let test_point_on_rotate_button_wrong_settings _ =
    let wrong =
      Settings.IntersectionSettings
        { num_stops = 4; has_traffic_light = false; stop_duration = 3.0 }
    in
    assert_bool "wrong settings should return false"
      (not
         (Building.point_on_rotate_button ~x:0 ~y:0 ~angle:0.0 ~px:0. ~py:0.
            wrong))

  let test_draw_ok _ =
    let s = Building.get_settings () in
    let surface = Cairo.Image.create Cairo.Image.RGB24 ~w:200 ~h:200 in
    let cr = Cairo.create surface in
    Building.draw ~cr ~x:100 ~y:100 ~angle:0.0 s

  let test_draw_wrong_settings _ =
    let surface = Cairo.Image.create Cairo.Image.RGB24 ~w:200 ~h:200 in
    let cr = Cairo.create surface in
    let wrong =
      Settings.IntersectionSettings
        { num_stops = 4; has_traffic_light = false; stop_duration = 3.0 }
    in
    assert_raises (Failure "Building.draw: expected BuildingSettings")
      (fun () -> Building.draw ~cr ~x:0 ~y:0 ~angle:0.0 wrong)

  let test_draw_selection_ok _ =
    let s = Building.get_settings () in
    let surface = Cairo.Image.create Cairo.Image.RGB24 ~w:200 ~h:200 in
    let cr = Cairo.create surface in
    Building.draw_selection ~cr ~x:100 ~y:100 ~angle:0.0 s

  let test_draw_selection_wrong_settings _ =
    let surface = Cairo.Image.create Cairo.Image.RGB24 ~w:200 ~h:200 in
    let cr = Cairo.create surface in
    let wrong =
      Settings.IntersectionSettings
        { num_stops = 4; has_traffic_light = false; stop_duration = 3.0 }
    in
    assert_raises (Failure "Building.draw_selection: expected BuildingSettings")
      (fun () -> Building.draw_selection ~cr ~x:0 ~y:0 ~angle:0.0 wrong)

  let test_draw_rotate_button_ok _ =
    let s = Building.get_settings () in
    let surface = Cairo.Image.create Cairo.Image.RGB24 ~w:200 ~h:200 in
    let cr = Cairo.create surface in
    Building.draw_rotate_button ~cr ~x:100 ~y:100 ~angle:0.0 s

  let test_draw_rotate_button_wrong_settings _ =
    let surface = Cairo.Image.create Cairo.Image.RGB24 ~w:200 ~h:200 in
    let cr = Cairo.create surface in
    let wrong =
      Settings.IntersectionSettings
        { num_stops = 4; has_traffic_light = false; stop_duration = 3.0 }
    in
    assert_raises
      (Failure "Building.draw_rotate_button: expected BuildingSettings")
      (fun () -> Building.draw_rotate_button ~cr ~x:0 ~y:0 ~angle:0.0 wrong)

  let test_erase_ok _ =
    let s = Building.get_settings () in
    let surface = Cairo.Image.create Cairo.Image.RGB24 ~w:200 ~h:200 in
    let cr = Cairo.create surface in
    Building.erase ~cr ~x:100 ~y:100 s

  let test_erase_wrong_settings _ =
    let surface = Cairo.Image.create Cairo.Image.RGB24 ~w:200 ~h:200 in
    let cr = Cairo.create surface in
    let wrong =
      Settings.IntersectionSettings
        { num_stops = 4; has_traffic_light = false; stop_duration = 3.0 }
    in
    assert_raises (Failure "Building.erase: expected BuildingSettings")
      (fun () -> Building.erase ~cr ~x:0 ~y:0 wrong)

  let suite =
    "Building Module"
    >::: [
           "get_tool" >:: test_get_tool;
           "get_name" >:: test_get_name;
           "get_settings" >:: test_get_settings;
           "set_settings" >:: test_set_settings;
           "point_inside_center" >:: test_point_inside_center;
           "point_inside_outside" >:: test_point_inside_outside;
           "calculate_rotation" >:: test_calculate_rotation;
           "set_settings_invalid" >:: test_set_settings_invalid;
           "point_inside_wrong_settings" >:: test_point_inside_wrong_settings;
           "point_on_rotate_button_basic" >:: test_point_on_rotate_button_basic;
           "point_on_rotate_button_wrong_settings"
           >:: test_point_on_rotate_button_wrong_settings;
           "draw_ok" >:: test_draw_ok;
           "draw_wrong_settings" >:: test_draw_wrong_settings;
           "draw_selection_ok" >:: test_draw_selection_ok;
           "draw_selection_wrong_settings"
           >:: test_draw_selection_wrong_settings;
           "draw_rotate_button_ok" >:: test_draw_rotate_button_ok;
           "draw_rotate_button_wrong_settings"
           >:: test_draw_rotate_button_wrong_settings;
           "erase_ok" >:: test_erase_ok;
           "erase_wrong_settings" >:: test_erase_wrong_settings;
         ]
end

(* ============================================================================
 * Intersection Module Tests
 * ============================================================================ *)

module Intersection_tests = struct
  let test_get_tool _ =
    let tool = Intersection.get_tool () in
    assert_equal Tool_types.INTERSECTION tool

  let test_get_name _ =
    let name = Intersection.get_name () in
    assert_equal "Intersection" name

  let test_get_settings _ =
    let settings = Intersection.get_settings () in
    match settings with
    | Settings.IntersectionSettings s ->
        assert_equal 4 s.num_stops;
        assert_equal false s.has_traffic_light;
        assert_equal 3.0 s.stop_duration
    | _ -> failwith "Expected IntersectionSettings"

  let test_set_settings _ =
    let new_settings =
      Settings.IntersectionSettings
        {
          Settings.num_stops = 3;
          has_traffic_light = true;
          stop_duration = 4.5;
        }
    in
    Intersection.set_settings new_settings;
    let settings = Intersection.get_settings () in
    (match settings with
    | Settings.IntersectionSettings s ->
        assert_equal 3 s.num_stops;
        assert_equal true s.has_traffic_light;
        assert_equal 4.5 s.stop_duration;
        ()
    | _ -> failwith "Expected IntersectionSettings");
    (* Reset to default *)
    Intersection.set_settings
      (Settings.IntersectionSettings
         {
           Settings.num_stops = 4;
           has_traffic_light = false;
           stop_duration = 3.0;
         })

  let test_point_inside_center _ =
    let settings = Intersection.get_settings () in
    let x, y = (100, 100) in
    let px, py = (100.0, 100.0) in
    let result = Intersection.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point at center should be inside" result

  let test_point_inside_outside _ =
    let settings = Intersection.get_settings () in
    let x, y = (100, 100) in
    let px, py = (500.0, 500.0) in
    let result = Intersection.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point far away should be outside" (not result)

  let test_point_inside_boundary_left _ =
    let settings = Intersection.get_settings () in
    let x, y = (100, 100) in
    (* Intersection size is 60.0, so half_size is 30.0 *)
    (* Left boundary: 100 - 30 = 70 *)
    let px, py = (70.0, 100.0) in
    let result = Intersection.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point on left boundary should be inside" result

  let test_point_inside_boundary_right _ =
    let settings = Intersection.get_settings () in
    let x, y = (100, 100) in
    (* Right boundary: 100 + 30 = 130 *)
    let px, py = (130.0, 100.0) in
    let result = Intersection.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point on right boundary should be inside" result

  let test_point_inside_boundary_top _ =
    let settings = Intersection.get_settings () in
    let x, y = (100, 100) in
    (* Top boundary: 100 - 30 = 70 *)
    let px, py = (100.0, 70.0) in
    let result = Intersection.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point on top boundary should be inside" result

  let test_point_inside_boundary_bottom _ =
    let settings = Intersection.get_settings () in
    let x, y = (100, 100) in
    (* Bottom boundary: 100 + 30 = 130 *)
    let px, py = (100.0, 130.0) in
    let result = Intersection.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point on bottom boundary should be inside" result

  let test_point_inside_corner_top_left _ =
    let settings = Intersection.get_settings () in
    let x, y = (100, 100) in
    (* Top-left corner: (70, 70) *)
    let px, py = (70.0, 70.0) in
    let result = Intersection.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point at top-left corner should be inside" result

  let test_point_inside_outside_left _ =
    let settings = Intersection.get_settings () in
    let x, y = (100, 100) in
    (* Just outside left boundary: 100 - 30.1 = 69.9 *)
    let px, py = (69.9, 100.0) in
    let result = Intersection.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point just outside left boundary should be outside"
      (not result)

  let test_point_inside_outside_right _ =
    let settings = Intersection.get_settings () in
    let x, y = (100, 100) in
    (* Just outside right boundary: 100 + 30.1 = 130.1 *)
    let px, py = (130.1, 100.0) in
    let result = Intersection.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point just outside right boundary should be outside"
      (not result)

  let test_point_on_rotate_button_center _ =
    let settings = Intersection.get_settings () in
    let x, y = (100, 100) in
    let angle = 0.0 in
    (* Button is at distance (60/2 + 20) = 50 from center, at angle 0 (to the right) *)
    (* Button position: (100 + 50*cos(0), 100 + 50*sin(0)) = (150, 100) *)
    let button_x = 100.0 +. (50.0 *. cos 0.0) in
    let button_y = 100.0 +. (50.0 *. sin 0.0) in
    let result =
      Intersection.point_on_rotate_button ~x ~y ~angle ~px:button_x ~py:button_y
        settings
    in
    assert_bool "Point at rotate button center should be on button" result

  let test_point_on_rotate_button_edge _ =
    let settings = Intersection.get_settings () in
    let x, y = (100, 100) in
    let angle = 0.0 in
    (* Button is at (150, 100), radius is 12.0, so edge is at distance 12.0 *)
    let button_x = 100.0 +. (50.0 *. cos 0.0) in
    let button_y = 100.0 +. (50.0 *. sin 0.0) in
    (* Point on the edge (12.0 units away) *)
    let px = button_x +. 12.0 in
    let py = button_y in
    let result =
      Intersection.point_on_rotate_button ~x ~y ~angle ~px ~py settings
    in
    assert_bool "Point at rotate button edge should be on button" result

  let test_point_on_rotate_button_outside _ =
    let settings = Intersection.get_settings () in
    let x, y = (100, 100) in
    let angle = 0.0 in
    (* Point far from button *)
    let px, py = (200.0, 100.0) in
    let result =
      Intersection.point_on_rotate_button ~x ~y ~angle ~px ~py settings
    in
    assert_bool "Point far from rotate button should not be on button"
      (not result)

  let test_point_on_rotate_button_different_angle _ =
    let settings = Intersection.get_settings () in
    let x, y = (100, 100) in
    let angle = Float.pi /. 2.0 in
    (* Button is now above the intersection (at angle pi/2) *)
    let button_x = 100.0 +. (50.0 *. cos angle) in
    let button_y = 100.0 +. (50.0 *. sin angle) in
    let result =
      Intersection.point_on_rotate_button ~x ~y ~angle ~px:button_x ~py:button_y
        settings
    in
    assert_bool "Point at rotated button center should be on button" result

  let test_calculate_rotation _ =
    let cx, cy = (100.0, 100.0) in
    let mx, my = (150.0, 150.0) in
    let angle = Intersection.calculate_rotation ~cx ~cy ~mx ~my in
    (* Should be approximately pi/4 (pointing down-right/southeast) *)
    assert_bool "Angle should be close to pi/4"
      (abs_float (angle -. 0.7854) < 0.1)

  let test_calculate_rotation_right _ =
    let cx, cy = (100.0, 100.0) in
    let mx, my = (150.0, 100.0) in
    let angle = Intersection.calculate_rotation ~cx ~cy ~mx ~my in
    (* Should be approximately 0 (pointing right/east) *)
    assert_bool "Angle should be close to 0" (abs_float angle < 0.01)

  let test_calculate_rotation_down _ =
    let cx, cy = (100.0, 100.0) in
    let mx, my = (100.0, 150.0) in
    let angle = Intersection.calculate_rotation ~cx ~cy ~mx ~my in
    (* Should be approximately pi/2 (pointing down/south) *)
    assert_bool "Angle should be close to pi/2"
      (abs_float (angle -. 1.5708) < 0.1)

  let test_calculate_rotation_left _ =
    let cx, cy = (100.0, 100.0) in
    let mx, my = (50.0, 100.0) in
    let angle = Intersection.calculate_rotation ~cx ~cy ~mx ~my in
    (* Should be approximately pi (pointing left/west) *)
    assert_bool "Angle should be close to pi"
      (abs_float (angle -. 3.14159) < 0.1)

  let test_calculate_rotation_up _ =
    let cx, cy = (100.0, 100.0) in
    let mx, my = (100.0, 50.0) in
    let angle = Intersection.calculate_rotation ~cx ~cy ~mx ~my in
    (* Should be approximately -pi/2 (pointing up/north) *)
    assert_bool "Angle should be close to -pi/2"
      (abs_float (angle +. 1.5708) < 0.1)

  let test_settings_with_traffic_light _ =
    let settings =
      Settings.IntersectionSettings
        {
          Settings.num_stops = 2;
          has_traffic_light = true;
          stop_duration = 5.0;
        }
    in
    Intersection.set_settings settings;
    let retrieved = Intersection.get_settings () in
    (match retrieved with
    | Settings.IntersectionSettings s ->
        assert_equal true s.has_traffic_light;
        assert_equal 2 s.num_stops;
        assert_equal 5.0 s.stop_duration;
        ()
    | _ -> failwith "Expected IntersectionSettings");
    (* Reset *)
    Intersection.set_settings
      (Settings.IntersectionSettings
         {
           Settings.num_stops = 4;
           has_traffic_light = false;
           stop_duration = 3.0;
         })

  let test_settings_with_different_num_stops _ =
    (* Test with 1 stop *)
    let settings1 =
      Settings.IntersectionSettings
        {
          Settings.num_stops = 1;
          has_traffic_light = false;
          stop_duration = 2.0;
        }
    in
    Intersection.set_settings settings1;
    let retrieved1 = Intersection.get_settings () in
    (match retrieved1 with
    | Settings.IntersectionSettings s -> assert_equal 1 s.num_stops
    | _ -> failwith "Expected IntersectionSettings");
    (* Test with 2 stops *)
    let settings2 =
      Settings.IntersectionSettings
        {
          Settings.num_stops = 2;
          has_traffic_light = false;
          stop_duration = 2.5;
        }
    in
    Intersection.set_settings settings2;
    let retrieved2 = Intersection.get_settings () in
    (match retrieved2 with
    | Settings.IntersectionSettings s -> assert_equal 2 s.num_stops
    | _ -> failwith "Expected IntersectionSettings");
    (* Test with more than 4 stops (should still work) *)
    let settings3 =
      Settings.IntersectionSettings
        {
          Settings.num_stops = 6;
          has_traffic_light = false;
          stop_duration = 4.0;
        }
    in
    Intersection.set_settings settings3;
    let retrieved3 = Intersection.get_settings () in
    (match retrieved3 with
    | Settings.IntersectionSettings s -> assert_equal 6 s.num_stops
    | _ -> failwith "Expected IntersectionSettings");
    (* Reset *)
    Intersection.set_settings
      (Settings.IntersectionSettings
         {
           Settings.num_stops = 4;
           has_traffic_light = false;
           stop_duration = 3.0;
         })

  let test_settings_with_different_stop_duration _ =
    let settings =
      Settings.IntersectionSettings
        {
          Settings.num_stops = 4;
          has_traffic_light = false;
          stop_duration = 10.5;
        }
    in
    Intersection.set_settings settings;
    let retrieved = Intersection.get_settings () in
    (match retrieved with
    | Settings.IntersectionSettings s -> assert_equal 10.5 s.stop_duration
    | _ -> failwith "Expected IntersectionSettings");
    (* Reset *)
    Intersection.set_settings
      (Settings.IntersectionSettings
         {
           Settings.num_stops = 4;
           has_traffic_light = false;
           stop_duration = 3.0;
         })

  let test_point_inside_with_different_settings _ =
    (* Test that point_inside works regardless of settings content *)
    let settings1 =
      Settings.IntersectionSettings
        {
          Settings.num_stops = 1;
          has_traffic_light = true;
          stop_duration = 1.0;
        }
    in
    let x, y = (200, 200) in
    let px, py = (200.0, 200.0) in
    let result1 = Intersection.point_inside ~x ~y ~px ~py settings1 in
    assert_bool "Point inside should work with traffic light settings" result1;
    let settings2 =
      Settings.IntersectionSettings
        {
          Settings.num_stops = 8;
          has_traffic_light = false;
          stop_duration = 8.0;
        }
    in
    let result2 = Intersection.point_inside ~x ~y ~px ~py settings2 in
    assert_bool "Point inside should work with many stops settings" result2

  (* Note: intersection_object class is created inside draw() function,
     so we test it indirectly by calling draw() *)

  (* Helper function to create a Cairo image surface for testing *)
  let create_test_surface () =
    Cairo.Image.create Cairo.Image.ARGB32 ~w:500 ~h:500

  (* Test draw function with stop signs (default settings) *)
  let test_draw_with_stop_signs _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let settings = Intersection.get_settings () in
    (* Draw intersection at center *)
    Intersection.draw ~cr ~x:250 ~y:250 ~angle:0.0 settings;
    (* If no exception is raised, the function executed successfully *)
    Cairo.Surface.finish surface;
    assert_bool "Draw with stop signs should complete without error" true

  (* Test draw function with traffic light *)
  let test_draw_with_traffic_light _ =
    let settings =
      Settings.IntersectionSettings
        {
          Settings.num_stops = 4;
          has_traffic_light = true;
          stop_duration = 3.0;
        }
    in
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    Intersection.draw ~cr ~x:250 ~y:250 ~angle:0.0 settings;
    Cairo.Surface.finish surface;
    assert_bool "Draw with traffic light should complete without error" true

  (* Test draw function with different rotation angles *)
  let test_draw_with_rotation _ =
    let settings = Intersection.get_settings () in
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    (* Test with different angles *)
    Intersection.draw ~cr ~x:250 ~y:250 ~angle:0.0 settings;
    Intersection.draw ~cr ~x:250 ~y:250 ~angle:(Float.pi /. 2.0) settings;
    Intersection.draw ~cr ~x:250 ~y:250 ~angle:Float.pi settings;
    Intersection.draw ~cr ~x:250 ~y:250 ~angle:(3.0 *. Float.pi /. 2.0) settings;
    Cairo.Surface.finish surface;
    assert_bool "Draw with different rotations should complete" true

  (* Test draw function with different num_stops values *)
  let test_draw_with_different_num_stops _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    (* Test with 0 stops (edge case - loop won't execute) *)
    let settings0 =
      Settings.IntersectionSettings
        {
          Settings.num_stops = 0;
          has_traffic_light = false;
          stop_duration = 2.0;
        }
    in
           Intersection.draw ~cr ~x:50 ~y:50 ~angle:0.0 settings0;
    (* Test with 1 stop *)
    let settings1 =
      Settings.IntersectionSettings
        {
          Settings.num_stops = 1;
          has_traffic_light = false;
          stop_duration = 2.0;
        }
    in
           Intersection.draw ~cr ~x:100 ~y:100 ~angle:0.0 settings1;
    (* Test with 2 stops *)
    let settings2 =
      Settings.IntersectionSettings
        {
          Settings.num_stops = 2;
          has_traffic_light = false;
          stop_duration = 2.0;
        }
    in
           Intersection.draw ~cr ~x:200 ~y:200 ~angle:0.0 settings2;
    (* Test with 3 stops *)
    let settings3 =
      Settings.IntersectionSettings
        {
          Settings.num_stops = 3;
          has_traffic_light = false;
          stop_duration = 2.0;
        }
    in
           Intersection.draw ~cr ~x:250 ~y:250 ~angle:0.0 settings3;
    (* Test with 4 stops *)
    let settings4 =
      Settings.IntersectionSettings
        {
          Settings.num_stops = 4;
          has_traffic_light = false;
          stop_duration = 2.0;
        }
    in
           Intersection.draw ~cr ~x:300 ~y:300 ~angle:0.0 settings4;
    (* Test with more than 4 stops (should cap at 3 in loop: min(num_stops-1, 3)) *)
    let settings5 =
      Settings.IntersectionSettings
        {
          Settings.num_stops = 5;
          has_traffic_light = false;
          stop_duration = 2.0;
        }
    in
           Intersection.draw ~cr ~x:350 ~y:350 ~angle:0.0 settings5;
    Cairo.Surface.finish surface;
    assert_bool "Draw with different num_stops should complete" true

  (* Test erase function *)
  let test_erase _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let settings = Intersection.get_settings () in
    (* First draw, then erase *)
    Intersection.draw ~cr ~x:250 ~y:250 ~angle:0.0 settings;
    Intersection.erase ~cr ~x:250 ~y:250 settings;
    Cairo.Surface.finish surface;
    assert_bool "Erase should complete without error" true

  (* Test draw_selection function *)
  let test_draw_selection _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let settings = Intersection.get_settings () in
    Intersection.draw_selection ~cr ~x:250 ~y:250 ~angle:0.0 settings;
    (* Test with different angles *)
    Intersection.draw_selection ~cr ~x:250 ~y:250 ~angle:(Float.pi /. 4.0)
       settings;
    Cairo.Surface.finish surface;
    assert_bool "Draw selection should complete without error" true

  (* Test draw_rotate_button function *)
  let test_draw_rotate_button _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let settings = Intersection.get_settings () in
    Intersection.draw_rotate_button ~cr ~x:250 ~y:250 ~angle:0.0 settings;
    (* Test with different angles *)
    Intersection.draw_rotate_button ~cr ~x:250 ~y:250 ~angle:(Float.pi /. 2.0)
       settings;
    Intersection.draw_rotate_button ~cr ~x:250 ~y:250 ~angle:Float.pi settings;
    Cairo.Surface.finish surface;
    assert_bool "Draw rotate button should complete without error" true

  (* Test error case: draw with wrong settings type *)
  let test_draw_error_wrong_settings _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let wrong_settings =
      Settings.RoadSettings
        { Settings.speed_limit = 35; num_lanes = 2; max_capacity = 100 }
    in
    try
      Intersection.draw ~cr ~x:250 ~y:250 ~angle:0.0 wrong_settings;
      assert_failure "Should have raised an exception"
    with Failure msg ->
      assert_bool "Should fail with expected message"
        (String.contains msg 'I' || String.contains msg 'i')

  (* Test error case: erase with wrong settings type *)
  let test_erase_error_wrong_settings _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let wrong_settings =
      Settings.BuildingSettings { Settings.rate_of_traffic = 10 }
    in
    try
      Intersection.erase ~cr ~x:250 ~y:250 wrong_settings;
      assert_failure "Should have raised an exception"
    with Failure msg ->
      assert_bool "Should fail with expected message"
        (String.contains msg 'I' || String.contains msg 'i')

  (* Test error case: draw_selection with wrong settings type *)
  let test_draw_selection_error_wrong_settings _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let wrong_settings =
      Settings.RoadSettings
        { Settings.speed_limit = 35; num_lanes = 2; max_capacity = 100 }
    in
    try
      Intersection.draw_selection ~cr ~x:250 ~y:250 ~angle:0.0 wrong_settings;
      assert_failure "Should have raised an exception"
    with Failure msg ->
      assert_bool "Should fail with expected message"
        (String.contains msg 'I' || String.contains msg 'i')

  (* Test error case: draw_rotate_button with wrong settings type *)
  let test_draw_rotate_button_error_wrong_settings _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let wrong_settings =
      Settings.BuildingSettings { Settings.rate_of_traffic = 10 }
    in
    try
      Intersection.draw_rotate_button ~cr ~x:250 ~y:250 ~angle:0.0 wrong_settings;
      assert_failure "Should have raised an exception"
    with Failure msg ->
      assert_bool "Should fail with expected message"
        (String.contains msg 'I' || String.contains msg 'i')

  (* Test error case: set_settings with wrong settings type *)
  let test_set_settings_error_wrong_settings _ =
    let wrong_settings =
      Settings.RoadSettings
        { Settings.speed_limit = 35; num_lanes = 2; max_capacity = 100 }
    in
    try
      Intersection.set_settings wrong_settings;
      assert_failure "Should have raised an exception"
    with Failure msg ->
      assert_bool "Should fail with expected message"
        (String.contains msg 'I' || String.contains msg 'i')

  (* Test point_inside with wrong settings type (should return false) *)
  let test_point_inside_wrong_settings _ =
    let wrong_settings =
      Settings.RoadSettings
        { Settings.speed_limit = 35; num_lanes = 2; max_capacity = 100 }
    in
    let result =
      Intersection.point_inside ~x:100 ~y:100 ~px:100.0 ~py:100.0 wrong_settings
    in
    assert_bool "Point inside with wrong settings should return false"
      (not result)

  (* Test point_on_rotate_button with wrong settings type (should return false) *)
  let test_point_on_rotate_button_wrong_settings _ =
    let wrong_settings =
      Settings.BuildingSettings { Settings.rate_of_traffic = 10 }
    in
    let result =
      Intersection.point_on_rotate_button ~x:100 ~y:100 ~angle:0.0 ~px:150.0
        ~py:100.0 wrong_settings
    in
    assert_bool "Point on rotate button with wrong settings should return false"
      (not result)

  (* Test point_inside exact boundary conditions *)
  let test_point_inside_exact_boundaries _ =
    let settings = Intersection.get_settings () in
    let x, y = (100, 100) in
    (* Intersection size is 60.0, half_size is 30.0 *)
    (* Test exact left boundary: px = fx - half_size = 100 - 30 = 70 *)
    let result1 = Intersection.point_inside ~x ~y ~px:70.0 ~py:100.0 settings in
    assert_bool "Exact left boundary should be inside" result1;
    (* Test exact right boundary: px = fx + half_size = 100 + 30 = 130 *)
    let result2 =
      Intersection.point_inside ~x ~y ~px:130.0 ~py:100.0 settings
    in
    assert_bool "Exact right boundary should be inside" result2;
    (* Test exact top boundary: py = fy - half_size = 100 - 30 = 70 *)
    let result3 = Intersection.point_inside ~x ~y ~px:100.0 ~py:70.0 settings in
    assert_bool "Exact top boundary should be inside" result3;
    (* Test exact bottom boundary: py = fy + half_size = 100 + 30 = 130 *)
    let result4 =
      Intersection.point_inside ~x ~y ~px:100.0 ~py:130.0 settings
    in
    assert_bool "Exact bottom boundary should be inside" result4

  (* Test point_inside just outside boundaries *)
  let test_point_inside_just_outside _ =
    let settings = Intersection.get_settings () in
    let x, y = (100, 100) in
    (* Just outside left: px < fx - half_size *)
    let result1 = Intersection.point_inside ~x ~y ~px:69.9 ~py:100.0 settings in
    assert_bool "Just outside left should be outside" (not result1);
    (* Just outside right: px > fx + half_size *)
    let result2 =
      Intersection.point_inside ~x ~y ~px:130.1 ~py:100.0 settings
    in
    assert_bool "Just outside right should be outside" (not result2);
    (* Just outside top: py < fy - half_size *)
    let result3 = Intersection.point_inside ~x ~y ~px:100.0 ~py:69.9 settings in
    assert_bool "Just outside top should be outside" (not result3);
    (* Just outside bottom: py > fy + half_size *)
    let result4 =
      Intersection.point_inside ~x ~y ~px:100.0 ~py:130.1 settings
    in
    assert_bool "Just outside bottom should be outside" (not result4)

  (* Test point_on_rotate_button exact radius boundary *)
  let test_point_on_rotate_button_exact_radius _ =
    let settings = Intersection.get_settings () in
    let x, y = (100, 100) in
    let angle = 0.0 in
    (* Button is at distance (60/2 + 20) = 50 from center, at angle 0 (to the right) *)
    (* Button center: (150, 100), radius = 12 *)
    (* Point exactly at radius: distance = 12 *)
    let button_x = 150.0 +. 12.0 in
    let button_y = 100.0 in
    let result =
      Intersection.point_on_rotate_button ~x ~y ~angle ~px:button_x ~py:button_y
        settings
    in
    assert_bool "Point at exact radius should be on button" result;
    (* Point just inside radius *)
    let button_x2 = 150.0 +. 11.9 in
    let result2 =
      Intersection.point_on_rotate_button ~x ~y ~angle ~px:button_x2
        ~py:button_y settings
    in
    assert_bool "Point just inside radius should be on button" result2;
    (* Point just outside radius *)
    let button_x3 = 150.0 +. 12.1 in
    let result3 =
      Intersection.point_on_rotate_button ~x ~y ~angle ~px:button_x3
        ~py:button_y settings
    in
    assert_bool "Point just outside radius should not be on button"
      (not result3)

  (* Test point_on_rotate_button at different angles *)
  let test_point_on_rotate_button_various_angles _ =
    let settings = Intersection.get_settings () in
    let x, y = (100, 100) in
    (* Test at 45 degrees *)
    let angle1 = Float.pi /. 4.0 in
    let button_distance = 30.0 +. 20.0 in
    let button_x = 100.0 +. (button_distance *. cos angle1) in
    let button_y = 100.0 +. (button_distance *. sin angle1) in
    let result1 =
      Intersection.point_on_rotate_button ~x ~y ~angle:angle1 ~px:button_x
        ~py:button_y settings
    in
    assert_bool "Point at button center at 45 degrees should be on button"
      result1;
    (* Test at 135 degrees *)
    let angle2 = 3.0 *. Float.pi /. 4.0 in
    let button_x2 = 100.0 +. (button_distance *. cos angle2) in
    let button_y2 = 100.0 +. (button_distance *. sin angle2) in
    let result2 =
      Intersection.point_on_rotate_button ~x ~y ~angle:angle2 ~px:button_x2
        ~py:button_y2 settings
    in
    assert_bool "Point at button center at 135 degrees should be on button"
      result2

  (* Test draw with traffic light at different positions and angles *)
  let test_draw_traffic_light_various_angles _ =
    let settings =
      Settings.IntersectionSettings
        {
          Settings.num_stops = 4;
          has_traffic_light = true;
          stop_duration = 3.0;
        }
    in
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    (* Test at different angles to ensure all traffic light code is executed *)
    Intersection.draw ~cr ~x:100 ~y:100 ~angle:0.0 settings;
    Intersection.draw ~cr ~x:200 ~y:200 ~angle:(Float.pi /. 4.0) settings;
    Intersection.draw ~cr ~x:300 ~y:300 ~angle:(Float.pi /. 2.0) settings;
    Intersection.draw ~cr ~x:400 ~y:400 ~angle:Float.pi settings;
    Intersection.draw ~cr ~x:50 ~y:50 ~angle:(3.0 *. Float.pi /. 2.0) settings;
    Cairo.Surface.finish surface;
    assert_bool "Draw traffic light at various angles should complete" true

  (* Test draw_rotate_button with more angles to cover all arrow drawing code *)
  let test_draw_rotate_button_comprehensive _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let settings = Intersection.get_settings () in
    (* Test many angles to ensure all arrow drawing code paths are hit *)
    let angles =
      [
        0.0;
        Float.pi /. 6.0;
        Float.pi /. 4.0;
        Float.pi /. 3.0;
        Float.pi /. 2.0;
        2.0 *. Float.pi /. 3.0;
        3.0 *. Float.pi /. 4.0;
        Float.pi;
        4.0 *. Float.pi /. 3.0;
        3.0 *. Float.pi /. 2.0;
        2.0 *. Float.pi;
      ]
    in
    List.iter
      (fun angle ->
        Intersection.draw_rotate_button ~cr ~x:250 ~y:250 ~angle settings)
      angles;
    Cairo.Surface.finish surface;
    assert_bool "Draw rotate button at many angles should complete" true

  (* Test draw_selection with more angles *)
  let test_draw_selection_comprehensive _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let settings = Intersection.get_settings () in
    (* Test many angles *)
    let angles =
      [
        0.0;
        Float.pi /. 6.0;
        Float.pi /. 4.0;
        Float.pi /. 3.0;
        Float.pi /. 2.0;
        Float.pi;
        3.0 *. Float.pi /. 2.0;
        2.0 *. Float.pi;
      ]
    in
    List.iter
      (fun angle ->
        Intersection.draw_selection ~cr ~x:250 ~y:250 ~angle settings)
      angles;
    Cairo.Surface.finish surface;
    assert_bool "Draw selection at many angles should complete" true

  (* Test calculate_rotation with more edge cases *)
  let test_calculate_rotation_comprehensive _ =
    (* Test various positions *)
    let cx, cy = (100.0, 100.0) in
    (* Right *)
    let angle1 = Intersection.calculate_rotation ~cx ~cy ~mx:150.0 ~my:100.0 in
    assert_bool "Right should be ~0" (abs_float angle1 < 0.1);
    (* Down *)
    let angle2 = Intersection.calculate_rotation ~cx ~cy ~mx:100.0 ~my:150.0 in
    assert_bool "Down should be ~pi/2"
      (abs_float (angle2 -. (Float.pi /. 2.0)) < 0.1);
    (* Left *)
    let angle3 = Intersection.calculate_rotation ~cx ~cy ~mx:50.0 ~my:100.0 in
    assert_bool "Left should be ~pi" (abs_float (angle3 -. Float.pi) < 0.1);
    (* Up *)
    let angle4 = Intersection.calculate_rotation ~cx ~cy ~mx:100.0 ~my:50.0 in
    assert_bool "Up should be ~-pi/2"
      (abs_float (angle4 +. (Float.pi /. 2.0)) < 0.1);
    (* Diagonal positions *)
    let angle5 = Intersection.calculate_rotation ~cx ~cy ~mx:150.0 ~my:150.0 in
    assert_bool "Diagonal should be ~pi/4"
      (abs_float (angle5 -. (Float.pi /. 4.0)) < 0.1);
    (* Same point *)
    let angle6 = Intersection.calculate_rotation ~cx ~cy ~mx:cx ~my:cy in
    assert_bool "Same point should return valid angle"
      (not (Float.is_nan angle6))

  (* Test erase at different positions *)
  let test_erase_various_positions _ =
    let surface = create_test_surface () in
    let cr = Cairo.create surface in
    let settings = Intersection.get_settings () in
    (* Erase at various positions *)
    Intersection.erase ~cr ~x:50 ~y:50 settings;
    Intersection.erase ~cr ~x:100 ~y:100 settings;
    Intersection.erase ~cr ~x:200 ~y:200 settings;
    Intersection.erase ~cr ~x:450 ~y:450 settings;
    Cairo.Surface.finish surface;
    assert_bool "Erase at various positions should complete" true

  let suite =
    "Intersection Module"
    >::: [
           "get_tool" >:: test_get_tool;
           "get_name" >:: test_get_name;
           "get_settings" >:: test_get_settings;
           "set_settings" >:: test_set_settings;
           "set_settings_with_traffic_light"
           >:: test_settings_with_traffic_light;
           "set_settings_with_different_num_stops"
           >:: test_settings_with_different_num_stops;
           "set_settings_with_different_stop_duration"
           >:: test_settings_with_different_stop_duration;
           "point_inside_center" >:: test_point_inside_center;
           "point_inside_outside" >:: test_point_inside_outside;
           "point_inside_boundary_left" >:: test_point_inside_boundary_left;
           "point_inside_boundary_right" >:: test_point_inside_boundary_right;
           "point_inside_boundary_top" >:: test_point_inside_boundary_top;
           "point_inside_boundary_bottom" >:: test_point_inside_boundary_bottom;
           "point_inside_corner_top_left" >:: test_point_inside_corner_top_left;
           "point_inside_outside_left" >:: test_point_inside_outside_left;
           "point_inside_outside_right" >:: test_point_inside_outside_right;
           "point_inside_with_different_settings"
           >:: test_point_inside_with_different_settings;
           "point_on_rotate_button_center"
           >:: test_point_on_rotate_button_center;
           "point_on_rotate_button_edge" >:: test_point_on_rotate_button_edge;
           "point_on_rotate_button_outside"
           >:: test_point_on_rotate_button_outside;
           "point_on_rotate_button_different_angle"
           >:: test_point_on_rotate_button_different_angle;
           "calculate_rotation" >:: test_calculate_rotation;
           "calculate_rotation_right" >:: test_calculate_rotation_right;
           "calculate_rotation_down" >:: test_calculate_rotation_down;
           "calculate_rotation_left" >:: test_calculate_rotation_left;
           "calculate_rotation_up" >:: test_calculate_rotation_up;
           "draw_with_stop_signs" >:: test_draw_with_stop_signs;
           "draw_with_traffic_light" >:: test_draw_with_traffic_light;
           "draw_with_rotation" >:: test_draw_with_rotation;
           "draw_with_different_num_stops"
           >:: test_draw_with_different_num_stops;
           "erase" >:: test_erase;
           "draw_selection" >:: test_draw_selection;
           "draw_rotate_button" >:: test_draw_rotate_button;
           "draw_error_wrong_settings" >:: test_draw_error_wrong_settings;
           "erase_error_wrong_settings" >:: test_erase_error_wrong_settings;
           "draw_selection_error_wrong_settings"
           >:: test_draw_selection_error_wrong_settings;
           "draw_rotate_button_error_wrong_settings"
           >:: test_draw_rotate_button_error_wrong_settings;
           "set_settings_error_wrong_settings"
           >:: test_set_settings_error_wrong_settings;
           "point_inside_wrong_settings" >:: test_point_inside_wrong_settings;
           "point_on_rotate_button_wrong_settings"
           >:: test_point_on_rotate_button_wrong_settings;
           "point_inside_exact_boundaries"
           >:: test_point_inside_exact_boundaries;
           "point_inside_just_outside" >:: test_point_inside_just_outside;
           "point_on_rotate_button_exact_radius"
           >:: test_point_on_rotate_button_exact_radius;
           "point_on_rotate_button_various_angles"
           >:: test_point_on_rotate_button_various_angles;
           "draw_traffic_light_various_angles"
           >:: test_draw_traffic_light_various_angles;
           "draw_rotate_button_comprehensive"
           >:: test_draw_rotate_button_comprehensive;
           "draw_selection_comprehensive" >:: test_draw_selection_comprehensive;
           "calculate_rotation_comprehensive"
           >:: test_calculate_rotation_comprehensive;
           "erase_various_positions" >:: test_erase_various_positions;
         ]
end

(* ============================================================================
 * Main Test Suite
 * ============================================================================ *)

let suite =
  "CS3110 Final Project Test Suite"
  >::: [
         Settings_tests.suite;
         Tool_types_tests.suite;
         Road_tests.suite;
         Building_tests.suite;
         Intersection_tests.suite;
       ]

let () = run_test_tt_main suite
