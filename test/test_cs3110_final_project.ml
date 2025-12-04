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
           "intersection_settings_creation" >:: test_intersection_settings_creation;
           "settings_variant_building" >:: test_settings_variant_building;
           "settings_variant_road" >:: test_settings_variant_road;
           "settings_variant_intersection" >:: test_settings_variant_intersection;
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
    let x, y = 100, 100 in
    let px, py = 100.0, 100.0 in
    let result = Road.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point at center should be inside" result

  let test_point_inside_outside _ =
    let settings = Road.get_settings () in
    let x, y = 100, 100 in
    let px, py = 500.0, 500.0 in
    let result = Road.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point far away should be outside" (not result)

  let test_calculate_rotation _ =
    let cx, cy = 100.0, 100.0 in
    let mx, my = 150.0, 100.0 in
    let angle = Road.calculate_rotation ~cx ~cy ~mx ~my in
    (* Should be approximately 0 (pointing right/east) *)
    assert_bool "Angle should be close to 0" (abs_float angle < 0.01)

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
    let x, y = 100, 100 in
    let px, py = 100.0, 100.0 in
    let result = Building.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point at center should be inside" result

  let test_point_inside_outside _ =
    let settings = Building.get_settings () in
    let x, y = 100, 100 in
    let px, py = 500.0, 500.0 in
    let result = Building.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point far away should be outside" (not result)

  let test_calculate_rotation _ =
    let cx, cy = 100.0, 100.0 in
    let mx, my = 100.0, 150.0 in
    let angle = Building.calculate_rotation ~cx ~cy ~mx ~my in
    (* Should be approximately pi/2 (pointing down/south) *)
    assert_bool "Angle should be close to pi/2" (abs_float (angle -. 1.5708) < 0.1)

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
    let x, y = 100, 100 in
    let px, py = 100.0, 100.0 in
    let result = Intersection.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point at center should be inside" result

  let test_point_inside_outside _ =
    let settings = Intersection.get_settings () in
    let x, y = 100, 100 in
    let px, py = 500.0, 500.0 in
    let result = Intersection.point_inside ~x ~y ~px ~py settings in
    assert_bool "Point far away should be outside" (not result)

  let test_calculate_rotation _ =
    let cx, cy = 100.0, 100.0 in
    let mx, my = 150.0, 150.0 in
    let angle = Intersection.calculate_rotation ~cx ~cy ~mx ~my in
    (* Should be approximately pi/4 (pointing down-right/southeast) *)
    assert_bool "Angle should be close to pi/4"
      (abs_float (angle -. 0.7854) < 0.1)

  let suite =
    "Intersection Module"
    >::: [
           "get_tool" >:: test_get_tool;
           "get_name" >:: test_get_name;
           "get_settings" >:: test_get_settings;
           "set_settings" >:: test_set_settings;
           "point_inside_center" >:: test_point_inside_center;
           "point_inside_outside" >:: test_point_inside_outside;
           "calculate_rotation" >:: test_calculate_rotation;
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

