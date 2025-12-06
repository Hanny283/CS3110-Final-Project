(* settings.ml - central definitions for tool configuration *)

type building_settings = { mutable rate_of_traffic : int }

type road_settings = {
  mutable speed_limit : int;
  mutable num_lanes : int;
  mutable max_capacity : int;
}

type intersection_settings = {
  mutable num_stops : int;
  mutable has_traffic_light : bool;
  mutable stop_duration : float;
}

type settings =
  | BuildingSettings of building_settings
  | RoadSettings of road_settings
  | IntersectionSettings of intersection_settings
