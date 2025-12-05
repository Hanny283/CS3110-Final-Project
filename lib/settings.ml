(*rate of traffic for buildings*)
type building_settings = { mutable rate_of_traffic : int }

(*Settings for roads. Speed_limit is a int that represents the fastest a car 
can go, num_lanes is a int that represents how many lanes the road has, 
max_capacity is an int that represents the max number of cars each road can 
have.*)
type road_settings = {
  mutable speed_limit : int;
  mutable num_lanes : int;
  mutable max_capacity : int;
}

(*Setings for intersection. num_stops is the number of stop signs that is
represented as an int, has_traffic_light is a bool true we add traffic lights
(not implemented) or false no traffic lights. stop_duration is a float
that represents how long we want a car to stop at a stop sign.*)
type intersection_settings = {
  mutable num_stops : int;
  mutable has_traffic_light : bool;
  mutable stop_duration : float;
}

(*choosing the type of settings you want*)
type settings =
  | BuildingSettings of building_settings
  | RoadSettings of road_settings
  | IntersectionSettings of intersection_settings
