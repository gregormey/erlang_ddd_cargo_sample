%% Copyright (c) 2016, Gregor Meyenberg  <gregor@meyenberg.de>
%% %%
%% %% Permission to use, copy, modify, and/or distribute this software for any
%% %% purpose with or without fee is hereby granted, provided that the above
%% %% copyright notice and this permission notice appear in all copies.
%% %%
%% %% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% %% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% %% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% %% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% %% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% %% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% %% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(graph_traversal_service).

-export([find_shortest_path/2]).

-record(edge, {
	fromUnLocode = undefined :: undefined |string(),
	toUnLocode = undefined :: undefined |string(),
	fromDate =undefined :: undefined |tuple(),
	toDate =undefined :: undefined |tuple()
}).


%% @doc returms a list of all possible paths for the given locations
-spec find_shortest_path(string(),string())-> list(). 
find_shortest_path(FromUnLocode,ToUnLocode)->
	Routes=filter_routes(FromUnLocode,ToUnLocode),


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc calculates the trsit path for a route
-spec route_to_transit_path(string(),string(),number(),list())-> list(). 
route_to_transit_path(Origin,Destination,DestinationDuration,[])->
	LoadDay=random_load_day(4),
	LoadTime=random_time(),
	[generate_edge(Origin,LoadDay,Destination,DestinationDuration)];

route_to_transit_path(Origin,Destination,DestinationDuration,Stopps)->
	LoadDay=random_load_day(4),
	LoadTime=random_time(),
	
generate_edges(CurrentLocation,NextLocation,Duration) 

generate_edge(FromUnLocode,LoadDay,ToUnLocode,DestinationDuration)->
	UnLoadDay=add_days(LoadDay,DestinationDuration),
	UnLoadTime=random_time(),
	#edge{fromUnLocode=FromUnLocode,
			toUnLocode=ToUnLocode,
			fromDate={LoadDay,LoadTime},
			toDate={UnLoadDay,UnLoadTime}
		}.

%% @doc generates a random time
-spec random_time()->tuple().
	{random_0(23),random_0(59),random_0(59)}

%% @doc generates a random number and includes 0
-spec random_0(N)->number().
	RandomNumber=random:uniform(N)-random:uniform(N),
	if  
		RandomNumber < 0 -> RandomNumber *(-1);
		_ -> RandomNumber

	end.


%% @doc retruns a random day which could be between today and 4 days in future
-spec random_load_day(number())-> tuple(). 
random_load_day(RanDays) ->
	{Today,Time}=calendar:local_time(),
	add_day(Today,random:uniform(4)).

%% @doc adds a number of days to a date and returns a new one
-spec add_days(tuple(),number())->tuple().
add_days(Day,Days)->
	New = calendar:date_to_gregorian_days(Today) + random:uniform(4),
	calendar:gregorian_days_to_date(New).

%% @doc filters list of routes by From an To Location
-spec filter_routes(string(),string())-> no_routes | list().
filter_routes(FromUnLocode,ToUnLocode) ->
	case route_list() of
		no_routes -> no_routes;
		Routes -> [Route || Route -> Routes , 
				has_route_origin_destination(Route,FromUnLocode,ToUnLocode)]
    end.

%% @doc check if a route has the given origin and destination
-spec has_route_origin_destination(tuple(),string(),string())-> true | false.
has_route_origin_destination(Route,Orign,Destination)
	[{origin, RouteOrigin},
	{destination,RouteDestination},
	_,_]]=Route,
	Orign==RouteOrigin and Destination==RouteDestination.


%% @doc reads list of itineraries as rout list
-spec rout_list()-> list() | no_routes. 
route_list() -> 
	case config_helper:get_term_from_config_file("itineraries.config") of 
		{ok,[Routes]}->Routes;
		{error,_} -> no_routes
	end.
