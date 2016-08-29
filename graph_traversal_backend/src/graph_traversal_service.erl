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

-behaviour(gen_server).

%% API functions
-export([start_link/0]).
-export([find_shortest_path/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(edge, {
	fromUnLocode = undefined :: undefined |string(),
	toUnLocode = undefined :: undefined |string(),
	fromDate =undefined :: undefined |tuple(),
	toDate =undefined :: undefined |tuple()
}).

-record(route, {
	origin = undefined :: undefined |string(),
	destination = undefined :: undefined |string(),
	duration =undefined :: undefined |number(),
	stopps =undefined :: undefined |list()
}).

-opaque route() :: #route{}.
-export_type([route/0]).

-opaque edge() :: #edge{}.
-export_type([edge/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the server
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).



%% @doc returns a list of all possible paths for the given locations
-spec find_shortest_path(string(),string())-> list(). 
find_shortest_path(FromUnLocode,ToUnLocode)->
	gen_server:call(?MODULE,{find_shortest_path,FromUnLocode,ToUnLocode}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
init([]) ->
    {ok, ?MODULE}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
% returns a list of all possible paths for the given locations
handle_call({find_shortest_path,FromUnLocode,ToUnLocode}, _From, State) ->
  Routes=filter_routes(FromUnLocode,ToUnLocode),
  {reply, 
	[route_to_transit_path(Route) || Route <-Routes ]
  ,State};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
handle_info(timeout, State) ->
	{stop, timeout, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc calculates the trsit path for a route
-spec route_to_transit_path(route())-> list(). 
route_to_transit_path(#route{origin=Origin,destination=Destination,duration=Duration,stopps=[]})->
	LoadDateTime={random_load_day(4),random_time()},
	[generate_edge(Origin,LoadDateTime,Destination,Duration)];

route_to_transit_path(Route=#route{origin=Origin,stopps=[Stop|Stopps]})->
	LoadDateTime={random_load_day(4),random_time()},
	{StopLocation,StopDuration}=Stop,
	next_edge([generate_edge(Origin,LoadDateTime,StopLocation,StopDuration)],Route,Stopps).

%% @doc calculates the trsit path to the next stop of a route
-spec next_edge(list(),route(),list())-> list().
next_edge(Edges,Route,[Stop|Stopps])->
	LastEdge=lists:last(Edges),
	{ToDay,_}=LastEdge#edge.toDate,
	LoadDateTime={add_days(ToDay,1),random_time()},
	{StopLocation,StopDuration}=Stop,
	next_edge(Edges++[generate_edge(LastEdge#edge.toUnLocode,LoadDateTime,StopLocation,StopDuration)],Route,Stopps);

next_edge(Edges,#route{destination=Destination,duration=Duration},[]) ->
	LastEdge=lists:last(Edges),
	{ToDay,_}=LastEdge#edge.toDate,
	LoadDateTime={add_days(ToDay,1),random_time()},
	Edges++[generate_edge(LastEdge#edge.toUnLocode,LoadDateTime,Destination,Duration)].

%% @doc create a edge of a route by the given parameters
-spec generate_edge(string(),tuple(),string(),number())-> edge().	
generate_edge(FromUnLocode,LoadDateTime,ToUnLocode,DestinationDuration)->
	{LoadDay,LoadTime}=LoadDateTime,
	UnLoadDay=add_days(LoadDay,DestinationDuration),
	UnLoadTime=random_time(),
	#edge{fromUnLocode=FromUnLocode,
			toUnLocode=ToUnLocode,
			fromDate={LoadDay,LoadTime},
			toDate={UnLoadDay,UnLoadTime}
		}.

%% @doc generates a random time
-spec random_time()->tuple().
random_time()->
	{random_0(23),random_0(59),random_0(59)}.

%% @doc generates a random number and includes 0
-spec random_0(number())->number().
random_0(N)->
	RandomNumber=random:uniform(N)-random:uniform(N),
	if  
		RandomNumber < 0 -> RandomNumber *(-1);
		true -> RandomNumber

	end.


%% @doc retruns a random day which could be between today and 4 days in future
-spec random_load_day(number())-> tuple(). 
random_load_day(RanDays) ->
	{Today,_}=calendar:local_time(),
	add_days(Today,random:uniform(RanDays)).

%% @doc adds a number of days to a date and returns a new one
-spec add_days(tuple(),number())->tuple().
add_days(Day,Days)->
	New = calendar:date_to_gregorian_days(Day) + random:uniform(Days),
	calendar:gregorian_days_to_date(New).

%% @doc filters list of routes by From an To Location
-spec filter_routes(string(),string())-> no_routes | list().
filter_routes(FromUnLocode,ToUnLocode) ->
	case route_list() of
		no_routes -> no_routes;
		Routes -> [Route || Route <- Routes , 
				has_route_origin_destination(Route,FromUnLocode,ToUnLocode)]
    end.

%% @doc check if a route has the given origin and destination
-spec has_route_origin_destination(route(),string(),string())-> true | false.
has_route_origin_destination(#route{origin=RouteOrigin,destination=RouteDestination},Orign,Destination)->
	(Orign==RouteOrigin) and (Destination==RouteDestination).


%% @doc reads list of itineraries as rout list
-spec route_list()-> list() | no_routes. 
route_list() -> 
	case config_helper:get_term_from_config_file("itineraries.config") of 
		{ok,[ConfigRoutes]}->[generate_route(ConfigRoute)||ConfigRoute<-ConfigRoutes];
		{error,_} -> no_routes
	end.

%% @doc mapps a configured route to a roue record
-spec generate_route(list())-> route().   
generate_route(ConfigRoute)->
	[
        {origin, Origin},
        {destination,Destination},
        {duration, Duration},
        {stopps,Stopps}
    ] = ConfigRoute,
    #route{origin=Origin,destination=Destination,duration=Duration,stopps=Stopps}.

