
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
%%

-module(cargo_aggregate).

-behaviour(gen_server).

%% API functions
-export([start_link/0]).
-export([create/4]).
-export([assign_to_route/2]).
-export([process_unsaved_changes/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
	id = undefined :: undefined | string(),
	changes=[] :: list()
}).

-opaque state() :: #state{}.
-export_type([state/0]).


%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the server
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc create a new cargo
-spec create(pid(),string(),string(),string()) -> ok.
create(Pid,Id,Origin,Destination) ->
	gen_server:cast(get_child_pid(Pid),{create,Id,Origin,Destination}).

%% @doc set itinerary to exitsing cargo
-spec create(pid(),list()) -> ok.
assign_to_route(Pid,Legs) ->
	gen_server:cast(get_child_pid(Pid),{assign_to_route,Legs}).

-spec process_unsaved_changes(pid(),fun()) -> ok.
process_unsaved_changes(Pid, Saver)->
	gen_server:cast(get_child_pid(Pid),{process_unsaved_changes,Saver}).

-spec load_from_history(pid(),list())
load_from_history(Pid, Events)-> ok.
	gen_server:cast(get_child_pid(Pid),{load_from_history,Events}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
init([]) ->
    {ok, #state{},45000}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
% apply event cargo_created
handle_cast({create,Id,Origin,Destination},State) ->
	{noreply,
	 apply_new_event({cargo_created,Id,Origin,Destination,erlang:localtime()}, 
	 					State#)
	};
% apply event assign_to_route
handle_cast({assign_to_route,Legs},State) ->
	{noreply,
	 apply_new_event({route_assigned_to_cargo,Legs}, 
	 					State)
	};
% persist changes
handle_cast({process_unsaved_changes, Saver},State) ->
	Id = State#state.id,
	Saver(Id, lists:reverse(State#state.changes)),
	{noreply,
	 State#state{changes=[]}
	};

% append loaded events 
handle_cast({load_from_history, Events},State) ->
	NewState = apply_many_events(Events, #state{}),
	{noreply,
	 NewState
	};

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

%% @doc
%% put a new event inc. payload to the list of changes of a conrete
%% cargo aggregate
-spec apply_new_event(tuple(), state())->state().
apply_new_event(Event, State) ->
	NewState = apply_event(Event, State),
	CombinedChanges = [Event] ++ NewState#state.changes,
	NewState#state{changes=CombinedChanges}.

-spec apply_many_events(list(), state())->state().
apply_many_events([], State) ->
	State;
apply_many_events([Event|Rest], State) ->
	NewState = apply_event(Event, State),
	apply_many_events(Rest, NewState).

-spec apply_event(event(), state())->state().
apply_event({cargo_created,Id,_Origin,_Destination,_Date_created},State)->
	gproc:reg({n, l, {cargo_aggregate, Id}}),
	State#state{id=Id}.

apply_event(_Event, State)->
	State. % For some events, we don't have state to mutate
%% @doc
%% finds the pid of a cargo_aggregate process from its supervisor process
-spec get_child_pid(pid()) -> pid().
get_child_pid(ParentPid) ->
	[{_,ChildPid,_,_}]=supervisor:which_children(ParentPid),
	ChildPid.

