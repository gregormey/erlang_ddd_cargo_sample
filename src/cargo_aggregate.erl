
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
-spec create(pid(),string(),string(),string()) -> ok.
assign_to_route(Pid,Id,Legs) ->
	gen_server:cast(get_child_pid(Pid),{assign_to_route,Id,Legs}).

-spec process_unsaved_changes(pid(),fun()) -> ok.
process_unsaved_changes(Pid, Saver)->
	gen_server:cast(get_child_pid(Pid),{process_unsaved_changes,Saver}).


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
	 					State#state{id=Id})
	};
% apply event assign_to_route
handle_cast({assign_to_route,Id,Legs},State) ->
	{noreply,
	 apply_new_event({route_assigned_to_cargo,Id,Legs}, 
	 					State#state{id=Id})
	};
% persist changes
handle_cast({process_unsaved_changes, Saver},State) ->
	Id = State#state.id,
	Saver(Id, lists:reverse(State#state.changes)),
	{noreply,
	 State#state{changes=[]}
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
	CombinedChanges = [Event] ++ State#state.changes,
	State#state{changes=CombinedChanges}.
%% @doc
%% finds the pid of a cargo_aggregate process from its supervisor process
-spec get_child_pid(pid()) -> pid().
get_child_pid(ParentPid) ->
	[{_,ChildPid,_,_}]=supervisor:which_children(ParentPid),
	ChildPid.

