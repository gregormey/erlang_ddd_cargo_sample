
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
-export([create/3]).
-export([process_unsaved_changes/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(route_specification,{
	origin = undefined :: string(),
	destination = undefined :: string()
}).

-record(state, {
	id = undefined :: string(),
	route_specification = #route_specification{},
	date_created =undefined :: tuple(),
	changes=[] :: list()
}).

-opaque route_specification() :: #route_specification{}.
-export_type([route_specification/0]).

-opaque state() :: #state{}.
-export_type([state/0]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(Pid,Origin,Destination) ->
	gen_server:call(Pid,{create,Origin,Destination}).

process_unsaved_changes(Pid, Saver)->
	gen_server:call(Pid,{process_unsaved_changes,Saver}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{},45000}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({create,Origin,Destination},_From,State) ->
	{reply,ok,
	 apply_new_event({cargo_created,Origin,Destination, erlang:localtime()}, State)
	};

handle_call({process_unsaved_changes, Saver},_From,State) ->
	Id = State#state.id,
	Saver(Id, lists:reverse(State#state.changes)),
	{reply,ok,
	 State#state{changes=[]}
	};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
	{stop, timeout, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

apply_new_event(Event, State) ->
	NewState = apply_event(Event, State),
	CombinedChanges = [Event] ++ NewState#state.changes,
	NewState#state{changes=CombinedChanges}.


apply_event({cargo_created, Origin,Destination,DateCreated}, State) ->
	State#state{
		id=cargo_repository:generate_tracking_id(), 
		date_created = DateCreated, 
		route_specification=#route_specification{origin=Origin,destination=Destination}
	}.

