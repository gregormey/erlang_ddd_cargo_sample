
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

-module(booking_command_handler).

-behaviour(gen_event).

%% API functions
-export([start_link/0,
         add_handler/0,
         delete_handler/0]).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API functions
%%%===================================================================


%% @doc Creates an event manager
-spec start_link() -> {ok, Pid} | {error, Error}.
start_link() ->
    gen_event:start_link({local, ?MODULE}).


%% @doc Adds an event handler
-spec add_handler() -> ok | {'EXIT', Reason} | term().
add_handler() ->
    event_manager:add_handler(?MODULE, []).
    
%% @doc Removes an event handler
-spec delete_handler() -> ok | {'EXIT', Reason} | term()
delete_handler() ->
    event_manager:delete_handler(?MODULE, []).
%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%% @private
%% @doc Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
-spec init(Args) -> {ok, State}.
init([]) ->
    {ok, #state{}}.

%% @private
-spec handle_event(Event, State) -> {ok, State} | {swap_handler, Args1, State1, Mod2, Args2} | remove_handler.
%% @doc handel command to book a new cargo
handle_event({book_new_cargo,Id,Origin,Destination}, State) ->
    {ok, Pid} = cargo_sup:start_link(),
    cargo_aggregate:create(Pid,Id, Origin, Destination), 
    cargo_repository:save(Pid),
	{ok, State};

%% @private
handle_event(_Event, State) ->
    {ok, State}.

%% @private
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
