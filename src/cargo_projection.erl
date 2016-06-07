
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

-module(cargo_projection).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

% API Function Exports
-export([start_link/0, project_new_cargo/4]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% API Function Definitions
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%% creates the state of a created cargo as a read projection
-spec project_new_cargo(string(),string(),string(),tuple())-> ok.
project_new_cargo(Id,Origin,Destination,DateCreated) ->
	gen_server:cast(?SERVER, {project_new_cargo, Id,Origin,Destination,DateCreated}).

%% gen_server Function Definitions
%% @private
init(Args) ->
    {ok, Args}.
%% @private
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
%% @private
handle_cast({project_new_cargo, Id,Origin,Destination,DateCreated}, State) ->
	ok=read_store:add({cargo,Id,Origin,Destination,DateCreated}), 
	{noreply, State};
%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.
%% @private
handle_info(_Info, State) ->
    {noreply, State}.
%% @private
terminate(_Reason, _State) ->
    ok.
%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


