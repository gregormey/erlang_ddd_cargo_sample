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


-module(event_manager).

%% The EventManager for our app
%% API
-export([start_link/0, add_handler/2, delete_handler/2, send_command/1,
	publish_event/1]).
-define(SERVER, ?MODULE).

%% API functions
-spec start_link() -> {ok, pid()} | ignore | {error,_}.
start_link() ->
    gen_event:start_link({local, ?SERVER}).

-spec add_handler(atom(),term())-> ok | {'EXIT',term()} | term().
add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

-spec delete_handler(atom(),term())-> term() | {error,module_not_found} | {'EXIT',term()}.
delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

-spec send_command(tuple())-> ok.
send_command(Command) -> 
	gen_event:notify(?SERVER, Command).

-spec publish_event(tuple())-> ok.
publish_event(Event) ->
	gen_event:notify(?SERVER, Event).