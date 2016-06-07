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

-module(event_store).

-export([init/0,get_events/1,append_events/2, delete/1]).

-record(stream, {
    id = undefined :: string(),
    events=[] :: list()
}).

-opaque stream() :: #stream{}.
-export_type([stream/0]).

%% API
%% @doc
%% create table for the event store
-spec init() -> ok | already_exists.
init() ->
    case mnesia:create_table(stream, [{attributes, record_info(fields, stream)},{disc_copies,[node()]}]) of
        {atomic,ok} -> ok;
        {aborted,{already_exists,stream}} -> already_exists
    end.
%% @doc
%% persists a new event on a aggregate and publishes the event
-spec append_events(string(),list()) -> ok. 
append_events(Id,Events) ->
	StoredEvents = get_raw_events(Id),
	NewEvents = lists:reverse(Events),
    CombinedEvents = NewEvents ++ StoredEvents,
    ok=mnesia_utile:store(#stream{id=Id,events=CombinedEvents}),  
    lists:foreach(fun (Event) -> event_manager:publish_event(Event) end, NewEvents).

%% @doc
%% returns all events for a aggregate identified by a ID
-spec get_events(string()) -> list(). 
get_events(Id) ->
	lists:reverse(get_raw_events(Id)).

%% @doc
%% remove a events stream identified a aggregate id
-spec delete(string()) -> ok. 
delete(Id) ->
    mnesia_utile:remove(stream, Id). 

%% Internals
get_raw_events(Key) ->
    case mnesia_utile:find_by_id(stream, Key)  of
        #stream{events=Events} -> Events;
        not_found -> []
    end.
