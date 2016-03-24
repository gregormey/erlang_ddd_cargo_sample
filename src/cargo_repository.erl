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


-module(cargo_repository).

-export([save/1]).
-export([generate_tracking_id/0]).
-export([load_by_tracking_id/1]).

generate_tracking_id()->
	uuid:to_string(uuid:uuid5(oid, "tracking_id")).

save(Pid) ->
	Saver = fun(Id, Events) -> event_store:append_events(Id, Events) end,
	cargo_aggregate:process_unsaved_changes(Pid, Saver).

load_by_tracking_id(Tracking_Id) ->
	read_store:load_cargo_by_tracking_id(Tracking_Id). 

