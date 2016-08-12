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

%% @doc
%% persists events on a aggregate idetified by its process ID
-spec save(pid()) -> ok.
save(Pid) ->
	Saver = fun(Id, Events) -> event_store:append_events(Id, Events) end,
	cargo_aggregate:process_unsaved_changes(Pid, Saver).

load_from_event_store(Id) ->
	case event_store:get_events(Id) of 
		[] -> 
			not_found;
		Events -> 
			{ok, Pid} = cargo_sup:start_link(),
			cargo_aggregate:load_from_history(Pid, Events),
			{ok, Pid}
	end.


get_by_id(Id) ->
	case gproc:where({n,l, {cargo_aggregate, Id}}) of
		undefined -> load_from_event_store(Id);
		Pid -> {ok, Pid}
	end.
