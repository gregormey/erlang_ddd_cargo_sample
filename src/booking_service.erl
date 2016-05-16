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


-module(booking_service).

-export([book_new_cargo/2, 
		load_cargo_for_routing/1,
		list_all_cargos/0]).

book_new_cargo(Origin,Destination)->
	Id=cargo_repository:generate_tracking_id(),
	case event_manager:send_command({book_new_cargo, Id ,Origin,Destination}) of
		ok -> Id;
		Err -> Err
	end.	


load_cargo_for_routing(Tracking_Id)->
	cargo_read_store:load_cargo_by_tracking_id(Tracking_Id) . 

list_all_cargos()->
	cargo_read_store:all(). 	

