%% Copyright (c) 2016, Gregor Meyenberg  <gregor@meyenberg.de>
%% 
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(booking_service_SUITE).
-compile(export_all).

-import(ct_helper, [doc/1]).
   

all() -> [book_new_cargo, list_all_cargos].

init_per_suite(Config) ->
    Config.

book_new_cargo(_)->
	doc("Add a new cargo. And loads it from the read store"),
	ok=booking_service:book_new_cargo("Hamburg", "Hong Kong").


list_all_cargos(_)->
	doc("List cargos").


