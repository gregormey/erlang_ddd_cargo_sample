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
   

all() -> [book_new_cargo, load_cargo, list_all_cargos, list_shipping_locations].

init_per_suite(_Config) ->
    [{id,generate_tracking_id()}].

book_new_cargo(Config)->
	doc("Add a new cargo."),
	Id=get_id_from_config(Config),
	ok=booking_service:book_new_cargo(Id,"Hamburg", "Hong Kong").

load_cargo(Config)->
	doc("Load a specific cargo."),
	Id=get_id_from_config(Config),
	{cargo,Id,_,_}=booking_service:load_cargo_for_routing(Id). 

list_all_cargos(Config)->
	doc("List cargos"),
	Id=get_id_from_config(Config),
	[{cargo,Id,_,_}]=booking_service:list_all_cargos().

list_shipping_locations(_Config)->
	doc("It lists available shipping locations"),
	Locations=booking_service:list_shipping_locations(),
	true=is_list(Locations),
	[{LocationCode,_}|_]=Locations,
	5=string:len(LocationCode).



%%% internal
get_id_from_config(Config)->
	{id,Id}=lists:last(Config),
	Id.

generate_tracking_id()->
	uuid:uuid_to_string(uuid:get_v4()).


