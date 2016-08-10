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

-module(graph_traversal_service_SUITE).
-compile(export_all).

-import(ct_helper, [doc/1]).
   

all() -> [find_shortest_path_DEHAM_USNYC,find_shortest_path_CNHKG_DEHAM,find_shortest_path_CNHKG_DEHAM].

init_per_suite(_Config) ->
    [].

find_shortest_path_DEHAM_USNYC(_Config)->
	doc("Calculates traversal graph from priv/itineraries.config for DEHAM to USNYC."),
	Edges=graph_traversal_service:find_shortest_path("DEHAM", "USNYC"),
	[
		[{edge,"DEHAM","USNYC",{{_,_,_},{_,_,_}},{{_,_,_},{_,_,_}}}],
		[
			{edge,"DEHAM","NLRTM",{{_,_,_},{_,_,_}},{{_,_,_},{_,_,_}}},
			{edge,"NLRTM","SESTO",{{_,_,_},{_,_,_}},{{_,_,_},{_,_,_}}},
			{edge,"SESTO","USNYC",{{_,_,_},{_,_,_}},{{_,_,_},{_,_,_}}}
		]
	]=Edges.

find_shortest_path_CNHKG_DEHAM(_Config)->
	doc("Calculates traversal graph from priv/itineraries.config for CNHKG to DEHAM."),
	Edges=graph_traversal_service:find_shortest_path("CNHKG","DEHAM"),
	[
		[
			{edge,"CNHKG","USNYC",{{_,_,_},{_,_,_}},{{_,_,_},{_,_,_}}},
			{edge,"USNYC","SESTO",{{_,_,_},{_,_,_}},{{_,_,_},{_,_,_}}},
			{edge,"SESTO","DEHAM",{{_,_,_},{_,_,_}},{{_,_,_},{_,_,_}}}
		]
	]=Edges.

find_shortest_path_DEHAM_NLRTM(_Config)->
	doc("Calculates traversal graph from priv/itineraries.config for DEHAM to NLRTM."),
	Edges=graph_traversal_service:find_shortest_path("DEHAM","NLRTM"),
	[[{edge,"DEHAM","NLRTM",{{_,_,_},{_,_,_}},{{_,_,_},{_,_,_}}}]]=Edges.
