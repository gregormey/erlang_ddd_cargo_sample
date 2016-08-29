{application, graph_traversal_backend, [
	{description, "Graph Traversal Backend for Erlang DDD Cargo Sample"},
	{vsn, "0.0.1"},
	{modules, ['graph_traversal_backend_app','graph_traversal_backend_sup','graph_traversal_service']},
	{registered, [graph_traversal_backend_sup]},
	{applications, [kernel,stdlib]},
	{mod, {graph_traversal_backend_app, []}}
]}.