{application, 'cache', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['cache_app','cache_srv','cache_sup']},
	{registered, [cache_sup]},
	{applications, [kernel,stdlib]},
	{optional_applications, []},
	{mod, {cache_app, []}},
	{env, []}
]}.