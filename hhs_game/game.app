{application, game,
 [
	{description, 'The game server'},
	{vsn, "1.0"},
	{modules, [game_app, game_supervisor, hall, room]},
	{registered, [hall, room]},
	{applications, [kernel, stdlib]},
	{mod, {game_app, []}},
	{start_phases, []}
 ]
}.