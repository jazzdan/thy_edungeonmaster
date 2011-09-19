%
%% thy_client.erl
%% client prototype for thy_server
%%

-module(thy_client).
-export([start/0]).

start() ->
	io:format("YOU ARE THY DUNGEONMAN!~n~n"),
	{ok, Input} = io:fread("Enter a name:", "~s"),
	[PlayerName|Tail] = Input,
	io:format("~n"),
	{ok, Input2} = io:fread("Enter a node: (user@IP):", "~s"),
	[NodeName|Tail] = Input2,
	RealNodeName = list_to_atom(NodeName),
	io:format("Ye find yeself in yon dungeon. Ye see a SCROLL. Behind ye scroll is a FLASH. Obvious exits are NORTH, SOUTH and DENNIS.~n"),
	rpc:call(RealNodeName, kvs, store, [PlayerName, 1]),
	playerLoop(PlayerName, RealNodeName).

playerLoop(PlayerName, RealNodeName) ->
	{ok, Input} = io:fread("Enter a command:", "~s"),
	[PlayerInput|Tail] = Input,
	io:format("~n"),
	if PlayerInput == "quit" ->
		quit(PlayerName, RealNodeName);
		true -> %% else
			ToPrint = rpc:call(RealNodeName, thy_server_mp, location, [PlayerName, rpc:call(RealNodeName, kvs, lookup, [PlayerName]), PlayerInput]),
			io:format([ToPrint]),
			io:format("~n"),
			ToPrint2 = rpc:call(RealNodeName, kvs, getKeys, [rpc:call(RealNodeName, kvs, lookup, [PlayerName])]),
			io:format("~n"),
			io:format("The following players are in this location:"),
			io:format("~n"),
			io:fwrite("~62p~n", [ToPrint2]),
			io:format("~n"),
			playerLoop(PlayerName, RealNodeName)
	end.

quit(PlayerName, RealNodeName) ->
	rpc:call(RealNodeName, kvs, eraseKey, [PlayerName]),
	io:format("Thanks for playing! kthxbai").
