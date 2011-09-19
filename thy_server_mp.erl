% --------------------------------
% -- kvs.erl - Key Value Server
% -------------------------------
% first do erl -name whomever@IP -setcookie alpaca
% then you can store things like this rpc:call('whomever@IP', kvs, store, [number, 8675309]).
% and retrieve things like this rpc:call('whomever@IP', kvs, lookup, [number]).
% on each host you need to do kvs:start(). to start the server.
% maybe we could say rpc:call('whomever@IP', kvs, lookup, [location]).
-module(thy_server_mp).
-define(else, true).

%-----
% Public
% ----

-export ([start/0, location/3]).

start() -> Pid = spawn(fun() -> serverLoop() end ),
	register(lvs, Pid).

location(PlayerName, CurrentLocation, Command) -> remote(PlayerName, CurrentLocation, Command).

%-----
% Private
%------

remote(Request1, Request2, Request3) -> lvs ! {self(), Request1, Request2, Request3},
		receive
			{lvs, Response} -> Response
		end.
census() ->
	Census1 = rpc:call('gerard@69.164.211.135', kvs, getKeys, [1]),
	Census2 = rpc:call('gerard@69.164.211.135', kvs, getKeys, [2]),
	Census3 = rpc:call('gerard@69.164.211.135', kvs, getKeys, [3]),
	Census4 = rpc:call('gerard@69.164.211.135', kvs, getKeys, [4]),
        io:format("~p~p~p~p~n", [Census1, Census2, Census3, Census4]).
serverLoop() ->
	receive

		%% Location 1
		{FromPid, PlayerName, 1, "lookScroll"} ->
			FromPid ! {lvs, "Parchment, definitely parchment. I'd recognize it anywhere."},
			io:format("~p has looked at the Scroll from location 1.~n", [PlayerName]),	
			serverLoop();

		{FromPid, PlayerName, 1, "lookFlask"} ->
			FromPid ! {lvs, "Looks like you could quaff some serious mead out of that thing."},
			io:format("~p has looked at the Flask from location 1.~n", [PlayerName]),
			serverLoop();
	
		{FromPid, PlayerName, 1, "getScroll"} ->
			FromPid ! {lvs, "Ye takes the scroll and reads of it. It doth say: BEWARE, READER OF THE SCROLL, DANGER AWAITS TO THE- The SCROLL disappears in thy hands with ye olde ZAP!"},
			io:format("~p got the Scroll from location 1.~n", [PlayerName]),
			serverLoop();

		{FromPid, PlayerName, 1, "getFlask"} ->
			FromPid ! {lvs, "Ye cannot get the FLASK. It is firmly bolted to a wall which is bolted to the rest of the dungeon which is probably bolted to a castle. Never you mind."},
			io:format("~p got the Flask from location 1.~n", [PlayerName]),
			serverLoop();

		{FromPid, PlayerName, 1, "North"} ->
			FromPid ! {lvs, "You go NORTH through yon corrider. You arrive at PARAPETS. Ye see a ROPE. Obvious exits are SOUTH"},
			rpc:call('gerard@69.164.211.135', kvs, store, [PlayerName, 2]),
			io:format("~p has gone North from location 1. ~n", [PlayerName]),
			census(),
			serverLoop();
		%% Location 2
		{FromPid, PlayerName, 2, "lookParapets"} ->
			FromPid ! {lvs, "Well, they're parapets. This much we know for sure."},
			io:format("~p has looked at the Parapets from location 2.~n", [PlayerName]),
			serverLoop();

		{FromPid, PlayerName, 2, "lookRope"} ->
			FromPid ! {lvs, "It looks okay. You've seen better."},
			io:format("~p has looked at the Rope from location 2.~n", [PlayerName]),
			serverLoop();

		{FromPid, PlayerName, 2, "getRope"} ->
			FromPid ! {lvs, "You attempt to take ye ROPE but alas it is enchanted! It glows a mustard red and smells like a public privy. The ROPE wraps round your neck and hangs you from parapets. With your last breath, you wonder what parapets are. GAME OVER."},
			io:format("~p got the Rope from location 2.~n", [PlayerName]),
			serverLoop();

		{FromPid, PlayerName, 2, "South"} ->
			FromPid ! {lvs, "Ye find yeself in yon dungeon. Ye see a SCROLL. Behind ye scroll is a FLASK. Obvious exits are NORTH, SOUTH and DENNIS."},
			rpc:call('gerard@69.164.211.135', kvs, store, [PlayerName, 1]),
			io:format("~p has gone South from location 2.~n", [PlayerName]),
			census(),
			serverLoop();

		%% Location 3
		{FromPid, PlayerName, 1, "South"} ->
			FromPid ! {lvs, "You head south to an embankment. Or maybe a chasm. You can't decide which. Anyway, ye spies a TRINKET. Obvious exits are NORTH."},
			rpc:call('gerard@69.164.211.135', kvs, store, [PlayerName, 3]),
			io:format("~p has gone South from location 1.~n", [PlayerName]),
			census(),
			serverLoop();

		{FromPid, PlayerName, 3, "lookTrinket"} ->
			FromPid ! {lvs, "Quit looking! Just get it already."},
			io:format("~p has looked at the Trinket from location 3.~n", [PlayerName]),
			serverLoop();
		
		{FromPid, PlayerName, 3, "getTrinket"} ->
			FromPid ! {lvs, "Ye getsts yon TRINKET and discover it to be a bauble. You rejoice at your good fortune. You shove the TRINKET in your pouchel. It kinda hurts. Another trinket appears in its place!"},
			io:format("~p got the Trinket from location 2.~n", [PlayerName]),
			serverLoop();
		
		{FromPid, PlayerName, 3, "North"} ->
			FromPid ! {lvs, "Ye find yeself in yon dungeon. Ye see a SCROLL. Behind ye scroll is a FLASK. Obvious exits are NORTH, SOUTH, and DENNIS."},
			rpc:call('gerard@69.164.211.135', kvs, store, [PlayerName, 1]),
			io:format("~p has gone North from location 3.~n", [PlayerName]),
			census(),
			serverLoop();

		%% Location 4 
		{FromPid, PlayerName, 1, "Dennis"} ->
			FromPid ! {lvs, "Ye arrive at Dennis. He wears a sporty frock coat and a long jimberjam. He paces about nervously. Obvious exits are NOT DENNIS."},
			rpc:call('gerard@69.164.211.135', kvs, store, [PlayerName, 4]),
			io:format("~p has gone to Dennis.~n", [PlayerName]),
			census(),
			serverLoop();

		{FromPid, PlayerName, 4, "talk"} ->
			FromPid ! {lvs, "You engage Dennis in leisurely discussion. Ye learns that his jimberjam was purchased on sale at a discount market and that he enjoys pacing about nervously. You become bored and begin thinking about parapets."},
			io:format("~p talked to Dennis.~n", [PlayerName]),
			serverLoop();

		{FromPid, PlayerName, 4, "lookDennis"} ->
			FromPid ! {lvs, "That jimberjam really makes the outfit."},
			io:format("~p looked at Dennis.~n", [PlayerName]),
			serverLoop();
		{FromPid, PlayerName, 4, "lookJimberjam"} ->
			FromPid ! {lvs, "Man, that art a nice jimberjam."},
			io:format("~p  looked at Dennis' jimberjam.~n", [PlayerName]),
			serverLoop();
		{FromPid, PlayerName, 4, "notDennis"} ->
			FromPid ! {lvs, "Ye find yeself in yon dungeon. Ye see a SCROLL. Behind ye scroll is a FLASH. Obvious exits are NORTH, SOUTH, AND DENNIS."},
			rpc:call('gerard@69.164.211.135', kvs, store, [PlayerName, 1]),
			io:format("~p has gone NOT DENNIS.~n", [PlayerName]),
			census(),
			serverLoop();

		{FromPid, PlayerName, Location, "census"} ->
			PlayerCensus = rpc:call('gerard@69.164.211.135', kvs, getKeys, [Location]),
			FromPid ! {lvs, PlayerCensus},
			io:format("~p has asked for the census of his/her location.~n", [PlayerName]),
			serverLoop();

		{FromPid, _, _, "help"} ->
			FromPid ! {lvs, "There are prefixes, such as look, and get, that act upon in-game objects such as TRINKET. An example of a command might be getTrinket. To move between areas enter compass directions such as North. To exit the game, type quit. To get an update of the players in your area, type census."},
			serverLoop();

		%% else
		{FromPid, PlayerName, _, _} ->
			FromPid ! {lvs, "Does not computeth."},
			io:format("~p issued an invalid command.~n", [PlayerName]),
			serverLoop()
	end.
