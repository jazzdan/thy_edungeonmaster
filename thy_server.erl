%%
%% thy_server.erl
%% An erlang port of thy_dungeonman, located at http://www.homestarrunner.com/dungeonman.html
%% This is non-commercial use, so the use of the story line and gameplay style is allowed according to
%% the terms of use, located at http://www.homestarrunner.com/legal.html
%%

-module(thy_server).  
-export([start/0]).

start() -> SID = spawn(fun serverLoop/0),
	io:format("YOU ARE THY DUNGEONMAN!~n~n"), %% hax: allows intro screen to be displayed first instead of calculating location
    io:format("Ye find yeself in yon dungeon. Ye see a SCROLL. Behind ye scroll is a FLASK. Obvious exits are NORTH, SOUTH and DENNIS.~n"),
	playerLoop(1, SID). %% initiates the player at location 1, or the starting area.

	playerLoop(CurrentLocation, ServerPID) ->
	{ok, Input} = io:fread("Enter a command:", "~s"),
	[PlayerInput|Tail] = Input,
	io:format("~n"), %% hax: to print new line, couldn't figure out how to do it most of the time inline.
	if PlayerInput == "quit" -> %% if a player enters quit, it is caught here and calls the quit function.
		quit();
		true -> %% else
			{NewLocation, ToPrint} = rpc(ServerPID, CurrentLocation, PlayerInput), %% gets the updated location and description by passing current location and input to the server.
			io:format([ToPrint]), %% prints the description.
			io:format("~n"),
			playerLoop(NewLocation, ServerPID) 
	end.

quit() ->
		io:format("Thanks for playing! kthxbai"),
		init:stop(). %% stops the process, closes the shell window too though...

rpc(Pid, CurrentLocation, Request) -> %% sends request to the server
    Pid ! {self(), CurrentLocation, Request},
    receive
		{Pid, NewLocation, Response} -> %% receives response from serve
			{NewLocation, Response}
    end.

serverLoop() -> %% server loop, uses three parameters, From (PID), location, and request. This is because the location + the request determines the response
%% in other words, each location has more than one response depending on the request.
	receive
		%% Location 1
		{From, 1, "lookScroll"} ->
			From !  {self(), 1, "Parchment, definitely parchment. I'd recognize it anywhere."},
			serverLoop();

		{From, 1, "lookFlask"} ->
			From !  {self(), 1, "Looks like you could quaff some serious mead out of that thing."},
			serverLoop();
		
		{From, 1, "getScroll"} ->
			From ! {self(), 1, "Ye takes the scroll and reads of it. It doth say: BEWARE, READER OF THE SCROLL, DANGER AWAITS TO THE- The SCROLL disappears in thy hands with ye olde ZAP!"},
			serverLoop();
			
		{From, 1, "getFlask"} ->
			From ! {self(), 1, "Ye cannot get the FLASK. It is firmly bolted to a wall which is bolted to the rest of the dungeon which is probably bolted to a castle. Never you mind."},
			serverLoop();
		%% Location 2
		{From, 1, "North"} ->
			From ! {self(), 2, "You go NORTH through yon corrider. You arrive at PARAPETS. Ye see a ROPE. Obvious exits are SOUTH."},
			serverLoop(); %% passes back new location as a result of going North.
			
		{From, 2, "lookParapets"} ->
			From !  {self(), 2, "Well, they're parapets. This much we know for sure."},
			serverLoop();
			
		{From, 2, "lookRope"} ->
			From !  {self(), 2, "It looks okay. You've seen better."},
			serverLoop();
			
		{From, 2, "getRope"} ->
			From ! {self(), 2, "You attempt to take ye ROPE but alas it is enchanted! It glows a mustard red and smells like a public privy. The ROPE wraps round your neck and hangs you from parapets. With your last breath, you wonder what parapets are. GAME OVER."},
			init:stop();
			
		{From, 2, "South"} ->
			From !  {self(), 1, "Ye find yeself in yon dungeon. Ye see a SCROLL. Behind ye scroll is a FLASK. Obvious exits are NORTH, SOUTH and DENNIS."},
			serverLoop();
		%% Location 3
		{From, 1, "South"} ->
			From ! {self(), 3, "You head south to an embankment. Or maybe a chasm. You can't decide which. Anyway, ye spies a TRINKET. Obvious exits are NORTH."},
			serverLoop();
		{From, 3, "lookTrinket"} ->
			From ! {self(), 3, "Quit looking! Just get it already."},
			serverLoop();
			
		{From, 3, "getTrinket"} ->
			From ! {self(), 3, "Ye getsts yon TRINKET and discover it to be a bauble. You rejoice at your good fortune. You shove the TRINKET in your pouchel. It kinda hurts. Another trinket appears in its place!"},
			serverLoop();
		{From, 3, "North"} ->
			From ! {self(), 1, "Ye find yeself in yon dungeon. Ye see a SCROLL. Behind ye scroll is a FLASK. Obvious exits are NORTH, SOUTH and DENNIS."},
			serverLoop();
		%% Location 4
		{From, 1, "Dennis"} ->
			From ! {self(), 4, "Ye arrive at Dennis. He wears a sporty frock coat and a long jimberjam. He paces about nervously. Obvious exits are NOT DENNIS."},
			serverLoop();
		{From, 4, "talk"} ->
			From ! {self(), 4, "You engage Dennis in leisurely discussion. Ye learns that his jimberjam was purchased on sale at a discount market and that he enjoys pacing about nervously. You become bored and begin thinking about parapets."},
			serverLoop();
		{From, 4, "lookDennis"} ->
			From ! {self(), 4, "That jimberjam really makes the outfit."},
			serverLoop();
		{From, 4, "lookJimberjam"} ->
			From ! {self(), 4, "Man, that art a nice jimberjam."},
			serverLoop();
		{From, 4, "notDennis"} ->
			From ! {self(), 1, "Ye find yeself in yon dungeon. Ye see a SCROLL. Behind ye scroll is a FLASK. Obvious exits are NORTH, SOUTH and DENNIS."},
			serverLoop();
		%% Help for all locations, had trouble with using _
		{From, 1, "help"} ->
			From ! {self(), 1, "There are prefixes, such as look, and get, that act upon in-game objects such as TRINKET. An example of a command might be getTrinket. To move between areas enter compass directions such as North. To exit the game, type quit."},
			serverLoop();
		{From, 2, "help"} ->
			From ! {self(), 2, "There are prefixes, such as look, and get, that act upon in-game objects such as TRINKET. An example of a command might be getTrinket. To move between areas enter compass directions such as North. To exit the game, type quit."},
			serverLoop();
		{From, 3, "help"} ->
			From ! {self(), 3, "There are prefixes, such as look, and get, that act upon in-game objects such as TRINKET. An example of a command might be getTrinket. To move between areas enter compass directions such as North. To exit the game, type quit."},
			serverLoop();
		{From, 4, "help"} ->
			From ! {self(), 4, "There are prefixes, such as look, and get, that act upon in-game objects such as TRINKET. An example of a command might be getTrinket. To move between areas enter compass directions such as North. To exit the game, type quit."},
			serverLoop();
			
		% Look for all locations, had trouble using _
		
		{From, 1, "look"} ->
			From ! {self(), 1, "Ye find yeself in yon dungeon. Ye see a SCROLL. Behind ye scroll is a FLASK. Obvious exits are NORTH, SOUTH and DENNIS."},
			serverLoop();
			
		{From, 2, "look"} ->
			From ! {self(), 2, "You go NORTH through yon corrider. You arrive at PARAPETS. Ye see a ROPE. Obvious exits are SOUTH."},
			serverLoop();
			
		{From, 3, "look"} ->
			From ! {self(), 3, "You head south to an embankment. Or maybe a chasm. You can't decide which. Anyway, ye spies a TRINKET. Obvious exits are NORTH."},
			serverLoop();
			
		{From, 4, "look"} ->
			From ! {self(), 4, "Ye arrive at Dennis. He wears a sporty frock coat and a long jimberjam. He paces about nervously. Obvious exits are NOT DENNIS.a"},
			serverLoop();
			
		%% Quit for all locations.
		{From, _, "quit"} ->
			From ! {self(), 1, "Thanks for playing!"};
		%% Catch-all for all locations
		{From, 1, _} ->
			From ! {self(), 1, "Does not computeth."},
			serverLoop();
			{From, 2, _} ->
				From ! {self(), 2, "Does not computeth."},
				serverLoop();
			{From, 3, _} ->
				From ! {self(), 3, "Does not computeth."},
				serverLoop();
			{From, 4, _} ->
				From ! {self(), 4, "Does not computeth."},
				serverLoop();
		
		{From, Location, Other} ->
			From ! {self(), Location, {error,Other}} %% Error message for debugging, shouldn't even happen anymore.
    end.


