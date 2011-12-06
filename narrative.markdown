# Crafty Erlang: Your new favorite scripting language?

## An elegant language for small projects

The common perception of Erlang is that it's a good language for big projects where you need massive scalability, distribution, fault-tolerance and so on.
The language itself is weird and ugly, and it's got a lot of annoying restrictions, but that's what you have to put up with to get the good stuff.
I want to challenge both sides of that, and say that Erlang is also a great language for small projects - even little scripts - and that it's really beautiful once you understand it.
It just has its own way of doing things.
And a key here is that it does have A Way of Doing Things;
There are design patterns and programming idioms that you can follow.
It's not rocket science; you can learn it.
So what I'm going to do here is show you some of those patterns and idioms, walk through a couple of simple programs, and hopefully give you what you need to know (and a bit of a nudge) to get started having fun with Erlang.

## Synergistic Weirdness

Erlang has what I've come to think of as "synergistic weirdness".
When I was first starting out with Erlang, there were all these things that struck me as weird.
Like, I want to write a `for` loop that increments a counter, and Erlang's like, "Nope. No can do."
You can't increment a counter because variables are immutable, and there's no `for` loop.
There are no loop controls, period.
And there are no classes or objects, while we're at it.
How do I *do* anything in this language?

Then there's all this weird extra stuff.
There's efficient tail recursion.
Ok, fine, but how often do I write recursive functions? About never.
There's pattern matching and guard clauses.
That's kinda cool, but I'm still not sure how much I'd use it.
All of the inter-process communication (IPC) stuff - process spawning and message passing - is definitely cool if you're writing a big concurrent app, but otherwise?
All this stuff is fine, but how often would you actually use any of it?
The answer is, "All the time."
All these isolated bits of weirdness combine to something very elegant.
For example, let's take a look at...

## Recursion

You don't use recursion much in OO languages because (a) you rarely need to, and (b) it's scary - you have to be careful about how you update your data structures.
Recursive methods tend to have big warning comments, and nobody dares touch them.
And this is self-reinforcing: Since it's not used much, it remains this scary, poorly-understood concept.

In Erlang, recursion takes the place of all of the looping constructs and iterators that you would use in an object-oriented (OO) language.
Because it's used for everything, there are well-established patterns for writing recursive functions.
Since you use it all the time, you get used to it.
Erlang's immutable variables actually simplify recursion, because they force you to be clear about how you're changing your data at each step of the recursion.
Pattern matching and guard expressions make recursion really powerful and expressive, because they let you break out the stages of a recursion in a very declarative way.
Let's look at the basics of recursion in Erlang with a very simple example: munging a list of data.

Like a story, a recursive function has a beginning, a middle, and an end.
The beginning and end are usually the easiest parts, so let's tackle those first.
The beginning of a recursion is just a function that takes the input, sets up any initial state, ouput accumulators, etc., and recurses.
In this case, we take an Input list and set up an empty output list.

    %% beginning
    func(Input) ->
        Output = [],
        func(Input, Output).

The end stage is also easy to define.
We pattern-match on an empty input list, and return our output list.
(I'll get to why we reverse the output list in a minute.)

    %% end
    func([], Output) -> lists:reverse(Output).

The middle stage defines what we do with any single element in the list, and how we move on to the next one.
Here, we just pop the first element off the input list, munge it to create a new element, push that onto the output list, and recurse with the newly-diminished input and newly-extended output.
(And note that we add our new element at the beginning of the list, rather than the end.)

    %% middle
    func([First | Rest], Output) ->
        NewFirst = munge(First),
        func(Rest, [NewFirst | Output]);

That's all there is to the basics of recursion.
You may have multiple inputs and outputs, and there could be multiple middle and end functions to handle different cases (and we'll see a more interesting example in a minute), but the basic pattern is the same.

## Digression: Backwards Lists

Why do build our output list backwards?
Why don't we just add new elements to the end, and not have to reverse it when we're done?
This was one of those little Erlang weirdnesses that really bugged be until I understood it.
The key is that lists in Erlang are not just arrays; they're linked lists, and critically, _singly_-linked lists.
So when you create a list like

    Foo = [cat, dog].

You get a logical list structure that looks like

    Foo
    |
    cat - dog

If you create a new list by prepending an element to Foo,

    Bar = [monkey | Foo].

The logical structure now looks like

    Bar      Foo
    |        |
    monkey - cat - dog

And if you create another new list by prepending more elements to Foo,

    Baz = [elephant, tiger | Foo].

The logical structure will now look like

    Baz       Bar      Foo
    |         |        |
    |         monkey - cat - dog
    |                 /
    elephant - tiger /

So the new lists are efficiently re-using Foo's elements, but this only works because Foo itself is immutable.
If you could add elements onto the end of Foo, or modify its elements in place, you'd see that change in every list built off of Foo.


## Back to recursion: Bowling Game

A more interesting example of recursion is the bowling game.
This is a standard programming exercise - write a program to calculate the score for bowling.
It's fairly simple, but not trivial.
Your input is a list of rolls (number of pins knocked down), and your output is just a number, a final score.
There's also this concept of frames you have to keep track of; the game has a fixed number of frames, but the number of rolls may vary.
The score for a frame may depend on rolls you made in other frames.
Writing this in an OO language, and trying to break this functionality cleanly out into classes, can be tricky.
In Erlang, we're just going to define a recursive function that takes a list of rolls and returns a number.

Again, we start by defining the beginning of the recursion.
We get a list of rolls, set our initial frame to 1 and score to 0, and recurse.

    %% Beginning: score/1 -> score/3
    score(Rolls) ->
        Frame = 1,
        Score = 0,
        score(Rolls, Frame, Score).

The end is even simpler.
There are ten frames in a game, so when our frame count gets to 11, we're done.
Just return our score.

    %% End
    score(_Rolls, 11, Score) -> Score.

For the middle, we're going to start with the normal case for scoring a frame, ignoring strikes and spares.
Here, we just pop the next two rolls off the input, add them to our total score, and recurse with an incremented frame.

    %% Middle
    score([Roll1, Roll2 | Rest], Frame, Score) ->
        NewScore = Score + Roll1 + Roll2,
        score(Rest, Frame + 1, NewScore).

Now we need to deal with the strike and spare cases.
Erlang's pattern matching lets us do this very cleanly.
For strikes, define a score function (in proper terms, a clause of the score function) that matches when the first roll is a 10.
For spares, we use a guard expression to match only when the next two rolls add up to 10.
In both cases, we need to look at rolls in following frames (2 for a strike, 1 for a spare) and add those to our score.

    %% Strike
    score([10 | Rest], Frame, Score) ->
        [Bonus1, Bonus2 | _] = Rest,
        NewScore = Score + 10 + Bonus1 + Bonus2,
        score(Rest, Frame + 1, NewScore);

    %% Spare
    score([Roll1, Roll2 | Rest], Frame, Score) when Roll1 + Roll2 == 10 ->
        [Bonus1 | _] = Rest,
        NewScore = Score + 10 + Bonus1,
        score(Rest, Frame + 1, NewScore);

That's pretty much it for the scoring rules.
We still need to handle incomplete frames, so by the time we're done with that, the whole thing looks like this.

    score(Rolls) -> score(Rolls, 1, 0).

    score(_Rolls, 11, Score) -> Score;

    score([10 | Rest], Frame, Score) ->
        score(Rest, Frame + 1, Score + 10 + strike_bonus(Rest));

    score([Roll1, Roll2 | Rest], Frame, Score) when (Roll1 + Roll2 == 10) ->
        score(Rest, Frame + 1, Score + 10 + spare_bonus(Rest));

    score([Roll1, Roll2 | Rest], Frame, Score) ->
        score(Rest, Frame + 1, Score + Roll1 + Roll2);

    score([Roll1], _Frame, Score) -> Score + Roll1;
    score([], _Frame, Score) -> Score.


    spare_bonus([]) -> 0;
    spare_bonus([Bonus1 | _Rest]) -> Bonus1.

    strike_bonus([]) -> 0;
    strike_bonus([Only]) -> Only;
    strike_bonus([Bonus1, Bonus2 | _Rest]) -> Bonus1 + Bonus2.


Ok, that's an algorithm.
To turn it into a usable application, we need to put some sort of interface in front of it, and we need some way to store our data as the game progresses.
The simplest interface is the command line, so let's start with that.


## Sketching the CLI

We can start sketching out the command-line interface in the Erlang shell.
`io:get_line/1` prompts the user and reads a line from standard input.
We can enter a player name and a roll.
`string:tokens/2` will split that into separate strings.
`string:to_integer/1` will convert the roll to a number we can work with.

    Eshell V5.8.4  (abort with ^G)
    1> Line = io:get_line("Next> ").
    Next> colin 4
    "colin 4\n"
    2> [Player, RollText] = string:tokens(Line, " \t\n").
    ["colin","4"]
    3> {Roll, _} = string:to_integer(RollText).
    {4,[]}

Now we need somewhere to store it.
A dictionary will let us keep track of multiple players.
`dict:new/0` creates it.
`dict:append/3` takes a key-value pair and adds the value to the list of values for that key.
Note that it does _not_ replace the key's value (`dict:store/3` does that).
`dict:find/2` returns the value for a key, which in this case is a list of rolls.

    4> GameData = dict:new().
    {dict,0,...
    5> NewGameData = dict:append(Player, Roll, GameData).
    {dict,1,...
    6> {ok, Rolls} = dict:find(Player, NewGameData).
    {ok,[4]}

Finally, we pass the list of rolls to our scoring function (not very interesting yet).

    7> Score = bowling_game:score(Rolls).
    4

Normally now, you'd throw a while loop arounds this stuff, but this is Erlang, so we wrap it in a recursive function.

    loop(GameData) ->
        Line = io:get_line("Next> "),
        [Player, RollText] = string:tokens(Line, " \t\n"),
        {Roll, _} = string:to_integer(RollText),
        NewGameData = dict:append(Player, Roll, GameData),
        {ok, Rolls} = dict:find(Player, NewGameData).
        Score = bowling_game:score(Rolls).
        io:format("New score for ~s: ~p~n", [Player, Score]),
        loop(NewGameData).

This is the middle of a recursion, so we need to add a beginning.
That's simple enough - invoke loop with a new dictionary.
We could put this in a module and call it from the Erlang shell,
but it's easier if we wrap it in an Escript.
(If you're not familiar with Escript, it lets you run Erlang code as a script, the way you would with Perl, Python, Ruby, or whatever.
You don't even need to define a module, just a main/1 function that takes the command-line parameters as a list of strings.)

    #!/usr/local/bin/escript
    #
    # scorekeeper.erl 

    -import(bowling_game).

    main(_) -> loop(dict:new()).

    loop(GameData) ->
        ...

That's the beginning and middle of the recursion.
We're not going to bother defining an end - you can just `ctrl-c` out of the loop.
Here's a sample of what we get when we run it:

    $ ./scorekeeper.erl 
    Next> colin 3
    New score for colin: 3
    Next> colin 4
    New score for colin: 7
    Next> colin 10
    New score for colin: 17
    Next> colin 3
    New score for colin: 23
    Next> 

(Note that it correctly handles the strike!)

## Webify!

So that's a command-line interface.
Now let's turn it into a simple web app.
We're going to have a single-page rich web client that will send Ajax requests to a REST service, and use Javascript to update its display.
The REST service will have pretty much the same API: It'll take a player and a roll, and return the player's new score.
We'll use jQuery on the front end and Spooky on the back end.
Spooky is a very simple web application framework, much Ruby's Sinatra, if you're familiar with that.

The other change here is that we'll have to deal with concurrency.
The command line is inherently sequential, but web services are inherently concurrent.
We'll need to create a mini-service which will control access to our bowling data.

A bit of a digression:
I've seen it argued that Erlang is one of the most truly object-oriented languages.
The original theory behind object-oriented design was that objects would be like living things that talk to each other.
Rather than having a dumb data structure that you manipulate, you would send messages to an object, requesting that it give you information, update its state, perform a calculation, or whatever.
You would have an interface to talk to it, but you couldn't know or manipulate its state directly.
Most OO languages implement this by wrapping a dumb data structure in a bunch of smart (or not so smart) accessor methods - usually optional.
What Erlang does is create processes to manage access to data.
Rather than data having associated methods, Erlang has processes that own data.
The only way to get to the data is to talk to the process, and that's where IPC comes in.
So what does that look like?

Well, remember the command-line loop?
Let's break that up into sections.

    loop(GameData) ->
        %% receive input
        Line = io:get_line("Next> "),
        [Player, RollText] = string:tokens(Line, " \t\n"),

        %% process input
        {Roll, _} = string:to_integer(RollText),
        NewGameData = dict:append(Player, Roll, GameData),
        {ok, Rolls} = dict:find(Player, NewGameData).
        Score = bowling_game:score(Rolls).

        %% print output
        io:format("New score for ~s: ~p~n", [Player, Score]),

        %% recurse with new state
        loop(NewGameData).

Now here's the message-handling loop.

    loop(GameData) ->
        %% receive input
        receive {From, {append, Player, RollText}} ->

            %% process input - this is the same
            {Roll, _} = string:to_integer(RollText),
            NewGameData = dict:append(Player, Roll, GameData),
            {ok, Rolls} = dict:find(Player, NewGameData),
            Score = bowling_game:score(Rolls),

            %% send updated score to requesting process
            From ! Score,

            %% recurse with new state
            loop(NewGameData)
        end.

That's it.
Instead of waiting for command-line input, we wait for a message.
Instead of printing our response, we send a message back.
GameData is a local variable to loop/1.
Nobody else can see it; there's only one process that can change it.

