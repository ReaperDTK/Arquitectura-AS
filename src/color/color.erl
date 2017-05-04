
-module(color).

%%%%%%%%%%%%%%  API  %%%%%%%%%%%%%%

-export([print/2, get_colors/0, get_modes/0,
         print_available_colors/0, print_available_modes/0]).

%%%%%%%%%%%%%%  type definition  %%%%%%%%%%%%%%

-define(reset, <<"\e[0m">>).
-define(foreground(Color),
    Color(Text) -> [<<"\e[0;">>, foreground(Color), <<"m">>, Text, ?reset]).

-type opt() :: black | red | green | yellow | blue | purple | cyan | white |
    light_black | light_red | light_green | light_yellow | light_blue |
    light_purple | light_cyan | light_white | normal | bold | underline |
    blink | default.

%%%%%%%%%%%%%%  API functions  %%%%%%%%%%%%%%

% @doc Prints a string in a given format.
print(IOData, Opts) ->
    io:format("~s~n", [p(IOData, Opts)]).

% @doc Retrieves a list containing all available colors.
get_colors() ->
    [black, red, green, yellow, blue, purple, cyan, white,
     light_black, light_red, light_green, light_yellow,
     light_blue, light_purple, light_cyan, light_white, default].

 % @doc Retrieves a list containing all available modes.
 get_modes() ->
    [normal, bold, italic, underline].

% @doc Prints a list containing all available colors.
print_available_colors() ->
    io:format("~n"),
    available_colors(get_colors(), 1).

% @doc Prints a list containing all available modes.
print_available_modes() ->
    io:format("~n"),
    available_modes(get_modes(), 1).

% @doc Format and color `IOData' according to `Opts'.
-spec p(iodata(), [opt()]) -> iodata().
p(IOData, Opts) ->
    {Modes, Colors} = parse(Opts),
    [<<"\e[">>, Modes, Colors, <<"m">>, IOData, ?reset].

%%%%%%%%%%%%%%  internal functions  %%%%%%%%%%%%%%

parse(Opts) -> parse(Opts, {mode(normal), <<>>}).

parse([], Result) ->
    Result;
parse([Mode|Opts], {<<>>, Colors})
  when Mode == normal; Mode == bold; Mode == italic; Mode == underline ->
    parse(Opts, {mode(Mode), Colors});
parse([Mode|Opts], {Modes, Colors})
  when Mode == normal; Mode == bold; Mode == italic; Mode == underline ->
    Bin = mode(Mode),
    parse(Opts, {<<Modes/binary, ";", Bin/binary>>, Colors});
parse([Color|Opts], {Mode, <<>>}) ->
    Bin = foreground(Color),
    parse(Opts, {Mode, <<";", Bin/binary>>});
parse([Color|Opts], {Mode, Colors}) ->
    Bin = background(Color),
    parse(Opts, {Mode, <<Colors/binary, ";", Bin/binary>>});
parse(SingleOpt, Result) ->
    parse([SingleOpt], Result).

mode(normal)    -> <<"0">>;
mode(bold)      -> <<"1">>;
mode(italic)    -> <<"3">>;
mode(underline) -> <<"4">>;
mode(_)         -> error(invalid_mode).

foreground(default)      -> <<"0">>;
foreground(black)        -> <<"30">>;
foreground(red)          -> <<"31">>;
foreground(green)        -> <<"32">>;
foreground(yellow)       -> <<"33">>;
foreground(blue)         -> <<"34">>;
foreground(purple)       -> <<"35">>;
foreground(cyan)         -> <<"36">>;
foreground(white)        -> <<"37">>;
foreground(light_black)  -> <<"90">>;
foreground(light_red)    -> <<"91">>;
foreground(light_green)  -> <<"92">>;
foreground(light_yellow) -> <<"93">>;
foreground(light_blue)   -> <<"94">>;
foreground(light_purple) -> <<"95">>;
foreground(light_cyan)   -> <<"96">>;
foreground(light_white)  -> <<"97">>;
foreground(_)            -> error(invalid_foreground_color).

background(black)        -> <<"40">>;
background(red)          -> <<"41">>;
background(green)        -> <<"42">>;
background(yellow)       -> <<"43">>;
background(blue)         -> <<"44">>;
background(purple)       -> <<"45">>;
background(cyan)         -> <<"46">>;
background(white)        -> <<"47">>;
background(light_black)  -> <<"100">>;
background(light_red)    -> <<"101">>;
background(light_green)  -> <<"102">>;
background(light_yellow) -> <<"103">>;
background(light_blue)   -> <<"104">>;
background(light_purple) -> <<"105">>;
background(light_cyan)   -> <<"106">>;
background(light_white)  -> <<"107">>;
background(_)            -> error(invalid_background_color).

available_colors([], Int) ->
        io:format("~nTotal available: ~B~n~n", [Int-1]);
available_colors([Color|NextColors], Int) ->
    String = io_lib:format("~s~n", [Color]),
    io:format(p(String, [Color])),
    available_colors(NextColors, Int+1).

available_modes([], Int) ->
        io:format("~nTotal available: ~B~n~n", [Int-1]);
available_modes([Mode|NextModes], Int) ->
    String = io_lib:format("~s~n", [Mode]),
    io:format(p(String, [Mode])),
    available_modes(NextModes, Int+1).
