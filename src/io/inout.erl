-module(inout).

%%MÓDULO para obtener la entrada de texto. Ademas analiza si lo que entra es un comando reconocido y comprueba si se han introducido un numero
%% valido de argumentos

-define(AUX,aux).

-export([get_line/0,print_Msg/2,print_error/2,print_error/1,print_info/1]).


%% Comandos reconocidos
-define(COMMANDS, [
    %% {comm_atom, string_representations, num_args}
    {exit,"/exit",0},
    {help,"/help",0},{help,"/h",0},
    {join,"/join",1},
    {leave,"/leave",0}
]).



get_line()->
    S=?AUX:droplast(io:get_line("")),
    case S of
        "" -> empty;
        _->case string:chr(S,$/) of
            1 -> chk_comm(string:tokens(S," "),?COMMANDS);
            _ -> {msg,S}
        end
    end.



chk_comm([Input|_],[]) ->
    {error,no_command,Input};
chk_comm([Input|T],[{Atom,Input,Args}|_]) when Args=< length(T)->
    {Atom,string:join(T," ")};
chk_comm([Input|_],[{_,Input,_}|_]) ->
    {error,number_of_arguments,Input};
chk_comm(Input,[_|T]) -> chk_comm(Input,T).


print_Msg(From,Msg)->
    M=io_lib:format("~p : ~s ~n",[From,Msg]),
    color:print(M,cyan).


print_error(InfoError) ->
    M=io_lib:format("~s",[InfoError]),
    color:print(M,red).
print_error(Error,Cause)->
    M=io_lib:format("ERROR ~s : ~p ",[Cause,Error]),
    color:print(M,red).

print_info(Info) -> M=io_lib:format("~s",[Info]), color:print(M,green).
