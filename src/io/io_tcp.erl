-module(io_tcp).


-define(PORTNOS, [9090, 9091, 9092, 9093, 9094, 9095, 9096, 9097, 9098, 9099]).

%% Comandos reconocidos
-define(COMMANDS, [
    %% {comm_atom, string_representations, num_args}
    {exit,"/exit",0}
]).
-export([init/0,get_line/0,print_Msg/2,print_error/2,print_error/1,print_info/1]).


ports_loop([],_) -> no_port;
ports_loop([Port|T],Opts)->
    case gen_tcp:listen(Port,Opts) of
        {ok,Socket} -> io:format("Tu puerto es: ~p ~n",[Port]),Socket;
        {error,_} -> ports_loop(T,Opts)
    end.

init() ->
    Opts= [{reuseaddr,true},{active,true}],
    Socket = ports_loop(?PORTNOS,Opts),
    case Socket of
        no_port -> io:format("YOU ARE REALLY REALLY FUCKED ;) ~n");
        _ -> socket_con(Socket)
    end.


socket_con(Socket)->
    {ok,SockCli} = gen_tcp:accept(Socket,60000),
    gen_tcp:controlling_process(SockCli,self()),
    register(sender,spawn(fun()-> send_control(SockCli,Socket) end)),
    inet:setopts(SockCli, [{reuseaddr, true},{active,true}]),
    client:start().


delete_newline(Msg)->
    case string:rchr(Msg, $\n) of
        0 -> Msg;
        _ -> lists:droplast(Msg)
    end.

delete_return(Msg) ->
    case string:rchr(Msg, $\r) of
        0 -> Msg;
        _ -> lists:droplast(Msg)
    end.

get_line()->
    receive
        {tcp,S,M} ->
            io:format("received ~p~n",[M]),
            Msg= delete_return(delete_newline(M)),
            case Msg of
                "" -> empty;
                _->case string:chr(Msg,$/) of
                    1 -> CMD=chk_comm(string:tokens(Msg," "),?COMMANDS),
                        case CMD of
                            {exit,_} -> gen_tcp:close(S);
                            _ -> ok
                        end,
                        CMD;
                    _ -> {msg,Msg}
                end
            end;
        {tcp_closed,_} -> io:format("Exiting\n"),
            {exit,ok};
        _-> ok

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
    send_to_socket(M).


print_error(InfoError) ->
    M=io_lib:format("~s~n",[InfoError]),
    send_to_socket(M).
print_error(Error,Cause)->
    M=io_lib:format("ERROR ~s : ~p ~n",[Cause,Error]),
    send_to_socket(M).

print_info(Info) -> M=io_lib:format("~s~n",[Info]), send_to_socket(M).



send_to_socket(M) ->
    io:format(M),
    sender ! {msg,M}.


send_control(S1,S2)->
    receive
        {msg,M} -> gen_tcp:send(S1,M),send_control(S1,S2);
        stop -> ok
    end.
