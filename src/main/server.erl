-module(server).


-export([start/0, stop/0]).

-define(IO, inout).

%% Llama al comando epmd -names para comprobar si epmd se esta ejecutando,
%% si no lo ejecuta
check_epmd()->
    case os:cmd("epmd -names") of
        "epmd: Cannot connect to local epmd\n" -> os:cmd("epmd -daemon"),  ok;
        _ -> ok
    end.

start()->
    case node() of
        %% Si no has declarado el nodo al arrancar el shell de erlang,
        %% te deja ponerle ahora un nombre
        'nonode@nohost' -> check_epmd(),
            net_kernel:start([list_to_atom(lists:droplast(io:get_line("Server name ( name@ip ): ")))]);
        _ -> ok
    end,
    erlang:set_cookie(node(), chat),
    Control=self(),
    Pid = spawn(fun() ->   init_srv(Control) end),
    %% Registra el pid como server para este shell de erlang
    register(server, Pid),
    %% Registra el pid como server globalmente, pàra todos los nodos
    global:register_name(server, Pid).

stop() ->
    exit(whereis(server), stop).


init_srv(Control) ->
    %% Hace que cuando mandan un exit(PID, Reason) en lugar de hacer saltar
    %% una excepcion el proceso reciba un mensaje {'EXIT', From, Reason}
    process_flag(trap_exit, true),
    listen_loop([], Control).

listen_loop(U, Control)->
    receive
        %% LLega una peticion de conexión al servidor
        {con, A, Name, From} ->
            case add_user(A, Name, U) of
                {ok, NewList} ->
                    From ! {ok, joined},
                    listen_loop(NewList, Control);
                {error, user_repeated} ->
                    From ! {error, user_repeated},
                    listen_loop(U, Control)
            end;
        %% Desconexión del servidor
        {disc, A, Name} ->
            listen_loop([X || X <- U,  X/={A, Name}], Control);
        %% Mensaje
        {msg, M, Name} ->
            send_msg({msg, M, Name}, Name, U),
            listen_loop(U, Control);
        %% Controla que el unico proceso que puede cerrar el servidor es el que
        %% lo ha creado
        {'EXIT', Control, stop} ->global:unregister_name(server) , ok;
        %% Ignora el resto de mensajes
        _ -> listen_loop(U, Control)
    end.


%% Envia mensaje a los usuarios
%% Cuando se implemente bien el tema de usuarios hay que cambiarlo puesto que el
%% server por ahora solo guarda una lista de PIDS de usuario
send_msg(_, _, []) -> ok;
send_msg(M, Name, [{_, Name}|T]) ->
    send_msg(M, Name, T);
send_msg(M, Name, [{H, _}|T]) ->
    H ! M, send_msg(M, Name, T).

%% Devuelve una lista con el usuario introducido o un error si está repetido
add_user_aux(A, Name, [], ReturnList) -> {ok, [{A, Name}|ReturnList]};
add_user_aux(_, Name, [{_, Name}|_], _) -> {error, user_repeated};
add_user_aux(A, Name, [H|T], ReturnList) ->
    add_user_aux(A, Name, T, [H|ReturnList]).

add_user(A, Name, U) -> add_user_aux(A, Name, U, []).
