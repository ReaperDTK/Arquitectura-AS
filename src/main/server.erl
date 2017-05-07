-module(server).


-export([start/0, stop/0]).

%% Llama al comando epmd -names para comprobar si epmd se esta ejecutando, si no lo ejecuta
check_epmd()->
    case  os:cmd("epmd -names") of
        "epmd: Cannot connect to local epmd\n" -> os:cmd("epmd -daemon"), ok;
        _ -> ok
    end.

start()->
    case node() of
        %% Si no has declarado el nodo al arrancar el shell de erlang, te deja ponerle ahora un nombre
        'nonode@nohost' -> check_epmd(),
            net_kernel:start([list_to_atom(lists:droplast(io:get_line("Server name ( name@ip ): ")))]);
        _ -> ok
    end,
    erlang:set_cookie(node(),chat),
    Control=self(),
    Pid = spawn(fun() ->   init_srv(Control) end),
    %% Registra el pid como server para este shell de erlang
    register(server,Pid),
    %% Registra el pid como server globalmente, pàra todos los nodos
    global:register_name(server,Pid).

stop() ->
    exit(whereis(server),stop).


init_srv(Control) ->
    %% Hace que cuando mandan un exit(PID,Reason) en lugar de hacer saltar una excepcion
    % el proceso reciba un mensaje {'EXIT',From,Reason}
    process_flag(trap_exit,true),
    listen_loop([],Control).

listen_loop(U,Control)->
    receive
        %% LLega una peticion de conexión al servidor
        {con,A} -> listen_loop([A|U],Control);
        %% Desconexión del servidor
        {disc,A} -> listen_loop([X || X <- U, X/=A],Control);
        %%Mensaje
        {msg,M,From} ->  send_msg({msg,M,From},From,U), listen_loop(U,Control);
        %% Controla que el unico proceso que puede cerrar el servidor es el que lo ha creado
        {'EXIT',Control,stop} ->global:unregister_name(server) ,ok;
        %% Ignora el resto de mensajes
        _ -> listen_loop(U,Control)
    end.


%% Envia mensaje a los usuarios
%% Cuando se implemente bien el tema de usuarios hay que cambiarlo puesto que el server por ahora solo guarda una lista de PIDS de usuario
send_msg(_,_,[]) -> ok;
send_msg(M,F,[F|T]) -> send_msg(M,F,T);
send_msg(M,F,[H|T]) -> H ! M,send_msg(M,F,T).
