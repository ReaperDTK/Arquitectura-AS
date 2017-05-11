-module(client).

-export([start/0]).

-define(RETRIES, 3).



-define(IO, inout).


%%inicio
start()->
    erlang:set_cookie(node(), chat),
    LLoop= spawn(fun() -> listen_loop() end),
    connect_node(LLoop).

%%conecta con el nodo del shell del servidor, NO CON EL SERVIDOR
connect_node(Loop)->
    %% Pregunta el nombre para unirse a la sala
    ?IO:print_info("Name to join chatroom? "),
    {_,Name} = ?IO:get_line(),
    %% Pregunta el nombre del nodo al usuario y lo convierte a un atom.
    ?IO:print_info("Server name@ip (/exit to quit) : "),
    case ?IO:get_line() of
        {exit, _} -> Loop ! stop, ok;
        {msg, S} -> Server = list_to_atom(S),
            %% Intenta conectarse
            case net_kernel:connect_node(Server) of
                true ->
                    %% El tiempo de espera es para asegurarse de que la conexión
                    %% se ha completado antes de hacer el global:whereis_name
                    timer:sleep(3000),
                    con_control(Loop, Name);
                false ->
                    ?IO:print_error("Server not found. Try Again"),
                    %% Falla al conectarse, lo reitenta
                    connect_node(Loop)
            end;
        _ -> ?IO:print_error("Non suported command. Try Again"),
            connect_node(Loop)
        end.

%% Se encarga de la conectarse correctamente con el servidor
con_control(Loop, Name) ->

    %% Mensaje de registro con el PID del listen_loop para registrarlo en el
    %% servidor
    RegUserMsg = {con, Loop, Name, self()},

    %% Comprueba si es capaz de conectarse al servidor. Si se ha conseguido
    %% conectar al nodo y el servidor está iniciado debería ir
    case global:whereis_name(server) of
        undefined ->
            %% Falla. Pregunta al cliente que quiere hacer
            ?IO:print_error("Couldn't establish connection with the server"),
            con_control_error(Loop, Name);
           %% Se conecta correctamente. Le envia el mensaje de registro.
        PidServer ->
            ?IO:print_info("Connecting...\n") ,
            PidServer ! RegUserMsg,
            case wait_for_answer() of
                ok -> input_loop(Loop, Name);
                _ -> connect_node(Loop)
            end
    end.

wait_for_answer() ->
    receive
        {ok, joined} ->
            ?IO:print_info("Joined"), ok;
        {error, user_repeated} ->
            ?IO:print_error("There's already an user with that name. "),
            error
    end.

con_control_error(Loop, Name)->
    ?IO:print_info("Type r to retry , c to change server or q to exit"),
    case ?IO:get_line() of
        {msg, "c"} ->  connect_node(Loop);
        {msg, "r"} ->con_control(Loop, Name);
        {msg, "q"} -> Loop ! stop,   ok;
        _ -> ?IO:print_error("Non suported entry."), con_control_error(Loop, Name)
   end.

%% Loop para entrada de texto
input_loop(LLoop, Name)->
    ?IO:print_info(string:concat(Name, ":")),
    M=?IO:get_line(),
    case  M of
        empty ->
            input_loop(LLoop, Name);
        {error, ERROR, I}-> ?IO:print_error(ERROR, I),
            input_loop(LLoop, Name);
        {exit, _} ->
            LLoop ! stop, global:whereis_name(server) ! {disc, LLoop},  ?IO:print_info("Good Bye!");
        {C, Args} ->
            global:whereis_name(server) ! {C, Args, Name},
            input_loop(LLoop, Name)

    end.

%%Loop que espera por mensajes del servidor
listen_loop()->
    receive
        {msg, M, Name} ->
            %% Imprime el mensaje y se pone a esperar de nuevo
            ?IO:print_Msg(Name, M),
            listen_loop();
        stop ->
            %% Se desconecta de todos los nodos y termina su ejecución.
            lists:foreach(fun(Node)->
                erlang:disconnect_node(Node) end,  nodes()),
            ok;
        _ -> listen_loop()
    end.
