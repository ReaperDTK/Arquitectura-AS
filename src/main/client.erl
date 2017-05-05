-module(client).

-export([start/0]).

-define(RETRIES,3).

%%inicio
start()->
    erlang:set_cookie(node(),chat),
    LLoop= spawn(fun() -> listen_loop() end),
    conect_node(LLoop).

%%conecta con el nodo del shell del servidor, NO CON EL SERVIDOR
conect_node(Loop)->
    %% Pregunta el nombre del nodo al usuario y lo convierte a un atom.
    case list_to_atom(lists:droplast(io:get_line("Server name@ip (q to quit) : "))) of
        q -> Loop ! stop ,ok;
        Server ->
            %% Intenta conectarse
            case net_kernel:connect_node(Server) of
                true ->
                    %%El tiempo de espera es para asegurarse de que la conexión se ha completado antes de hacer el global:whereis_name
                    timer:sleep(3000),
                    con_control(Loop);
                false ->
                    io:format("Server not found. Try Again\n"),
                    %% Falla al conectarse, lo reitenta
                    conect_node(Loop)
            end
        end.



%% Se encarga de la conectarse correctamente con el servidor
con_control(Loop) ->

    %% Mensaje de registro con el PID del listen_loop para registrarlo en el servidor
    RegUserMsg = {con,Loop},

    %% Comprueba si es capaz de conectarse al servidor. Si se ha conseguido conectar al nodo y el servidor está iniciado debería ir
    case global:whereis_name(server) of
        undefined ->
            %% Falla. Pregunta al cliente que quiere hacer
            case lists:droplast(io:get_line("Couldn't establish connection with the server. Type r to retry , c to change server or q to exit\n")) of
                "c" ->  conect_node(Loop);
                "r" ->con_control(Loop);
                "q" -> Loop ! stop,  ok
           end;
           %% Se conecta correctamente. Le envia el mensaje de registro.
       PidServer -> io:format("Conected\n") , PidServer ! RegUserMsg, input_loop(Loop)
    end.



%% Loop para entrada de texto
input_loop(LLoop)->
    M=lists:droplast(io:get_line("")),
    ToSend = {msg,M,LLoop},
    case  M of
        "" -> input_loop(LLoop);
        "/exit" ->LLoop ! stop , global:whereis_name(server) ! {disc,LLoop},  ok;
        _ ->
            global:whereis_name(server) ! ToSend,
            input_loop(LLoop)
    end.


%%Loop que espera por mensajes del servidor
listen_loop()->
    receive
        {msg,M,From} ->
            %% Imprime el mensaje y se pone a esperar de nuevo
            io:format("~p : ~p \n",[From,M]),
             listen_loop();
        stop ->
            %% Se desconecta de todos los nodos y termina su ejecución.
            lists:foreach(fun(Node)-> erlang:disconnect_node(Node) end, nodes()),
            ok;
        _ -> listen_loop()
    end.
