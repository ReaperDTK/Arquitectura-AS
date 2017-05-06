-module(client).

-export([start/0]).

-define(RETRIES,3).



-define(IO,inout).


%%inicio
start()->
    erlang:set_cookie(node(),chat),
    LLoop= spawn(fun() -> listen_loop() end),
    conect_node(LLoop).

%%conecta con el nodo del shell del servidor, NO CON EL SERVIDOR
conect_node(Loop)->
    %% Pregunta el nombre del nodo al usuario y lo convierte a un atom.
    ?IO:print_info("Server name@ip (/exit to quit) : "),
    case ?IO:get_line() of
        {exit,_} -> Loop ! stop ,ok;
        {msg,S} -> Server=list_to_atom(S),
            %% Intenta conectarse
            case net_kernel:connect_node(Server) of
                true ->
                    %%El tiempo de espera es para asegurarse de que la conexión se ha completado antes de hacer el global:whereis_name
                    timer:sleep(3000),
                    con_control(Loop);
                false ->
                    ?IO:print_error("Server not found. Try Again"),
                    %% Falla al conectarse, lo reitenta
                    conect_node(Loop)
            end;
        _ -> ?IO:print_error("Non suported command. Try Again"),
            conect_node(Loop)
        end.



%% Se encarga de la conectarse correctamente con el servidor
con_control(Loop) ->

    %% Mensaje de registro con el PID del listen_loop para registrarlo en el servidor
    RegUserMsg = {con,Loop},

    %% Comprueba si es capaz de conectarse al servidor. Si se ha conseguido conectar al nodo y el servidor está iniciado debería ir
    case global:whereis_name(server) of
        undefined ->
            %% Falla. Pregunta al cliente que quiere hacer
            ?IO:print_error("Couldn't establish connection with the server"),
            con_control_error(Loop);
           %% Se conecta correctamente. Le envia el mensaje de registro.
       PidServer -> ?IO:print_info("Conected\n") , PidServer ! RegUserMsg, input_loop(Loop)
    end.

con_control_error(Loop)->
    ?IO:print_info("Type r to retry , c to change server or q to exit"),
    case ?IO:get_line() of
        {msg,"c"} ->  conect_node(Loop);
        {msg,"r"} ->con_control(Loop);
        {msg,"q"} -> Loop ! stop,  ok;
        _ -> ?IO:print_error("Non suported entry."), con_control_error(Loop)
   end.
%% Loop para entrada de texto
input_loop(LLoop)->
    M=?IO:get_line(),
    case  M of
        empty ->
            input_loop(LLoop);
        {error,ERROR,I}-> ?IO:print_error(ERROR,I),
            input_loop(LLoop);
        {exit,_} ->LLoop ! stop , global:whereis_name(server) ! {disc,LLoop},  ok;
        {C,Args} ->
            global:whereis_name(server) ! {C,Args,LLoop},
            input_loop(LLoop)

    end.


%%Loop que espera por mensajes del servidor
listen_loop()->
    receive
        {msg,M,From} ->
            %% Imprime el mensaje y se pone a esperar de nuevo
            ?IO:print_Msg(From,M),
             listen_loop();
        stop ->
            %% Se desconecta de todos los nodos y termina su ejecución.
            lists:foreach(fun(Node)-> erlang:disconnect_node(Node) end, nodes()),
            ok;
        _ -> listen_loop()
    end.
