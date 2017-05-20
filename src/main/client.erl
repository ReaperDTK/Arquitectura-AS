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

    %% Pregunta el nombre del nodo al usuario y lo convierte a un atom.
    ?IO:print_info("Server name@ip (/exit to quit) : "),
    case ?IO:get_line() of
        {exit, _} -> Loop ! stop, ok;
        {help,_}-> ?IO:print_info("type /exit to exit the chat \n type xxx to xxxx"),connect_node(Loop);
        {msg, S} -> Server = list_to_atom(S),
            %% Intenta conectarse
            case net_kernel:connect_node(Server) of
                true ->
                    %% El tiempo de espera es para asegurarse de que la conexión
                    %% se ha completado antes de hacer el global:whereis_name
                    timer:sleep(3000),
                    con_control(Loop,Server);
                false ->
                    ?IO:print_error("Server not found. Try Again"),
                    %% Falla al conectarse, lo reitenta
                    connect_node(Loop)
            end;
        _ -> ?IO:print_error("Non suported command. Try Again"),
            connect_node(Loop)
        end.

 %% Se encarga de la conectarse correctamente con el servidor
con_control_ant(Loop) ->

    %% Mensaje de registro con el PID del listen_loop para registrarlo en el
    %% servidor


    %% Comprueba si es capaz de conectarse al servidor. Si se ha conseguido
    %% conectar al nodo y el servidor está iniciado debería ir
    case global:whereis_name(server) of
        undefined ->
            %% Falla. Pregunta al cliente que quiere hacer
            ?IO:print_error("Couldn't establish connection with the server"),
            con_control_error(Loop,nonode);
           %% Se conecta correctamente. Le envia el mensaje de registro.
        PidServer ->
            ?IO:print_info("Connecting...\n") ,
            reg_user_name(Loop,PidServer)
    end.

con_control(Loop,ServerNode) ->

    %% Mensaje de registro con el PID del listen_loop para registrarlo en el
    %% servidor
    {server,ServerNode} ! {ping,self()},
    receive
        {pang,PidServer} ->
            ?IO:print_info("Connecting...\n") ,
            reg_user_name(Loop,PidServer);
        _ -> ?IO:print_error("Couldn't establish connection with the server"),
            con_control_error(Loop,ServerNode)
    after
        6000 -> ?IO:print_error("Couldn't establish connection with the server"),
                con_control_error(Loop,ServerNode)
    end.

reg_user_name(Loop, PidServer)->%% Pregunta el nombre para unirse a la sala
  ?IO:print_info("Name to join chatroom? "),
  {_,Name} = ?IO:get_line(),
  RegUserMsg = {con, Loop, Name, self()},
  PidServer ! RegUserMsg,
  case wait_for_answer() of
      {ok,CRoom,Server} -> register(inputloop,self()), input_loop(Loop, Name,CRoom,Server);
      _ -> reg_user_name(Loop,PidServer)
  end.


wait_for_answer() ->
    receive
        {ok, joined,CRoom,Server} ->
            ?IO:print_info("Joined"),
            io:format("SUPUTAMADRE!!!!! ~n"),
            Server ! {ping,self()},
            {ok,CRoom,Server};
        {error, user_repeated} ->
            ?IO:print_error("There's already an user with that name. "),
            error
    end.

con_control_error(Loop,ServerNode)->
    ?IO:print_info("Type r to retry , c to change server or q to exit"),
    case ?IO:get_line() of
        {msg, "c"} ->  connect_node(Loop);
        {msg, "r"} ->con_control(Loop,ServerNode);
        {msg, "q"} -> Loop ! stop,   ok;
        _ -> ?IO:print_error("Non suported entry."), con_control_error(Loop,ServerNode)
   end.

%% Loop para entrada de texto

input_loop(LLoop, Name,ChatRoom,Server)->
    ?IO:print_info(string:concat(Name, ":")),
    M = ?IO:get_line(),
    case  M of
        empty ->
            input_loop(LLoop, Name,ChatRoom,Server);
        {error, ERROR, I}-> ?IO:print_error(ERROR, I),
            input_loop(LLoop, Name,ChatRoom,Server);
        {exit, _} ->
            LLoop ! stop, Server ! {disc,LLoop,Name,ChatRoom}, unregister(inputloop), ?IO:print_info("Good Bye!");
          {help,_}-> ?IO:print_info("type /exit to exit the chat \n type xxx to xxxx \n"),input_loop(LLoop, Name,ChatRoom,Server);
        {msg,Msg} ->
            Server ! {msg, Msg, Name,ChatRoom},
            input_loop(LLoop, Name,ChatRoom,Server);
        {join,Args} ->
            Server ! {join, Args, {LLoop,Name},ChatRoom},
            receive
                {ok,NEWCR} -> io:format("~p~n",[NEWCR]),input_loop(LLoop,Name,NEWCR,Server);
                {error,_}-> input_loop(LLoop, Name,ChatRoom,Server)
            end;
        {leave,_} ->
            Server ! {leave, {LLoop,Name},ChatRoom},
            receive
                {ok,NEWCR} -> io:format("~p~n",[NEWCR]),input_loop(LLoop,Name,NEWCR,Server);
                {error,_}-> input_loop(LLoop, Name,ChatRoom,Server)
            end;
        {whisper, Args} ->
            Server ! {whisper, Args, Name, ChatRoom},
            input_loop(LLoop, Name, ChatRoom,Server);
        {create, Args} ->
            Server ! {create, Args, {LLoop, Name}},
            receive
                {ok, NEWCR} -> io:format("~p~n", [NEWCR]),
                               input_loop(LLoop, Name, NEWCR,Server);
                _ -> input_loop(LLoop, Name, ChatRoom,Server)
            end;
        {C, Args} ->
            Server ! {C, Args, {LLoop,Name},ChatRoom},
            input_loop(LLoop, Name,ChatRoom,Server)

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
        {room_changed,NewCR} ->INFO=io_lib:format("Welcome to the ~p",[NewCR]) ,?IO:print_info(INFO), inputloop ! {ok,NewCR}, listen_loop();
        {room_created, NewCR} -> INFO = io_lib:format("Room ~p has been created", [NewCR]),
                                 ?IO:print_info(INFO),
                                 inputloop ! {ok, NewCR},
                                 listen_loop();
         {error,ERROR} ->?IO:print_error(ERROR), inputloop!  {error,ERROR}, listen_loop();
        _ -> listen_loop()
    end.
