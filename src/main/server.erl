-module(server).


-export([start/0, stop/0,create_room/1,get_users/1]).

-define(AUX,aux).

-define(MAX_CHATROOMS,10).

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
            net_kernel:start([list_to_atom(?AUX:droplast(io:get_line("Server name ( name@ip ): ")))]);
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

init_rooms([]) ->
    ok;

init_rooms([H|T]) ->
    chatroom_manager ! {create_room,H},
    init_rooms(T).

init_srv(Control) ->
    %% Hace que cuando mandan un exit(PID, Reason) en lugar de hacer saltar
    %% una excepcion el proceso reciba un mensaje {'EXIT', From, Reason}
    process_flag(trap_exit, true),
    register(main,spawn(fun()-> chatroom_loop(main,[]) end)),
    register(chatroom_manager,spawn(fun()-> chatroom_manager([main]) end)),
    init_rooms(getRooms()),
    listen_loop([], Control,[main]).

    %% Recupera los nombres de las salas de un archivo.
getRooms() ->
    {ok, RoomList} = file:consult("./rooms.txt"),
    % Cambiar la ruta en cada ordenador.
    RoomList.

create_room(Name) ->
    chatroom_manager ! {create_room,Name}.

get_users(Room)->
    Room ! {get_users,self()},
    receive
        Users -> Users
    end.

listen_loop(U, Control,ChatRooms)->
    receive
        %% LLega una peticion de conexión al servidor
        {con, A, Name, From} ->
            case add_user(A, Name, U) of
                {ok, NewList} ->
                    main ! {add,{A,Name}},
                    From ! {ok, joined,main},
                    listen_loop(NewList, Control,ChatRooms);
                {error, user_repeated} ->
                    From ! {error, user_repeated},
                    listen_loop(U, Control,ChatRooms)
            end;
        {leave,User,ChatRoom}->
            chatroom_manager ! {join,main,User,ChatRoom},
            listen_loop(U, Control,ChatRooms);
        %% Desconexión del servidor
        {join,Args,User,ChatRoom} ->
            chatroom_manager ! {join,Args,User,ChatRoom},
            listen_loop(U, Control,ChatRooms);
        {disc, A, Name,ChatRoom} ->
            catch ChatRoom ! {leave,{A,Name}},
            listen_loop([X || X <- U,  X/={A, Name}], Control,ChatRooms);
        {create, Args, User} ->
            chatroom_manager ! {create_room_client, Args, User},
            listen_loop(U, Control, ChatRooms);
        %% Mensaje
        {msg, M, Name,ChatRoom} ->
            catch ChatRoom ! {msg,M,Name},
%                chatroom_manager ! {msg, M, Name,ChatRoom},
            listen_loop(U, Control,ChatRooms);
        {whisper, Args, Name, ChatRoom} ->
            catch ChatRoom ! {whisper, Args, Name},
            listen_loop(U, Control,ChatRooms);
        %% Controla que el unico proceso que puede cerrar el servidor es el que
        %% lo ha creado
        {'EXIT', Control, stop} ->global:unregister_name(server) , chatroom_manager ! close_manager, ok;
        %% Ignora el resto de mensajes
        _ -> listen_loop(U, Control,ChatRooms)
    end.



chatroom_manager(ChatRooms) ->
    receive
        {join,main,User,ChatRoom} ->
            catch ChatRoom ! {leave,User},
            {Pid,_}=User,
            main ! {add,User} ,Pid ! {room_changed,main},
            chatroom_manager(ChatRooms);
        {join,Args,User,ChatRoom} ->
            [String|_]=string:tokens(Args," "),
            NewCR=list_to_atom(String),
            {Pid,_}=User,
            case lists:member(NewCR,ChatRooms) of
                true -> catch ChatRoom ! {leave,User},
                        NewCR ! {add,User},
                        Pid ! {room_changed,NewCR};
                _ -> Pid ! {error,"No chatroom with that name"}
            end,
            chatroom_manager(ChatRooms);
        {create_room,Name} ->
            try register(Name,spawn(fun()-> chatroom_loop(Name,[]) end)) of
                true -> chatroom_manager([Name|ChatRooms]) ;
                _ -> chatroom_manager(ChatRooms)
            catch
                _ -> chatroom_manager(ChatRooms)
            end;
        {create_room_client, Args, User} ->
            [String | _] = string:tokens(Args, " "),
            NewCR = list_to_atom(String),
            {Pid, UserName} = User,
            case lists:member(NewCR, ChatRooms) of
                true -> Pid ! {error, "There is already a chatroom with that name"};
                _ ->
                    try register(NewCR, spawn(fun()-> chatroom_loop(NewCR, []) end)) of
                        true -> io:format("~p has created a new room: ~p~n", [UserName, NewCR]),
                                Pid ! {room_created, NewCR},
                                chatroom_manager([NewCR | ChatRooms]);
                        _ -> chatroom_manager(ChatRooms)
                    catch
                        _ -> chatroom_manager(ChatRooms)
                    end
            end,
            chatroom_manager(ChatRooms);
        {msg, M, Name,ChatRoom} ->
        case lists:member(ChatRoom,ChatRooms) of
            true -> ChatRoom ! {msg, M, Name};
            _ -> ok
        end,
        chatroom_manager(ChatRooms);
        close_manager -> lists:foreach(fun(E) -> E ! stop end,ChatRooms) ,ok;
        _-> chatroom_manager(ChatRooms)
    end.

chatroom_loop(Name,Users) ->
    receive
        {add,User} -> io:format("~p~n",[Name]) ,chatroom_loop(Name,[User|Users]);
        {msg,Msg,User} ->io:format("sending msg~n"), send_msg({msg, Msg, User}, User, Users), chatroom_loop(Name,Users);
        {whisper, Args, User} ->
            [Dest|Msg] = string:tokens(Args, " "),
            case whisper({msg, string:join(Msg, " "), User}, User, Dest, Users) of
                {Error, Reason} ->
                    whisper({msg, string:concat(Error, Reason), system}, system, User, Users);
                ok -> ok
            end,
            chatroom_loop(Name, Users);
        {leave,User} -> chatroom_loop(Name,[X || X <- Users, X/=User]);
        stop -> ok
    end.

whisper(_, _, _, []) -> {"error: ", "name_does_not_exist"};
whisper(_, Name, Name, _) -> {"error: ", "live_a_life"};
whisper(M, _, Dest, [{H, Dest}|_]) -> H ! M, ok;
whisper(M, Name, Dest, [_|T]) -> whisper(M, Name, Dest, T).

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
