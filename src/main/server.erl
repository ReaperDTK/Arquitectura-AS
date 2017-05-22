-module(server).


-export([start/0, stop/0,create_room/1,get_users/1,start_master/0,start_sserver/1,set_as_recovery/1]).

-define(AUX,aux).

-define(MAX_CHATROOMS,10).


%%----------------------------------------------------------------------%%
%% FUNCIONES AUXILIARES
%%----------------------------------------------------------------------%%

%% Llama al comando epmd -names para comprobar si epmd se esta ejecutando,
%% si no lo ejecuta
check_epmd()->
    case os:cmd("epmd -names") of
        "epmd: Cannot connect to local epmd\n" -> os:cmd("epmd -daemon"),  ok;
        _ -> ok
    end.


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

init_rooms([]) ->
    ok;

init_rooms([H|T]) ->
    chatroom_manager ! {create_room,H},
    init_rooms(T).

stop() ->
    exit(whereis(server), stop).

%%----------------------------------------------------------------------%%
%% IMPLEMENTACION SERVER UNICO
%%----------------------------------------------------------------------%%
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



init_srv(Control) ->
    %% Hace que cuando mandan un exit(PID, Reason) en lugar de hacer saltar
    %% una excepcion el proceso reciba un mensaje {'EXIT', From, Reason}
    process_flag(trap_exit, true),
    register(main,spawn(fun()-> chatroom_loop(main,[]) end)),
    register(chatroom_manager,spawn(fun()-> chatroom_manager([main]) end)),
    global:register_name(main,whereis(main)),
    global:register_name(chatroom_manager,whereis(chatroom_manager)),
    catch init_rooms(getRooms()),
    listen_loop([], Control,[main]).





listen_loop(U, Control,ChatRooms)->
    receive
        %% LLega una peticion de conexión al servidor
        {ping,From} -> From ! {pang,self()} , listen_loop(U,Control,ChatRooms);
        {con, A, Name, From} ->
            case add_user(A, Name, U) of
                {ok, NewList} ->
                    main ! {add,{A,Name}},
                    From ! {ok, joined,main,self()},
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
            catch global:whereis_name(ChatRoom) ! {msg,M,Name},
%                chatroom_manager ! {msg, M, Name,ChatRoom},
            listen_loop(U, Control,ChatRooms);
        {whisper, Args, Name, ChatRoom} ->
            catch global:whereis_name(ChatRoom) ! {whisper, Args, Name},
            listen_loop(U, Control,ChatRooms);
        {available_rooms,User} -> catch (global:whereis_name(chatroom_manager) ! {get_rooms,User}),
            listen_loop(U, Control, ChatRooms);
        %% Controla que el unico proceso que puede cerrar el servidor es el que
        %% lo ha creado
        {'EXIT', Control, stop} ->global:unregister_name(server) , chatroom_manager ! close_manager, ok;
        %% Ignora el resto de mensajes
        _ -> listen_loop(U, Control,ChatRooms)
    end.




%%----------------------------------------------------------------------%%
%% IMPLEMENTACION DE MULTISERVIDOR
%%----------------------------------------------------------------------%%

set_as_recovery(Master) ->
    erlang:set_cookie(node(), chat),
    erlang:monitor_node(Master,true),
    %% SI detecta que el nodo se ha caido, intenta detener el server, si existe, e inicia un nuevo master
	receive
		{nodedown, _} ->(catch server ! stop) , start_master(), rpc:abcast(nodes(),server,reconect)
	end.


start_sserver(Master)->
    erlang:set_cookie(node(), chat),
    Control=self(),
    net_kernel:connect_node(Master),
    timer:sleep(3000),
    Pid = spawn(fun() ->   init_ssrv(Control) end),
    %% Registra el pid como server para este shell de erlang
    register(server, Pid).

start_master()->
    erlang:set_cookie(node(), chat),
    Control=self(),
    Pid = spawn(fun() ->   init_mltsrv(Control) end),
    %% Registra el pid como server para este shell de erlang
    register(server, Pid),
    %% Registra el pid como server globalmente, pàra todos los nodos
    global:register_name(server, Pid).

init_ssrv(Control)->
    global:whereis_name(server) ! {reg_server,self()},
    process_flag(trap_exit, true),
    register(main,spawn(fun()-> chatroom_loop(main,[]) end)),
    register(chatroom_manager,spawn(fun()-> chatroom_manager([main]) end)),
    server_listen_loop([], Control,[main]).

init_mltsrv(Control) ->
        %% Hace que cuando mandan un exit(PID, Reason) en lugar de hacer saltar
        %% una excepcion el proceso reciba un mensaje {'EXIT', From, Reason}
        process_flag(trap_exit, true),
        register(main,spawn(fun()-> chatroom_loop(main,[]) end)),
        global:register_name(main,whereis(main)),
        P=spawn(fun()-> chatroom_manager([main]) end),
        register(chatroom_manager,P),
        global:register_name(chatroom_manager,P),
        init_rooms(getRooms()),
        master_listen_loop([], Control,[]).

%% MULTISERVIDOR : SERVER MAESTRO BALANCEADOR DE CARGA
master_listen_loop(U,Control,Servers) ->
    receive
        {ping,From} -> From ! {pang,self()} , master_listen_loop(U,Control,Servers);
        {reg_server,Pid} -> io:format("Servidor registrado~n"), master_listen_loop(U,Control,[Pid|Servers]);
        {reg_server,Pid,Users} -> io:format("Servidor registrado~n"), master_listen_loop(lists:append(U,Users),Control,[Pid|Servers]);
        {unreg_server,Pid} -> master_listen_loop(U,Control,[X|| X<- Servers, X/=Pid]);
        {con, A, Name, From} ->
            ServerNth = rand:uniform(length(Servers)),
            Server = lists:nth(ServerNth,Servers),
            io:format("~p ~p ~n",[ServerNth,Server]),
            case add_user(A, Name, U) of
                {ok, NewList} ->
                    main ! {add,{A,Name}},
                    From ! {ok, joined,main,Server},
                    master_listen_loop(NewList, Control,Servers);
                {error, user_repeated} ->
                    From ! {error, user_repeated},
                    master_listen_loop(U, Control,Servers)
            end;
        {disc, A, Name,ChatRoom} ->
            catch global:whereis_name(ChatRoom)! {leave,{A,Name}},
            master_listen_loop([X || X <- U,  X/={A, Name}], Control,Servers);
        {'EXIT', Control, stop} ->global:unregister_name(server), unregister(server) , chatroom_manager ! close_manager, ok
    end.


%% MULTISERVIDOR: NODO DE LA RED DE SERVERS
server_listen_loop(U, Control,ChatRooms)->

    receive
        {ping,From} -> From ! {pang,self()} , server_listen_loop([From|U],Control,ChatRooms);
        %% LLega una peticion de conexión al servidor
        {con,Name,From} -> server_listen_loop([{Name,From}|U],Control,ChatRooms);
        {leave,User,ChatRoom}->
            chatroom_manager ! {join,main,User,ChatRoom},
            server_listen_loop(U, Control,ChatRooms);
        %% Desconexión del servidor
        {join,Args,User,ChatRoom} ->
            chatroom_manager ! {join,Args,User,ChatRoom},
            server_listen_loop(U, Control,ChatRooms);
        {disc, A, Name,ChatRoom} ->
            catch global:whereis_name(server) ! {disc, A, Name,ChatRoom},
            catch global:whereis_name(ChatRoom) ! {leave, Name},
            server_listen_loop([X || X <- U,  X/={A, Name}], Control,ChatRooms);
        %% Mensaje
        {msg, M, Name,ChatRoom} ->
                io:format("Send message to ~p~n",[global:whereis_name(ChatRoom)]),
                catch global:whereis_name(ChatRoom) ! {msg, M, Name},
                %chatroom_manager ! {msg, M, Name,ChatRoom},
                server_listen_loop(U, Control,ChatRooms);
        {whisper, Args, Name, ChatRoom} ->
            catch global:whereis_name(ChatRoom) ! {whisper, Args, Name},
            server_listen_loop(U, Control,ChatRooms);
        {create, Args, User} ->
            chatroom_manager ! {create_room_client, Args, User},
            server_listen_loop(U, Control, ChatRooms);
        {available_rooms,User} -> catch (global:whereis_name(chatroom_manager) ! {get_rooms,User}),
            server_listen_loop(U, Control, ChatRooms);
        %% Controla que el unico proceso que puede cerrar el servidor es el que
        %% lo ha creado
        {'EXIT', Control, stop} -> unregister(server) ,  chatroom_manager ! close_manager;
        reconect -> timer:sleep(3000), global:whereis_name(server) ! {reg_server,self(),U};
        %% Ignora el resto de mensajes
        _ -> server_listen_loop(U, Control,ChatRooms)
    end.



%%----------------------------------------------------------------------%%
%% IMPLEMENTACION FUNCIONES DE SALAS
%%----------------------------------------------------------------------%%


%%Funcion que maneja las salas .
chatroom_manager(ChatRooms) ->
    receive
        {join,Args,User,ChatRoom} ->
            [String|_]=string:tokens(Args," "),
            NewCR=list_to_atom(String),
            {Pid,_}=User,
            case global:whereis_name(NewCR) of
                undefined -> Pid ! {error,"No chatroom with that name"};
                CRPid -> case catch global:whereis_name(ChatRoom) ! {leave,User} of
                                {leave,User} ->
                                    CRPid ! {add,User},
                                    Pid ! {room_changed,NewCR};
                                _ -> Pid !{error,"Something went wrong. Try joining again"}
                            end
            end,
            chatroom_manager(ChatRooms);
        {create_room,Name} ->
            Pid=spawn(fun()-> chatroom_loop(Name,[]) end),
            case (catch global:whereis_name(chatroom_manager) ! {create_room,Name,Pid,self()}) of
                {create_room,Name,Pid,_} -> chatroom_manager([Name |ChatRooms]);
                _ -> io:format("FALLA ~n") ,Pid ! stop, chatroom_manager(ChatRooms)
            end;
        {create_room_client, Args, _} ->
            [String | _] = string:tokens(Args, " "),
            Name = list_to_atom(String),
            Pid=spawn(fun()-> chatroom_loop(Name,[]) end),
            case (catch global:whereis_name(chatroom_manager) ! {create_room,Name,Pid,self()}) of
                {create_room,Name,Pid,_} -> ok;
                _ -> io:format("FALLA ~n") ,Pid ! stop
            end,
            chatroom_manager(ChatRooms);
        {create_room,Name,Pid,From} ->
                try global:register_name(Name,Pid) of
                    yes -> chatroom_manager([Name |ChatRooms]) ;
                    _ -> chatroom_manager(ChatRooms),From ! {failed,Pid}
                catch
                    _ -> chatroom_manager(ChatRooms),From ! {failed,Pid}
                end;
        {msg, M, Name,ChatRoom} ->
        case lists:member(ChatRoom,ChatRooms) of
            true -> ChatRoom ! {msg, M, Name};
            _ -> ok
        end,
        chatroom_manager(ChatRooms);
        close_manager -> lists:foreach(fun(E) -> catch (global:whereis_name(E) ! stop) end,ChatRooms) , ok;
        {failed,Pid} -> Pid ! stop, chatroom_manager([X|| X <-ChatRooms, X/=Pid]);
        {get_rooms,User} -> User ! {info,io_lib:format("ChatRooms ~p ",[ChatRooms])}, chatroom_manager(ChatRooms);
        _-> chatroom_manager(ChatRooms)
    end.

chatroom_loop(Name,Users) ->
    receive
        {add,User} ->
            io:format("~p~n",[Name]),
            {_,UserName}=User, Msg=io_lib:format("~s Joined the ChatRoom~n",[UserName]),
            send_msg({info,Msg},User,Users) ,
            chatroom_loop(Name,[User|Users]);
        {msg,Msg,User} ->
            io:format("sending msg~n"),
            send_msg({msg, Msg, User}, User, Users),
            chatroom_loop(Name,Users);
        {whisper, Args, User} ->
            [Dest|Msg] = string:tokens(Args, " "),
            case whisper({msg, string:join(Msg, " "), User}, User, Dest, Users) of
                {Error, Reason} ->
                    whisper({msg, string:concat(Error, Reason), system}, system, User, Users);
                ok -> ok
            end,
            chatroom_loop(Name, Users);
        {leave,User} ->
            io:format("someones leaves ~p~n",[Name]),
            {_,UserName}=User, Msg=io_lib:format("~s left ~p chatroom ~n",[UserName,Name]),
            send_msg({info,Msg},User,Users),
            chatroom_loop(Name,[X || X <- Users, X/=User]);
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
