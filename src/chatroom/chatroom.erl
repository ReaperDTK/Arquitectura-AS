-module(chatroom).



create(Name) -> {Name,[]}.


add_user_aux([],User) -> ok;
add_user_aux([User|_],User)-> already_in_room;
add_user_aux([H|T] ,User) -> add_user_aux(T,User).
add_user({Name,L},User)->
    case add_user_aux of
        ok -> {Name,[User|L]};
        _ -> {Name,L}
    end.



%No implementado todavia. Solo por si en unfuturo se dice hace salas privadas
create_private(Name,Password)->{Name,Password,[]}.
