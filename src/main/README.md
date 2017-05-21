# Chat #


## Servidor

Para iniciar el modulo server es necesario iniciar primero una shell de erlang de la siguiente manera

    erl -name server@ip

Donde server podeis ponerle el nombre que querais.
La ip es importante que le pongáis una accesible por los clientes. Para probar todo en una sola maquina, vale 127.0.0.1 .
`server@ip` es lo que os pedira el cliente para conectaros al servidor

Para arrancar el servidor del chat `server:start()` y para cerrarlo `server:stop()`

### Multiservidor
Si se quiere arrancar el multiservidor. Es necesario arrancar tantos shell de erlang como servidores se quieran usar, y uno para el server balanceador de carga(master). Primero hay que arrancar master con `server:start_master()` y despues en el resto de terminales arrancar los servidores con `server:start_sserver('master@ipmaster')`


## Cliente

Para iniciar el modulo client es necesario iniciar primero una shell de erlang de la siguiente manera

    erl -name cliente@hostname

Donde pone cliente, podeis ponerle el nombre que querais, pero es necesario que sea diferente en el caso de que inicieis varias shell en la misma maquina o cuando la ip o hostname sea el mismo.

En lugar de la ip del PC  o hostname, podéis poner cualquier otra cosa, pero en ese caso os puede salir un warning al conectar los nodos, aunque debería funciona igual.

Una vez en el shell el cliente se inicia con `client:start().`
Cuando se inicie ya saldrán unos mensajes que irán informando de lo que ocurre.

Una vez conectado al servidor, para salir escribe `/exit`



Más info del funcionamiento e implementación en los comentarios del código



-------------
