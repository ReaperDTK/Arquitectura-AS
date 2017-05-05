# Chat #

Esta implementado lo que sería la funcionalidad basica de un chat.
Aunque se imprimen mensajes ,  no he usado todavía el modulo de colores.
No esta implementado lo de los nombres de usuario , por ahora se utilizan los PID para identificar a los clientes.


##Servidor##

Para iniciar el modulo server es necesario iniciar primero una shell de erlang de la siguiente manera

    erl -name server@ip

Donde server podeis ponerle el nombre que querais.
La ip es importante que le pongáis una accesible por los clientes. Para probar todo en una sola maquina, os vale 127.0.0.1 .
`server@ip` es lo que os pedira el cliente para conectaros al servidor

El modulo server tiene implementado una función que te permite asignarle un nombre al nodo durante la ejecución sin que falle, así que podéis iniciar la terminal con `erl` y luego iniciar el servidor. Os preguntara que pongáis un nombre del tipo `server@ip`

Para arrancar el servidor del chat `server:start()` y para cerrarlo `server:stop()`

Para mas info sobre la implementación revisad los comentarios del código

##Cliente ##

Para iniciar el modulo client es necesario iniciar primero una shell de erlang de la siguiente manera

    erl -name cliente@hostname

Donde pone cliente, podeis ponerle el nombre que querais, pero es necesario que sea diferente en el caso de que inicieis varias shell en la misma maquina o cuando la ip o hostname sea el mismo.

En lugar de la ip del PC  o hostname, podéis poner cualquier otra cosa, pero en ese caso os puede salir un warning al conectar los nodos, aunque debería funciona igual.

Una vez en el shell el cliente se inicia con `client:start().`
Cuando lo iniciéis ya saldrán unos mensajes que irán informando de lo que ocurre.

Una vez conectado al servidor, para salir escribe `/exit`



Para mas info del funcionamiento, mirad los comentarios del código



------------- 
