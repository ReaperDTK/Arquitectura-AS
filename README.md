# CHAT cliente-servidor #

## makefile

### Compilar

Para compilar basta con ejecutar `make` en un terminal en la carpeta raíz del proyecto.

Se compilarán todos los archivos con extensión **erl** que se encuentren en la carpeta **src** y los archivos **beam** resultantes se situarán en la carpeta **bin**

### Ejecutar

Para ejecutar,  nos situamos en la carpeta **bin** y ejecutar un shell de erlang en el terminal tal como se explica en [src/main ](https://github.com/ReaperDTK/Arquitectura-AS/blob/master/src/main/README.md)

También se puede iniciar usando en la carpeta raíz del proyecto

    make run name=nombrecliente@ipcliente

y

    make run name=nombreserver@ipserver


Utilizando despues las funciones de los módulos **client** y **server** tal como se explica en [src/main ](https://github.com/ReaperDTK/Arquitectura-AS/blob/master/src/main/README.md)

#### SSL

Para iniciar SSL , se tiene que usar en un terminal en la raíz del proyecto

    make run_ssl_server name=servername@ipserver
    
y

    make run_ssl_client name=clientname@ipclient

>Es posible que al ejecutar con SSL salten unas advertencias al conectar varios clientes diciendo que no hay certificado. Esto es debido a que intenta conectar los nodos clientes entre si debido a que la conexion de nodos en erlang es transitiva  [(Documentación nodos de Erlang)](http://erlang.org/doc/reference_manual/distributed.html#id88219). En un principio los clientes no deberian porque tener certificado (similar al funcionamiento de HTTPS). Se intentara solucionarlo en futuros commits
### Clean

`make clean`  eliminará todos los archivos **bean** de la carpeta **bin**
