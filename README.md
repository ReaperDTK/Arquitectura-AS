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


Utilizando despues las funciones del modulo **server** tal como se explica en [src/main ](https://github.com/ReaperDTK/Arquitectura-AS/blob/master/src/main/README.md) (El cliente se inicia solo)

#### SSL

Para iniciar SSL , se tiene que usar en un terminal en la raíz del proyecto

    make run_ssl_server name=servername@ipserver

y

    make run_ssl_client name=clientname@ipclient


### Clean

`make clean`  eliminará todos los archivos **bean** de la carpeta **bin**
