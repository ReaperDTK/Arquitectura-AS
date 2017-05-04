
Descripción
-----------
Imprime cadenas de caracteres a color en línea de comandos en `erlang`. Los colores soportados son 17:

- `black`
- `red`
- `green`
- `yellow`
- `blue`
- `purple`
- `cyan`
- `white`
- `light_black`
- `light_red`
- `light_green`
- `light_yellow`
- `light_blue`
- `light_purple`
- `light_cyan`
- `light_white`
- `default` (el propio del Terminal)

Los modos soportados son:

- `normal`
- `bold`
- `italic`
- `underline`

API
---

El módulo `color` exporta las siguientes funciones:

- `print(String, [FontColor, BackgroundColor, Mode1, (...) , ModeN)`
- `get_colors()`
- `get_modes()`
- `print_available_colors()`
- `print_available_modes()`


Imprimir mensajes
-----------------

<img src=" https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/bold.png">
<img src=" https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/bold.png">
<img src=" https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/red_bold.png">
<img src=" https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/red_blue_bold.png">
<img src=" https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/multi.png">

La función `print` imprime en el Terminal una cadena de caracteres. Además:
- Imprime un `~n` después del mensaje.
- Restaura el color y modo por defecto después de imprimir el mensaje (i.e. como si no se hubiera llamado a la función).

Si queremos imprimir **sólo** un mensaje a color, o con un modo, haremos:

```erlang
color:print("Message", red).
color:print("Message", bold).
```
<center> <img src=" https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/red.png"></center><br>
<center><img src=" https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/bold.png"></center><br>

- Si queremos imprimir un mensaje con **color** y un **modo**:

```erlang
color:print("Message", [red, bold]).
```

<center><img src=" https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/red_bold.png"></center><br>

- Si queremos imprimir un mensaje con **color** y un **fondo**:

```erlang
color:print("Message", [red, cyan]).
```

<center><img src=" https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/red_blue_bold.png"></center><br>

- Si queremos imprimir un mensaje con **color**, un **fondo** y un **modo**:

```erlang
color:print("Message", [red, cyan, bold]).
```

<center> <img src=" https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/multi.png"> </center><br>

- Nótese que si introducimos más de dos colores, empleará el primero como color de la fuente y el último como color de fondo. Sí conservará todos los modos:

```erlang
color:print("Message", [red, cyan, bold, underline, black, italic, green]).
```

Demás funcionalidades
---------------------
