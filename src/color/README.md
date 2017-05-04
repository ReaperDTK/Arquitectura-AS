
# Color #

## Descripción
Imprime cadenas de caracteres a color en línea de comandos en `erlang`. <br>
Los colores soportados son:

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


## API

El módulo `color` exporta las siguientes funciones:

- `print(String, [FontColor, BackgroundColor, Mode1, (...) , ModeN)`
- `get_colors()`
- `get_modes()`
- `print_available_colors()`
- `print_available_modes()`


## Imprimir mensajes

La función `print` imprime en el Terminal una cadena de caracteres. Además:
- Imprime un `~n` después del mensaje.
- Restaura el color y modo por defecto después de imprimir el mensaje (i.e. como si no se hubiera llamado a la función).

Si queremos imprimir **sólo** un mensaje a color, o con un modo:

```erlang
color:print("Message", red).
color:print("Message", bold).
```

<p align="center"><img src ="https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/red.png"/>
</p>
<p align="center"><img src ="https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/bold.png"/>
</p>

- Si queremos imprimir un mensaje con **color** y un **modo**:

```erlang
color:print("Message", [red, bold]).
```

<p align="center"><img src ="https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/red_bold.png"/>
</p>

- Si queremos imprimir un mensaje con **color** y un **fondo**:

```erlang
color:print("Message", [red, cyan]).
```

<p align="center"><img src ="https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/red_blue.png"/>
</p>

- Si queremos imprimir un mensaje con **color**, un **fondo** y un **modo**:

```erlang
color:print("Message", [red, cyan, bold]).
```

<p align="center"><img src ="https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/red_blue_bold.png"/>
</p>

- Nótese que si introducimos más de dos colores, empleará el primero como color de la fuente y el último como color de fondo. Sí conservará todos los modos:

```erlang
color:print("Message", [red, cyan, bold, underline, black, italic, green]).
```

<p align="center"><img src ="https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/multi.png"/>
</p>

## Demás funcionalidades

El resto de funciones son auxiliares. Podrán implementarse más en un futuro, en caso de necesidad.

- Podemos obtener la lista de colores y modos disponibles llamando a las funciones `get_colors` y `get_modes`:

```erlang
List = color:get_colors()
Modes = color:get_modes()
```
- Si queremos ver si los colores y modos son compatibles en nuestro Terminal, una forma rápida de hacerlo es llamando a las funciones `print_available_colors` y `print_available_modes`:

```erlang
color:print_available_colors()
color:print_available_modes()
```

<p align="center"><img src ="https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/available_colors.png"/>
</p>
<p align="center"><img src ="https://github.com/ReaperDTK/Arquitectura-AS/blob/master/img/available_modes.png"/>
</p>
