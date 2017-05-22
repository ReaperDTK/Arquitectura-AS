# Tareas / Casos de Uso

#### Obligatorio
- [x] **Unirse a una sala**
    - *Un usuario puede unirse a una sala*
- [x] **Crear salas desde servidor**
    - *El proceso del servidor es capaz de crear salas en las que los usuarios de los clientes pueden comunicarse via broadcast.*
- [x] **Nombre por usuario**
    - *El sistema le solicita al usuario un nombre con el que identificarse y será su nombre mientras esté en el programa.*
- [x] **Desconexión del usuario del programa**.
    - *Logout, liberar nombre y avisar a sala.*
- [x] **Capacidad de cambiarse de servidor**
    - *El usuario puede elegir el servidor al que unirse al iniciar **
- [x] **Enviar mensajes a la sala**  
    - *Broadcast a users que comparten sala (lo hace el server)*
- [x] **Comandos**
    - **Falta descripción**
- [x] **Crear Salas desde el cliente**
    - *El usuario tiene ca capacidad de crear salas, a la que luego pueden unirse otros usuarios.*
- [x] **Multiservidor**
    - *Existe una red de servidores. El cliente se conecta a un server maestro, y este asignará al cliente a un servidor*
- [ ] **Configuración de salas vía archivo: Hacer que no pete si no hay archivo.**
#### Opcional
- [x] **Comunicación privada entre usuarios**
    - *Por ejemplo mediante salas privadas.*
- [ ] **Persistencia de salas y usuarios (registro de ambos)**
- [x] **Iniciar servidor con salas ya creadas (indicadas en un archivo)**
- [ ] **Configuración vía archivo**
- [ ] **~~Fichero con cadenas de texto predeterminadas en distintos idiomas~~**
- [x] **Seguridad via SSL**

#### Mejoras estéticas

- [ ] **El servidor avisa a la sala (broadcast) en los siguientes casos:**
    - [ ] *Un usuario se ha unido a la sala.*
    - [ ] *Un usuario ha abandonado la sala.*
    - [ ] *Se ha creado una nueva sala.*
- [ ] **Cuando se usa SSL se muestra un mensaje similar a Whatsapp: "Los mensajes que envías están seguros con cifrado de extremo a extremo."**
- [ ] **Añadir operación en el módulo `client` para que pueda obtener la lista de salas disponibles.**
    
