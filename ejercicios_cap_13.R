# Datos relacionales correspondientes al capítulo 13 del libro R para Ciencia de Datos


--------------------
## Librerías 
--------------------  
  
library(tidyverse)
library(datos)
# library(nycflights13)
library(here)

------------------------
## Dataset 
------------------------
# Tablas
  
# tabla aerolíneas: código_carrier(clave principal o primaria), nombre
View(nycflights13::airlines)

# tabla avión: código_cola (clave principal o primaria), anio, tipo, empresa, modelo, asientos, velocidad, nro_motor, tipo_motor
View(nycflights13::planes)


# tabla vuelos: anio, mes, dia, hora, código_carrier, código_cola, origen, destino, horario_partida, horario_arribo,entre otros.
View(nycflights13::flights)

# tabla aeropuertos: faa(clave principal o primaria), nombre, latitud, longitud, zona horaria, etc. 
# El campo faa sirve para relacionarse con vuelos y con clima. Origen sólo letras, en cambio destino
View(nycflights13::airports)

# tabla clima: origen, anio, mes, dia,hora, humedad, presión, precipitación, etc. Clave pricipal compuesta: origen, anio, mes, dia,hora,
View(nycflights13::weather)

# simple consulta de aeropuertos
nycflights13::airports %>% filter(faa=="ATL")  #EWR: nueva york

------------------
# Claves
------------------
  
datos::aerolineas   # aerolinea/codigo_carrier(clave principal o primaria), nombre
datos::aviones      # codigo_cola (clave principal o primaria), anio, tipo, fabricante, modelo, nro_motores, asientos, velocidad, tipo_motor
datos::aeropuertos  # codigo_aeropuerto (clave principal o primaria), nombre, latitud, longitud, altura, zona_horaria, horario_verano, zona_horaria_americana, etc. 
datos::clima        # origen, anio, mes, dia, hora, temperatura, punto_rocio, humedad, direccion_viento, velocidad_viento, velocidad_rafaga, precipitación, etc. Clave pricipal compuesta: origen, anio, mes, dia,hora.
datos::vuelos  #anio, mes, dia, hora,vuelo, origen, destino,  codigo_cola, codigo_carrier, horario_salida, salida_programada, atraso_salida, horario_llegada,entre otros.

# Verifico  las claves primarias

# Verifico que las claves primarias sean únicas en aviones
aviones %>%
  count(codigo_cola) %>%
  filter(n > 1)

# Verifico que las claves primarias sean únicas en aerolineas
aerolineas %>%
  count(aerolinea) %>%
  filter(n>1)
# Verifico que las claves primarias sean únicas en aerolineas
aeropuertos %>% 
  count(codigo_aeropuerto)%>%
  filter(n>1)
# no es clave única ¿Entonces?
clima %>% 
  count(anio, mes, dia, hora, origen)%>%
  filter(n>1)

vuelos %>% 
  count(anio, mes, dia, hora, vuelo) %>%
  filter(n>1)


# Probamos agregando otra clave, por ejemplo codigo_cola
vuelos %>% 
  count(anio, mes, dia, hora, vuelo, codigo_cola) %>%
  filter(n>1)


# ¿Entonces?
  
clima2 <- clima %>% mutate(nro_fila= row_number())
View(clima2)

clima2 %>% 
  count(anio, mes, dia, hora, origen, nro_fila)%>%
  filter(n>1)

vuelos_con_clave <- vuelos %>% mutate(id= row_number())

vuelos_con_clave %>% 
  count(id, anio, mes, dia, hora, vuelo)%>%
  filter(n>1)

vuelos_con_clave %>% 
  count(id)%>%
  filter(n>1)

--------------------------
# UNIONES
--------------------------  
# Uniones de Transformación: permite combinar variables a partir de dos tablas. 

# 1. Busca coincidencias de observaciones de acuerdo a sus claves y
# 2. copia las variables de una tabla en la otra.

# Nota: Tal como mutate(), las funciones de unión agregan variables hacia la derecha, por lo que si tienes muchas variables 
# inicialmente las nuevas variables no se imprimirán

# Sintaxis: 
#   tabla_1 %>%
#   inner_join(tabla_2, by = "key")  key es la clave principal o primaria


vuelos2 <- vuelos %>%
  select(anio:dia, hora, origen, destino, codigo_cola, aerolinea)
vuelos2

#Supongamos que queremos incluir el nombre completo de la aerolínea en vuelos2.

vuelo_con_aerolinea <- vuelos2 %>%
  select(-origen, -destino) %>%
  left_join(aerolineas, by = "aerolinea")
View(vuelo_con_aerolinea)

# El resultado de unir aerolineas y vuelos2 es la inclusión de una variable adicional: nombre. 
# Esta es la razón de que llamemos unión de transformación a este tipo de unión.

# ¿Por qué utilizamos un left_join en lugar de un inner_join? Para que no se eliminen observaciones

 vuelos2 %>%
  select(-origen, -destino) %>%
  inner_join(aerolineas, by = "aerolinea") # se eliminan 10 observaciones donde no haya coincidencias con las claves

 # Otra Opción:
 #vuelos2 %>%select(-origen, -destino)%>% filter(aerolinea == 'NA') 
 
 # Opción 2:  podríamos obtener el mismo resultado usando mutate() junto a las operaciones
 # de filtro de R base:
   vuelos2 %>%
   select(-origen, -destino) %>%
   mutate(nombre = aerolineas$nombre[match(aerolinea, aerolineas$aerolinea)])
 
 
 #Entendiendo la uniones
 
 x <- tribble(
   ~key, ~val_x,
   1, "x1",
   2, "x2",
   3, "x3"
 )
x

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)
-------------------
# Union interna
-------------------
  
union_interna <- x %>% inner_join(y, by="key")
 
# union_interna: Trae como resultado una tabla con sólo dos filas u observaciones, en las cuales donde hubo coincidencia
 
--------------------
# union_izquierda
-------------------  
  
 #left_join()
union_izquierda<- x %>% left_join(y, by="key")

#Entonces obtengo una tabla de salida con 3 filas y 2 columnas.
----------------
# union_derecha
----------------  
  
#right_join()
union_derecha <-x %>% right_join(y, by="key")

# Como resutado obtengo una tabla de 3 filas y 2 columnas, donde en la tercer fila, tengo un valor nulo para x.  

-----------------
# union_completa
-----------------
  
#full_join()

union_completa <- x %>% full_join(y, key="key")

 #Obtengo una tabla de salida de 4 filas ya que se mantienen las observaciones de x e y.
 
 
 # ** Importante: La unión que más frecuentemente se usa es la unión izquierda: es recomendable 
 # usarla para buscar datos adicionales en otra tabla, ya que preserva las observaciones originales 
 # incluso cuando no hay coincidencias entre ambas tablas. La unión izquierda debiera ser tu
 # unión por defecto, a menos que tengas un motivo importante para preferir una de las otras.
 

--------------------------------
# diagrama de Venn
-------------------------------
 # Otra forma de ilustrar diferentes tipos de uniones es mediante un diagrama de Venn.
 # Los diagramas de venn faciltan el entendemiento de las uniones.
 # Es decir, tengo 2 conjuntos 'X' e 'Y' y dependiendo de la unión obtendré un resultado distinto. 
 
 #Importante: Un inner se puede comparar con la intersección entre los dos conjuntos, x e y. 
 # Un left_join es todo lo de mi primer conjunto más la intersección con el segundo conjunto.
 # Un right_join() es todo lo del segundo conjunto más la intersección con el primer conjunto.
 # un full_join es todo lo del primer y del segundo conjunto incluida la intersección entre ambos.
  
  
#----------------------------------
#Definiendo las columnas clave
#----------------------------------
 # Hasta acá, los pares de tablas siempre se han unido de acuerdo a una única variable y esa variable
 # tiene el mismo nombre en ambas tablas. Ej: vuelos con aerolineas

 # Pero es posible utilizar otros valores de by= para conectar las tablas: 
 
 # 1. by = NULL
 # * by = NULL, opción por defecto, la cual usa todas las variables que aparecen en ambas tablas,
 # lo que se conoce como unión natural Ej: vuelos2 con clima
 
 union_natural<- vuelos2 %>%
   left_join(clima)
 union_natural
 
 # 2. by= "x"
 # *  Un vector de caracteres, by= "x", es similar a una unión natural, pero usa algunas de las variables comunes. 
 # Por ejemplo, vuelos y aviones tienen la variable anio, pero esta significa cosas distintas en 
 # cada tabla por lo que queremos unir por codigo_cola.
 
 
 vuelos_con_avion <- vuelos2 %>% left_join (aviones, by="codigo_cola")
 vuelos_con_avion
 
# 3. by = c("a" = "b")
# *  Un vector de caracteres con nombres, by = c("a" = "b"). Esto va a unir la variable a en 
# la tabla x con la variabla b en la tabla y. Las variables de x se usarán en el output.

 union_aeropuerto<- vuelos2 %>% left_join(aeropuertos, by=c("origen"= "codigo_aeropuerto"))
union_aeropuerto 
#-----------------------------
# Otras implementaciones
#-----------------------------

# base::merge() Se pueden realizar los cuatro tipos de uniones de transformación con r base, utilizando merge()
# Entonces, esta pequeña tabla que compara las sentencias de dplyr con la de merge
    #  dplyr                    # merge()
------------------------------------------------------------------------
  inner_join(x, y)  similar a   merge(x,y)
  left_join(x, y)	  similar a   merge(x, y, all.x = TRUE)
  right_join(x, y)  similar a  	merge(x, y, all.y= TRUE),
  full_join(x, y)	  similar a   merge(x, y, all.x = TRUE, all.y = TRUE
                        
# Esta pequeña tabla que compara las sentencias de dplyr con las de	SQL

#  dplyr                    # SQL
------------------------------------------------------------------------

inner_join(x, y, by = "z")	 SELECT * FROM X INNER JOIN Y USING (Z)
left_join(x, y, by = "z")	   SELECT * FROM X LEFT OUTER JOIN Y USING (Z)
right_join(x, y, by = "z")	 SELECT * FROM x RIGHT OUTER JOIN y USING (z)
full_join(x, y, by = "z")	   SELECT * FROM X FULL OUTER JOIN Y USING (Z)
  

# Un Paquete que es útil para ejecutar sentencias desde R es sqldf

#-----------------------
# Uniones de filtro #
#-----------------------

# semi_join()

destinos_populares <- vuelos %>%
  count(destino, sort = TRUE) %>%
  head(10)m        #toma los diez lugares con mayor catidad de apariciones


destinos_populares %>% left_join (aeropuertos, by= c("destino"="codigo_aeropuerto"))

#Para encontrar cada vuelo que fue a alguno de esos destinos, es posible construir un filtro:
 
 vuelos %>% semi_join(destinos_populares)
 
# Anti_join 
anti_join()        # mantiene las filas que no tienen coincidencias:
  
# Las anti uniones son útiles para encontrar desajustes. Por ejemplo, 
# podría interesarte saber que existen muchos vuelos que no tienen coincidencias en aviones.
 
vuelos %>% anti_join(aviones, by="codigo_cola")

vuelos %>%
  anti_join(aviones, by = "codigo_cola") %>%
  count(codigo_cola, sort = TRUE)
 
#-------------------
# Más ejemplos 
--------------------
#Dataset: Bandas
-------------------
  
# Tablas
dplyr::band_members
dplyr::band_instruments

#------------------------------  
# Unión interna o Inner Join
  
band_members %>% inner_join(band_instruments)

# Unión izquierda

band_members %>% left_join(band_instruments)

# Unión derecha 

band_members %>% right_join(band_instruments)

# Full Join

band_members %>% full_join(band_instruments)
------------------------------
# Dataset: Superheroes
-------------------------------
  
# primer dataset  
superheroes <- tibble::tribble(
    ~name, ~alignment,  ~gender,          ~publisher,
    "Magneto",      "bad",   "male",            "Marvel",
    "Storm",     "good", "female",            "Marvel",
    "Mystique",      "bad", "female",            "Marvel",
    "Batman",     "good",   "male",                "DC",
    "Joker",      "bad",   "male",                "DC",
    "Catwoman",      "bad", "female",                "DC",
    "Hellboy",     "good",   "male", "Dark Horse Comics"
  )

# segundo dataset
publishers <- tibble::tribble(
  ~publisher, ~yr_founded,
  "DC",       1934L,
  "Marvel",       1939L,
  "Image",       1992L
)

#-------------------------------------------
## inner_join(superheroes, publishers)

tabla_inner <- superheroes %>% inner_join (publishers)

## full_join(superheroes, publishers)

tabla_full <- superheroes %>% full_join (publishers)  

