#Incio del analisis estadistico

library(tidyverse)
library(dplyr)
library(ggplot2)

setwd("C:/Users/Venejuegos/Documents/GitHub/Netflix-Proyecto")
getwd() #Directorio de trabajo

#Analizaremos la data y sus variables

view(Netflix)
str(Netflix)

#Transcribir nombre de variables

colnames(Netflix) <- c("Titulo" , "Genero" , "Lenguaje" , "Puntaje" , "Estreno" ,
                       "Duracion" , "Año")
view(Netflix)

### Nuestro primer objetivo es analizar la variable "Genero" ###

class(Netflix$Genero)
Netflix$Genero <- factor(Netflix$Genero)
Gen <- Netflix$Genero #Transformar caracter a factor 

table(Gen)

#Se observa que es un total de 114 niveles asi que vamos agruparlos para minimizar

plot(Gen)

Gen <- c(rep("Documentary" ,159) , rep("Drama" ,77) , rep("Comedy" ,49) , 
        rep("Romantic comedy" ,39) , rep("Thriller" ,33) , rep("Comedy-drama" ,14) ,
        rep("Other" ,212))

#Limpiamos los datos para convertir 114 niveles a 6

Gen <- factor(Gen,
             levels = c("Comedy" , "Comedy-drama" , "Documentary" , "Drama" , 
                        "Other" , "Romantic comedy" , "Thriller") ,
             labels = c("Comedia" , "Comedia" , "Documental" , "Drama" , "Otros" ,
                        "Romance" , "Thriller"))

table(Gen)
plot(Gen) #Observamos en un grafico de barras las cantidades de titulos por generos

#Estelizamos nuestro grafico

G  <- ggplot(mapping = aes(x = factor(Gen))) + geom_bar(fill = "Purple") + coord_flip()      

#Vamos a mejorar aspectos de nuestro grafico

G + ggtitle("Cantidad de Titulos por Genero") +
   theme(plot.title = element_text(family = "sans", 
                                   size=rel(1.8),
                                   vjust=0.5 , hjust=0.5 ,
                                   face = "italic" ,
                                   color="Black"))+
labs(x="Genero",y="Titulos")+
  theme(axis.title.x = element_text(face="bold",vjust=0.5, color="Purple", size=rel(1.5)))+
  theme(axis.title.y = element_text(face="bold",vjust=0.5, color="Black", size=rel(1.5)))

## De esta forma tenemos nuestro analisis "Cantidad de Titulos por Genero" ##

Parte 2

#### Ahoa vamos a evaluar nuestra variable Puntaje ####

class(Netflix$Puntaje)
table(Netflix$Puntaje)

# Crearemos una nueva variable de la calidad de cada titulo segun su puntaje

rangos  <- c(0, 5, 7, 10)

Netflix <- Netflix %>% mutate(Calidad=cut(Puntaje, breaks = rangos,
                              labels = c("Baja" , "Media" , "Alta")))

view(Netflix)
table(Netflix$Calidad) 

# Gracias a esto podemos determinar la calidad de cada titulo segun su puntaje

C <- ggplot(Netflix, aes(x=Gen, y=Puntaje, fill=Calidad)) + geom_col(position ="dodge")
C    #podemos observar la calidad de generos por puntajes

# Procedemos a colocar un mejor estilo a nuestra grafica

C + ggtitle("Calidad de Titulos por Genero") +
  theme(plot.title = element_text(family = "sans", 
                                  size=rel(1.8),
                                  vjust=0.5 , hjust=0.5 ,
                                  face = "italic" ,
                                  color="Black"))+
  labs(x="Genero",y="Puntaje")+
  theme(axis.title.x = element_text(face="bold",vjust=3, color="Brown", size=rel(1)))+
  theme(axis.title.y = element_text(face="bold",vjust=3, color="Brown", size=rel(1)))

# Obervamos que la calidad por generos es casi equitativa

## De esta forma ya pudimos analizar la calidad de Generos segun los puntajes ""

Parte 3

#### Ahora vamos a evaluar por separado cada uno de estos generos ####
