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
Genero <- Netflix$Genero #Transformar caracter a factor

table(Genero)
#Se observa que es un total de 114 niveles asi que vamos agruparlos para minimizar

plot(Genero)

Genero <- c(rep("Documentary" ,159) , rep("Drama" ,77) , rep("Comedy" ,49) , 
        rep("Romantic comedy" ,39) , rep("Thriller" ,33) , rep("Comedy-drama" ,14) ,
        rep("Other" ,212))

#Limpiamos los datos para convertir 114 niveles a 6

Genero <- factor(Genero,
             levels = c("Comedy" , "Comedy-drama" , "Documentary" , "Drama" , 
                        "Other" , "Romantic comedy" , "Thriller") ,
             labels = c("Comedia" , "Comedia" , "Documental" , "Drama" , "Otros" ,
                        "Romance" , "Thriller"))

table(Genero)
plot(Genero) #Observamos en un grafico de barras las cantidades de titulos por generos

#Estelizamos nuestro grafico

G  <- ggplot(mapping = aes(x = factor(Genero))) + geom_bar(fill = "Purple") + coord_flip()      

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

