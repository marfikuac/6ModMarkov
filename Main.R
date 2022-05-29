rutaArchivoEventos<-"6_bitacoraRegistrosNavegador_MC.csv"
rutaArchivoCalificaciones="1_2_Calificaciones_MC.csv"

#Para obtener la matriz de transición:
library(markovchain)
#Para las consultas y el procesamiento de los datos:
library(rlist)
#Para usar la sintaxis de tubería:
library(pipeR)
#Para el mapa de calor:
library(plot.matrix)

#Obtener los datos como un objeto de tipo data.frame.
eventos<-read.csv(rutaArchivoEventos,sep="\t",fileEncoding="x-mac-hebrew")
calificaciones<-read.csv(rutaArchivoCalificaciones,sep="\t",fileEncoding="x-mac-hebrew")

#Obtener los estados posibles.
estados<-list.parse(eventos)%>>%
  list.group(Evento)%>>%
  list.map(.i-1L)

#Obtener solo el valor numérico de los estados (excluyendo el nombre).
M<-as.integer(estados)

lst<-
  #Convertir a lista.
  list.parse(eventos)%>>%
  #Agregar campos previos de consulta y ordenamiento.
  list.select(Enrollment,Evento,Evento2=estados[[Evento]],Momento=as.POSIXct(FechaHora,format="%d/%m/%y %H:%M"))%>>%
  list.select(Enrollment,Evento,Evento2,Momento,Momento2=as.numeric(Momento))%>>%
  #Ordenar lista por fecha de manera ascendente.
  list.sort(Momento2)%>>%
  #Agrupar por matrícula.
  list.group(Enrollment)%>>%
  #Obtener solo la cadena de markov como vector de enteros.
  list.map(.%>>%list.map(Evento2))%>>%
  list.map(as.integer(.))%>>%
  list.select(Enrollment=.name,CadenaDeMarkov=.)%>>%
  #Calcular y agregar la matriz de transición de estados
  #(de un paso) con ayuda de la biblioteca markovchain.
  list.select(Enrollment,CadenaDeMarkov,MatrizDeTransicion=markovchainFit(CadenaDeMarkov,possibleStates=M)$estimate@transitionMatrix)%>>%
  #Adjuntar la matrícula obteniéndola del archivo de calificaciones.
  list.join(list.parse(calificaciones),Enrollment)%>>%
  #Formalizar las propiedades de cada elemento de la lista.
  list.select(Enrollment,CadenaDeMarkov,MatrizDeTransicion,Calificacion=Final_Grade)

#Identificar las calificaciones aprobatorias y reprobatorias.
califsAprob<-c("A","B","C")
califsReprob<-c("D","E")

#Obtener solo los alumnos que aprobaron.
alumsAprob<-lst%>>%
  list.filter(Calificacion %in% califsAprob)

#Obtener solo los alumnos que aprobaron.
alumsReprob<-lst%>>%
  list.filter(Calificacion %in% califsReprob)

#(Opcional) Obtener los alumnos restantes (valores erróneos).
# alumsOtros<-lst%>>%
#   list.filter(!(Calificacion %in% c(califsAprob,califsReprob)))

#(Opcional) Obtener los alumnos agrupados por calificación.
# alumsPorCalif<-lst%>>%
#   list.group(Calificacion)

#Para una lista de matrices con las mismas dimensiones,
#obtener una matriz que sea el promedio de ellas,
#(Promedio de cada elemento en la misma posición de
#las matrices.)
calcularPromedioDeMatrices <- function(matrices, rows, cols) {
  result <- matrix(0, rows, cols)
  for (i in 1:rows) {
    for (j in 1:cols) {
      zSum <- 0
      for (k in 1:length(matrices)) {
        zSum <- zSum + matrices[[k]][i, j]
      }
      zMean <- zSum / length(matrices)
      result[i, j] <- zMean
    }
  }
  return (result)
}

#Calcular el promedio de transiciones de un estado a otro con las matrices de transición de los alumnos aprobados y mostrar los resultados con un mapa de calor.
promedioAprobados<-calcularPromedioDeMatrices(alumsAprob%>>%list.map(MatrizDeTransicion),length(M),length(M))
plot(promedioAprobados,breaks=range(promedioAprobados))

#Calcular el promedio de transiciones de un estado a otro con las matrices de transición de los alumnos reprobados y mostrar los resultados con un mapa de calor.
promedioReprobados<-calcularPromedioDeMatrices(alumsReprob%>>%list.map(MatrizDeTransicion),length(M),length(M))
plot(promedioReprobados,breaks=range(promedioReprobados))
