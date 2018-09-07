setwd("D:/Clases/NOTAS")

clean <- function (filename) {
  # read
  datos <- read.csv(file=filename, header=FALSE, sep=",")
  # remove useless stuff
  datos <- datos[,2:4]
  datos <- datos[5:nrow(datos),]
  # names
  colnames(datos) <- c("Carnet", "Nombre", "Nota")
  # arreglar ceros
  datos$Nota <- as.numeric(as.character(datos$Nota))
  datos$Nota <- ifelse(is.na(datos$Nota), 0, datos$Nota)
  return(datos)
}

removeFails <- function (df) {
  df <- df[!(is.na(df$Nombre) | df$Nombre==""),]
  return(df)
}
removeZeros <- function (df) {
  df <- df[!(df$Nota==0),]
}

genStats <- function (nums, chars) {
  allPracticas <- c()
  allEnsayos <- c()
  allCuestionarios <- c()
  donePracticas <- c()
  doneEnsayos <- c()
  doneCuestionarios <- c()
  cantPracticas <- c(0)
  cantEnsayos <- c(0)
  cantCuestionarios <- c(0)
  promediosPracticas <- c()
  promediosEnsayos <- c()
  promediosCuestionarios <- c()
  promediosZeroPracticas <- c()
  promediosZeroEnsayos <- c()
  promediosZeroCuestionarios <- c()
  for (num in nums) {
    for (char in chars) {
      thisPractica <- clean(paste0("p", char, num, ".csv"))
      thisEnsayo <- clean(paste0("e", char, num, ".csv"))
      thisCuestionario <- clean(paste0("c", char, num, ".csv"))
      
      thisPractica <- removeFails(thisPractica)
      thisEnsayo <- removeFails(thisEnsayo)
      thisCuestionario <- removeFails(thisCuestionario)
      
      allPracticas <- rbind(allPracticas, thisPractica)
      allEnsayos <- rbind(allEnsayos, thisEnsayo)
      allCuestionarios <- rbind(allCuestionarios, thisCuestionario)
      
      zeroPracticas <- removeZeros(thisPractica)
      zeroEnsayos <- removeZeros(thisEnsayo)
      zeroCuestionarios <- removeZeros(thisCuestionario)
      
      donePracticas <- rbind(donePracticas, zeroPracticas)
      doneEnsayos <- rbind(doneEnsayos, zeroEnsayos)
      doneCuestionarios <- rbind(doneCuestionarios, zeroCuestionarios)
    }
    cantPracticas <- c(cantPracticas, nrow(donePracticas) - sum(cantPracticas))
    cantEnsayos <- c(cantEnsayos, nrow(doneEnsayos) - sum(cantEnsayos))
    cantCuestionarios <- c(cantCuestionarios, nrow(doneCuestionarios) - sum(cantCuestionarios))
    
    promediosPracticas <- c(promediosPracticas, mean(thisPractica$Nota))
    promediosEnsayos <- c(promediosEnsayos, mean(thisEnsayo$Nota))
    promediosCuestionarios <- c(promediosCuestionarios, mean(thisCuestionario$Nota))
    
    promediosZeroPracticas <- c(promediosZeroPracticas, mean(zeroPracticas$Nota))
    promediosZeroEnsayos <- c(promediosZeroEnsayos, mean(zeroEnsayos$Nota))
    promediosZeroCuestionarios <- c(promediosZeroCuestionarios, mean(zeroCuestionarios$Nota))
  }
  cantPracticas <- cantPracticas[2:length(cantPracticas)]
  cantEnsayos <- cantEnsayos[2:length(cantEnsayos)]
  cantCuestionarios <- cantCuestionarios[2:length(cantCuestionarios)]
  
  # PLOTS
  maax <- length(unique(allEnsayos$Nombre))
  
  plot(nums, cantPracticas, type="l", xlab="", ylab="", ylim=c(0,maax), main="Practicas realizados")
  plot(nums, cantEnsayos, type="l", xlab="", ylab="", ylim=c(0,maax), main="Ensayos realizados")
  plot(nums, cantCuestionarios, type="l", xlab="", ylab="", ylim=c(0,maax), main="Cuestionarios realizados")
  
  #hist(doneEnsayos$Nota, xlab="nota", ylab="", main="Notas de ensayos entregados")  
  
  hist(allEnsayos$Nota, xlab="nota", ylab="", main="Notas de todos los ensayos")
  hist(allPracticas$Nota, xlab="nota", ylab="", main="Notas de todas las practicas")
  hist(allCuestionarios$Nota, xlab="nota", ylab="", main="Notas de todos los cuestionarios")
  
  cantPracticas <- c(cantPracticas, nrow(donePracticas))
  cantEnsayos <- c(cantEnsayos, nrow(doneEnsayos))
  cantCuestionarios <- c(cantCuestionarios, nrow(doneCuestionarios))
  
  promediosPracticas <- c(promediosPracticas, mean(allPracticas$Nota))
  promediosEnsayos <- c(promediosEnsayos, mean(allEnsayos$Nota))
  promediosCuestionarios <- c(promediosCuestionarios, mean(allCuestionarios$Nota))
  
  promediosZeroPracticas <- c(promediosZeroPracticas, mean(donePracticas$Nota))
  promediosZeroEnsayos <- c(promediosZeroEnsayos, mean(doneEnsayos$Nota))
  promediosZeroCuestionarios <- c(promediosZeroCuestionarios, mean(doneCuestionarios$Nota))
  
  # CSV
  
  df <- data.frame(cantPracticas, cantEnsayos, cantCuestionarios, promediosPracticas, promediosEnsayos, promediosCuestionarios, promediosZeroPracticas, promediosZeroEnsayos, promediosZeroCuestionarios)
  df <- t(df)
  row.names(df) <- c("Practicas realizadas", "Ensayos entregados", "Cuestionarios realizados", "Promedio de todas las practicas", "Promedio de todos los ensayos", "Promedio de todos los cuestionarios", "Promedio de practicas realizadas", "Promedio de ensayos entregados", "Promedio de cuestionarios realizados")
  colnames(df) <- c("Semana 1", "Semana 2", "Semana 3", "Semana 4", "Semana 5", "Semana 6", "Total")
  View(df)
  write.table(df, "_resultados.csv", sep=",", col.names=TRUE, append=FALSE)
}

genStats(c(1, 2, 3, 4, 5, 6), c("a", "b", "c", "d"))
