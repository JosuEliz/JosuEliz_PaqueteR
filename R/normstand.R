# Normalize and standarize the variables

normalizar.estandarizar <- function (x){
  if(!is.data.frame(x)){ #el caso que el input es un atributo y no un dataset
    if (is.numeric(x)){ #si la entrada es numerica
      normalizado <- (x-min(x))/(max(x)-min(x))
      estandarizado <- (x-mean(x))/(sd(x))
    }
    else{ #si la entrada no es numerica, no se podra calcular los valores normalizados
      #y estandarizados y devolveremos NA
      normalizado <- NA
      estandarizado <- NA
    }
    return(list(normalizado=normalizado,estandarizado=estandarizado))
  }
  else{#si es un dataset
    normalizado <- matrix(ncol=ncol(x), nrow=nrow(x))# si la entrada es un dataset, la
    #salida tendra mas de un valor, por ello es necesario inicializar la matriz
    estandarizado <- matrix(ncol=ncol(x), nrow=nrow(x)) #inicializar la matriz
    for(i in 1:ncol(x)){
      if (is.numeric(x[[i]])){ #si la entrada es numerica
        normalizado[,i]<- (x[[i]]-min(x[[i]]))/(max(x[[i]])-min(x[[i]])) #en cada columna
        #de la nueva matriz insertamos los resultados
        estandarizado[,i] <- (x[[i]]-mean(x[[i]]))/(sd(x[[i]])) #en cada columna de la
        #nueva matriz insertamos los resultados
      }
      else{ #si la entrada no es numerica, no se podra calcular los valores normalizados y
        #estandarizados y devolveremos NA
        normalizado <- NA
        estandarizado <- NA
      }
    }
  }
  return(list(normalizado=normalizado,estandarizado=estandarizado))
}

