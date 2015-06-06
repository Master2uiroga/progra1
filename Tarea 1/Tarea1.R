cat("
888888888888                                               88  
     88                                                  ,d88  
     88                                                888888  
     88 ,adPPYYba, 8b,dPPYba,  ,adPPYba, ,adPPYYba,        88  
     88 \"\"     `Y8 88P'   \"Y8 a8P_____88 \"\"     `Y8        88  
     88 ,adPPPPP88 88         8PP\"\"\"\"\"\"\" ,adPPPPP88        88  
     88 88,    ,88 88         \"8b,   ,aa 88,    ,88        88  
     88 `\"8bbdP\"Y8 88          `\"Ybbd8\"' `\"8bbdP\"Y8        88  

")

#            88                                  88            88                    88  
#            ""                                  ""            ""                  ,d88  
#                                                                                888888  
#  ,adPPYba, 88  ,adPPYba, 8b,dPPYba,  ,adPPYba, 88  ,adPPYba, 88  ,adPPYba,         88  
# a8P_____88 88 a8P_____88 88P'   "Y8 a8"     "" 88 a8"     "" 88 a8"     "8a        88  
# 8PP""""""" 88 8PP""""""" 88         8b         88 8b         88 8b       d8        88  
# "8b,   ,aa 88 "8b,   ,aa 88         "8a,   ,aa 88 "8a,   ,aa 88 "8a,   ,a8"        88  
#  `"Ybbd8"' 88  `"Ybbd8"' 88          `"Ybbd8"' 88  `"Ybbd8"' 88  `"YbbdP"'         88  
#           ,88                                                                          
#         888P"                                                                          
cat("1. Para 0 < n \u2264 100, escribe una función  que calcule los n primeros términos
de la sucesión de Fibonacci\n")

##' @title Fibonacci
##' @description Función que calcula los n primeros términos de la sucesión de fibonacci
##' @param n el número entero no negativo hasta donde calcular los términos de la sucesión de fibonacci
##' @return un vector numérico con los n primeros términos de la sucesión de fibonacci
Fibonacci <- function(n){
  if (is.na(n) || is.null(n) || !is.numeric(as.vector(n)) || length(as.vector(n)) > 1 || n < 0 || n > 100) {
    stop("n: debe ser una variable de tipo numérico positivo unidimensional menor o igual 100\n")
  } else {
    f <- 1
    k <- floor(n)
    nombres <- "F\u2081" ## almacena los nombres de las variables
    if (n > 2) {
      f <- c(1, 1)
      for (i in 2:k) {
        indice <- as.character(i) ## se reemplaza el índice numérico por un subindice unicode textual
        indice <- gsub("0","\u2080",indice)
        indice <- gsub("1","\u2081",indice)
        indice <- gsub("2","\u2082",indice)
        indice <- gsub("3","\u2083",indice)
        indice <- gsub("4","\u2084",indice)
        indice <- gsub("5","\u2085",indice)
        indice <- gsub("6","\u2086",indice)
        indice <- gsub("7","\u2087",indice)
        indice <- gsub("8","\u2088",indice)
        indice <- gsub("9","\u2089",indice)
        nombres <- c(nombres, paste("F",indice,sep=""))
        f <- c(f, f[i - 1] + f[i])
      }
    }
    names(f) <- nombres
    return(f[1:k])
  }
}
cat("\n Ejemplo de los n = 10 primeros números de la sucesión de Fibonacci\n")
print(Fibonacci(10))

readline("Pulse [Enter \u21B2] para avanzar al siguiente ejercicio.\n")

#            88                                  88            88                 ad888888b,  
#            ""                                  ""            ""                d8"     "88  
#                                                                                        a8P  
#  ,adPPYba, 88  ,adPPYba, 8b,dPPYba,  ,adPPYba, 88  ,adPPYba, 88  ,adPPYba,          ,d8P"   
# a8P_____88 88 a8P_____88 88P'   "Y8 a8"     "" 88 a8"     "" 88 a8"     "8a       a8P"      
# 8PP""""""" 88 8PP""""""" 88         8b         88 8b         88 8b       d8     a8P'        
# "8b,   ,aa 88 "8b,   ,aa 88         "8a,   ,aa 88 "8a,   ,aa 88 "8a,   ,a8"    d8"          
# `"Ybbd8"'  88  `"Ybbd8"' 88          `"Ybbd8"' 88  `"Ybbd8"' 88  `"YbbdP"'     88888888888  
#           ,88                                                                               
#         888P"                                                                               
cat("2. Escribe un programa que convierta cualquier número en base diez a su 
equivalente en las siguientes bases: {2,8}\n")

##' @title BasePa10
##' @descriptionf función que convierte un número de base P a 10
##' @param n el número a convertir
##' @param p la base del sistema numérico a convertir
BasePa10 <- function(n,p){
  bP <- 0
  if (n != 0) {
    if (p < 10) {
      if (n < 0) {
        bP <- -BasePa10(-n,p)
      } else {
        dif <- n
        i <- 1
        while (dif > 0) {
          dif <- (dif - floor(n / (10 ^ i)) * (10 ^ i))/(10 ^ (i - 1))
          bP <- bP + dif * (p ^ (i - 1))
          dif <- floor(n / (10 ^ i))*(10 ^ i)
          i <- i + 1
        }
      }
    } else {
      cat("No soportado\n")
    }
  }
  return(bP)
}

##' @title Base10aP
##' @descriptionf función que convierte un número de base 10 a P
##' @param n el número a convertir
##' @param p la base del sistema numérico a convertir
Base10aP <- function(n,p) {
  b10 <- 0
  if (p < 10) {
    div <- n
    res <- div %% p
    v <- res
    while (div >= p) {
      div <- floor(div/p)
      res <- div %% p
      v <- c(res,v)
    }
    for (i in 1:length(v)) {
      b10 <- b10 + v[length(v) - i + 1] * (10 ^ (i - 1))
    }
  } else {
    cat("No soportado\n")
  }
  return(b10)
}

cat("\n Ejemplo de n = 10 convertido a base 2 y 8 respectivamente
    10\u2081\u2080 = ", Base10aP(10,2),"\u2082
    10\u2081\u2080 = ", Base10aP(10,8),"\u2088\n", sep = "")
cat("\n BONUS Ejemplo de n = 10 en base 2 y 8 respectivamente convertido a base 10
    10\u2082 = ", BasePa10(10,2),"\u2081\u2080
    10\u2088 = ", BasePa10(10,8),"\u2081\u2080\n", sep = "")

readline("Pulse [Enter \u21B2] para avanzar al siguiente ejercicio.\n")

#            88                                  88            88                 ad888888b,  
#            ""                                  ""            ""                d8"     "88  
#                                                                                        a8P  
#  ,adPPYba, 88  ,adPPYba, 8b,dPPYba,  ,adPPYba, 88  ,adPPYba, 88  ,adPPYba,          aad8"   
# a8P_____88 88 a8P_____88 88P'   "Y8 a8"     "" 88 a8"     "" 88 a8"     "8a         ""Y8,   
# 8PP""""""" 88 8PP""""""" 88         8b         88 8b         88 8b       d8            "8b  
# "8b,   ,aa 88 "8b,   ,aa 88         "8a,   ,aa 88 "8a,   ,aa 88 "8a,   ,a8"    Y8,     a88  
#  `"Ybbd8"' 88  `"Ybbd8"' 88          `"Ybbd8"' 88  `"Ybbd8"' 88  `"YbbdP"'      "Y888888P'  
#           ,88                                                                               
#         888P"                                                                               

cat("3. En 1682, Leibniz descubrió la siguiente serie: 
    \u03a3\u2099\u208c\u2081\u2070\u2070 (-1)\u207f/(2n+1) = 1 - 1/3 + 1/5 - ... = \u03c0/4
Escribe una función que calcule e imprima en pantalla el valor de:
    4\u03a3\u2099\u208c\u2080\u1d50 (-1)\u207f/(2n+1)
para 0 < m < 10\u2078\n")

##' @title LeibnizPi
##' @description función que aproxima el valor de Pi usando la formula de Leibniz
##' @param m número máximo de sumandos en la formula
LeibnizPi <- function(m){
  if (is.na(m) || is.null(m) || !is.numeric(as.vector(m)) || m <= 0) {
    stop("m: debe ser una variable de tipo numérico no negativo\n")
  } else {
    l <- length(m)
    s <- rep(0, l)
    k <- floor(m)
    names(s) <- paste("m =",k)
    for (j in 1:l) {
      for (n in 0:k[j]) {
        s[j] <- s[j] + (((-1) ^ n) / ((2 * n) + 1))
      }
    }
    return(4*s)
  }
}
cat("Ejemplo usando m = 10, 100, ..., 1000000
    \\ m \t10\t100\t1000\t10000\t100000\t1000000
    \u03c0  ", LeibnizPi(10 ^ (1:6)), "\n")

readline("Pulse [Enter \u21B2] para avanzar al siguiente ejercicio.\n")

#            88                                  88            88                        ,d8    
#            ""                                  ""            ""                      ,d888    
#                                                                                    ,d8" 88    
#  ,adPPYba, 88  ,adPPYba, 8b,dPPYba,  ,adPPYba, 88  ,adPPYba, 88  ,adPPYba,       ,d8"   88    
# a8P_____88 88 a8P_____88 88P'   "Y8 a8"     "" 88 a8"     "" 88 a8"     "8a    ,d8"     88    
# 8PP""""""" 88 8PP""""""" 88         8b         88 8b         88 8b       d8    8888888888888  
# "8b,   ,aa 88 "8b,   ,aa 88         "8a,   ,aa 88 "8a,   ,aa 88 "8a,   ,a8"             88    
#  `"Ybbd8"' 88  `"Ybbd8"' 88          `"Ybbd8"' 88  `"Ybbd8"' 88  `"YbbdP"'              88    
#           ,88                                                                                 
#         888P"                                                                                 

cat("4. Escribe una función que calcule \u221ax para x \u2208 \u211d por el método de la bisección\n")

##' @title Raiz2Biseccion
##' @description calcula la raíz cuadrada de un número por el método de la bisección
##' @param x el número a calcular la raíz cuadrada
Raiz2Biseccion <- function(x){
  if (is.na(x) || is.null(x) || !is.numeric(as.vector(x)) || length(as.vector(x)) > 1 || x < 0) {
    stop("x: debe ser una variable de tipo numérico positivo unidimensional\n")
  }
  a <- 0
  b <- x
  m <- (a + b) / 2
  i <- 0
  while (i < 1000000) {
    if (m * m == x) {
      return(m)
    } else {
      if (m * m > x) {
        b <- m
        m <- (a + b) / 2
      } else {
        a <- m
        m <- (a + b) / 2
      }
      i <- i + 1
    }
  }
  cat("Demasiadas iteraciones\n")
  return(m)
}

cat("Ejemplo usando x = 64\n")
print(Raiz2Biseccion(64))

readline("Pulse [Enter \u21B2] para avanzar al siguiente ejercicio.\n")

#            88                                  88            88                8888888888   
#            ""                                  ""            ""                88           
#                                                                                88  ____     
#  ,adPPYba, 88  ,adPPYba, 8b,dPPYba,  ,adPPYba, 88  ,adPPYba, 88  ,adPPYba,     88a8PPPP8b,  
# a8P_____88 88 a8P_____88 88P'   "Y8 a8"     "" 88 a8"     "" 88 a8"     "8a    PP"     `8b  
# 8PP""""""" 88 8PP""""""" 88         8b         88 8b         88 8b       d8             d8  
# "8b,   ,aa 88 "8b,   ,aa 88         "8a,   ,aa 88 "8a,   ,aa 88 "8a,   ,a8"    Y8a     a8P  
#  `"Ybbd8"' 88  `"Ybbd8"' 88          `"Ybbd8"' 88  `"Ybbd8"' 88  `"YbbdP"'      "Y88888P"   
#           ,88                                                                               
#         888P"                                                                               

cat("5. Escribe una función que calcule \u221ax para x \u2208 \u211d por el método de la secante\n")

##' @title Raiz2Secante
##' @description calcula la raíz cuadrada de un número por el método de la secante
##' @param x el número a calcular la raíz cuadrada
Raiz2Secante <- function(x) {
  if (is.na(x) || is.null(x) || !is.numeric(as.vector(x)) || length(as.vector(x)) > 1 || x < 0) {
    stop("x: debe ser una variable de tipo numérico positivo unidimensional\n")
  }
  i <- 2
  p <- 0
  p0 <- 0
  p1 <- x
  q0 <- 0
  q1 <- p1 ^ 2 - x
  while (i < 1000000) {
    p <- p1 - q1 * ((p1 - p0) / (q1 - q0))
    if ( p * p - x == 0) {
      return(p)
    }
    i <- i + 1 
    p0 <- p1
    q0 <- q1
    p1 <- p
    q1 <- p ^ 2 - x
  }
  cat("Demasiadas iteraciones\n")
  return(p)
}

cat("Ejemplo usando x = 64\n")
print(Raiz2Secante(64))
