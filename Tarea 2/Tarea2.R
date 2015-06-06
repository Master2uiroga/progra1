cat("
888888888888                                            ad888888b,  
     88                                                d8\"     \"88  
     88                                                        a8P  
     88 ,adPPYYba, 8b,dPPYba,  ,adPPYba, ,adPPYYba,         ,d8P\"   
     88 \"\"     `Y8 88P\'   \"Y8 a8P_____88 \"\"     `Y8       a8P\"      
     88 ,adPPPPP88 88         8PP\"\"\"\"\"\"\" ,adPPPPP88     a8P\'        
     88 88,    ,88 88         \"8b,   ,aa 88,    ,88    d8\"          
     88 `\"8bbdP\"Y8 88          `\"Ybbd8\"\' `\"8bbdP\"Y8    88888888888

")
readline("Pulse [Enter \u21B2] para comenzar.\n")

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
cat("
1. La regla o método de Simpson es un método de integración numérica
que se utiliza para obtener la aproximación de una integral bajo el su-
puesto de que:
    
    \u222b f(x)dx \u2248 (b - a)/6 [f(a) + 4f((a + b)/2) + f(b)]
    
para b - a suficientemente pequeño. En el caso de que [a, b] no sea lo
suficientemente pequeño, éste se divide en n subintervalos (con n par),
de manera que x\u1d62 = a + hi, donde h = (b - a)/n para i = 0,1,...,n.
Aplicando Simpson a cada subintervalo \u03c7 = [x\u2c7c\u208b\u2081,x\u2c7c\u208a\u2081], j = 1,3,...,n - 1,
tenemos
    
    \u222b\u1d6a f(x)dx = (x\u2c7c\u208a\u2081 - x\u2c7c\u208b\u2081)/3 [f(x\u2c7c\u208b\u2081) + 4f(x\u2c7c) + f(x\u2c7c\u208a\u2081)]
    
sumando las integrales de todos los subintervalos, llegamos a
    
    \u222b\u2090\u1d47 f(x)dx \u2248 h/3 [f(x\u2080) + 2 \u03a3\u2c7c\u208c\u2081\u207f\u00b2\u207b\u00b9 f(x\u2082\u2c7c) + 4 \u03a3\u2c7c\u208c\u2081\u207f\u00b2 f(x\u2c7c\u208b\u2081) + f(x\u2099)]
    
Implementa la regla de Simpson para calcular la integral:
    
    \u222b\u2090\u1d47 f(x)dx
    
donde  f(x), a y b son parámetros de la función y h \u2264 0.001.

")

##' @title IntegralSimpson
##' @description función que calcula la integral aproximada de una función sobre un intervalo con el método de Simpson
##' @param a el límite inferior de integración
##' @param b el límite superior de integración
##' @param n el número de intervalos pares a usar
##' @param f la función numérica a calcular la integral
IntegralSimpson <- function(a, b, n, f) {
  if (is.na(n) || is.null(n) || !is.numeric(as.vector(n)) || n %% 2 == 1) {
    stop("n: debe ser una variable de tipo numérico entero no negativo par\n")
  } else if (!is.function(f)) {
    stop("f debe ser una función numérica primitiva\n")
  }
  h = (b - a) / n
  s = f(a) + f(b)
  for (i in seq(1, n, 2)) {
    s <- s + 4 * f(a + i * h)
  }
  for (i in seq(2, n - 1, 2)) {
    s <- s + 2 * f(a + i * h)
  }
  return(s * h / 3)
}
cat("Ejemplo usando f(x) = 1/x, [a, b] = [1, e], n = 100
    \u222b\u2081\u1d49 (1/x)dx =",IntegralSimpson(1,exp(1),100,function(x){1/x}),"\n")

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
cat(
"Suponga que se desea estudiar el número de éxitos X antes de un fracaso
(rachas ganadoras) en el juego de volados, considere además que se
pueden usar monedas cargadas con probabilidad de éxito (E) p y de
fracaso (F) q = 1 − p, Crea una función en R que produzca tal juego.
La función “de rachas” recibirá de argumento el valor p y devolverá
el número de éxitos antes de un fracaso. Contesta las preguntas: ¿si
p = 0.5 cuál es la longitud promedio de una racha? ¿si p = 0.7 cuál
es la longitud promedio de una racha? ¿si p = 0.9 cuál es la longitud
promedio de una racha?

")

##' @title Volado
##' @description función que modela un juego de n volados con probabilidad p
##' @param n el número de volados a lanzar
##' @param p la probabilidad de ganar
Volados <- function(n,p){ 
  ## la función funciona similar a return(rbinom(n,1,p))
  if (!is.numeric(n) || n <= 0 || floor(n) - n != 0) {
    stop("n debe ser un número entero no negativo")
  } else if (p < 0 && p > 1) {
    stop("p debe ser un número entre 0 y 1")
  }
  v <- NULL
  nombres <- NULL
  for (i in 1:n) {
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
    v <- c(v,rbinom(1,1,p))
    nombres <- c(nombres,paste("V",indice))
  }
  names(v) <- nombres
  return(v)
}
    
##' @title Rachas
##' @description función que calcula el número de rachas ganadoras antes de un fracaso
##' @param p la probabilidad de ganar
Rachas <- function(p){
  l <- length(p)
  x <- rep(0,l)
  for (i in 1:l) {
    ganando <- TRUE
    while (ganando) {
      if (rbinom(1,1,p[i]) == 0) {
        ganando = FALSE
      } else {
        x[i] <- x[i] + 1
      }
    }
  }
  return(x)
}
# valores del ejercicio
p <- c(0.5, 0.7, 0.9)
x <- 0
for (i in 1:10000) {
  x <- x + Rachas(p)
}
x <- x/10000
cat("Ejemplo usando p = 0.5, 0.7, ..., 0.9, y estimando el promedio con 10000 muestras
    \\ p 0.5\t0.7\t0.9
    X  ", x, "\n")

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
#         
cat(
"3. La simulación Monte Carlo es un método que emplea números aleato-
rios U(0, 1) para resolver ciertos problemas determinísticos donde el
transcurrir del tiempo no juega un papel sustancial. Algunos autores
definen la simulación Monte Carlo como cualquier simulación que in-
volucra el uso de números aleatorios. El nombre de método o simulación
Monte Carlo se originó durante la Segunda Guerra Mundial, cuando
esta metodología fue aplicada a problemas relacionados al desarrollo de
la bomba atómica. Suponga que queremos evaluar la integral
    
    I = \u222b\u2090\u1d47 g(x)dx
    
donde g(x) es una función que toma valores reales que no es analití-
camente integrable. En la práctica, la simulación Monte Carlo podría
probablemente no ser usada para evaluar una integral sencilla, dado que
hay técnicas más eficientes de análisis numérico para este propósito. Es
más probable que se use para problemas sobre integrales múltiples con
un integrando mal comportado. Para ver como este problema deter-
minístico puede ser enfocado por una simulación Monte Carlo, sea
Y = (b - a)g(X)
una variable aleatoria y X una variable aleatoria U(a, b). Entonces el
valor esperado de Y es
    
    E(Y) = E[(b - a)g(X)]
         = (b - a)E[g(X)]
         = \u222b\u2090\u1d47 g(x)f\u1d6a(x)dx
         = (b - a)(\u222b\u2090\u1d47 g(x)dx))/(b - a)
         = I
    
donde f\u1d6a(x) = 1/(b - a) es la función de densidad de probabilidad de una
variable aleatoria U(a, b).
Así pues, el problema de evaluar la integral se reduce al de estimar el valor
esperado E(Y).
En particular, estimaremos E(Y) = I por la media muestral

    Y(n) =  \u03a3\u1d62\u208c\u2081\u207f Y\u1d52 / n = (b - a)(\u03a3\u1d62\u208c\u2081\u207f g(X\u1d62))/n

donde X\u2081, X\u2082 X son v.a.i.i.d. U(a,b)

Utilice el método Monte Carlo para aproximar las siguientes integrales
    
    i) b > a \u2265 0 calcule \u222b\u2090\u1d47 (x/\u03c3\u00b2) exp(-x\u00b2/2\u03c3\u00b2) dx
    
    ii) \u03bb > 0, k > 0, b > a \u2265 0 calcule \u222b\u2090\u1d47 k/\u03bb (x/\u03bb)\u1d4f\u207b\u00b9 exp(-(x\u03bb)\u1d4f) dx
    
    iii) \u222b\u2083\u00b3 \u222b\u2083\u00b3 1/(2\u03c0) exp(-(x\u00b2 + y\u00b2)/2) dxdy

")

##' @title IntegralMonteCarlo
##' @description Función que aproxima la integral de una función por simulación de MonteCarlo
##' @param a límite superior de integración
##' @param b límite inferior de integración
##' @param f función a aproximar la integral
##' @param n tamaño de la muestra de montecarlo
IntegralMonteCarlo <- function(a, b, f, n) {
  if (!is.function(f)) {
    stop("f no es una función primitiva")
  } else if (!is.numeric(n) || n < 0 || n != floor(n)) {
    stop("n debe ser un número entero no negativo")
  } else if (!is.numeric(a) || !is.numeric(b)) {
    stop("Los límites de integración a y b deben ser números reales")
  }
  if (a > b) {
    warning("Volteando intervalo de integración y cambiando de signo la integral")
    return(-IntegralMonteCarlo(b, a, f, n))
  }
  return((b - a) * sum(f(runif(n, a, b))) / n)
}
## valores hardcodeados  a través de una muestra aleatoria
s <- 1.163089 # rnorm(1,1,1)
b <- 2.935383 # rexp(1,1)
a <- 0.328295 # runif(1,0,b)
l <- 3.616501 # rexp(1,1)
k <- 4.168754 # rexp(1,1)
n <- 1000000
cat("Calculado usando n = ",n,"
    i) Calculando con parámetros adicionales \u03c3 = ",s,", a = ",a,", b = ",b,"
    \u222b\u2090\u1d47 (x/\u03c3\u00b2) exp(-x\u00b2/2\u03c3\u00b2) dx \u2248 ",
    IntegralMonteCarlo(a, b,
                       function(x){(x / s ^ 2) * exp(-x ^ 2 / (2 * s ^ 2))},
                       n),"
    
    ii) Calculando con parámetros adicionales \u03bb = ",l,", k = ",k,", a = ",a,", b = ",b,"
    \u222b\u2090\u1d47 k/\u03bb (x/\u03bb)\u1d4f\u207b\u00b9 exp(-(x\u03bb)\u1d4f) dx \u2248 ",
    IntegralMonteCarlo(a, b,
                       function(x){
                        ((k / l) * (x / l) ^ (k - 1)) * exp(-(x * l) ^ k)},
                       n),"
    
    iii) \u222b\u2083\u00b3 \u222b\u2083\u00b3 1/(2\u03c0) exp(-(x\u00b2 + y\u00b2)/2) dxdy \u2248",
    IntegralMonteCarlo(-3, 3,
                       function(x){
                         IntegralMonteCarlo(-3, 3,
                                            function(y){(1 / (2 * pi)) * exp(-(x ^ 2 + y ^ 2)/2)},
                                            n)},
                       n),"
")

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
cat(
"4. Escribe un programa que calcule la factorización en números primos
para 1 \u2264 n \u2264 1000 sí y sólo sí n no es número primo. Si el número
proporcionado es primo, indicarlo en la salida. El programa deberá
continuar ejecutándose hasta que el usuario indique lo contrario. Ejemplo
de pantalla inicial:

****** BIENVENIDO *******
Este programa calcula los factores primos de n \u2208 [1, 1000]
Indica un número entero entre 1 y 1000: 756
La descomposición en factores primos de 756 es:(2 2 )(3 3 )(7)
\n")

##' @title FactorizarPrimos
##' @description Función que calcula la factorización de primos de un número entero
##' @return una matriz con los factores primos ordenados por el primo y potencia
FactorizarPrimos <- function(){
  cat("****** BIENVENIDO *******
Este programa calcula los factores primos de n \u2208 [1, \u221e)\n")
  entero <- FALSE
  n <- NULL
  while (!entero) {
    cat("Indica un número entero entre mayor a 1: ") ## también sirve para 1000 por si lo quiere probar
    n <- scan(nmax = 1)
    n <- as.numeric(n)
    if (!is.null(n) && is.numeric(n) && n > 1 && floor(n) - n == 0) { 
      entero <- TRUE
    } else {
      cat("\nEso no es un número entero mayor a 1\n")
    }
  }
  if (n <= 3){
    cat(n,"es primo.\n")
    return(c(n,1))
  }
  ## La siguiente rutina describe la generación de divisiores
  ## através de la generación de números con la criba de eratóstenes
  nums <- 1:n
  facts <- ""
  factores <- NULL
  lista <- rep("P", n)           ## marcamos a todos los números como primos por omisión
  for (i in 2:(n - 1)) {        ## iteramos sobre todos los números mayores o iguales a 2
    ni <- nums[i]               ## tomamos el número actual
    if (lista[i] == "P") {      ## si resulta ser primo 
      j <- ni                   ## iteramos desde el número
      while (j < n) {           ## iteramos desde el número
        lista[j] <- "C"         ## marcamos a los otros como compuestos pues son multiplos
        j <- j + ni             ## en incrementos de ese mismo número
      }                         ## posteriormente
      d <- ni                   ## iteramos los multiplos del primo selecto
      pot <- 0                                  
      while (n %% d == 0 && d < n) {      ## dividiendo el número
        d <- ni ^ pot
        pot <- pot + 1          ## para encontrar la máxima potencia donde es divisible
      }                         ## al final lo añadimos a los factores primos
      if (pot > 2) {
        factores <- rbind(factores,c("p" = ni, "potencia" = pot - 2))
        indice = ""
        if (pot > 3){
          indice <- as.character(pot - 2) ## se reemplaza la potencia por un superíndice unicode textual
          indice <- gsub("0", "\u2070", indice)
          indice <- gsub("1", "\u00b9", indice)
          indice <- gsub("2", "\u00b2", indice)
          indice <- gsub("3", "\u00b3", indice)
          indice <- gsub("4", "\u2074", indice)
          indice <- gsub("5", "\u2075", indice)
          indice <- gsub("6", "\u2076", indice)
          indice <- gsub("7", "\u2077", indice)
          indice <- gsub("8", "\u2078", indice)
          indice <- gsub("9", "\u2079", indice)
        }
        fact <- paste("(", ni, indice, ")", sep = "")
        facts <- paste(facts, fact, sep = "")
      }
    }
  }
  if (is.null(factores)) {
    cat(n, "es primo.\n")
    return(c(n,1))
  }
  cat("La descomposición en factores primos de ", n, " es: ", facts, "\n", sep="")
  return(factores)
}
FactorizarPrimos()

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
cat(
"5. Implementa en R un generador de números pseudoaleatorios utilizando
el algoritmo propuesto por Wichman & Hill (1982).
  1. Dar ix, iy, iz enteros mayores a cero y menores a 30,000
  2. Calcular
      ix = {(171ix)mod 177} - 2ix/177
      iy = {(172iy)mod 176} - 35iy/176
      iz = {(170iz)mod 178} - 63ix/178
  3. Evaluar:
      Si ix \u2264 0 entonces ix = ix + 30269
      Si iy \u2264 0 entonces iy = iy + 30307
      Si iz \u2264 0 entonces iz = iz + 30323
  4. Hacer u = (ix/30269 + iy/30307 + iz/30323) mod 1
  5. Repetir pasos 2 a 4 n veces
Dependiendo de la máquina, u puede ser 0 ó 1 en alguna iteración; en
tal caso, se redefine a u como u ± eps, en donde eps es la precisión de la
máquina. En R deberás utilizar el valor .Machine$double.eps para obtener
el épsilon de la máquina.
")

##' @title rwichmannhill
##' @description Función que genera números pseudoaleatorios utilizando el algoritmo propuesto por Wichman & Hill (1982)
##' @param n cantidad de rondas de generación
##' @param ix semilla entera entre 0 y 30000
##' @param iy semilla entera entre 0 y 30000
##' @param iz semilla entera entre 0 y 30000
##' @return un vector con una sucesión de números aleatorios
rwichmannhill <- function(n, ix, iy, iz) {
  if (!is.numeric(ix) || ix <= 0 || ix >= 30000 || ix != floor(ix) || length(ix) > 1) {
    stop("ix debe ser un entero unidimensional mayor a 0 y menor a 30000\n")
  } else if (!is.numeric(iy) || iy <= 0 || iy >= 30000 || iy != floor(iy) || length(iy) > 1) {
    stop("iy debe ser un entero unidimensional mayor a 0 y menor a 30000\n")
  } else if (!is.numeric(iz) || iz <= 0 || iz >= 30000 || iz != floor(iz) || length(iz) > 1) {
    stop("iz debe ser un entero unidimensional mayor a 0 y menor a 30000\n")
  }
  eps <- .Machine$double.eps
  # 1.
  x <- ix
  y <- iy
  z <- iz
  u <- NULL
  for (i in 1:n) {
    # 2.
    x <- ((171 * x) %% 177) - ((2 * x) / 177)
    y <- ((172 * y) %% 176) - ((35 * y) / 176)
    z <- ((170 * z) %% 178) - ((63 * z) / 178)
    # 3.
    if (x <= 0) {
      x = x + 30269
    }
    if (y <= 0) {
      y = y + 30307
    }
    if (z <= 0) {
      z = z + 30323
    }
    # 4.
    r <- ((x / 30269) + (y / 30307) + (z / 30323)) %% 1
    if (r == 0) {
      r <- r + eps
    } else if (r == 1) {
      r <- r - eps
    }
    u <- c(u, r)
  } # 5.
  return(u)
}
#valores de las semillas
x <- 1
y <- 1
z <- 1
n <- 10
cat("Ejemplo de generación de n = ",n," números pseudoaleatorios con la semilla (",x,", ",y,", ",z,")\n",
sep = "")
print(rwichmannhill(n, x, y, z))
