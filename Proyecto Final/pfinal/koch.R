cat("                                             
88                              88           
88                              88           
88                              88           
88   ,d8  ,adPPYba,   ,adPPYba, 88,dPPYba,   
88 ,a8\"  a8\"     \"8a a8\"     \"\" 88P\'    \"8a  
8888[    8b       d8 8b         88       88  
88`\"Yba, \"8a,   ,a8\" \"8a,   ,aa 88       88  
88   `Y8a `\"YbbdP\"\'   `\"Ybbd8\"\' 88       88  
                                             
                Sistema Iterativo de Funciones")
readline("Pulse [Enter \u21B2] para comenzar.")

cat(
"Para el proyecto final programarán el fractal conocido como Fractal de Koch.
Niels Fabian Helge von Koch (Estocolmo, 25 de enero de 1870 - 11 de marzo de 1924)
fue un matemático sueco, cuyo nombre se ha asignado a una famosa curva fractal
llamada curva copo de nieve de Koch, una de las primeras curvas fractales en ser
descritas.

Von Koch escribió muchos artículos sobre teoría de números. Uno de sus resultados
fue el teorema que probaba que la hipótesis de Riemann es equivalente al Teorema de
los números primos (1901). También describió la curva que lleva su nombre, curva de
Koch, en un artículo del año 1904 titulado \"Acerca de una curva continua que no posee
tangentes y obtenida por los métodos de la geometría elemental.\"

Para construir el fractal se comienza con un triángulo equilátero (supongamos que sus
lados son de longitud L). En el centro de cada lado se añade otro nuevo triángulo
equilátero de lado 1/3 del anterior, obteniendo así una bonita estrella de David (Nivel
1). Si seguimos con este proceso una y otra vez el resultado nos irá recordando a un
perfecto copo de nieve.

¿Ya tienes una idea de como debe ser el algoritmo? Te voy a ayudar un poco. Tal como
puede apreciarse en la figura de arriba, este fractal se forma partiendo de un triángulo
equilátero cuyos lados tienen longitud L, al que llamaremos fractal de Nivel 0 (como
se había dicho anteriormente). En una primera transformación, cada lado se divide en
tres segmentos de igual longitud. El segmento del medio se retira y se remplaza por
dos segmentos de longitud (1/3)L que forman con los segmentos adyacentes un ángulo
\u03b1 = 60° (ojo con la longitud del ángulo). La figura que se obtiene es el fractal del
Nivel 1.

En una segunda transformación, se vuelve a dividir cada uno de los segmentos obtenidos
en tres segmentos de igual longitud, se retiran los segmentos del medio y se remplazan
nuevamente por dos segmentos, que tienen una longitud esta vez de (1/3)\u00b2L = (1/9)L,
que forman con los segmentos adyacentes un ángulo \u03b1 = 60° . Con esto se obtiene el
fractal de Nivel 2 (nota como la potencia está relacionada con el nivel del fractal).

Este proceso de transformación se continúa sucesivamente. Se entiende que el copo
de nieve de Koch, es la figura que se obtiene cuando el número correspondiente al
nivel del fractal tiene a infinito (nosotros haremos hasta 8 iteraciones). Su borde es
una curva densamente quebrada que tiene la notable propiedad de que sus partes son
autosemejantes con el segmento total al que pertenecen.
")

###############################################################################
##' @title orto
##' @description computa el vector ortogonal de un vector
##' @param v el vector a ortogonalizar
##' @return el vector ortogonalizado
orto <- function(v){
  if (!is.numeric(v) || length(v) != 2) {
    stop("El vector debe ser un vector numérico de dos dimensiones")
  }
  return(c(-v[2], v[1]))
}

###############################################################################
##' @title koch
##' @description función que grafica el Fractal de Koch hasta 8 niveles
##' la implementación difiere ya que no se usan segmentos de recta y más bien
##' se utiliza un enfoque vectorial para encontrar los puntos.
##' @param n el nivel hasta donde graficar
##' @return la lista de los vértices del Fractal
koch <- function(n) {
  if (n < 0 || length(n) != 1 || !is.numeric(n)) {
    stop("El nivel debe ser un número entero")
  }
  if (n > 8) {
    warning("Detectado n > 8 el programa puede no tener los recursos suficientes")
    warning("Usando n = 8.")
    return(koch(8))
  }
  th <- 3 ^ 0.5 # constante de Theodorus
  ## inicializamos el triángulo equilátero
  p <- rbind(c(-1, 0), c(0, th), c(1,0), c(-1, 0)) 
  colnames(p) <- c("x", "y")
  if (n == 0) {
    np <- nrow(p) ## número de puntos  
    plot(x = p[, 1],
         y = p[, 2],
         type = "p",
         main = "Nivel 0",
         sub = paste("Número de puntos ", np - 1, sep = ""),
         xlab = "",
         ylab = "",
         asp = 1,
         pch = 8,
         cex = 2,
         col = rainbow(np))
    lines(p[, 1], p[, 2])
  } else {
    for (i in 1:n) {
      paux <- p
      pnuevos <- NULL
      m <- nrow(p) - 1## - 1
      for (j in 1:m) {
        ## tomamos los vectores extremos en un lado
        a <- p[j,] 
        b <- p[j + 1,]
        #       b 
        #       /^
        #      / |
        #     /  |
        #    /   |
        #   /    |
        # a<_____|  
        
        ## calculamos los dos puntos tercios de los vectores
        p1 <- ((b - a) / 3) + a
        p3 <- ((b - a) * 2 / 3) + a
        #       b 
        #       /^
        #   p3 / |
        #     /  |
        # p1 /   |
        #   /    |
        # a<_____|  
        
        ## anclamos en el punto medio
        ## y escalamos en la dirección del ortogonal de p3 - pm
        ## justo el escalar es la constante de Theodorus
        ## pues dado un triangulo equilatero con semilado ||p3 - pm||
        ## ||th orto(p3 -pm)|| ^2 =  2||p3 - pm|| ^ 2 + ||p3 - pm|| ^2
        ## => th ^ 2 ||orto(p3 - pm)|| ^ 2 = 3 ||pr - pm|| ^ 2
        ## y como la norma de un vector es la misma que la de su ortogonal
        ## => th ^ 2 = 3
        ## => th = sqrt(3)
        pm <- (a + b)/2
        p2 <- pm + (th * orto(p3 - pm))
        #         b 
        #        /^
        #p2 ___ / |
        #  \   /  |
        #   \ /   |
        #    /    |
        #  a<_____|  
        
        pnuevos <- rbind(p1, p2, p3)
        ## al final sólo apilamos los vectores recopilados, con los nuevos
        ## y al tope ponemos el extremo 'derecho' 
        ## así garantizamos que los puntos se podrán unir en orden
        paux <- rbind(paux[1:(1 + 4 * (j - 1)),], pnuevos, b)
      }
      p <- paux
      np <- nrow(p) ## número de puntos  
      plot(x = p[, 1],
           y = p[, 2],
           type = "p",
           main = paste("Nivel ", i, sep = ""),
           sub = paste("Número de puntos ",np - 1, sep = ""),
           xlab = "",
           ylab = "",
           asp = 1,
           pch = 8,
           cex = 2,
           col = rainbow(np))
      lines(p[, 1], p[, 2])
    } # fin del for
  } # fin del  else (n > 0)
  return(p);
}

n <- 5
cat("\n Ejemplo usando n =",n)
print(koch(5))
