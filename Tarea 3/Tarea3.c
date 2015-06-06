#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <assert.h>
#include <limits.h>
#define ROJO      "\x1b[31m"
#define VERDE     "\x1b[32m"
#define AMARILLO  "\x1b[33m"
#define AZUL      "\x1b[34m"
#define MAGENTA   "\x1b[35m"
#define CYAN      "\x1b[36m"
#define SINCOLOR  "\x1b[0m"
#define NELEMS(x) (sizeof(x)/sizeof(*(x)))
#define __author__ "Víctor Gerardo González Quiroga"

/*******************************************************************************
 * banner. Imprime un banner de bienvenida que dice 'Tarea 3'.
 */
void banner(){
	puts(AZUL "                                                                    ");
	puts("888888888888                                            ad888888b,  ");
	puts("     88                                                d8\"     \"88  ");
	puts("     88                                                        a8P  ");
	puts("     88 ,adPPYYba, 8b,dPPYba,  ,adPPYba, ,adPPYYba,         aad8\"   ");
	puts("     88 \"\"     `Y8 88P\'   \"Y8 a8P_____88 \"\"     `Y8         \"\"Y8,   ");
	puts("     88 ,adPPPPP88 88         8PP\"\"\"\"\"\"\" ,adPPPPP88            \"8b  ");
	puts("     88 88,    ,88 88         \"8b,   ,aa 88,    ,88    Y8,     a88  ");
	puts("     88 `\"8bbdP\"Y8 88          `\"Ybbd8\"\' `\"8bbdP\"Y8     \"Y888888P\'  ");
	puts("                                                                    ");
	puts("                                                                    " SINCOLOR);
}

/*******************************************************************************
 * pulsa_enter. Punto de pausa para continuar.
 */
void pulsa_enter() {
  char enter = 0;
  puts(CYAN "Pulsa [Enter \u21B2] para continuar." SINCOLOR);
  while (enter != '\r' && enter != '\n') {
    enter = getchar();
  }
}

/*******************************************************************************
 * limpiar_stdin. Función que "limpia" la entrada estándar de ser necesario, como
 * por ejemplo cuando se esperaba un tipo primitivo y se recibió otro.
 * @returns 1 cuando se ha limpiado la consola
 */
int limpiar_stdin() {
  while (getchar() != '\n' );
  return 1;
}

/*******************************************************************************
 * print_arr. Imprime un arreglo de números enteros en la consola.
 * @param a[] el arreglo a imprimir.
 * @param n la longitud del arreglo.
 */
void print_arr(int a[], int n) {
  int i;
  for (i = 0; i < n; i++) {
    printf(VERDE "a[%d] = %d\n" SINCOLOR, i, a[i]);
  }
}

/******************************************************************************
 * swap. Intercambia dos valores enteros en la memoria.
 * @param *a el primer valor.
 * @param *b el segundo valor.
 */
void swap(int *a, int *b) {
  int t = *a;
  *a = *b;
  *b = t;
}

/******************************************************************************
 * quicksort. Ordena un arreglo de números enteros  con quick sort.
 * @param a[] el arreglo a ordenar.
 * @param m el número de indice mínimo desde donde ordenar.
 * @param n el número de índice máximo hasta donde ordenar.
 */
void quicksort(int a[],int m,int n) {
  int i,j,k,piv;
  if (n < -1) {
    puts(ROJO "Error: n debe ser un entero positivo" SINCOLOR);
  }
  if (m < 0){
    puts(AMARILLO "Aviso: Detectado m < 0, usando m = 0" SINCOLOR);
    quicksort(a, 0, n);
  }
  if (m < n) {
    piv = (m + n) / 2;
    swap(&a[m], &a[piv]);
    k = a[m];
    i = m + 1;
    j = n;
    while (i <= j) {
      while ((i <= n) && (a[i] <= k)) {
        i++;
      }
      while ((j >= m) && (a[j] > k)) {
        j--;
      }
      if(i < j)
      swap(&a[i], &a[j]);
    }
    swap(&a[m], &a[j]);
    quicksort(a, m, j - 1);
    quicksort(a, j + 1, n);
  }
} // quicksort

/********************************************************************************
 * combos. Función que computa las combinaciones de un número en otro.
 * @param n el número de elementos en el conjunto.
 * @param m el número de elementos a escoger en el conjunto.
 * @returns las combinaciones de n en m.
 */
long int combos(int n, int m){
  // se escogio un long int, por compatibilidad en sistemas 32 bits
  long int cnm = 1L, // C(n,m)
         dendo = n, // dividendo (n)(n - 1)...(n - k)(n - k - 1) == n / (n - k)!
           sor = 1L,  // divisor (1)(2)...(k - 1)(k) == k!
            k = (m > n - m)? n - m : m;    // número de operandos

  while (sor <= k) {
    if (dendo % sor == 0L) {
      cnm *= dendo / sor;
    } else if (cnm % sor == 0L){
      cnm = (cnm / sor) * dendo;
    } else {
      cnm = (cnm * dendo) / sor;
    }
    dendo--;
    sor++;
  }
  return cnm;
} // combos

/******************************************************************************
 * complejo. Estructura que representa un número complejo.
 * @param re la parte real del número complejo.
 * @param im la parte imaginaria del número complejo.
 */
typedef struct Complejo {
  double re;
  double im;
} complejo;

/******************************************************************************
 * *complejo_cons. Constructor de un "complejo".
 * @param re la parte real del número complejo.
 * @param im la parte imaginaria del número complejo.
 * @returns un complejo nuevo incializado en la parte real e imaginaria previas.
 * @seealso struct Complejo.
 */
complejo *complejo_cons(double re, double im) {
  complejo *z = malloc(sizeof(complejo));
  assert(z != NULL);
  z->re = re;
  z->im = im;
  return z;
}

/******************************************************************************
 * complejo_dest, Destructor de un "complejo".
 * @param *z número complejo a destruir.
 * @seealso struct Complejo.
 */
void complejo_dest(complejo *z) {
  assert(z != NULL);
  free(z);
}

/******************************************************************************
 * complejo_arg. Computa el argumento de un número complejo.
 * @param *z el número complejo a computar su argumento.
 * @seealso struct Complejo.
 */
double complejo_arg(complejo *z) {
  return atan2(z->im, z->re);
}

/******************************************************************************
 * complejo_mod. Computa el módulo de un número complejo.
 * @param *z el número complejo a computar su argumento.
 * @seealso struct Complejo.
 */
double complejo_mod(complejo *z) {
  double r = z->re,
         i = z->im;
  return sqrt((r * r) + (i * i));
}

/******************************************************************************
 * complejo_mod. Computa el módulo de un número complejo.
 * @param *z el número complejo a computar su argumento.
 * @seealso struct Complejo.
 */
void complejo_print(complejo *z) {
  printf("(%f, %f)", z->re, z->im);
}

/******************************************************************************
 * complejo_mod. Computa la suma de dos números complejos.
 * @param *z un sumando.
 * @param *w el otro sumando.
 * @seealso struct Complejo.
 */
complejo *complejo_suma(complejo *z, complejo *w){
  return complejo_cons(z->re + w->re, z->im + w->im);
}

/******************************************************************************
 * complejo_mod. Computa la resta de dos números complejos.
 * @param *z el minuendo.
 * @param *w el sustraendo.
 * @seealso struct Complejo.
 */
complejo *complejo_rest(complejo *z, complejo *w){
  return complejo_cons(z->re - w->re, z->im - w->im);
}

/******************************************************************************
 * complejo_mod. Computa la multiplicación de dos números complejos.
 * @param *z un multiplicando.
 * @param *w el otro multiplicando.
 * @seealso struct Complejo.
 */
complejo *complejo_mult(complejo *z, complejo *w){
  double a = z->re,
         b = z->im,
         c = w->re,
         d = w->im;
  return complejo_cons((a * c) - (b * d), (b * c) + (a * d));
}

/******************************************************************************
 * complejo_divi. Computa la división de dos números complejos.
 * @param *z el dividendo.
 * @param *w el divisor.
 * @seealso struct Complejo.
 */
complejo *complejo_divi(complejo *z, complejo *w){
  double a = z->re,
         b = z->im,
         c = w->re,
         d = w->im,
         m2 = (c * c) + (d * d);
  if (m2 == 0) {
    puts(ROJO "Error: División sobre 0" SINCOLOR);
    return EXIT_FAILURE;
  }
  return complejo_cons(((a * c) + (b * d)) / m2, ((b * c) - (a * d)) / m2);
}

/******************************************************************************
 * complejo_menu. Solicita 2 números complejos para luego realizar operaciones
 * entre éstos y además los representa polarmente y computa sus módulos.
 * @seealso problema3.
 */
void complejo_menu(){
  complejo *z, *w;
  double re,  // parte real
         im;  // parte imaginaria
  int k = 0; // contador auxiliar
  char c = '\n'; // carácter auxiliar

  puts("Ingresa los valores del primer número complejo:");
  do { // escaneamos el valor real de z
    if (k > 0) {
      puts(AMARILLO "Aviso: La parte real debe ser un número real en notación decimal." SINCOLOR);
    }
    printf(CYAN "Re(z) = " SINCOLOR);
    k++;
  } while (((scanf("%lf%c", &re, &c) != 2 || c != '\n') && limpiar_stdin()));
  // mientras no sea un double

  k = 0;
  do { // escaneamos el valor imaginario de z
    if (k > 0) {
      puts(AMARILLO "Aviso: La parte imaginaria debe ser un número real en notación decimal." SINCOLOR);
    }
    printf(CYAN "Im(z) = " SINCOLOR);
    k++;
  } while (((scanf("%lf%c", &im, &c) != 2 || c != '\n') && limpiar_stdin()));
  // mientras no sea un doble
  z = complejo_cons(re, im);
  printf(VERDE "z = ");
  complejo_print(z);
  puts("" SINCOLOR);

  puts("Ingresa los valores del segundo número complejo:");
  k = 0;
  do { // escaneamos el valor real de w
    if (k > 0) {
      puts(AMARILLO "Aviso: La parte real debe ser un número real en notación decimal." SINCOLOR);
    }
    printf(CYAN "Re(w) = " SINCOLOR);
    k++;
  } while (((scanf("%lf%c", &re, &c) != 2 || c != '\n') && limpiar_stdin()));
  // mientras no sea un double

  k = 0;
  do { // escaneamos el valor imaginario de w
    if (k > 0) {
      puts(AMARILLO "Aviso: La parte real debe ser un número real en notación decimal." SINCOLOR);
    }
    printf(CYAN "Im(w) = " SINCOLOR);
    k++;
  } while (((scanf("%lf%c", &im, &c) != 2 || c != '\n') && limpiar_stdin()));
  // mientras no sea un double

  w = complejo_cons(re, im);
  printf(VERDE "w = ");
  complejo_print(w);
  puts("\n" SINCOLOR);

  int continuar = 1; // bandera booleana
  while (continuar) {
    k = 0;
    int opc = 0;
    do {
      if (k > 0) {
        puts(AMARILLO "Aviso: La opcion debe ser un número entero." SINCOLOR);
      }
      puts("Ingresa el número de tu opción seleccionada [?] o cualquier otro valor para salir.");
      puts("\t[1] Sumar");
      puts("\t[2] Restar");
      puts("\t[3] Multiplicar");
      puts("\t[4] Dividir");
      puts("\t[5] Calcular módulos");
      puts("\t[6] Representar polarmente");
      printf(CYAN "\n[?] = " SINCOLOR);
      k++;
    } while (((scanf("%d%c", &opc, &c) != 2 || c != '\n') && limpiar_stdin()));

    switch (opc) {
      case 1:
        printf(VERDE "z + w = ");
        complejo_print(complejo_suma(z, w));
        printf("\n" SINCOLOR);
        break;

      case 2:
        printf(VERDE "z - w = ");
        complejo_print(complejo_rest(z, w));
        printf("\n" SINCOLOR);
        break;

      case 3:
        printf(VERDE "z * w = ");
        complejo_print(complejo_mult(z, w));
        printf("\n" SINCOLOR);
        break;

      case 4:
        printf(VERDE "z / w = ");
        complejo_print(complejo_divi(z, w));
        printf("\n" SINCOLOR);
        break;

      case 5:
        printf(VERDE "|z| = %f\n", complejo_mod(z));
        printf("|w| = %f\n" SINCOLOR, complejo_mod(w));
        break;

      case 6:
        printf(VERDE "z = %fcis(%f)\n" SINCOLOR, complejo_mod(z), complejo_arg(z));
        printf(VERDE "w = %fcis(%f)\n" SINCOLOR, complejo_mod(w), complejo_arg(w));
        break;

      default:
        continuar = 0;
        break;
    } // fin del switch opc
  } // fin del while continuar
  // desalojamos los complejos de la memoria
  complejo_dest(z);
  complejo_dest(w);
} // complejo_menu

/******************************************************************************
 * dia_semana. Computa el día de la semana basado en el año, mes y día usando
 * el algoritmo de Zeller.
 * @param anio el año.
 * @param mes el mes.
 * @param dia el día (numérico).
 * @returns el día de la semana de 0 a 6.
 */
int dia_semana(int anio, int mes, int dia){
  int m,
      y;
  if (mes == 1 || mes == 2) {
    m = mes + 12; // enero y febrero se tratan como los meses 13 y 14
    y = anio - 1; // del año anterior
  } else {
    m = mes;
    y = anio;
  }
  int d = dia,
      c = y / 100; // dígitos del siglo
  y = y % 100;     // dígitos de la década
  return ((d + ((13 * (m + 1)) / 5) + y + (y / 4) + (c / 4) + (5 * c)) - 1) % 7;
}

/******************************************************************************
 * es_bisiesto. Verifica si un año es bisiesto.
 * @param anio el año a verificar.
 * @returns un valor booleano verificando si el año es bisiesto.
 */
int es_bisiesto(int anio){
  return ((anio % 4 == 0) && (!(anio % 100 == 0) || (anio % 400 == 0)));
}

/******************************************************************************
 * tiene_31. Verifica si un mes tiene 31 días.
 * @param mes el mes a verificar
 * @return un valor booleanor verificando si el mes tiene 31 días.
 */
int tiene_31(int mes){
  return (mes <= 7)? (mes % 2 == 1) : (mes % 2 == 0);
}

/******************************************************************************
 * calendario_print. Imprime un calendario literalmente de la siguiente forma
 *
 *  D  L  M  M  M  J  V  S
 *                 1  2  3
 *  4  5  6  7  8  9 10 11
 * 12 13 14 15 16 17 18 19
 * 20 21 22 23 24 25 26 27
 * 28 29 30
 *
 * @param mes el mes del calendario.
 * @param anio el año del calendario.
 */
void print_calendario(int mes, int anio){
  char c;
  int i, j;
  int d1 = dia_semana(anio, mes, 1), // día inicial (en la semana)
        d = 0,                         // día actual
        bisiesto = es_bisiesto(anio),  // verificador de año bisiestp
        tiene31 = tiene_31(mes);       // verificador de mes con 31 días

  // Se imprimen los días de la semana
  puts(VERDE " D  L  M  M  J  V  S");

  // Se imprime la semana inicial
  for (j = 0; j < 21; j += 3) {
    if (j / 3 >= d1) {
      d++;
      printf(" %d ", d);
    } else {
      printf("   ");
    }
  }
  putchar('\n');

  // Se imprimen las demás semanas
  for (i = 0; i < 5; i++) {
    for (j = 0; j < 21; j += 3) {
      d++; // vamos aumentando los días
      int decn = d / 10, // decenas
          unid = d % 10; // unidades
      if (mes == 2) { // si estamos en el mes de febrero
        if (d <= 28) { // si el día es menor a 28 lo imprimimos
          (decn == 0)? printf(" %d ", unid): printf("%d%d ", decn, unid);
        } else if (d == 29 && bisiesto) {
          printf("29 "); // imprimimos 29 sólo sí el año es bisiesto
        } else {
          printf("   "); // si no, imprimimos espacios en blanco
        }
      } else if (tiene31 && d == 31) {
        printf("31 "); // imprimimos 31 sólo si el mes tiene 31 días
      } else if(d <= 30) { // si no siempre se imprime el día correspondiente
        (decn == 0)? printf(" %d ", unid): printf("%d%d ", decn, unid);
      } else {  // cuando ya hemos avanzado en todos los días posibles
        printf("   ");  // no queda más que imprimir espacios en blancos
      }
    } // fin for( j... iterador de días
    putchar('\n'); // cambiamos de línea
  } // fin for( i... iterador de semanas
  printf(SINCOLOR);

} // print_calendario

/*******************************************************************************
 * problema1. Solicita al usuario números enteros (máximo 20) y los imprime en
 * la pantalla ordenados.
 * @returns código de salida exitosa o fallida.
 */
int problema1() {
  puts("\nProblema 1. Escribe un programa que solicita al usuario números");
  puts("enteros (máximo 20) y los imprima en pantalla ordenados. Puedes");
  puts("utilizar el algoritmo de la burbuja.");
  pulsa_enter();

  puts("\nBienvenido a la Resolución del Problema 1.");
  int n = 1, // dimensión del arreglo (número de elementos)
      k = 0; // contador auxiliar
  char c = '\n'; //carácter auxiliar
  do { // escanea el número de elementos
    if (k > 0) { //imprime si se han detectado errores de formato al escanear
      puts(AMARILLO "Aviso: El número de elementos debe ser un entero no negativo menor o igual a 20." SINCOLOR);
    }
    puts("Ingrese el número de elementos a ordenar:");
    printf(CYAN "n = " SINCOLOR);
    k++;
  } while (((scanf("%d%c", &n, &c) != 2 || c != '\n') && limpiar_stdin()) || n < 1 || n > 20);
  // mientras haya errores al escanear

  int i; // índice del arreglo
  int arr[n]; //arreglo de números
  puts("Ingresa los elementos a ordenar:");
  for (i = 0; i < n; i++) {
    k = 0;
    do {
      if (k > 0) {
        puts(AMARILLO "Aviso: Los elementos deben ser números enteros." SINCOLOR);
        puts("Ingresa los elementos a ordenar:");
      }
      printf(CYAN "a[%d] = " SINCOLOR,i);
      k++;
    } while (((scanf("%d%c", &arr[i], &c) != 2 || c != '\n') && limpiar_stdin()));
  }
  puts("\nOrdenando el arreglo de enteros con QuickSort...");
  quicksort(arr, 0, n-1);
  print_arr(arr, n);
  puts("");
  return EXIT_SUCCESS;
} // problema1

/*******************************************************************************
 * problema2. Solicita al usuario dos números enteros
 * @returns código de salida exitosa o fallida.
 */
int problema2() {
  puts("\nProblema 2. A partir de la fórmula para calcular las combinaciones de");
  puts("n elementos tomados de m escribe un programa que solicite al usuario");
  puts("ambos valores e imprima el resultado en pantalla. El programa debe");
  puts("preguntar al usuario si desea continuar o finalizar.");
  pulsa_enter();

  puts("\nBienvenido a la resolución del Problema 2.");
  int continuar = 1; // bandera booleana de sucesión
  while (continuar) {
   int n = 1,
       m = 1,
       k = 0;
   char c = '\n';
    k = 0;
    do { // escanea el número de elementos en el conjunto
      if (k > 0) { //imprime si se han detectado errores de formato al escanear
        puts(AMARILLO "Aviso: El número de elementos del conjunto debe ser un entero positivo" SINCOLOR);
      }
      puts("Ingresa el número de elementos del conjunto:");
      printf(CYAN "n = " SINCOLOR);
      k++;
    } while (((scanf("%d%c", &n, &c) != 2 || c != '\n') && limpiar_stdin()) || n < 0);
    k = 0;
    do { // escanea el número de elementos a escoger en el conjunto
      if (k > 0) { //imprime si se han detectado errores de formato al escanear
        puts(AMARILLO "Aviso: El número de elementos a escoger debe ser un entero positivo menor o igual a n" SINCOLOR);
      }
      puts("\nIngresa el número de elementos a escoger:");
      printf(CYAN "m = " SINCOLOR);
      k++;
    } while (((scanf("%d%c", &m, &c) != 2 || c != '\n') && limpiar_stdin()) || m < 0 || m > n);

    puts("\nComputando combinaciones...");
    long int cnm = combos(n,m);
    printf(VERDE "C(%d, %d) = %ld\n" SINCOLOR, n, m, cnm);

    puts(CYAN "\n¿Deseas volver a calcular otro número de combinaciones? [S/n]" SINCOLOR);
    c = getchar();
    continuar = (c == 'S' || c == 's' || c == '1' || &c == NULL || c == '\0')? 1 : 0;
  }
  puts("");
  return EXIT_SUCCESS;
} // problema2

/******************************************************************************
 * problema3. Solicita al usuario dos números complejos e implementa un menú
 * para interactuar con los números complejos.
 * @seealso complejo_menu
 */
int problema3(){
  puts("\nProblema 3. Utiliza las estructuras para escribir un programa que pida");
  puts("dos números complejos: z\u2081= a + bi y z\u2082 = c + di con a, b, c, d \u2208 \u211D, y");
  puts("sea capaz de realizar las siguientes operaciones (implementa también un");
  puts("menú:");
  puts("\t1. Suma,");
  puts("\t2. Resta");
  puts("\t3. Multiplicación");
  puts("\t4. División");
  puts("\t5. Módulo");
  puts("\t6. Representación polar");
  pulsa_enter();

  puts("\nBienvenido a la resolución del Problema 3.");
  int continuar = 1;
  char c;
  while (continuar) {
    complejo_menu();
    puts(CYAN "\n¿Deseas volver a ingresar números complejos? [S/n]" SINCOLOR);
    c = getchar();
    continuar = (c == 'S' || c == 's' || c == 1 || &c == NULL || c == '\0')? 1 : 0;
  }
  puts("");
  return EXIT_SUCCESS;
} // problema 3

int problema4() {
  puts("\nProblema 4. Dados un año y un mes, escribe un programa que muestre");
  puts("en pantalla el calendario correspondiente. Por ejemplo, si el usuario");
  puts("ingresa el año 2011 y el mes de septiembre el programa debe mostrar en");
  puts("pantalla lo siguiente:");
  puts("Mes: Septiembre");
  puts("Año: 2011");
  puts("s: Mes siguiente.");
  puts("a: Mes anterior.");
  puts("S: Mismo mes, año anterior.");
  puts("A: Mismo mes, año siguiente.\n");
  puts("  D  L  M  M  M  J  V  S");
  puts("                 1  2  3");
  puts("  4  5  6  7  8  9 10 11");
  puts(" 12 13 14 15 16 17 18 19");
  puts(" 20 21 22 23 24 25 26 27");
  puts(" 28 29 30");
  pulsa_enter();

  puts("\nBienvenido a la resolución del Problema 4.");
  int m = 9,    // mes
      y = 2011; // año
  int k = 0;
  char c = '\n';
  const char *meses[] = {"Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                         "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"};
  do{
    if (k > 0 ) {
      puts(AMARILLO "Aviso: La opción debe ser un número entero entre 1 y 12" SINCOLOR);
    }
    puts("Selección del Mes.");
    puts("Ingresa el número de tu opción seleccionada [?] o cualquier otro valor para salir.");
    int i;
    for (i = 0; i < 12; i++) {
        printf("\t[%d] %s\n", (i + 1), meses[i]);
    }
    printf(CYAN "[?] = " SINCOLOR);
    k++;
  } while (((scanf("%d%c", &m, &c) != 2 || c != '\n') && limpiar_stdin()) || m < 1 || m > 12);

  k = 0;
  do {
    if (k > 0) {
      puts(AMARILLO "Aviso: El año debe ser un número entero.");
    }
    puts("Ingresa el año");
    printf(CYAN "Año = " SINCOLOR);
  } while(((scanf("%d%c", &y, &c) != 2 || c != '\n') && limpiar_stdin()));

  int continuar = 1,
      yact = y, // año actual
      mact = m; // mes actual
  while (continuar) {
    char cc;
    k = 0;
    puts("");
    do{
      if (k > 0){
        puts(AMARILLO "Aviso: Debes ingresar un solo carácter" SINCOLOR);
      }
      puts("Ingresa la letra de tu opción seleccionada [?] o cualquier otra para salir.");
      puts("\t[s]: Mes siguiente");
      puts("\t[a]: Mes anterior");
      puts("\t[S]: Mismo mes, año siguiente");
      puts("\t[A]: Mismo mes, año anterior");
      printf(VERDE "%s - %d\n", meses[mact - 1], yact);
      print_calendario(mact, yact);
      printf(CYAN "[?] = " SINCOLOR);
      k++;
    } while (((scanf("%c%c", &cc, &c) != 2 || c != '\n') && limpiar_stdin()));
    switch (cc) {
      case 's':
        if (mact == 12) {
          mact = 1;
          yact++;
        } else {
          mact = (mact == 11)? 12 : mact + 1;
        }
        break;

      case 'a':
        if (mact == 1) {
          mact = 12;
          yact--;
        } else {
          mact--;
        }
        break;

      case 'S':
        yact++;
        break;

      case 'A':
        yact--;
        break;

      default:
        continuar = 0;
        break;
    }
  }

  return EXIT_SUCCESS;
}

/******************************************************************************
 * main. Programa principal que corre las resoluciones de los Problemas en orden
 * y sin interrupciones.
 */
int main(){
  banner();
  char c = 0;
  puts(CYAN "Pulsa [Enter \u21B2] para comenzar." SINCOLOR);
  while (c != '\r' && c != '\n') {
    c = getchar();
  }

  int p1 = problema1();
  while (p1 != EXIT_SUCCESS) {
    perror(ROJO "Error: Problema 1" SINCOLOR "\n");
    puts(CYAN "¿Deseas volver a correr el Problema 1? [S/n]" SINCOLOR);
    c = getchar();
    p1 = (c == 'S' || c == 's' || c == 1 || &c == NULL || c == '\0')? problema1() : EXIT_SUCCESS;
  }

  int p2 = problema2();
  while (p2 != EXIT_SUCCESS) {
    perror(ROJO "Error: Problema 2" SINCOLOR "\n");
    puts(CYAN "¿Deseas volver a correr el Problema 2? [S/n]" SINCOLOR);
    c = getchar();
    p2 = (c == 'S' || c == 's' || c == 1 || &c == NULL || c == '\0')? problema2() : EXIT_SUCCESS;
  }

  int p3 = problema3();
  while (p3 != EXIT_SUCCESS) {
    perror(ROJO "Error: Problema 3" SINCOLOR "\n");
    puts(CYAN "¿Deseas volver a correr el Problema 3? [S/n]" SINCOLOR);
    c = getchar();
    p3 = (c == 'S' || c == 's' || c == 1 || &c == NULL || c == '\0')? problema3() : EXIT_SUCCESS;
  }

  int p4 = problema4();
  while (p4 != EXIT_SUCCESS) {
    perror(ROJO "Error: Problema 4" SINCOLOR "\n");
    puts(CYAN "¿Deseas volver a correr el Problema 4? [S/n]" SINCOLOR);
    c = getchar();
    p4 = (c == 'S' || c == 's' || c == 1 || &c == NULL || c == '\0')? problema4() : EXIT_SUCCESS;
  }

  return 0;
}
