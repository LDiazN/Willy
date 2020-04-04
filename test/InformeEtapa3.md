# Informe Proyecto de Traductores  
# Entrega III
## Luis Diaz 15-10420
## Nathalia Silvera 12-10921


Willy* es un lenguaje con el cual es posible dar instrucciones a Willy, 
que es un robot y para que este pueda trabajar libremente en una cuadrícula 
(dejar y poner objetos), es controlado por un programa que contiene las 
definiciones del mundo, de los procedimientos y el programa principal.


Para interpretar las acciones/ movimientos de Willy implementamos 
un proyecto el cual se divide en 3 etapas


# Etapa III: Analisis de contexto e interpretacion.
Etapa que consiste en obtener la representacion abstracta del mundo,
y los programas para Willy. Ademas en este proceso se detectan los
errores en la especificacion del mundo.

# **Diseno** 
Se realizaron los siguientes archivos: Expresiones.hs, SymbolTable.hs
PrintTools.hs, ContextAnalyzer.hs, Parser.y y Willy.hs, testMonad.hs,
test.hs, Lexer.x, ProgramState.hs, Interpreter.hs y Simulator.hs. 
Especificamente para esta entrega se añadieron los ultimos tres archivos.


**Archivo Expresions.hs:** comprende la lista de expresiones derivadas
de las expresiones primitivas. Las cuales están definidas en los 
constructores: ProgPart-> clase que contiente el mundo y las tareas del 
mismo, WorldStmnt-> clase de instrucciones que definen al mundo de Willy, 
TaskStmnt-> clase de instrucciones programadas permitidas, 
GoalTest->clase de condiciones, BoolExpr->clase con las expresiones booleanas. 


**Archivo SymbolTable.hs:** contiene la tabla con los símbolos a usar.
Los cuales están definidos en los constructores: SymType,que posee la información 
del tipo de estos símbolos, Symbol, el cual tiene los posibles símbolos a usar, 
SymbolTable,es la estructura de datos mediante la que se define la 
tabla de símbolos. Por ultimo, este archivo contiene los msjs según los 
errores que puedan ser generados.


**Archivo ContextAnalyzer.hs:** dado un AST este archivo retorna la tabla de 
simbolos correspondiente. Contiene la funcion analyzer, en la cual procesa
todas las tareas del mundo, verifica los errores y los imprime de ser
necesario. La funcion processProgPart, obtiene el nombre del simbolo y 
verifica si este puede ser insertado en la tabla, tambien verifica si el 
mundo es un mundo valido, busca el contexto del mismo y si es valido agrega
el contexto del mundo en el contexto actual. Funcion addWorldIntr, se agregan 
las propiedades del mundo. Las funciones checkTypeExst, checkBoolExpr son 
funciones verifican auxiliares para verificar si existe el tipo y si el 
booleano es correcto, en caso de no existir o no ser correcto debe ser un 
error y es agregado a la funcion addError la cual agrega los errores al mundo.


**Archivo PrintTools.hs:** en este archivo se analiza el contexto y la 
sintaxis del programa para imprimir la tabla de simbolos. La funcion 
printSymTable imprime la tabla de simbolos pasada como argumento. Y la 
funcion processInstructions recibe el contexto actual del programa e 
imprime la instrucciones según sea el caso. Funcion printProg recibe un
AST e imprime un programa asociado formateado segun las especificaciones
dadas (imprime todas las instrucciones y el nombre del mundo y tarea).


**Archivo Parser.y:** Dado que la herramienta utilizada fue Happy, necesitamos 
este archivo con el formato que exige Happy para asi poder generar el parser 
que recibe el lenguaje.  


**Archivo Willy.hs:** contiene el main para compilar el programa, en este 
se verifica el nombre del archivo a compilar, revisa que el archivo exista
y si existe pasa a leer su contenido y los procesa con el ProgramState, 
Interpreter y los muestra con Simulator. Ademas proporciona opcines para
ejecutar el programa -m para ejecutar el programa paso a paso, -a realiza la 
ejecucion de cada valor en un tiempo dado por el usuario y si lo no da entonces 
la misma es inmediata, -q realiza una ejecucion del mundo segun el tiempo dado por
el usuario, de lo contrario la ejecucion es instantanea.  En caso contrario 
el programa mostrara el error correspondiente en consola.


**Archivo ProgramState.hs:** contiene el estado del programa (mundo de willy),
para ello se crea una data ProcessState con los posibles estados del programa 
(ejecutandose, error y fin del programa). Data Item, contiene los objetos del 
programa junto con el simbolo que lo representa la cantidad, y type ItemSet 
define conjunto de objetos. data Object define los diferentes objetos que 
puede tener el mundo de willy (por ejemplo la paredes). data Orientation, contiene
la posible orientacion que pueda tener o tomar willy. data Willy contiene la siguiente
informacion: (posicion actual, orientaciond de la mira de willy y id al tipo de objeto).
data ProgramState mapea la posicon actual de willy en el mundo, la capacidad de 
la cesta, una tabla de simbolos para el mundo, nombre del mundo y las tareas 
asociadas y el tamaño. data RunTimeError es una lista con los errores que pueda 
tener el programa. initProgramState dada la tabla de simbolos y tareas del mundo 
esta funcion retorna un estado del programa willy.

**Archivo Interpreter.hs:** Dado una tabla de simbolos retornada por el analizador
de contexto, un string que representa una tarea y un estado del programa. Ejecuta
el programa en el mundo de willy llamando a las funciones correspondientes definidas 
en ProgramState. runInst esta funcion se utiliza para ejecutar cada instruccion del
mundo de willy (move, turn-left,turn-right,etc).


**Archivo Simulator.hs:** Este archivo contiene las funciones para mostrar
en consola (imprimir), los cambios que se vayan realizando sobre el mundo 
de Willy, es decir los movimientos y acciones que este realice. La funcion
printWorldMap, muestra el mundo de willy, con su posicion y orientacion actual, 
sus paredes y espacios libres. printSensors funcion para mostrar en pantalla
si las celdas adyacentes estan libres. La funcion printBasket es mostrar los 
elementos de la cesta de willy, si esta vacia o cuantos estan en ella. 
printItems muestra cuantos objetos que hay en el mundo.
printVars para imprimir el estado de las variables booleanas. printAlls, para
imprimir todo. 

**Carpeta demo:** Esta carpeta contiene archivos con el formato .txt para 
demostrar las funciones de Willy.  

**Ejecucion:** Ejecutar el comando "Make" en la carperta principal,
luego debe usar el ejecutable ./Willy [nombre del archivo] [nombre de task] [options]

