#Informe Proyecto de Traductores  
#Entrega II
## Luis Diaz 15-10420
## Nathalia Silvera 12-10921


Willy* es un lenguaje con el cual es posible dar instrucciones a Willy, 
que es un robot y para que este pueda trabajar libremente en una cuadrícula 
(dejar y poner objetos), es controlado por un programa que contiene las 
definiciones del mundo, de los procedimientos y el programa principal.


Para interpretar las acciones/ movimientos de Willy implementamos 
un proyecto el cual se divide en 3 etapas


#Etapa II: Analisis sintactico y construccion de arbol abstracto.
Etapa que consiste en obtener la representacion abstracta del mundo,
y los programas para Willy. Ademas en este proceso se detectan los
errores en la especificacion del mundo.

#**Diseno** 
Se realizaron los sigueintes archivos: Expresiones.hs, SymbolTable.hs
PrintTools.hs, ContextAnalyzer.hs, Parser.y y Willy.hs.


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
y si existe pasa a leer su contenido y los procesa con los tokens (usando 
con Lexer.x), si el resultado del lexer tiene errores (verificado con la 
funcion displayTokens) retorna un error del lexer, de lo contrario el 
programa continua la ejecucion para realizar la impresion correspodiente.


**Ejecucion:** Ejecutar el comando "Make" en la carperta principal,
luego debe usar el ejecutable Willy + el nombre del archivo.

