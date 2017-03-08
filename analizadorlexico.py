#!/usr/bin/env python
# -*- coding: utf-8 -*-
# ------------------------------------------------------------
# ANALIZADOR LEXICO
# ------------------------------------------------------------

import ply.lex as lex
import re
import codecs
import os
import sys







#Palabras reservadas del lenguaje java
reservadas = [
'abstract',
'assert',
'boolean',
'break',
'byte',
'case',
'catch',
'char',
'class',
'const',  
'continue',
'default',
'do',
'double',
'else',
'enum',
'extends',
'final',
'finally',
'float',
'for',
'goto',
'if',
'implements',
'import',
'instanceof',
'int',
'interface',
'long',
'native',
'new',
'package',
'private',
'protected',
'public',
'return',
'short',
'static',
'strictfp',
'super',
'switch',
'synchronized',
'this',
'throw',
'throws',
'transient',
'try',
'void',
'volatile',
'while',
'string',
'length',
'true',
'false',
'null',
]



# LISTA DE TONKENS
tokens = reservadas+[
   #OPERADORES DE COMPARACION DE IGUALDAD
   'IGUALDAD',
   'DIFERENTE',

   #BULEANOS
   'Y',
   'O',

   #OPERADORES DE COMPARACION
   'MENORQUE',
   'MAYORQUE',
   'MENORIGUAL',
   'MAYORIGUAL',

   #OPERADORES ARITMETICOS
   'MAS',
   'MENOS',
   'MULTIPLICACION',
   'DIVISION',
   'MODULO',
   'POTENCIA',

   #SIMBOLOS
   'IGUAL',
   'PARENTIZQUIERDO',
   'PARENTDERECHO',
   'CORCHETEIZQUIERDO',
   'CORCHETEDERECHO',
   'LLAVEIZQUIERDA',
   'LLAVEDERECHA',
   'PUNTO',
   'COMIILLASENCILLA',
   'COMILLASDOBLES',
   'PUNTOYCOMA',
   'COMA',
   'ADMIRACION',

   #COMENTARIOS
   'COMENTARIOSIMPLE',
   'COMENTARIOMULTIPLE',
   'COMENTARIOERROR',

   #CONSTANTES
   'NUMERO',
   'BINARIO',
   'ENTERO',
   'FLOTANTE',

   #CARACTERES
   'CADENA',
   'CADENAERROR',

   #IDENTIFICADOR
   'IDENTIFICADOR',

   #ESPECIALES Y VOCALES CON TILDE
   'ENYEMINUSCULA',
   'ENYEMINUSCULA',
   'AMINUSCULATILDE',
   'AMAYUSCULATILDE',
   'EMINUSCULATILDE',
   'EMAYUSCULATILDE',
   'IMINUSCULATILDE',
   'IMAYUSCULATILDE',
   'OMINUSCULATILDE',
   'OMAYUSCULATILDE',
   'UMINUSCULATILDE',
   'UMAYUSCULATILDE',
   
   ]
#Reglas de expresión regular para símbolos simples

#OPERADORES DE COMPARACION DE IGUALDAD
def t_IGUALDAD(t):
	r'=='
	return t

t_DIFERENTE = r'!='

#BULEANOS
t_Y = r'&&'
t_O = r'\|\|'

#OPERADORES DE COMPARACION
t_MENORQUE = r'\<'
t_MAYORQUE = r'\>'
t_MENORIGUAL = r'\<='
t_MAYORIGUAL = r'\>='

#OPERADORES ARITMETICOS
t_MAS = r'\+'
t_MENOS = r'-'
t_MULTIPLICACION = r'\*'
t_DIVISION = r'/'
t_MODULO = r'%'
t_POTENCIA = r'\^'

#SIMBOLOS
t_IGUAL = r'='
t_PARENTIZQUIERDO = r'\('
t_PARENTDERECHO = r'\)'
t_CORCHETEIZQUIERDO = r'\['
t_CORCHETEDERECHO = r'\]'
t_LLAVEIZQUIERDA = r'\{'
t_LLAVEDERECHA = r'\]'
t_PUNTO = r'\.'
t_COMIILLASENCILLA = r'\''
t_COMILLASDOBLES = r'\"'
t_PUNTOYCOMA = r'\;'
t_COMA = r'\,'
t_ADMIRACION = r'\!'

#COMENTARIOS
def t_COMENTARIOSIMPLE(t):
  r'\/\/.*'
  pass


def t_COMENTARIOMULTIPLE(t):
  r'\/\*'
  pass

def t_COMENTARIOERROR(t):
	r'\/\*.*'
	print 'Error, no cerro el comentario', t.value, 'en la linea ', t.lexer.lineno
	pass


#CONSTANTES
def t_NUMERO(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:


        print "El valor no es correcto %d", t.value
        t.value = 0
    return t


def t_BINARIO(t):
    r'b\'[10]+\''
    b = ""
    for i in t.value:
        if i == "0" or i == "1":
            b = b + str(i)
    decimal = int(str(b), 2)
    t.value = decimal
    return t


def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)    
    return t
            
    


def t_FLOTANTE(t):
    r'[+-]?\d+(\.)(\d+([eE][+-]?\d+)?)'
    return t

#CARACTERES
def t_CADENA(t):
    r'"(.|\n|\t)*"'
    return t


def t_CADENA_ERROR(t):
    r'"(.|\n|\t)*'
    print "Error, no cerro la cadena", t.value, ", en la linea ", t.lexer.lineno
    pass


#IDENTIFICADOR
t_IDENTIFICADOR  = r'[a-zA-Z][a-zA-Z0-9ñÑáÁéÉíÍóÓúÚ\.]*'



'''
-> CADENAS QUE CONTIENEN CARACTERES IGNORADOS (VOCALES CON TILDES Y ESPECIALES)
-> Un identificador no puede iniciar con numero ni con una ñ o las vocales tildadas
-> Un identificador puede contener la letra ñ y las vocales tildadas en mayusculas y minusculas
'''
#ESPECIALES Y VOCALES CON TILDE

t_ignore_ENYEMINUSCULA = r'ñ'
t_ignore_ENYEMAYUSCULA = r'Ñ'
t_ignore_AMINUSCULATILDE = r'á'
t_ignore_AMAYUSCULATILDE = r'Á'
t_ignore_EMINUSCULATILDE = r'é'
t_ignore_EMAYUSCULATILDE= r'É'
t_ignore_IMINUSCULATILDE = r'í'
t_ignore_IMAYUSCULATILDE= r'Í'
t_ignore_OMINUSCULATILDE = r'ó'
t_ignore_OMAYUSCULATILDE= r'Ó'
t_ignore_UMINUSCULATILDE = r'ú'
t_ignore_UMAYUSCULATILDE= r'Ú'

#CARACTERES QUE SE RECONOCEN Y SE IGNORAN
t_ignore  = ' \t'
  

# RECONOCE LOS SALTOS DE LINEA
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)




# MANEJO DE ERRORES
def t_error(t): 	
  print("No se reconoce el caracter '%s'" % t.value[0]),"en la linea ", t.lexer.lineno
  t.lexer.skip(1)
 

def p_expression_name(t):
    'expression : NAME'
    try:
        t[0] = names[t[1]]
    except LookupError:
        print("Nombre no definido '%s'" % t[1])
        t[0] = 0  
  

#Funcion para buscar las pruebas
def buscarFicheros(directorio):
	ficheros = []
	numeroArchivo = ''
	respuesta = False 
	contador = 1

	for base, dirs, files, in os.walk(directorio):
		ficheros.append(files)

	for file in files:
		print str(contador)+". "+file
		contador = contador+1

	while respuesta == False:
		numArchivo = raw_input('\nNumero del test: ')
		for file in files:
			if file == files[int(numArchivo)-1]:
				respuesta = True
				break

	print "Has escogido \"%s\"\n" %files[int(numArchivo)-1]
	return files[int(numArchivo)-1]			

		
		 




#pruebas
directorio = '/home/george/Escritorio/compiladores/primeraEntrega/'
archivo = buscarFicheros(directorio)
test = directorio+archivo
fp = codecs.open(test,"r","utf-8")
cadena = fp.read()
fp.close()




# CONSTRUYE EL ANALIZADOR
ANALIZADOR = lex.lex()



# Test it out
data = ''' vari9able x y z
3 + 4 * 10
  + -20 *2
'''

# Give the lexer some input
ANALIZADOR.input(cadena)

# Tokenize
while True:
    tok = ANALIZADOR.token()
    if not tok: 
        break      # No more input
    print(tok)
    
