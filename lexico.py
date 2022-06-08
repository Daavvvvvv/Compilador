import ply.lex as ply
#se importa la libreria ply y la nombramos como ply

#resultado del analisis, que se quedara en una lista
resul_lexico= []
#declaramos nuestras palabras reservadas
reservada= (
    'INCLUDE',
    'USING',
    'NAMESPACE',
    'STD',
    'COUT',
    'CIN',
    'GET',
    'CADENA',
    'RETURN',
    'VOID',
    'INT',
    'ENDL',
)
#Declaramos nuestros tokens
tokens= reservada +(
    'IDENTIFICADOR',
    'ENTERO',
    'ASIGNAR',

    'SUMA',
    'RESTA',
    'MULT',
    'DIV',
    'POTENCIA',
    'MODULO',

    'MINUSMINUS',
    'PLUSPLUS',
    'SI',
    'SINO',
    'MIENTRAS',
    'PARA',
    'AND',
    'OR',
    'NOT',
    'MENORQUE',
    'MENORIGUAL',
    'MAYORQUE',
    'MAYORIGUAL',
    'IGUAL',
    'DISTINTO',
    'NUMERAL',

    'PARIZQ',
    'PARDER',
    'CORIZQ',
    'CORDER',
    'LLAIZQ',
    'LLADER',

    #Otros
    'PUNTOCOMA',
    'COMA',
    'COMDOB',
    'MAYORDER', # >>
    'MAYORIZQ', # <<
)

#Reglas de expresiones regulares para token de contexto simple
#Se define todos y cada uno de los tokens

t_SUMA= r'\+'
t_RESTA= r'-'
t_MINUSMINUS= r'\-\-'

t_MULT= r'\*'
t_DIV= r'/'
t_MODULO= r'\%'
t_POTENCIA=r'(\*(2) | \^)'

t_ASIGNAR= r'='
# Expresiones Logicas
t_AND= r'\&\&'
t_OR= r'\|{2}'
t_NOT= r'\!'
t_MENORQUE=r'<'
t_MAYORQUE=r'>'
t_PUNTOCOMA=r';'
t_COMA=r','
t_PARIZQ=r'\('
t_PARDER=r'\)'
t_CORIZQ=r'\['
t_CORDER=r'\]'
t_LLAIZQ=r'{'
t_LLADER=r'}'
t_COMDOB=r'\"'

#se define las reservadas de las funciones que faltan

def t_INCLUDE(t):
    r'include'
    return t

def t_USING(t):
    r'using'
    return t

def t_NAMESPACE(t):
    r'namespace'
    return t

def t_STD(t):
    r'std'
    return t

def t_COUT(t):
    r'cout'
    return t

def t_CIN(t):
    r'cin'
    return t

def t_GET(t):
    r'get'
    return t

def t_ENDL(t):
    r'endl'
    return t

def t_SINO(t):
    r'else'
    return t

def t_SI(t):
    r'else'
    return t

def t_RETURN(t):
    r'return'
    return t

def t_VOID(t):
    r'void'
    return t

def t_MIENTRAS(t):
    r'while'
    return t

def t_PARA(t):
    r'for'
    return t

#Se define los numeros enteros
def t_ENTERO(t):
    r'\d+'
    t.value= int(t.value)
    return t

def t_CADENA(t):
    r'\"?(w+ \ *\w*\d* \ *)\"?'
    return t

def t_NUMERAL(t):
    r'\#'
    return t

def t_PLUSPLUS(t):
    r'\+\+'
    return t

def t_MENORIGUAL(t):
    r'<='
    return t

def t_MAYORIGUAL(t):
    r'>='
    return t

def t_IGUAL(t):
    r'=='
    return t

def t_MAYORDER(t):
    r'<<'
    return t

def t_MAYORIZQ(t):
    r'>>'
    return t

def t_DISTINITO(t):
    r'!='
    return t

#Cuantos saltos de linea hay en el programa
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_comments(t):
    r'/\*(.|\n)*?\*/'
    t.lexer.lineno += t.value.count('\n')
    print("Comentario de multiple linea")

def t_comments_ONELine(t):
    r'\/\/(.)*\n'
    t.lexer.lineno += 1
    print("Comentario de una linea")

t_ignore = '\t'

#Se define el error si un token es invalido sino esta reconocido en el lenguaje
def t_error(t):
    global resul_lexico
    cadena_final= "\n** Token no valido en la linea {:4}Valor{:16}Posicion{:4}".format(str(t.lineno), str(t.value),str(t.lexpos))
    
    resul_lexico.append(cadena_final)
    t.lexer.skip(1)

def ingreso_info(datos_ingresados):
    global resul_lexico
    #Se crea para ingresar el metodo lex que se creo inicialmente

    analizador= ply.lex()
    #Se crea la entrada de los tokens al analizador
    analizador.input(datos_ingresados)

    resul_lexico.clear()
    #Se creara el ciclo
    while True:
        tok= analizador.token()
        #que si no encuentra un token se detenga
        if not tok:
            break
        #en caso contrario imprima el token
        cadena_final= "Linea: {:4} Tipo: {:16} Valor: {:16} Posicion {:4}".format(str(tok.lineno), str(tok.type), str(tok.value), str(tok.lexpos))
        
        resul_lexico.append(cadena_final)
    return resul_lexico

#instanciamos el analizador lexico gracias a la ayuda de la libreria PLY
analizador= ply.lex()

#se crea un ciclo
if __name__ =='__main__':
    while True:
        #donde la entrada sera los datos que ingresemos
        datos_ingresados= input("ingrese:\n ")
        ingreso_info(datos_ingresados)
        print(resul_lexico)



