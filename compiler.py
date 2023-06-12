import ply.lex as lex

class tokenizer(object):

    reserved = {
        'def' : 'DEF',
        'var' : 'VAR',
        'int' : 'INT',
        'vector' : 'VECTOR',
        'str' : 'STR',
        'for' : 'FOR',
        'to' : 'TO',
        'length' : 'LENGTH',
        'return' : 'RETURN',
        'while' : 'WHILE',
        'if' : 'IF',
        'else' : 'ELSE',
        'scan' : 'SCAN',
        'print' : 'PRINT',
        'list' : 'LIST',
        'exit' : 'EXIT',
        'null' : 'NULL',
    }

    literals = [ '[', ']', '?', ':', '+', '-', '*', '/', '%', '<', '>', '!', '(', ')', ';', '#', '=']

    tokens = ['NUMBER', 'STRING', 'ID', 'COMMENT', 'EQUALS', 'LEQ', 'HEQ', 'NOTEQ', 'OR', 'AND']+ list(reserved.values())

    t_EQUALS = r'=='
    t_LEQ = r'<='
    t_HEQ = r'>='
    t_NOTEQ = r'!='
    t_OR = r'\|\|'
    t_AND = r'&&'


    def t_NUMBER(self, t):
        r'[0-9]+'
        t.value = int(t.value)
        return t
    
    def t_ID(self, t):
        r'[a-zA-Z_][a-zA-Z_0-9]*'
        t.type = self.reserved.get(t.value,'ID') 
        return t

    t_STRING = r'(\'[^\'\r\n]*\')|("[^"\r\n]*")'

    
    t_ignore_COMMENT = r'\#.*'
    
    t_ignore  = ' \t'

    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += len(t.value)

    def t_error(self, t):
        print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    def build(self,**kwargs):
        self.lexer = lex.lex(module=self, **kwargs)

    def test(self, data):
        self.lexer.input(data)
        while True:
             tok = self.lexer.token()
             if not tok: 
                 break
             print(tok)

t = tokenizer()
t.build()
data = open('input.txt', 'r')
t.test(data.read())


class Table:
    
    parametersList = []
        
    def __init__(self, name, type, isFunction, parametersNum, isAssignd = False, isDefined = True):
        self.name = name
        self.type = type
        self.isFunction = isFunction
        self.parametersNum = parametersNum
        self.isAssignd = isAssignd
        self.isDefined = isDefined
        