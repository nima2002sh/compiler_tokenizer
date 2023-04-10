import ply.lex as lex

class tokenizer:

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
        'null' : 'NULL'
    }

    literals = [ '[', ']', '?', ':', '+', '-', '*', '/', '%', '<', '>', '==', '>=', '<=', '!=', '||', '&&', '!', '(', ')', ';', '#']

    tokens = ( 'NUMBER', 'ID')

    def t_NUMBER(t):
        r'\d+'
        t.value = int(t.value)
        return t
    
    def t_ID(t):
        r'[a-zA-Z_][a-zA-Z_0-9]*'
        t.type = reserved.get(t.value,'ID')    # Check for reserved words
        return t

    def t_newline(t):
        r'\n+'
        t.lexer.lineno += len(t.value)
    
    def t_COMMENT(t):
        r'\#.*'
        pass
    