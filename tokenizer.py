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

    literals = [ '[', ']', '?', ':', '+', '-', '*', '/', '%', '<', '>', '==', '>=', '<=', '!=', '||', '&&', '!', '(', ')', ';']

    tokens = ( 'NUMBER', 'STRING')