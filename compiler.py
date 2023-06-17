import ply.lex as lex
import ply.yacc as yacc

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
        
    def __init__(self, name, returnType, parametrs, isFunction, parametersNum, isAssignd = False, isDefined = True):
        self.name = name
        self.returnType = returnType
        self.parametrs = parametrs
        self.isFunction = isFunction
        self.parametersNum = parametersNum
        self.isAssignd = isAssignd
        self.isDefined = isDefined

functionName = ['scan', 'print', 'list', 'length', 'exit']
info = {}
allIden = []
definedIden = []
allReturnType = []
cerFunction = ''
cerReturnType = ''

tScan = Table('scan', 'INT', [], True, 0)
info['scan'] = tScan

tPrint =  Table('print', 'INT', [], True, 0)
info['print'] = tPrint

tList = Table('list', 'VECTOR', ['INT'], True, 1)
info['list'] = tList

tLength = Table('length', 'INT', ['VECTOR'], True, 1)
info['length'] = tLength

tExit = Table('exit', 'NULL', ['INT'], True, 1)
info['exit'] = tExit

def checkDefined(t, line):
    if t in allIden:
        if not t in definedIden:
            print('Error in line', line, ':\n  ', t, 'is not defined!')
            return True
    return False

def checkFunction(t):
    if t in functionName:
        print('Function', t, 'already exist!')
    else:
        functionName.append(t)
        
def checkReturnType(t):
    if isinstance(t, int):
        return 'INT'
    if t in definedIden:
        return info[t].returnType

precedence = (
    ('left', '!', 'and', 'or', 'LEQ', 'HEQ', 'NOTEQ', 'EQUALS', '<', '>'),
    ('left', '=', ':'),
    ('left', '+', '-'),
    ('left', '*', '/'),
    ('left', '(', ')', '[', ']', '{', '}')
)

def p_prog(p):
    """prog : func prog"""
            
    if len(p) == 3:
        p[0] = [p[1], p[2]]

def p_func(p):
    """func : DEF type iden ( flist ) { body }
            | DEF type iden ( flist ) RETURN expr ;"""
    
    if p[7] == 'RETURN':
        checkFunction(p[3])

        fanction = Table(p[3], p[2], p[5], True, len(p[5]))
        info[p[3]] = fanction
        definedIden.append(p[3])
        
        if p[7] != cerReturnType:
            print(p, cerReturnType)
            print('Error in line', p.lineno(2), ': return type at function', p[2], "does not match")

        p[0] = [p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9]]
        
    else:
        checkFunction(p[3])

        fanction = Table(p[3], p[2], p[5], True, len(p[5]))
        info[p[3]] = fanction
        definedIden.append(p[3])
        
        if p[7] != cerReturnType:
            print(p, cerReturnType)
            print('Error in line', p.lineno(2), ': return type at function', p[2], "does not match")

        p[0] = [p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9]]
    
def p_body(p):
    """body : stmt body"""
    if len(p) == 3:
        p[0] = [p[1], p[2]]

def p_stmt(p):
    """stmt : expr ;
            | defvar ;
            | IF ( expr ) stmt
            | IF ( expr ) stmt ELSE stmt
            | WHILE ( expr ) stmt
            | FOR ( iden = expr TO expr ) stmt 
            | RETURN expr ;
            | { body }
            | func"""
    
    if len(p) == 2:     # func
        p[0] = p[1]
        
    if len(p) == 3:     # expr ; | defvar ;
        p[0] = p[1]
        
    if len(p) == 4:     # RETURN expr ; | { body }
        if p[1] == 'RETURN':   
            global cerReturnType
            cerReturnType = whatType(p[2])
        p[0] = p[2]
        
    if len(p) == 6:     # IF ( expr ) stmt | WHILE ( expr ) stmt
        if p[1] == 'IF':
            if p[3]:
                p[0] = p[5]
        else:
            while p[3]:
                p[0] = p[5]
                
    if len(p) == 8:     # IF ( expr ) stmt ELSE stmt
        if p[3]:
            p[0] = p[5]
        else:
            p[0] = p[7]
            
    if len(p) == 10:     # FOR ( iden = expr TO expr ) stmt 
        definedIden.append(p[3])
        p[0] = [p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9]]

def p_defvar(p):
    """defvar : VAR type iden
              | VAR type iden = expr"""

    p[0] = [p[2], p[3]]
    
    if p[2] not in ['INT', 'VECTOR', 'STR', 'NULL']:
        print('Error in line ', p.lineno(1), ': wrong type ')
        
    if len(p) == 4:
        info[p[3]] = Table(p[3], p[2], [], False, 0, False, False)
        allIden.append(p[3])

    else:
        info[p[3]] = Table(p[3], p[2], [], False, 0)
        allIden.append(p[3])
        definedIden.append(p[3])

def p_flist(p):
    """flist :
            | type iden
            | type iden , flist"""
            
    if len(p) == 1:
        p[0] = []
        
    elif len(p) == 3:
        definedIden.append(p[2])
        idenInfo[p[2]] = Table(p[2], p[1], [], False, 0, True, False)
        p[0] = [(p[1], p[2])]
        
    else:
        definedIden.append(p[2])
        denInfo[p[2]] = Table(p[2], p[1], [], False, 0, True, False)
        p[0] = [(p[1], p[2])] + p[4]

def p_clist(p):
    """clist :
            | expr
            | expr , clist"""

    if len(p) == 1:
        p[0] = []
        
    elif len(p) == 2:
        p[0] = [p[1]]

    else:
        p[0] = [p[1]] + p[3]

def p_type(p):
    """type : INT
            | VECTOR
            | STR
            | NULL"""
    p[0] = p[1]

def p_expr(p):
    """expr :=
            | expr [ expr] 
            | [ clist ] 
            | expr ? expr : expr 
            | expr + expr 
            | expr - expr 
            | expr * expr 
            | expr / expr 
            | expr % expr 
            | expr > expr 
            | expr < expr 
            | expr == expr 
            | expr >= expr 
            | expr <= expr 
            | expr != expr 
            | expr || expr 
            | expr && expr 
            | ! expr 
            | + expr 
            | - expr 
            | iden 
            | iden = expr 
            | iden ( clist ) 
            | NUMBER 
            | STR """
            
    if len(p) == 6:     # expr ? expr : expr
        if p[1]:
            p[0] = p[3]
        else:
            p[0] = p[5]
            
    if len(p) == 5:
        
        if p[2] == '(':     # iden ( clist )
            
            if not p[1] in functionName:
                print('Error in line', p.lineno(1), ': dont have such a function!')
                
            else:
                fanction = info[p[1]]
                
                if not fanction.parametersNum == len(p[3]):
                    print('Error in line', p.lineno(1), ': function', p[1], ' expects' ,info[p[1]].parametersNum, ' parameter but its given ', len(p[3]))
                    
                else:
                    parm_list = info[p[1]].parametrs
                    
                    for i in range(len(parm_list)):
                        if checkReturnType(p[3][i]) != parm_list[j]  :
                                print('Error in line' ,p.lineno(1) ,': wrong type for argument ', j+1, ' of function' ,p[1])
                                
                p[0] = p[1]
                
        elif p[2] == '[':       # expr [ expr ] 
            p[0] = p[3]
            
    if len(p) == 4:     
        if p[1] == '(':     # ( exp )
            p[0] = p[2]
        else:       # expr sign expr -> sign = {=,-,+,*,/,>,<,==,<=,>=,!=,||,&&}
            p[0] = [p[1], p[2], p[3]]
            if p[2] == '=' :
                info.get(p[1]).isAssign = True

    if len(p) == 3:     # ! expr | - expr | + expr
        p[0] = [p[1], p[2]]

    if len(p) == 2:     # iden | NUMBER | STR
        p[0] = p[1]
        if checkReturnType(p[1]) != 'NUMBER' and checkReturnType(p[1]) != 'STR':
            checkDefined(p[1], p.lineno(1))
    if not info.get(p[1]).isAssignd and not info.get(p[1]).isFunction:
        print('Error in Line ',p.lineno(1) , ':  Variable ', p[1] ,'is used before being assigned')

def p_iden(p):
    """iden : IDEN"""
    p[0] = p[1]
    allIden.append(p[0])


def p_number(p):
    """ num : NUMBER """
    p[0] = p[1]
    allIden.append(p[0])

def p_string(p):
    """ str : STR """
    p[0] = p[1]
    allIden.append(p[0])

def p_error(p):
    if p:
        print("Syntax error at token", p.type)
        print("Syntax error at '%s'" % p.value)
        print("line : '%s'" % p.lineno)
        print(p)
        parser.errok()
    else:
        print("Syntax error at EOF")


parser = yacc.yacc(debug=True)
parser.parse(tracking=True)