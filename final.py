# DONGMIN WU
# 110006921
import sys
names = {}
class Node:
    def __init__(self):
        print("init node")

    def evaluate(self):
        return 0

    def execute(self):
        return 0

class NumberNode(Node):

    def __init__(self, v):
        if('.' in v):
            self.value = float(v)
        else:
            self.value = int(v)

    def evaluate(self):
        return self.value

class ParenthesisNode(Node):

    def __init__(self,v):
        self.value = v

    def evaluate(self):
        return self.value.evaluate()

class StringNode(Node):

    def __init__(self, v):
        self.value = str(v)

    def evaluate(self):
        return self.value

class BooleanNode(Node):

    def __init__(self, v):
        if (v == 'False'):
            self.value = False
        elif (v == 'True'):
            self.value = True

    def evaluate(self):
        return self.value 


class IndexNode(Node):

    def __init__(self,input_list,indices):
        self.input_list = input_list
        self.indices = indices

    def evaluate(self):
        if(isinstance(self.input_list, VarNode) == True):
            temp = names[self.input_list.evaluate()]
            indices_list = self.indices.evaluate()
            for item in indices_list:
                temp = temp[item]
            return temp
        else:
            temp = self.input_list.evaluate()
            indices_list = self.indices.evaluate()
            for item in indices_list:
                temp = temp[item]
            return temp

        


class VarNode(Node):
    # construct a variable name for the variable
    def __init__(self, v):
       self.value = v

    def evaluate(self):
        return self.value

class ListNode(Node):

    def __init__(self, s):
        self.value = [s]

    def evaluate(self):
        empty_list = []
        for item in self.value:
            empty_list.append(item.evaluate())
        return empty_list

class ComparisonNode(Node):

    def __init__(self, op, v1, v2):
        self.op = op
        self.v1 = v1
        self.v2 = v2

    def evaluate(self):

        if(isinstance(self.v1, VarNode) == True):
            if(isinstance(self.v2, VarNode) == True):
                if (self.op == '<'):
                    return names[self.v1.evaluate()] < names[self.v2.evaluate()]
                elif (self.op == '>'):
                     return names[self.v1.evaluate()] > names[self.v2.evaluate()]
                elif (self.op == '<='):
                     return names[self.v1.evaluate()] <= names[self.v2.evaluate()]
                elif (self.op == '>='):
                     return names[self.v1.evaluate()] >= names[self.v2.evaluate()]
                elif (self.op == '=='):
                     return names[self.v1.evaluate()] == names[self.v2.evaluate()]
                elif (self.op == '<>'):
                     return names[self.v1.evaluate()] <> names[self.v2.evaluate()]
            else:
                if (self.op == '<'):
                    return names[self.v1.evaluate()] < self.v2.evaluate()
                elif (self.op == '>'):
                     return names[self.v1.evaluate()] > self.v2.evaluate()
                elif (self.op == '<='):
                     return names[self.v1.evaluate()] <= self.v2.evaluate()
                elif (self.op == '>='):
                     return names[self.v1.evaluate()] >= self.v2.evaluate()
                elif (self.op == '=='):
                     return names[self.v1.evaluate()] == self.v2.evaluate()
                elif (self.op == '<>'):
                     return names[self.v1.evaluate()] <> self.v2.evaluate()
        else:
            if(self.op == '<'):
                return self.v1.evaluate() < self.v2.evaluate()
            elif (self.op == '>'):
                return self.v1.evaluate() > self.v2.evaluate()
            elif (self.op == '<='):
                return self.v1.evaluate() <= self.v2.evaluate()
            elif (self.op == '>='):
                return self.v1.evaluate() >= self.v2.evaluate()
            elif (self.op == '=='):
                return self.v1.evaluate() == self.v2.evaluate()
            elif (self.op == '<>'):
                return self.v1.evaluate() <> self.v2.evaluate()

class BopNode(Node):
    def __init__(self, op, v1, v2):
        self.v1 = v1
        self.v2 = v2
        self.op = op

    def evaluate(self):
        
        try:
            if(isinstance(self.v1, VarNode) == True):
                if(isinstance(self.v2, VarNode) == True):
                    if (self.op == '+'):
                        return names[self.v1.evaluate()] + names[self.v2.evaluate()]
                    elif (self.op == '-'):
                         return names[self.v1.evaluate()] - names[self.v2.evaluate()]
                    elif (self.op == '**'):
                         return names[self.v1.evaluate()] ** names[self.v2.evaluate()]
                    elif (self.op == '*'):
                         return names[self.v1.evaluate()] * names[self.v2.evaluate()]
                    elif (self.op == '//'):
                         return names[self.v1.evaluate()] // names[self.v2.evaluate()]
                    elif (self.op == '/'):
                         return names[self.v1.evaluate()] / names[self.v2.evaluate()]
                    elif (self.op == '%'):
                         return names[self.v1.evaluate()] % names[self.v2.evaluate()]
                else:
                    if(self.op == '+'):
                        return names[self.v1.evaluate()] + self.v2.evaluate()
                    elif (self.op == '-'):
                         return names[self.v1.evaluate()] - self.v2.evaluate()
                    elif (self.op == '**'):
                         return names[self.v1.evaluate()] ** self.v2.evaluate()
                    elif (self.op == '*'):
                         return names[self.v1.evaluate()] * self.v2.evaluate()
                    elif (self.op == '//'):
                         return names[self.v1.evaluate()] // self.v2.evaluate()
                    elif (self.op == '/'):
                         return names[self.v1.evaluate()] / self.v2.evaluate()
                    elif (self.op == '%'):
                         return names[self.v1.evaluate()] % self.v2.evaluate()
            else:
                if (self.op == '+'):
                    return self.v1.evaluate() + self.v2.evaluate()
                elif (self.op == '-'):
                    return self.v1.evaluate() - self.v2.evaluate()
                elif (self.op == '**'):
                    return self.v1.evaluate() ** self.v2.evaluate()
                elif (self.op == '*'):
                    return self.v1.evaluate() * self.v2.evaluate()
                elif (self.op == '//'):
                    return self.v1.evaluate() // self.v2.evaluate()
                elif (self.op == '/'):
                    return self.v1.evaluate() / self.v2.evaluate()
                elif (self.op == '%'):
                    return self.v1.evaluate() % self.v2.evaluate()
        except TypeError:
            return "SEMANTICS ERROR"

class AndOrComparisonNode(Node):
    def __init__(self,comp,v1,v2):
        self.v1 = v1
        self.v2 = v2
        self.comp = comp

    def evaluate(self):
        if(self.comp == 'and'):
            return self.v1.evaluate() and self.v2.evaluate()
        elif(self.comp == 'or'):
            return self.v1.evaluate() or self.v2.evaluate()


class InNode(Node):

    def __init__(self,v1,v2):
        self.v1 = v1
        self.v2 = v2

    def evaluate(self):
        if(isinstance(self.v2, VarNode) == True):
            return self.v1.evaluate() in names[self.v2.evaluate()]
        else:
            return self.v1.evaluate() in self.v2.evaluate()

class NotComparisonNode(Node):
    def __init__(self, v1):
        self.v1 = v1

    def evaluate(self):
        return not self.v1.evaluate()


class PrintNode(Node):
    def __init__(self, v):
        self.value = v

    def execute(self):
        if(isinstance(self.value, VarNode) == True):
            print(names[self.value.evaluate()])
        else:
            self.value = self.value.evaluate()
            print(self.value)

class AssignNode(Node):
    def __init__(self, name, v):
        self.name = name
        self.v = v

    def execute(self):
        if(isinstance(self.v , VarNode) == True):
            names[self.name.evaluate()] = names[self.v.evaluate()]
        else:
            names[self.name.evaluate()] = self.v.evaluate()

class BlockNode(Node):
    def __init__(self, s):
        self.sl = [s]

    def execute(self):
        for statement in self.sl:
            if statement != None:
                statement.execute()

class IfNode(Node):
    def __init__(self,condition, block):
        self.condition = condition
        self.block = block

    def execute(self):
        if(self.condition.evaluate() == True):
            self.block.execute()

class IfElseNode(Node):
    def __init__(self,condition,then_block,else_block):
        self.condition = condition
        self.then_block = then_block
        self.else_block = else_block

    def execute(self):
        if(self.condition.evaluate() == True):
            self.then_block.execute()
        else:
            self.else_block.execute()

class WhileNode(Node):

    def __init__(self, condition, block):
        self.condition = condition
        self.block = block
        

    def execute(self):
        stop_condition = not self.condition.evaluate()
        while(self.condition.evaluate() != stop_condition):
            self.block.execute()

           

tokens = (
    'LBRACE', 'RBRACE',
    'PRINT','LPAREN', 'RPAREN', 'SEMI', 'LESS', 'GREATER' ,'GREATERT' ,'LESST', 'EQUALCOM', 'NOTEQUAL',
    'NUMBER','POWER', 'FLOORDIVISION', 'MOD', 'BOOLEAN', 'STRING','AND','OR','NOT','IN', 'NAME', 'EQUAL',
    'PLUS','MINUS','TIMES','DIVIDE', 'LBRACKET', 'RBRACKET', 'COMMA','IF','ELSE', 'WHILE',
    )

# Tokens
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_COMMA = r'\,'
t_EQUALCOM = r'\=\='
t_EQUAL = r'\='
t_NOTEQUAL = r'\<\>'
t_MOD = r'\%'
t_LESST = r'\<\='
t_LESS = r'\<'
t_GREATERT = r'\>\='
t_GREATER = r'\>'
t_LBRACE  = r'\{'
t_RBRACE  = r'\}'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_SEMI  = r';'
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_POWER = r'\*\*'
t_TIMES   = r'\*'
t_FLOORDIVISION = r'\/\/'
t_DIVIDE  = r'/'


def t_PRINT(t):
    r'(print)'
    t.value = str('print')
    return t

def t_NUMBER(t):
    r'\-?\d*(\d\.|\.\d)\d* | \d+'
    try:
        t.value = NumberNode(t.value)
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t

def t_BOOLEAN(t):
    r'(True)|(False)'
    t.value = BooleanNode(t.value)
    return t

def t_AND(t):
    r'(and)'
    t.value = 'and'
    return t

def t_OR(t):
    r'(or)'
    t.value = 'or'
    return t

def t_NOT(t):
    r'(not)'
    t.value = 'or'
    return t

def t_IN(t):
    r'(in)'
    t.value = 'in'
    return t

def t_IF(t):
    r'(if)'
    t.value = 'if'
    return t

def t_ELSE(t):
    r'(else)'
    t.value = 'else'
    return t

def t_WHILE(t):
    r'(while)'
    t.value = 'while'
    return t

def t_STRING(t):
    r'\"([^\"\\])*\"|\'([^\'\\])*\''
    t.value = StringNode(t.value[1:-1])
    return t 

def t_NAME(t):
   r'[a-zA-Z_][a-zA-Z0-9_]*'
   t.value = VarNode(t.value)
   return t

# Ignored characters
t_ignore = ' \t'

def t_error(t):
    print("Syntax error at '%s'" % t.value)
    
# Build the lexer
import ply.lex as lex
lex.lex()

# Parsing rules
precedence = (
    ('left','OR'),
    ('left', 'AND'),
    ('left', 'NOT'),
    ('left', 'LESS','LESST','EQUALCOM','NOTEQUAL','GREATER','GREATERT'),
    ('left', 'IN'),
    ('left','PLUS','MINUS'),
    ('left','FLOORDIVISION'),
    ('left','MOD'),
    ('left','TIMES','DIVIDE'),
    ('right', 'POWER'),
    ('left','INDEX'),
    ('left','PARENTHESIS'),
    )
def p_block(t):
    """
    block : LBRACE inblock RBRACE
    """
    t[0] = t[2]

def p_block2(t):
    """
    empty_block : LBRACE RBRACE
    """
    t[0] = None

def p_inblock(t):
    """
    inblock : smt inblock
    """
    t[0] = t[2]
    t[0].sl.insert(0,t[1])

def p_inblock2(t):
    """
    inblock : smt
    """
    t[0] = BlockNode(t[1])
    
def p_smt(t):
    """
    smt : print_smt
        | assign_smt
        | if_smt
        | if_else_smt
        | while_smt
        | block
        | empty_block
    """
    t[0] = t[1]

def p_print_smt(t):
    """
    print_smt : PRINT LPAREN expression RPAREN SEMI
    """
    t[0] = PrintNode(t[3])

def p_assign_smt(t):
    """
    assign_smt : NAME EQUAL expression SEMI
    """
    t[0] = AssignNode(t[1], t[3])

def p_if_statement(t):
    """
    if_smt : IF LPAREN expression RPAREN LBRACE inblock RBRACE
    """
    t[0] = IfNode(t[3], t[6])

def p_if_else_statement(t):
    '''
    if_else_smt : IF LPAREN expression RPAREN LBRACE inblock RBRACE ELSE LBRACE inblock RBRACE
    '''
    t[0] = IfElseNode(t[3],t[6],t[10])

def p_while_statement(t):
    '''
    while_smt : WHILE LPAREN expression RPAREN LBRACE inblock RBRACE
    '''
    t[0] = WhileNode(t[3], t[6])

def p_expression(t):
    '''
    expression : parenthesis_expression %prec PARENTHESIS
                | binary_op_expression
                | compare_expression
                | and_or_expression
                | not_expression
                | list_expression
                | index_expression
                | in_expression
                | 
    '''
    t[0] = t[1]

def p_parenthesis_expression(t):
    '''
    parenthesis_expression : LPAREN parenthesis_content RPAREN
    '''
    t[0] = t[2]

def p_parenthesis_content(t):

    'parenthesis_content : expression'

    t[0] = ParenthesisNode(t[1])

def p_index_expression(t):
    '''
    index_expression : list_expression index %prec INDEX
                    |  STRING index %prec INDEX
                    |  NAME index %prec INDEX
    '''
    t[0] = IndexNode(t[1],t[2])

def p_index(t):
    '''
    index : LBRACKET NUMBER RBRACKET index
    '''
    t[0] = t[4]
    t[0].value.insert(0,t[2])

def p_index2(t):
    '''
    index : LBRACKET NUMBER RBRACKET 
    '''
    t[0] = ListNode(t[2])

def p_list_expression(t):
    '''
    list_expression : LBRACKET list_content RBRACKET
    '''
    t[0] = t[2]

def p_list_content_expression(t):
    '''
    list_content : expression COMMA list_content
    '''
    t[0] = t[3]
    t[0].value.insert(0,t[1])

def p_list_content_2(t):
    '''
    list_content : expression
    '''
    t[0] = ListNode(t[1])

def p_expression_binop(t):
    '''binary_op_expression : expression PLUS expression
                  | expression MINUS expression
                  | expression POWER expression
                  | expression TIMES expression
                  | expression MOD expression
                  | expression FLOORDIVISION expression
                  | expression DIVIDE expression'''

    t[0] = BopNode(t[2], t[1], t[3])

def p_comparison_expression(t):
    """
    compare_expression : expression LESS expression
                        | expression GREATER expression
                        | expression LESST expression
                        | expression GREATERT expression
                        | expression EQUALCOM expression
                        | expression NOTEQUAL expression
    """
    t[0] = ComparisonNode(t[2], t[1], t[3])

def p_and_or_comparison_expression(t):

    """
    and_or_expression : expression AND expression
                        | expression OR expression
    """
    t[0] = AndOrComparisonNode(t[2], t[1], t[3])

def p_not_expression(t):
    """
    not_expression : NOT expression
    """
    t[0] = NotComparisonNode(t[2])

def p_in_expression(t):
    """
    in_expression : expression IN expression 

    """
    t[0] = InNode(t[1], t[3])

def p_expression_factor(t):
    '''expression : factor
                  | literal
                  | boolean
                  | variable
                   '''
    t[0] = t[1]

def p_factor_number(t):
    'factor : NUMBER'
    t[0] = t[1]

def p_literal_string(t):
    'literal : STRING'
    t[0] = t[1]

def p_boolean(t):
    'boolean : BOOLEAN'
    t[0] = t[1]

def p_variable(t):
    'variable : NAME'
    t[0] = t[1]

def p_error(t):
    print("Syntax error at '%s'" % t.value)

import ply.yacc as yacc
yacc.yacc()
fd = open(sys.argv[1], 'r')
code = ""
for line in fd:
    code += line.strip()

#lex.input(code)
#while True:
#    token = lex.token()
#    if not token: break
#    print(token)
ast = yacc.parse(code)
ast.execute()


