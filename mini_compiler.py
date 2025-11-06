#!/usr/bin/env python3
"""
mini_compiler.py
Mini Compiler using Python + PLY (Lex-Yacc)

Run:
    python3 src/mini_compiler.py                  # built-in example
    python3 src/mini_compiler.py examples/sample1.txt   # run on a file
"""

import sys
import ply.lex as lex
import ply.yacc as yacc

# =========================================================
# 1. TOKENS (LEXICAL ANALYZER)
# =========================================================
tokens = (
    'INT','IF','ELSE','WHILE',
    'ID','NUMBER',
    'PLUS','MINUS','TIMES','DIVIDE',
    'ASSIGN','SEMI','COMMA',
    'LPAREN','RPAREN','LBRACE','RBRACE',
    'GT','LT','GE','LE','EQ','NE'
)

reserved = {
    'int':'INT','if':'IF','else':'ELSE','while':'WHILE'
}

# Token patterns
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_ASSIGN  = r'='
t_SEMI    = r';'
t_COMMA   = r','
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LBRACE  = r'\{'
t_RBRACE  = r'\}'
t_GE      = r'>='
t_LE      = r'<='
t_EQ      = r'=='
t_NE      = r'!='
t_GT      = r'>'
t_LT      = r'<'

t_ignore = ' \t'

def t_ID(t):
    r'[A-Za-z_][A-Za-z0-9_]*'
    t.type = reserved.get(t.value, 'ID')
    return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_comment(t):
    r'//.*'
    pass

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f"Illegal character: {t.value[0]!r} at line {t.lineno}")
    t.lexer.skip(1)

lexer = lex.lex()

# =========================================================
# 2. AST NODE CLASSES
# =========================================================
class Node: pass
class Program(Node):
    def __init__(self, decls, stmts): self.decls=decls; self.stmts=stmts
class Decl(Node):
    def __init__(self, ids): self.ids=ids
class Assign(Node):
    def __init__(self, name, expr): self.name=name; self.expr=expr
class If(Node):
    def __init__(self, cond, then_s, else_s=None): self.cond=cond; self.then_s=then_s; self.else_s=else_s
class While(Node):
    def __init__(self, cond, body): self.cond=cond; self.body=body
class Block(Node):
    def __init__(self, stmts): self.stmts=stmts
class Cond(Node):
    def __init__(self, left, op, right): self.left=left; self.op=op; self.right=right
class BinOp(Node):
    def __init__(self, op, left, right): self.op=op; self.left=left; self.right=right
class Num(Node):
    def __init__(self, val): self.val=val
class Var(Node):
    def __init__(self, name): self.name=name

# =========================================================
# 3. PARSER (SYNTAX ANALYSIS)
# =========================================================
precedence = (
    ('left','PLUS','MINUS'),
    ('left','TIMES','DIVIDE'),
)

def p_program(p):
    'program : decl_list stmt_list'
    p[0] = Program(p[1], p[2])

def p_decl_list_empty(p):
    'decl_list : '
    p[0] = []

def p_decl_list(p):
    'decl_list : decl decl_list'
    p[0] = [p[1]] + p[2]

def p_decl(p):
    'decl : INT id_list SEMI'
    p[0] = Decl(p[2])

def p_id_list_single(p):
    'id_list : ID'
    p[0] = [p[1]]

def p_id_list_more(p):
    'id_list : ID COMMA id_list'
    p[0] = [p[1]] + p[3]

def p_stmt_list_empty(p):
    'stmt_list : '
    p[0] = []

def p_stmt_list(p):
    'stmt_list : stmt stmt_list'
    p[0] = [p[1]] + p[2]

def p_stmt_assign(p):
    'stmt : ID ASSIGN expr SEMI'
    p[0] = Assign(p[1], p[3])

def p_stmt_if(p):
    '''stmt : IF LPAREN cond RPAREN stmt
            | IF LPAREN cond RPAREN stmt ELSE stmt'''
    if len(p) == 6:
        p[0] = If(p[3], p[5])
    else:
        p[0] = If(p[3], p[5], p[7])

def p_stmt_while(p):
    'stmt : WHILE LPAREN cond RPAREN stmt'
    p[0] = While(p[3], p[5])

def p_stmt_block(p):
    'stmt : LBRACE stmt_list RBRACE'
    p[0] = Block(p[2])

def p_cond(p):
    'cond : expr relop expr'
    p[0] = Cond(p[1], p[2], p[3])

# --- FIXED SECTION: Expanded relop rules ---
def p_relop_GT(p):  'relop : GT'; p[0] = p[1]
def p_relop_LT(p):  'relop : LT'; p[0] = p[1]
def p_relop_GE(p):  'relop : GE'; p[0] = p[1]
def p_relop_LE(p):  'relop : LE'; p[0] = p[1]
def p_relop_EQ(p):  'relop : EQ'; p[0] = p[1]
def p_relop_NE(p):  'relop : NE'; p[0] = p[1]
# ------------------------------------------------

def p_expr_binop(p):
    '''expr : expr PLUS term
            | expr MINUS term'''
    p[0] = BinOp(p[2], p[1], p[3])

def p_expr_term(p):
    'expr : term'
    p[0] = p[1]

def p_term_binop(p):
    '''term : term TIMES factor
            | term DIVIDE factor'''
    p[0] = BinOp(p[2], p[1], p[3])

def p_term_factor(p):
    'term : factor'
    p[0] = p[1]

def p_factor_num(p):
    'factor : NUMBER'
    p[0] = Num(p[1])

def p_factor_var(p):
    'factor : ID'
    p[0] = Var(p[1])

def p_factor_expr(p):
    'factor : LPAREN expr RPAREN'
    p[0] = p[2]

def p_error(p):
    if p:
        print(f"Syntax error at token {p.type}, value {p.value}, line {p.lineno}")
    else:
        print("Syntax error at EOF")

parser = yacc.yacc()

# =========================================================
# 4. SEMANTIC ANALYSIS
# =========================================================
class Semantic:
    def __init__(self):
        self.symbols = {}
        self.errors = []

    def register_decls(self, decls):
        for d in decls:
            for name in d.ids:
                if name in self.symbols:
                    self.errors.append(f"Redeclaration of {name}")
                else:
                    self.symbols[name] = {'initialized': False}

    def check_stmt_list(self, stmts):
        for s in stmts: self.check_stmt(s)

    def check_stmt(self, s):
        if isinstance(s, Assign):
            self.check_expr(s.expr)
            if s.name not in self.symbols:
                self.errors.append(f"Undeclared variable {s.name}")
            else:
                self.symbols[s.name]['initialized'] = True
        elif isinstance(s, If):
            self.check_cond(s.cond)
            self.check_stmt(s.then_s)
            if s.else_s: self.check_stmt(s.else_s)
        elif isinstance(s, While):
            self.check_cond(s.cond)
            self.check_stmt(s.body)
        elif isinstance(s, Block):
            self.check_stmt_list(s.stmts)

    def check_expr(self, e):
        if isinstance(e, Var):
            if e.name not in self.symbols:
                self.errors.append(f"Undeclared variable {e.name}")
            elif not self.symbols[e.name]['initialized']:
                self.errors.append(f"Variable {e.name} used before initialization")
        elif isinstance(e, BinOp):
            self.check_expr(e.left); self.check_expr(e.right)

    def check_cond(self, c):
        self.check_expr(c.left); self.check_expr(c.right)

# =========================================================
# 5. CODE GENERATION (TAC + OPTIMIZATION + ASSEMBLY)
# =========================================================
class TAC:
    def __init__(self):
        self.code = []
        self.temp_count = 0
        self.label_count = 0

    def new_temp(self):
        self.temp_count += 1
        return f"t{self.temp_count}"

    def new_label(self, prefix='L'):
        self.label_count += 1
        return f"{prefix}{self.label_count}"

    def emit(self, instr):
        self.code.append(instr)

    def gen_program(self, prog):
        for s in prog.stmts:
            self.gen_stmt(s)

    def gen_stmt(self, s):
        if isinstance(s, Assign):
            r = self.gen_expr(s.expr)
            self.emit(f"{s.name} = {r}")
        elif isinstance(s, If):
            else_lbl = self.new_label('L_else_')
            end_lbl  = self.new_label('L_end_')
            cond_r = self.gen_cond(s.cond)
            self.emit(f"ifFalse {cond_r} goto {else_lbl}")
            self.gen_stmt(s.then_s)
            self.emit(f"goto {end_lbl}")
            self.emit(f"{else_lbl}:")
            if s.else_s:
                self.gen_stmt(s.else_s)
            self.emit(f"{end_lbl}:")
        elif isinstance(s, While):
            start = self.new_label('L_start_')
            end   = self.new_label('L_end_')
            self.emit(f"{start}:")
            cond_r = self.gen_cond(s.cond)
            self.emit(f"ifFalse {cond_r} goto {end}")
            self.gen_stmt(s.body)
            self.emit(f"goto {start}")
            self.emit(f"{end}:")
        elif isinstance(s, Block):
            for st in s.stmts: self.gen_stmt(st)

    def gen_cond(self, cond):
        l = self.gen_expr(cond.left)
        r = self.gen_expr(cond.right)
        t = self.new_temp()
        self.emit(f"{t} = {l} {cond.op} {r}")
        return t

    def gen_expr(self, e):
        if isinstance(e, Num): return str(e.val)
        if isinstance(e, Var): return e.name
        if isinstance(e, BinOp):
            a = self.gen_expr(e.left)
            b = self.gen_expr(e.right)
            t = self.new_temp()
            self.emit(f"{t} = {a} {e.op} {b}")
            return t

def constant_fold(code):
    folded = []
    for line in code:
        parts = line.split()
        if len(parts) == 5 and parts[1] == '=':
            dest, _, a, op, b = parts
            if (a.lstrip('-').isdigit() and b.lstrip('-').isdigit()):
                ai, bi = int(a), int(b)
                val = None
                if op == '+': val = ai + bi
                elif op == '-': val = ai - bi
                elif op == '*': val = ai * bi
                elif op == '/': val = ai // bi if bi != 0 else None
                elif op == '==': val = int(ai == bi)
                elif op == '!=': val = int(ai != bi)
                elif op == '>': val = int(ai > bi)
                elif op == '<': val = int(ai < bi)
                elif op == '>=': val = int(ai >= bi)
                elif op == '<=': val = int(ai <= bi)
                if val is not None:
                    folded.append(f"{dest} = {val}")
                    continue
        folded.append(line)
    return folded

def emit_asm(code):
    asm = []
    for line in code:
        if line.endswith(':'):
            asm.append(line)
        elif line.startswith('ifFalse'):
            parts = line.split()
            cond, label = parts[1], parts[3]
            asm.append(f"CMP {cond}, 0")
            asm.append(f"JE {label}")
        elif line.startswith('goto'):
            asm.append(f"JMP {line.split()[1]}")
        elif '=' in line:
            lhs, rhs = [s.strip() for s in line.split('=',1)]
            toks = rhs.split()
            if len(toks) == 1:
                asm.append(f"LOAD {toks[0]}")
                asm.append(f"STORE {lhs}")
            elif len(toks) == 3:
                a, op, b = toks
                asm.append(f"LOAD {a}")
                if op == '+': asm.append(f"ADD {b}")
                elif op == '-': asm.append(f"SUB {b}")
                elif op == '*': asm.append(f"MUL {b}")
                elif op == '/': asm.append(f"DIV {b}")
                asm.append(f"STORE {lhs}")
            else:
                asm.append(f"; {line}")
        else:
            asm.append(f"; {line}")
    return asm

# =========================================================
# 6. DRIVER
# =========================================================
def compile_source(src_text):
    print("\n--- TOKENS ---")
    lexer.input(src_text)
    for tok in lexer:
        print(tok)

    ast = parser.parse(src_text)
    sem = Semantic()
    sem.register_decls(ast.decls)
    sem.check_stmt_list(ast.stmts)
    if sem.errors:
        print("\nSemantic Errors:")
        for e in sem.errors: print("  ", e)
        return

    tac = TAC()
    tac.gen_program(ast)

    print("\n--- THREE ADDRESS CODE (TAC) ---")
    for l in tac.code: print(" ", l)

    optimized = constant_fold(tac.code)
    print("\n--- OPTIMIZED TAC ---")
    for l in optimized: print(" ", l)

    print("\n--- PSEUDO ASSEMBLY ---")
    asm = emit_asm(optimized)
    for a in asm: print(" ", a)

def main(argv):
    if len(argv) >= 2:
        path = argv[1]
        with open(path, 'r') as f:
            src = f.read()
    else:
        src = """int a, b, c;
a = b * c + 45;
if (a > 100)
  a = a - 10;
else
  a = a + 5;
"""
        print("No input file provided — running built-in sample program.")

    compile_source(src)

if __name__ == "__main__":
    main(sys.argv)

