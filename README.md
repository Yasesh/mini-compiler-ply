#  Mini Compiler using Python (PLY)

This project implements a **Mini Compiler** in Python that accepts arithmetic expressions and control structures, verifies syntax and semantics, and generates **intermediate and optimized code**.

---

##  Features
- Lexical Analysis (Tokenization)
- Syntax Analysis using PLY (Yacc)
- Semantic Checking (Symbol Table)
- Intermediate Code Generation (TAC)
- Simple Optimization (Constant Folding)
- Pseudo Assembly Code Generation

---

##  Example

### **Input:**
```c
int a, b, c;
b = 2;
c = 3;
a = b * c + 45;
if (a > 100)
  a = a - 10;
else
  a = a + 5;
