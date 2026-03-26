# Python-CL Conformance Roadmap

**Target:** 100% compliance with Python Language Reference Conformance Test Suite (1,412 tests)  
**Current Status:** 30/1,412 tests passing (~2.1% complete)  
**Repository:** [python-spec-test-suite](https://github.com/soniccyclops-bot-collab/python-spec-test-suite)

## Current Implementation Status

### ✅ Completed Foundation
- **Lexer:** Python tokenization (numbers, strings, operators, keywords)
- **Parser:** Basic expression parsing with precedence climbing
- **AST:** Core node types (expressions, statements, literals)
- **Runtime:** Object model (int, float, complex, str, list, dict)
- **Environment:** LEGB scoping framework
- **Built-ins:** Partial arithmetic operations
- **Test Harness:** Integration with conformance test suite

### 🔧 Current Capabilities
- **Numeric literals:** Decimal, hex, binary, octal, float, complex (partial)
- **Arithmetic:** `+`, `-`, `*`, `/`, `//`, `**` (basic cases)
- **Expression evaluation:** Simple binary operations
- **Environment:** Variable binding and lookup

### ❌ Known Failures (30 failing tests)
1. **Lexer Issues:**
   - Uppercase prefixes (`0X`, `0O`, `0B`) not handled
   - Complex literal parsing incomplete (`3+4j` fails)
   
2. **Parser Issues:**
   - Unary minus not implemented (`-17`)
   - Modulo operator missing (`%`)
   - Operator precedence edge cases
   
3. **Type System Issues:**
   - Python float vs Lisp rational semantics
   - Type conversion inconsistencies

## Implementation Roadmap

### Phase 1: Core Language Foundation (Weeks 1-4)
**Goal:** Sections 2-3 fully compliant (300+ tests)

#### 1.1 Section 2: Lexical Analysis (Weeks 1-2)
**Tests:** ~180 tests across 2.1-2.8

**Section 2.1: Line Structure**
- [ ] Logical line construction
- [ ] Indentation handling (INDENT/DEDENT tokens)
- [ ] Comment processing
- [ ] Line continuation (backslash, implicit)

**Section 2.2: Other Tokens**
- [ ] Token recognition completeness
- [ ] Whitespace handling
- [ ] Error token generation

**Section 2.3: Names and Identifiers**
- [ ] Unicode identifier support
- [ ] Keyword vs identifier disambiguation
- [ ] Reserved word enforcement

**Section 2.4: Literals**
- [x] Integer literals (partial - fix uppercase prefixes)
- [ ] Floating point literals (complete edge cases)
- [ ] Complex literals (fix parsing)
- [ ] String/bytes literals (Section 2.5)

**Section 2.5: String and Bytes Literals**
- [ ] String prefix handling (r, u, b, f combinations)
- [ ] Triple-quoted strings
- [ ] Escape sequence processing
- [ ] F-string tokenization (basic)

**Section 2.6: Numeric Literals** ✅ (needs fixes)
- [x] Basic integer/float/complex (16/20 tests passing)
- [ ] Fix uppercase prefixes (`0X`, `0O`, `0B`)
- [ ] Fix complex number parsing
- [ ] Edge case handling

**Technical Implementation:**
```lisp
;; Priority fixes for Section 2.6
(defun scan-number (source start)
  ;; Fix: Handle uppercase prefixes
  (when (and (< (+ pos 1) len) (char= (char source pos) #\0))
    (case (char-downcase (char source (1+ pos)))
      ((#\x #\X) (setf radix 16 pos (+ pos 2)))  ; Accept both
      ((#\o #\O) (setf radix 8 pos (+ pos 2)))   ; Accept both  
      ((#\b #\B) (setf radix 2 pos (+ pos 2))))) ; Accept both
```

#### 1.2 Section 3: Data Model (Week 3)
**Tests:** ~85 tests

**Core Requirements:**
- [ ] Object identity and type system
- [ ] Attribute access (`obj.attr`)
- [ ] Special method resolution (`__add__`, `__str__`, etc.)
- [ ] Truth value testing
- [ ] Sequence/mapping protocols

**Technical Implementation:**
```lisp
;; Python object protocol
(defclass py-object ()
  ((py-type :accessor py-type)
   (py-dict :accessor py-dict :initform (make-hash-table))))

(defgeneric py-getattr (obj name))
(defgeneric py-setattr (obj name value))
(defgeneric py-call (func &rest args))
```

#### 1.3 Section 4: Execution Model (Week 4)  
**Tests:** ~45 tests

**Core Requirements:**
- [ ] LEGB scope resolution (complete implementation)
- [ ] Name binding semantics
- [ ] Global/nonlocal statements
- [ ] Exception propagation framework

### Phase 2: Expression System (Weeks 5-8)
**Goal:** Section 6 fully compliant (400+ tests)

#### 2.1 Section 6.1: Arithmetic Conversions (Week 5)
**Tests:** ~20 tests
- [ ] Numeric type coercion rules
- [ ] Mixed arithmetic behavior
- [ ] Overflow handling

#### 2.2 Section 6.2: Atoms (Week 5)
**Tests:** ~65 tests  
- [ ] Identifiers and literals (build on Section 2)
- [ ] Parenthesized expressions
- [ ] List/dict/set displays
- [ ] Generator expressions (basic)

#### 2.3 Section 6.3: Primaries (Week 6)
**Tests:** ~70 tests
- [ ] Attribute references (`obj.attr`)
- [ ] Subscripting (`obj[key]`)
- [ ] Slicing (`obj[start:end]`)
- [ ] Calls (`func(args)`)

#### 2.4 Section 6.4-6.16: Operators (Weeks 7-8)
**Tests:** ~250 tests across all operator sections

**Unary Operations (6.6):**
- [ ] Unary `+`, `-`, `~`, `not`
- [ ] Operator precedence

**Binary Operations (6.7-6.11):**
- [x] Arithmetic (`+`, `-`, `*`, `/`, `//`, `%`, `**`) - partial
- [ ] Shift operations (`<<`, `>>`)
- [ ] Bitwise operations (`&`, `|`, `^`)
- [ ] Comparisons (`<`, `>`, `<=`, `>=`, `==`, `!=`)
- [ ] Boolean operations (`and`, `or`)

**Advanced Expressions:**
- [ ] Assignment expressions (`:=` walrus operator)
- [ ] Conditional expressions (`x if cond else y`)
- [ ] Lambda expressions
- [ ] Expression lists and tuple formation

**Technical Priority - Fix Current Failures:**
```lisp
;; Immediate fixes needed
(defmethod py-eval-ast ((node py-binop) env)
  (let ((left (py-eval-ast (py-left node) env))
        (right (py-eval-ast (py-right node) env)))
    (case (py-op node)
      (+ (py-add left right))
      (- (py-sub left right))  
      (* (py-mul left right))
      (/ (py-div left right))       ; Fix: Python division always returns float
      (/\/ (py-floordiv left right)) ; Fix: Return proper type
      (% (py-mod left right))       ; Fix: Implement modulo
      (** (py-power left right)))))

;; Add unary operator support
(defclass py-unaryop (py-expr) ...)
(defmethod py-eval-ast ((node py-unaryop) env) ...)
```

### Phase 3: Statement System (Weeks 9-12)
**Goal:** Sections 7-8 fully compliant (600+ tests)

#### 3.1 Section 7: Simple Statements (Weeks 9-10)
**Tests:** ~250 tests

**Core Statements:**
- [ ] Expression statements
- [ ] Assignment statements (multiple targets, augmented)
- [ ] Assert statements
- [ ] Pass/del statements
- [ ] Return statements  
- [ ] Yield statements (generator basics)
- [ ] Raise statements (exception basics)
- [ ] Break/continue statements
- [ ] Import statements (basic)
- [ ] Global/nonlocal statements

#### 3.2 Section 8: Compound Statements (Weeks 11-12)
**Tests:** ~350 tests

**Control Flow:**
- [ ] If statements (`if/elif/else`)
- [ ] While statements  
- [ ] For statements (iteration protocol)
- [ ] Try statements (exception handling)
- [ ] With statements (context managers)

**Advanced Features:**
- [ ] Function definitions (`def`)
- [ ] Class definitions (`class`) 
- [ ] Async function definitions (`async def`)
- [ ] Match statements (Python 3.10+)

**Technical Implementation:**
```lisp
;; Control flow via conditions/restarts
(define-condition py-break-condition (condition) ())
(define-condition py-continue-condition (condition) ())
(define-condition py-return-condition (condition)
  ((value :initarg :value :reader return-value)))

(defmethod py-eval-ast ((node py-while) env)
  (restart-case
    (loop while (py-truthy-p (py-eval-ast (py-test node) env))
          do (handler-case
               (py-exec-stmts (py-body node) env)
             (py-break-condition () (return))
             (py-continue-condition () (continue))))))
```

### Phase 4: Advanced Features (Weeks 13-16)
**Goal:** Sections 5, 9 and advanced features (200+ tests)

#### 4.1 Section 5: Import System (Week 13)
**Tests:** ~55 tests
- [ ] Module loading mechanism
- [ ] Package structure
- [ ] Relative imports
- [ ] Import statement variants

#### 4.2 Section 9: Top-level Components (Week 14)
**Tests:** ~18 tests  
- [ ] Module structure
- [ ] Interactive input
- [ ] Expression input

#### 4.3 Advanced Language Features (Weeks 15-16)
- [ ] Generators and async/await
- [ ] Context managers
- [ ] Decorators
- [ ] Metaclasses (basic)
- [ ] Exception handling completeness

### Phase 5: Integration and Performance (Weeks 17-20)

#### 5.1 External Test Suite Integration (Week 17)
- [ ] Direct pytest integration
- [ ] CI/CD pipeline setup
- [ ] Automated regression testing

#### 5.2 Performance Optimization (Week 18)
- [ ] Bytecode compilation (optional)
- [ ] Object allocation optimization
- [ ] Built-in function performance

#### 5.3 Standard Library Compatibility (Weeks 19-20)
- [ ] Essential built-ins (`len`, `print`, `type`, etc.)
- [ ] Basic modules (`sys`, `os` - minimal)
- [ ] Error message compatibility

## Technical Architecture Decisions

### 1. Parser Strategy
**Current:** Recursive descent with precedence climbing  
**Future:** Consider PEG parser for complex syntax

### 2. Object Model
**Current:** CLOS-based with Python semantics overlay  
**Decision:** Continue with CLOS, add protocol layer

### 3. Execution Model
**Current:** AST interpretation  
**Future:** Optional bytecode compilation for performance

### 4. Memory Management
**Approach:** Leverage Lisp GC, add Python reference semantics where needed

### 5. Error Handling
**Approach:** Map Python exceptions to Lisp conditions

## Validation Strategy

### Continuous Integration
- **Unit Tests:** Run on every commit
- **Conformance Tests:** Run section-by-section validation  
- **External Suite:** Weekly full suite validation against real CPython

### Quality Metrics
- **Test Coverage:** Target 100% of implemented sections
- **Performance:** Within 10x of CPython for basic operations
- **Memory:** Reasonable memory usage for test suite

### Milestone Validation
- **Phase 1:** Sections 2-3 100% green
- **Phase 2:** Section 6 100% green  
- **Phase 3:** Sections 7-8 100% green
- **Phase 4:** All 25 sections 100% green

## Resource Estimates

### Time Investment
- **Total:** 20 weeks (5 months)
- **Weekly:** 15-20 hours development + testing
- **Critical Path:** Parser completeness, object model, control flow

### Technical Complexity
- **High:** Object model, exception handling, import system
- **Medium:** Expression evaluation, statement execution
- **Low:** Lexical analysis, basic arithmetic

### Risk Factors
- **Parser Complexity:** Python syntax edge cases
- **Semantic Accuracy:** Subtle Python behavior differences
- **Performance:** Lisp vs Python execution model differences
- **Ecosystem:** Built-in library compatibility requirements

## Success Criteria

### Primary Goals
- [x] ✅ **Repository established** with foundational architecture
- [ ] 🎯 **100% conformance** to 1,412-test Python Language Reference suite
- [ ] 🎯 **Performance target** within 10x of CPython for basic operations
- [ ] 🎯 **CI integration** with automated conformance validation

### Secondary Goals  
- [ ] **Educational value** as Python implementation reference
- [ ] **Lisp advocacy** demonstrating language implementation power
- [ ] **Research platform** for Python semantics exploration

## Next Immediate Actions

### Week 1 Priority (This Week)
1. **Fix Section 2.6 failures** (4 failing tests)
   - Uppercase prefix handling
   - Complex literal parsing  
   - Type conversion consistency
   
2. **Fix Section 6.7 failures** (8 failing tests)
   - Unary minus operator
   - Modulo operator implementation
   - Division type semantics
   - Floor division behavior

3. **Establish external suite integration**
   - Clone conformance suite into tests/conformance/
   - Wire actual pytest execution
   - Setup automated CI validation

### Success Metric for Week 1
- **Target:** 42/42 current tests passing (100% of implemented features)
- **Validation:** `make test-conformance` shows all green
- **Documentation:** This roadmap committed and updated

---

**This roadmap represents a systematic, engineering-driven approach to Python implementation with concrete milestones, technical specifications, and validation criteria. Each phase builds on the previous with clear success metrics and realistic timelines.**