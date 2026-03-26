# Makefile for Python-CL

.PHONY: help setup test test-unit test-conformance test-section repl clean lint docs install

# Default target
help:
	@echo "Python-CL Build System"
	@echo ""
	@echo "Targets:"
	@echo "  setup           - Install dependencies and setup development environment"
	@echo "  test            - Run all tests (unit + conformance)"
	@echo "  test-unit       - Run unit tests only"
	@echo "  test-conformance - Run conformance tests"
	@echo "  test-section    - Run tests for specific section (make test-section SECTION=2.6)"
	@echo "  repl            - Start Python-CL interactive REPL"
	@echo "  clean           - Clean generated files"
	@echo "  lint            - Run code quality checks"
	@echo "  docs            - Generate documentation"
	@echo "  install         - Install Python-CL system-wide"

# Setup development environment
setup:
	@echo "Setting up Python-CL development environment..."
	sbcl --eval "(ql:quickload :python-cl)" --eval "(quit)"
	@echo "Cloning conformance test suite..."
	@mkdir -p tests/conformance/
	@if [ ! -d "tests/conformance/python-spec-test-suite" ]; then \
		git clone https://github.com/soniccyclops-bot-collab/python-spec-test-suite.git tests/conformance/python-spec-test-suite; \
	fi
	@echo "Development environment ready!"

# Run all tests
test: test-unit test-conformance

# Run unit tests
test-unit:
	@echo "Running Python-CL unit tests..."
	sbcl --eval "(load \"~/quicklisp/setup.lisp\")" \
	     --eval "(push #P\"$(shell pwd)/\" asdf:*central-registry*)" \
	     --eval "(ql:quickload :python-cl/tests)" \
	     --eval "(python-cl/tests:run-tests)" \
	     --eval "(quit)"

# Run conformance tests
test-conformance:
	@echo "Running Python Language Reference conformance tests..."
	sbcl --eval "(load \"~/quicklisp/setup.lisp\")" \
	     --eval "(push #P\"$(shell pwd)/\" asdf:*central-registry*)" \
	     --eval "(ql:quickload :python-cl/tests)" \
	     --eval "(python-cl/tests:run-all-conformance-tests)" \
	     --eval "(quit)"

# Run tests for specific section
test-section:
	@if [ -z "$(SECTION)" ]; then \
		echo "Usage: make test-section SECTION=2.6"; \
		exit 1; \
	fi
	@echo "Running conformance tests for Section $(SECTION)..."
	sbcl --eval "(load \"~/quicklisp/setup.lisp\")" \
	     --eval "(push #P\"$(shell pwd)/\" asdf:*central-registry*)" \
	     --eval "(ql:quickload :python-cl)" \
	     --eval "(python-cl:test-section \"$(SECTION)\")" \
	     --eval "(quit)"

# Interactive REPL
repl:
	@echo "Starting Python-CL interactive REPL..."
	@echo "Available commands:"
	@echo "  (py-eval \"2 + 3\")     - Evaluate Python expression"
	@echo "  (py-tokens \"2 + 3\")   - Show tokenization"
	@echo "  (python-repl)          - Python-like REPL"
	@echo "  (run-conformance-tests) - Run conformance tests"
	sbcl --eval "(ql:quickload :python-cl)" \
	     --eval "(in-package :python-cl)"

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	find . -name "*.fasl" -delete
	find . -name "*~" -delete
	find . -name ".#*" -delete
	rm -rf tests/conformance/python-spec-test-suite/.git

# Code quality checks
lint:
	@echo "Running code quality checks..."
	@echo "Checking for compilation warnings..."
	sbcl --eval "(ql:quickload :python-cl)" \
	     --eval "(compile-file \"python-cl.asd\")" \
	     --eval "(quit)" || echo "Compilation warnings found"

# Generate documentation
docs:
	@echo "Generating documentation..."
	@echo "Creating API documentation..."
	mkdir -p docs/api/
	sbcl --eval "(ql:quickload :python-cl)" \
	     --eval "(describe 'python-cl:py-eval)" \
	     --eval "(quit)" > docs/api/main-interface.txt

# Install system-wide (optional)
install:
	@echo "Installing Python-CL..."
	@echo "Note: This will make python-cl available to all SBCL instances"
	sbcl --eval "(push #P\"$(PWD)/\" asdf:*central-registry*)" \
	     --eval "(ql:quickload :python-cl)" \
	     --eval "(quit)"

# Development utilities
dev-status:
	@echo "Python-CL Development Status:"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	@echo "Implementation Progress:"
	@echo "  ✓ Lexer (tokenization)"
	@echo "  ✓ Basic AST nodes"
	@echo "  ✓ Object system (int, float, complex, str, list, dict)"
	@echo "  ✓ Environment (LEGB scoping)"
	@echo "  ✓ Built-in functions"
	@echo "  ✓ Arithmetic operations"
	@echo "  ⏳ Parser (simple expressions only)"
	@echo "  ⏳ Full expression evaluation"
	@echo "  ⏳ Statement execution"
	@echo "  ⏳ Control flow"
	@echo "  ⏳ Function definitions"
	@echo "  ⏳ Import system"
	@echo ""
	@echo "Conformance Status:"
	@echo "  ✓ Section 2.6: Numeric literals (~20 tests)"
	@echo "  ✓ Section 6.7: Binary arithmetic (~22 tests)"  
	@echo "  ⏳ Sections 2.1-2.5: Lexical analysis (pending)"
	@echo "  ⏳ Section 3: Data model (pending)"
	@echo "  ⏳ Sections 4-9: Execution, imports, expressions, statements (pending)"
	@echo ""
	@echo "Target: 1,412 total conformance tests"
	@echo "Current: ~42 tests passing (3% complete)"

# Quick demo
demo:
	@echo "Python-CL Quick Demo:"
	@echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
	sbcl --eval "(ql:quickload :python-cl)" \
	     --eval "(format t \"~&Evaluating: 2 + 3~%\")" \
	     --eval "(format t \"Result: ~A~%\" (python-cl:py-eval \"2 + 3\"))" \
	     --eval "(format t \"~&Evaluating: 0xFF + 0b1010~%\")" \
	     --eval "(format t \"Result: ~A~%\" (python-cl:py-eval \"0xFF + 0b1010\"))" \
	     --eval "(format t \"~&Evaluating: 2 ** 8~%\")" \
	     --eval "(format t \"Result: ~A~%\" (python-cl:py-eval \"2 ** 8\"))" \
	     --eval "(quit)"

# Benchmark against conformance suite
benchmark:
	@echo "Benchmarking Python-CL against conformance test suite..."
	@echo "This will run our implementation against the 1,412-test suite"
	time make test-conformance

# Watch for changes and auto-test
watch:
	@echo "Watching for file changes..."
	@echo "Run 'make test' when files change"
	@command -v fswatch >/dev/null 2>&1 || { echo >&2 "fswatch required but not installed. Install with: brew install fswatch"; exit 1; }
	fswatch -o src/ tests/ | xargs -n1 -I{} make test-unit