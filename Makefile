# Makefile for Guile AmpCode Agent

PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin
SHAREDIR = $(PREFIX)/share/guile-ampcode-agent

GUILE = guile
GUILD = guild

# Source files
SCM_FILES = src/agent.scm \
            src/agent/client.scm \
            src/agent/message.scm \
            src/agent/tools.scm \
            src/tools/weather.scm \
            src/tools/calculator.scm

# Compiled files
GO_FILES = $(SCM_FILES:.scm=.go)

.PHONY: all compile clean install uninstall test

all: compile

compile: $(GO_FILES)

%.go: %.scm
	$(GUILD) compile -o $@ $<

clean:
	rm -f $(GO_FILES)
	find . -name "*.go" -delete

install: all
	install -d $(DESTDIR)$(BINDIR)
	install -d $(DESTDIR)$(SHAREDIR)/src/agent
	install -d $(DESTDIR)$(SHAREDIR)/src/tools
	install -m 755 guile-agent $(DESTDIR)$(BINDIR)/
	install -m 644 src/agent.scm $(DESTDIR)$(SHAREDIR)/src/
	install -m 644 src/agent/*.scm $(DESTDIR)$(SHAREDIR)/src/agent/
	install -m 644 src/tools/*.scm $(DESTDIR)$(SHAREDIR)/src/tools/
	@echo "Installation complete. Make sure to set ANTHROPIC_API_KEY environment variable."

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/guile-agent
	rm -rf $(DESTDIR)$(SHAREDIR)

test:
	@echo "Running tests..."
	$(GUILE) -L src tests/run-tests.scm

check: test

.PHONY: help
help:
	@echo "Guile AmpCode Agent - Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all       - Build everything (default)"
	@echo "  compile   - Compile Scheme files to bytecode"
	@echo "  clean     - Remove compiled files"
	@echo "  install   - Install to system"
	@echo "  uninstall - Remove from system"
	@echo "  test      - Run test suite"
	@echo "  help      - Show this help"
	@echo ""
	@echo "Variables:"
	@echo "  PREFIX    - Installation prefix (default: /usr/local)"
	@echo "  DESTDIR   - Destination directory for staged installs"