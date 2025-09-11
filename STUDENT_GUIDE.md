# Student Guide: Guile AmpCode Agent

Welcome! This guide will help you get the Guile AmpCode Agent running on your machine.

## Prerequisites Checklist

Before starting, ensure you have:

- [ ] Guile 3.0 or later installed
- [ ] Git installed
- [ ] A text editor (Emacs, VS Code, etc.)
- [ ] Basic familiarity with the terminal

### Installing Guile

**macOS:**
```bash
brew install guile
```

**Ubuntu/Debian:**
```bash
sudo apt-get update
sudo apt-get install guile-3.0
```

**Fedora:**
```bash
sudo dnf install guile30
```

**FreeBSD:**
```bash
pkg install guile3
```

### Verify Installation
```bash
guile --version
# Should show version 3.0 or higher
```

## Quick Start (No API Key Required)

1. **Clone the repository:**
```bash
git clone https://github.com/dsp-dr/guile-ampcode-agent.git
cd guile-ampcode-agent
```

2. **Run the tests to verify everything works:**
```bash
# Run all tests
guile tests/test-all.scm

# Or use make
make test
```

3. **Try the mock mode (no API key needed):**
```bash
./guile-agent --mock
```

## Full Setup (With Anthropic API)

### Step 1: Get an API Key

1. Go to [console.anthropic.com](https://console.anthropic.com)
2. Sign up or log in
3. Navigate to API Keys
4. Create a new key
5. Copy the key (starts with `sk-ant-api03-`)

### Step 2: Set Up Environment

**Option A: Environment Variable (Recommended)**
```bash
# Add to your ~/.bashrc or ~/.zshrc
export ANTHROPIC_API_KEY="sk-ant-api03-your-key-here"

# Reload your shell
source ~/.bashrc  # or ~/.zshrc
```

**Option B: Use .env file**
```bash
cp .env.example .env
# Edit .env and add your key
nano .env  # or your preferred editor
```

### Step 3: Run the Agent

```bash
# Basic usage
./guile-agent

# With specific model
./guile-agent -m claude-3-opus-20240229

# With inline API key (not recommended for security)
./guile-agent -k "your-api-key"

# Show help
./guile-agent -h
```

## Understanding the Code Structure

```
guile-ampcode-agent/
├── src/
│   ├── agent.scm           # Main conversation loop
│   ├── agent/
│   │   ├── client.scm      # API communication
│   │   ├── message.scm     # Message types
│   │   └── tools.scm       # Tool framework
│   └── tools/
│       ├── weather.scm     # Example tool
│       └── calculator.scm  # Example tool
├── tests/                  # Test files
├── experiments/            # Notes and research
└── guile-agent            # Main executable
```

## Running Tests

```bash
# Run all tests
make test

# Run specific test module
guile tests/test-messages.scm
guile tests/test-tools.scm

# Run with verbose output
guile tests/test-all.scm
```

## Common Issues and Solutions

### Issue: "command not found: guile"
**Solution:** Guile is not installed or not in PATH. See installation instructions above.

### Issue: "No API key provided"
**Solution:** Set the ANTHROPIC_API_KEY environment variable or use the -k flag.

### Issue: "Module not found" errors
**Solution:** Make sure you're running from the project root directory.

### Issue: Tests failing
**Solution:** 
```bash
# Clean and rebuild
make clean
make
make test
```

## Exercises for Students

### Exercise 1: Create a New Tool
Create a dice rolling tool in `src/tools/dice.scm`:

```scheme
(define-module (tools dice)
  #:use-module (agent tools)
  #:export (dice-tool))

(define dice-tool
  (make-tool
   "roll_dice"
   "Roll dice"
   '((type . "object")
     (properties . ((sides . ((type . "number")))))
     (required . #("sides")))
   (lambda (input)
     ;; Your implementation here
     )))
```

### Exercise 2: Extend the Weather Tool
Modify `src/tools/weather.scm` to:
- Add temperature ranges
- Include weather conditions (sunny, rainy, etc.)
- Support multiple units

### Exercise 3: Add Conversation Persistence
Implement saving/loading conversations to/from files.

## Learning Resources

- [Guile Manual](https://www.gnu.org/software/guile/manual/)
- [SRFI Documentation](https://srfi.schemers.org/)
- [Original Article](https://ampcode.com/how-to-build-an-agent)
- [Reference Implementation](https://ghuntley.com/agent/)

## Mock Mode for Development

To test without an API key, we provide a mock mode:

```bash
# Run in mock mode
./guile-agent --mock

# The mock mode simulates API responses for testing
```

## Debugging Tips

1. **Enable verbose output:**
```scheme
;; Add to your code
(set! %load-verbosely #t)
```

2. **Use the REPL for testing:**
```bash
guile
> (add-to-load-path "src")
> (use-modules (agent tools))
> (define registry (make-tool-registry))
> ;; Test your code interactively
```

3. **Check module loading:**
```bash
guile -c "(display %load-path)" | tr ':' '\n'
```

## Project Checklist

- [ ] Guile installed and working
- [ ] Repository cloned
- [ ] Tests passing
- [ ] Understand basic code structure
- [ ] Can run in mock mode
- [ ] (Optional) API key configured
- [ ] (Optional) Created a custom tool

## Need Help?

1. Check the error message carefully
2. Run the tests to identify issues
3. Review this guide's troubleshooting section
4. Check the experiments/ folder for additional notes
5. Open an issue on GitHub with:
   - Your OS and Guile version
   - The exact error message
   - Steps to reproduce

## Next Steps

After getting the basic agent working:

1. Study the code architecture
2. Create your own tools
3. Experiment with different prompts
4. Try integrating with other APIs
5. Build a web interface
6. Add persistence features

Good luck with your exploration of AI agents in Guile!