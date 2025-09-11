# Guile AmpCode Agent

[![Guile](https://img.shields.io/badge/Guile-2.2%2B-blue.svg)](https://www.gnu.org/software/guile/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Scheme](https://img.shields.io/badge/Scheme-R7RS-green.svg)](https://small.r7rs.org/)

A Guile implementation of an AI agent following the [ampcode.com](https://ampcode.com/how-to-build-an-agent) pattern for tool-based interactions with multiple LLM backends.

![Demo](demo/demo-improved.gif)

This project demonstrates how to build a functional AI agent in Scheme that can interact with multiple LLM backends and execute tools based on natural language requests.

## Quick Start

Three backends available for immediate use:

### 🎭 Mock Backend (No Setup Required)
```bash
./guile-agent-v2 -b mock
```
Perfect for testing and development - shows realistic tool invocations with structured output.

### 🦙 Ollama Backend (Local LLM)
```bash
ollama pull tinyllama  # 637MB model
./guile-agent-v2 -b ollama -m tinyllama
```
Run completely offline with local models.

### 🤖 Anthropic Backend (Production)
```bash
export ANTHROPIC_API_KEY="your-key"
./guile-agent-v2 -b anthropic
```
Full Claude integration with advanced reasoning.

## Features

- **Multi-Backend Architecture**: Mock, Ollama, and Anthropic support
- **Tool Registration System**: Extensible tool framework  
- **Structured Tool Calls**: Shows actual function calls and results
- **Clean Module Design**: Separate concerns for maintainability
- **Student-Friendly**: Comprehensive guides and examples

## Architecture

The agent follows the pattern described at https://ghuntley.com/agent/ and consists of:

- **Client Module**: Handles communication with Anthropic's API
- **Message Module**: Defines message types and serialization
- **Tools Module**: Tool registry and execution framework
- **Agent Module**: Main conversation loop and orchestration

## Prerequisites

- Guile 3.0 or later
- An Anthropic API key

## Installation

```bash
# Clone the repository
git clone https://github.com/dsp-dr/guile-ampcode-agent.git
cd guile-ampcode-agent

# Build (optional - compiles to bytecode)
gmake

# Install system-wide (optional)
sudo gmake install
```

## Usage

### Set your API key

```bash
export ANTHROPIC_API_KEY="your-api-key-here"
```

### Run the agent

```bash
./guile-agent
```

Or with options:

```bash
./guile-agent -k "your-api-key" -m "claude-3-opus-20240229"
```

### Available commands

- Type any message to interact with Claude
- Type `quit` to exit
- Claude can use the registered tools (weather, calculator) automatically

### Example interaction

```
> What's the weather in San Francisco?
Assistant: I'll check the weather in San Francisco for you.
The weather in San Francisco, CA is currently 22° C with partly cloudy skies.

> Calculate 15% tip on a $48.50 bill
Assistant: I'll calculate 15% tip on $48.50 for you.
Result: 7.275
A 15% tip on a $48.50 bill would be $7.28 (rounded up).
```

## Project Structure

```
guile-ampcode-agent/
├── src/
│   ├── agent.scm           # Main agent module
│   ├── agent/
│   │   ├── client.scm      # Anthropic API client
│   │   ├── message.scm     # Message types
│   │   └── tools.scm       # Tool registry
│   └── tools/
│       ├── weather.scm     # Weather tool
│       └── calculator.scm  # Calculator tool
├── experiments/            # Research and notes
├── guile-agent            # Main executable
├── Makefile               # Build system
└── README.md              # This file
```

## Extending with New Tools

To add a new tool, create a module in `src/tools/` following this pattern:

```scheme
(define-module (tools mytool)
  #:use-module (agent tools)
  #:export (my-tool))

(define my-tool
  (make-tool
   "tool_name"
   "Tool description for Claude"
   '((type . "object")
     (properties . ((param . ((type . "string")))))
     (required . #("param")))
   (lambda (input)
     ;; Tool implementation
     "Tool result")))
```

Then register it in the main executable:

```scheme
(register-tool! tools my-tool)
```

## Configuration

The agent can be configured through:

- Environment variables (ANTHROPIC_API_KEY)
- Command-line options (-k, -m)
- Code modifications for custom tools

## Development

```bash
# Run tests (when available)
gmake test

# Clean compiled files
gmake clean

# Get help on make targets
gmake help
```

## License

MIT

## Acknowledgments

- Built following the pattern from [ampcode.com](https://ampcode.com/how-to-build-an-agent)
- Reference implementation: [ghuntley.com/agent](https://ghuntley.com/agent/)
- Powered by [Anthropic's Claude](https://www.anthropic.com/)

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.