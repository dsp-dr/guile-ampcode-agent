# AmpCode Pattern Analysis for Guile Implementation

## Core Architecture Requirements

Based on analysis of https://ampcode.com/how-to-build-an-agent

### 1. Core Components
- **Language Model Client**: Interface to Claude/Anthropic API
- **Tool Registry**: System for registering and managing tools
- **Conversation Manager**: Handles message history and context
- **Input/Output Handler**: Terminal I/O for user interaction

### 2. Tool System Design
Each tool requires:
- Name (string identifier)
- Description (for LLM to understand when to use)
- Input Schema (JSON Schema format)
- Execution Function (actual implementation)

### 3. Message Structure
- Conversation as list of messages
- Support for text and tool-use content types
- Stateless design - full context sent each request

### 4. Execution Flow
1. Initialize agent with model client and tools
2. Main conversation loop:
   - Get user input
   - Add to conversation history
   - Send to model
   - Process response
   - Execute tools if requested
   - Return results to model
   - Continue until exit

## Guile-Specific Considerations

### Module Structure
```
(agent)          - Main agent module
(agent tools)    - Tool registry and base tool
(agent client)   - Anthropic API client
(agent message)  - Message types and handling
```

### Key Differences from Go Implementation
- Use SRFI-9 records instead of structs
- Use parameters for configuration
- Leverage Guile's JSON module for schema handling
- Use ice-9 match for pattern matching on message types

### Implementation Notes
- Keep it simple (~300 lines core)
- Tools are the extension mechanism
- Focus on clean separation of concerns