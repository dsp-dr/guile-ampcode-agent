# Ollama Setup Guide

Run AI models locally on your machine with Ollama backend support.

## Quick Start

### 1. Install Ollama

**macOS/Linux:**
```bash
curl -fsSL https://ollama.com/install.sh | sh
```

**Manual installation:**
Visit [ollama.com/download](https://ollama.com/download)

### 2. Start Ollama Server

```bash
# Start the Ollama service
ollama serve
# Keep this running in a separate terminal
```

### 3. Pull a Tiny Model

For reduced hardware requirements, use TinyLlama (1.1B parameters, ~637MB):

```bash
# Pull TinyLlama - perfect for testing
ollama pull tinyllama

# Other small models:
ollama pull phi       # Microsoft Phi-2 (2.7B, ~1.7GB)
ollama pull orca-mini # Orca Mini (3B, ~1.9GB)
```

### 4. Test Ollama

```bash
# Test the model directly
ollama run tinyllama "Hello, how are you?"
```

### 5. Use with Guile Agent

```bash
# List available models
./guile-agent-v2 -b ollama --list-models

# Run with TinyLlama (default)
./guile-agent-v2 -b ollama

# Run with specific model
./guile-agent-v2 -b ollama -m tinyllama

# Use custom Ollama server
./guile-agent-v2 -b ollama --ollama-host 192.168.1.100 --ollama-port 11434
```

## Recommended Models by Hardware

### Minimal Hardware (4GB RAM)
- **tinyllama** (1.1B) - Fastest, lowest requirements
- **phi** (2.7B) - Good balance of speed and quality

### Moderate Hardware (8GB RAM)
- **mistral** (7B) - Excellent quality
- **llama2** (7B) - Meta's open model
- **orca-mini** (3B) - Good for conversations

### Good Hardware (16GB+ RAM)
- **llama2:13b** - Larger Llama 2
- **codellama** - Specialized for code
- **mixtral** - MoE architecture

## Model Comparison

| Model | Size | RAM Required | Speed | Quality | Best For |
|-------|------|--------------|-------|---------|----------|
| tinyllama | 637MB | 2GB | Very Fast | Basic | Testing, Learning |
| phi | 1.7GB | 4GB | Fast | Good | General use |
| mistral | 4.1GB | 6GB | Moderate | Very Good | Production |
| llama2 | 3.8GB | 6GB | Moderate | Very Good | General AI |
| codellama | 3.8GB | 6GB | Moderate | Excellent | Code tasks |

## Troubleshooting

### "Connection refused" error
```bash
# Check if Ollama is running
curl http://localhost:11434/api/tags

# Start Ollama if not running
ollama serve
```

### Model not found
```bash
# List installed models
ollama list

# Pull the model
ollama pull tinyllama
```

### Slow responses
- Use a smaller model (tinyllama instead of llama2)
- Close other applications
- Check CPU usage with `top` or `htop`

### GPU Acceleration (Optional)

Ollama automatically uses GPU if available:

**NVIDIA:**
```bash
# Check CUDA support
nvidia-smi

# Ollama will use it automatically
```

**Apple Silicon:**
- Metal acceleration is automatic on M1/M2/M3

**AMD:**
- ROCm support varies by card

## Docker Setup (Alternative)

```bash
# CPU only
docker run -d -v ollama:/root/.ollama -p 11434:11434 --name ollama ollama/ollama

# With GPU (NVIDIA)
docker run -d --gpus=all -v ollama:/root/.ollama -p 11434:11434 --name ollama ollama/ollama

# Pull model in container
docker exec -it ollama ollama pull tinyllama

# Use with agent
./guile-agent-v2 -b ollama
```

## Example Session

```bash
$ ./guile-agent-v2 -b ollama -m tinyllama

=== OLLAMA Backend ===
Agent initialized. Type 'quit' to exit.
Available tools: weather, calculator
==========================================

> What's 25 + 17?
Assistant: I'll help you calculate that. Let me use the calculator tool.
Result: 42

> What's the weather like?
Assistant: I'll check the weather for you using the weather tool.
The weather in San Francisco, CA is currently 22° C with partly cloudy skies.

> quit
Goodbye!
```

## Performance Tips

1. **Model Selection**: Start with tinyllama for testing
2. **Context Length**: Shorter conversations are faster
3. **Temperature**: Lower values (0.1-0.3) are more deterministic
4. **CPU Threads**: Ollama uses all available by default

## Advanced Configuration

Create `~/.ollama/config.json`:
```json
{
  "host": "0.0.0.0",
  "models_path": "/custom/path/to/models",
  "keep_alive": "5m",
  "num_threads": 8
}
```

## Integration with Guile Agent

The Ollama backend in Guile Agent:
- Automatically detects tool mentions in responses
- Supports all Ollama models
- Handles connection errors gracefully
- Works completely offline

## Resources

- [Ollama Documentation](https://github.com/ollama/ollama)
- [Model Library](https://ollama.com/library)
- [API Reference](https://github.com/ollama/ollama/blob/main/docs/api.md)
- [Discord Community](https://discord.gg/ollama)