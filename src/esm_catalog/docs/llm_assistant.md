# LLM Assistant for ESM Catalog

A natural-language interface to the catalog, powered by a local LLM running on albedo GPU nodes.
Researchers can ask questions like *"what SSH data exists from the picontrol experiment?"*
or *"plot the global mean SSH over time"* without writing Python.

All data and model weights stay on-cluster — no external API calls are made.

---

## Architecture

```
Researcher (browser)
      ↓
Open WebUI  :3000       ← Apptainer (no Docker daemon needed)
      ↓  MCP stdio
esm-catalog mcp         ← this guide
      ↓  HTTP
STAC API  :23100        ← esm-catalog serve (existing)
      ↓
catalog.duckdb

Open WebUI also connects to:
Ollama  :11434          ← GPU SLURM job (qwen2.5:72b or llama3.3:70b)
```

---

## Step 1: Start Ollama on a GPU node

Submit a SLURM job that allocates a GPU and runs `ollama serve`.

Save as `slurm_ollama.sh`:

```bash
#!/bin/bash
#SBATCH --job-name=ollama
#SBATCH --partition=gpu
#SBATCH --gres=gpu:1
#SBATCH --time=08:00:00
#SBATCH --mem=64G
#SBATCH --output=ollama_%j.log

# Point Ollama at a writable model directory (quota-safe)
export OLLAMA_MODELS=$HOME/.ollama/models

ollama serve --host 0.0.0.0
```

Submit and note the assigned node name:

```bash
sbatch slurm_ollama.sh
# Watch the log until "Listening on 0.0.0.0:11434"
tail -f ollama_<JOBID>.log

# Note the node name
squeue -j <JOBID> -o "%N"   # e.g. gpu01
```

Pull the model (run once; weights are cached in `$OLLAMA_MODELS`):

```bash
# From the GPU node or via ssh
ollama pull qwen2.5:72b      # recommended — strong tool-use and coding
# Alternative smaller model:
# ollama pull llama3.3:70b
```

---

## Step 2: Start the STAC API

This is the existing `esm-catalog serve` command — skip if it is already running.

```bash
esm-catalog serve \
    --catalog ~/experiments/picontrol/catalog.duckdb \
    --port 23100
```

For multiple experiments, pass `--catalog` multiple times:

```bash
esm-catalog serve \
    --catalog ~/experiments/picontrol/catalog.duckdb \
    --catalog ~/experiments/historical/catalog.duckdb \
    --port 23100
```

---

## Step 3: Start the MCP server

```bash
pip install 'esm-catalog[mcp]'   # first time only

esm-catalog mcp --catalog-url http://localhost:23100
```

This runs in the foreground using stdio transport.  Keep it running in a terminal or tmux pane.

### Transport options

| Flag | Use case |
|---|---|
| `--transport stdio` | Default — Open WebUI launches it as a child process |
| `--transport sse --port 8001` | HTTP-based MCP clients |

---

## Step 4: Start Open WebUI

[Open WebUI](https://github.com/open-webui/open-webui) provides the browser-based chat interface.
On HPC systems without Docker daemon access, use Apptainer.

```bash
# Set GPU node hostname (from Step 1)
GPU_NODE=gpu01

apptainer run --nv \
    --env OLLAMA_BASE_URL=http://${GPU_NODE}:11434 \
    --env WEBUI_SECRET_KEY=$(openssl rand -hex 32) \
    --bind /tmp:/tmp \
    docker://ghcr.io/open-webui/open-webui:main
```

Open WebUI listens on port 3000.

### Register the MCP server in Open WebUI

1. Open **Settings → Tools → Add MCP Server**
2. Set **Name**: `ESM Catalog`
3. Set **Command**: `esm-catalog mcp --catalog-url http://localhost:23100`
4. Click **Save**

Open WebUI will launch the command as a child process and communicate via stdio.

---

## Step 5: Access from your laptop

The browser and STAC API run on albedo login or compute nodes inside the AWI network.
Forward the Open WebUI port to your laptop:

```bash
ssh -L 3000:localhost:3000 albedo0
```

Then open `http://localhost:3000` in your browser.

If you also want direct STAC API access (e.g. for pystac-client scripts):

```bash
ssh -L 3000:localhost:3000 -L 23100:localhost:23100 albedo0
```

---

## Available tools

The MCP server exposes four tools to the LLM:

| Tool | What it does |
|---|---|
| `list_collections` | List all experiment collections in the catalog |
| `get_collection_info` | Get variables, time range, spatial extent, item count for a collection |
| `search_items` | Find files by collection, variable, and date range; returns file paths |
| `run_python` | Execute Python with xarray/matplotlib; returns plot file paths |

---

## Example session

```
User:  "What experiments are in the catalog?"
LLM:   [calls list_collections]
       → "picontrol-fesom, picontrol-echam, historical-fesom"

User:  "How much SSH data is there in picontrol-fesom?"
LLM:   [calls get_collection_info("picontrol-fesom")]
       [calls search_items("picontrol-fesom", variable="ssh", limit=1)]
       → "1200 monthly files, 850–1850 CE"

User:  "Plot the global mean SSH for the first 10 years"
LLM:   [calls search_items("picontrol-fesom", variable="ssh",
                            start_date="0850-01-01", end_date="0860-12-31")]
       [calls run_python with xarray + matplotlib code]
       → displays plot inline in the chat
```

---

## Troubleshooting

### `esm-catalog mcp` not found

```bash
pip install 'esm-catalog[mcp]'
```

### Ollama connection refused

Check that the SLURM job is still running and Ollama is listening:

```bash
squeue -u $USER | grep ollama
ssh ${GPU_NODE} curl -s http://localhost:11434/api/tags
```

### MCP server cannot reach the STAC API

Verify the API is running and the URL is correct:

```bash
curl http://localhost:23100/collections
```

### Open WebUI cannot reach Ollama

Ensure `OLLAMA_BASE_URL` uses the node hostname (not `localhost`) since Open WebUI runs
in a container:

```bash
--env OLLAMA_BASE_URL=http://gpu01:11434   # correct
--env OLLAMA_BASE_URL=http://localhost:11434  # wrong inside container
```

---

## Model recommendations

| Model | Size | Notes |
|---|---|---|
| `qwen2.5:72b` | 72B | Best tool-use and code generation; recommended |
| `llama3.3:70b` | 70B | Strong alternative; similar capability |
| `qwen2.5:32b` | 32B | Faster; fits on a single A100 40GB |
| `qwen2.5-coder:32b` | 32B | Code-optimised; good for `run_python` heavy sessions |

Smaller models (7B–14B) can work but produce less reliable tool calls for complex queries.
