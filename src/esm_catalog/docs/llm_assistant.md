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
STAC API  :23006        ← esm-catalog serve (already running)
      ↓
catalog.duckdb

Open WebUI also connects to one of:
vLLM   :8000   (OpenAI-compatible)  ← recommended: models pre-downloaded
Ollama :11434                       ← alternative: pull models on first use
```

The MCP server code is LLM-agnostic — the same `esm-catalog mcp` command works with both backends.

---

## Step 1: Start the LLM backend on a GPU node

Choose one backend. **vLLM is recommended** — model weights are already downloaded to
`/albedo/work/projects/paleo_work/esm-catalog/llm-models`.

### Option A: vLLM (recommended — models pre-downloaded)

```bash
MODELS_DIR=/albedo/work/projects/paleo_work/esm-catalog/llm-models

# Start Qwen 2.5 72B AWQ (default)
bash ${MODELS_DIR}/start_qwen.sh

# Or start Llama 3.3 70B AWQ
sbatch --export=MODEL=llama ${MODELS_DIR}/serve-llm.slurm
```

Watch the log until the server is ready:

```bash
# Find the job ID
squeue -u $USER | grep vllm-serve

# Tail the log (path printed by sbatch)
tail -f /albedo/work/projects/paleo_work/esm-catalog/llm-models/serve-vllm-serve-<JOBID>.log
# Ready when you see: "Application startup complete."

# Note the GPU node name
squeue -j <JOBID> -o "%N"   # e.g. gpu01
```

vLLM exposes an **OpenAI-compatible API** at `http://<node>:8000/v1`.

Verify it is up:

```bash
GPU_NODE=gpu01
curl -s http://${GPU_NODE}:8000/v1/models | python3 -m json.tool
```

### Option B: Ollama (alternative — requires model download on first use)

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

export OLLAMA_MODELS=$HOME/.ollama/models
ollama serve --host 0.0.0.0
```

```bash
sbatch slurm_ollama.sh
tail -f ollama_<JOBID>.log   # wait for "Listening on 0.0.0.0:11434"
squeue -j <JOBID> -o "%N"    # note GPU node name

# Pull model once (weights cached in $OLLAMA_MODELS)
ollama pull qwen2.5:72b
```

---

## Step 2: Verify the STAC API is reachable

The STAC API is already running at port **23006**. Confirm it responds:

```bash
curl -s http://localhost:23006/collections | python3 -m json.tool
```

If it is not running, start it:

```bash
esm-catalog serve \
    --catalog ~/experiments/picontrol/catalog.duckdb \
    --port 23006
```

---

## Step 3: Start the MCP server

```bash
pip install 'esm-catalog[mcp]'   # first time only

esm-catalog mcp --catalog-url http://localhost:23006
```

This runs in the foreground using stdio transport. Keep it running in a terminal or tmux pane.
Open WebUI will launch it automatically as a child process once registered (see Step 4).

### Transport options

| Flag | Use case |
|---|---|
| `--transport stdio` | Default — Open WebUI launches it as a child process |
| `--transport sse --port 8001` | HTTP-based MCP clients |

---

## Step 4: Start Open WebUI

[Open WebUI](https://github.com/open-webui/open-webui) provides the browser-based chat interface.
On HPC systems without a Docker daemon, use Apptainer.

### With vLLM (Option A)

Pull the image once to the shared work directory (avoids home quota):

```bash
module load apptainer
apptainer pull \
    /albedo/work/projects/paleo_work/esm-catalog/containers/open-webui.sif \
    docker://ghcr.io/open-webui/open-webui:main
```

Create a persistent data directory and launch (Open WebUI listens on port **8080**):

```bash
GPU_NODE=gpu-005   # node from Step 1
mkdir -p /albedo/work/projects/paleo_work/esm-catalog/containers/open-webui-data

module load apptainer
apptainer exec \
    --pwd /app/backend \
    --env OPENAI_API_BASE_URL=http://${GPU_NODE}:8000/v1 \
    --env OPENAI_API_KEY=not-used \
    --env WEBUI_SECRET_KEY=$(openssl rand -hex 32) \
    --env DATA_DIR=/data \
    --bind /albedo/work/projects/paleo_work/esm-catalog/containers/open-webui-data:/data \
    --bind /tmp:/tmp \
    /albedo/work/projects/paleo_work/esm-catalog/containers/open-webui.sif \
    bash start.sh
```

Notes:
- `--pwd /app/backend` is required — Apptainer does not inherit Docker's `WORKDIR`.
- Read-only filesystem errors for static files on startup are cosmetic and can be ignored.
- The image is already pulled to the path above on albedo.

In Open WebUI, add the connection manually if it is not auto-detected:

1. **Settings → Connections → OpenAI API**
2. Set **API Base URL**: `http://<GPU_NODE>:8000/v1`
3. Set **API Key**: `not-used` (any non-empty value)
4. Click **Save** — the model `qwen2.5-72b` or `llama3.3-70b` should appear in the model selector.

### With Ollama (Option B)

```bash
GPU_NODE=gpu01
mkdir -p /albedo/work/projects/paleo_work/esm-catalog/containers/open-webui-data

module load apptainer
apptainer exec \
    --pwd /app/backend \
    --env OLLAMA_BASE_URL=http://${GPU_NODE}:11434 \
    --env WEBUI_SECRET_KEY=$(openssl rand -hex 32) \
    --env DATA_DIR=/data \
    --bind /albedo/work/projects/paleo_work/esm-catalog/containers/open-webui-data:/data \
    --bind /tmp:/tmp \
    /albedo/work/projects/paleo_work/esm-catalog/containers/open-webui.sif \
    bash start.sh
```

Open WebUI listens on port **8080** in both cases.

### Register the MCP server in Open WebUI

Open WebUI's **Manage Tool Servers** expects an OpenAPI-compatible HTTP endpoint, not a
stdio command. Use `mcpo` as a bridge:

```bash
pip install mcpo   # first time only
mcpo --port 8001 -- esm-catalog mcp --catalog-url http://localhost:23006
```

This exposes the MCP tools as an OpenAPI server at `http://localhost:8001`.
Keep it running in a separate terminal or tmux pane.

Then in Open WebUI:

1. Open **Settings → Integrations → Manage Tool Servers → + (Add)**
2. Set **Name**: `ESM Catalog`
3. Set **URL**: `http://localhost:8001`
4. Leave **OpenAPI Spec** as `URL → openapi.json` (default)
5. Click **Save** — the connection check should pass

In each chat, click the **+** icon in the message input bar and enable **ESM Catalog**
before sending your first message.

---

## Step 5: Access from your laptop

The STAC browser, STAC API, and Open WebUI run on albedo nodes inside the AWI network.
Forward all relevant ports to your laptop in one SSH command:

```bash
# Open WebUI only
ssh -L 8080:localhost:8080 albedo0

# Open WebUI + STAC API + STAC browser
ssh -L 8080:localhost:8080 -L 23006:localhost:23006 -L 23005:localhost:23005 albedo0
```

Then open:

| Service | URL |
|---|---|
| Chat (Open WebUI) | `http://localhost:8080` |
| STAC Browser | `http://localhost:23005` |
| STAC API | `http://localhost:23006` |

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

### vLLM not responding

```bash
# Check the job is still running
squeue -u $USER | grep vllm-serve

# Check the log for errors
tail -50 /albedo/work/projects/paleo_work/esm-catalog/llm-models/serve-vllm-serve-<JOBID>.log

# Test the API directly from the login node
curl -s http://${GPU_NODE}:8000/v1/models
```

### Ollama connection refused

```bash
squeue -u $USER | grep ollama
ssh ${GPU_NODE} curl -s http://localhost:11434/api/tags
```

### Open WebUI cannot reach the LLM

Ensure the URL uses the GPU node hostname, not `localhost`, since Open WebUI runs in a container:

```bash
# vLLM — correct
--env OPENAI_API_BASE_URL=http://gpu01:8000/v1

# Ollama — correct
--env OLLAMA_BASE_URL=http://gpu01:11434

# Wrong in both cases (resolves to the container itself)
--env OPENAI_API_BASE_URL=http://localhost:8000/v1
```

### MCP server cannot reach the STAC API

```bash
curl -s http://localhost:23006/collections
```

### Open WebUI shows no model after restarting vLLM

Each SLURM job may land on a different GPU node. When restarting vLLM, check the new node:

```bash
squeue -u $USER | grep vllm-serve   # note the NODELIST column, e.g. gpu-003
```

Then restart Open WebUI with the updated `OPENAI_API_BASE_URL`:

```bash
--env OPENAI_API_BASE_URL=http://gpu-003:8000/v1
```

### Model not appearing in Open WebUI (vLLM)

The model name in the selector matches `--served-model-name` in `serve-llm.slurm`
(`qwen2.5-72b` or `llama3.3-70b`). If it does not appear, refresh the connections page
or restart Open WebUI after the vLLM server is fully up.

---

## Model recommendations

### vLLM (pre-downloaded at `/albedo/work/projects/paleo_work/esm-catalog/llm-models`)

| Model | Served name | Notes |
|---|---|---|
| Qwen 2.5 72B AWQ | `qwen2.5-72b` | Best tool-use and code generation; default |
| Llama 3.3 70B AWQ | `llama3.3-70b` | Strong alternative |

Start with `bash start_qwen.sh` (Qwen) or `sbatch --export=MODEL=llama serve-llm.slurm` (Llama).

**Quantization note:** the slurm script uses `--quantization awq_marlin`, which automatically
converts AWQ weights to the faster Marlin kernel at load time. This gives significantly higher
token throughput on A100s compared to plain `--quantization awq`. Do not change it back to `awq`.

### Ollama (pulled on demand)

| Model | Pull command | Notes |
|---|---|---|
| `qwen2.5:72b` | `ollama pull qwen2.5:72b` | Recommended |
| `llama3.3:70b` | `ollama pull llama3.3:70b` | Strong alternative |
| `qwen2.5:32b` | `ollama pull qwen2.5:32b` | Faster; fits A100 40GB |

Smaller models (7B–14B) can work but produce less reliable tool calls for complex queries.
