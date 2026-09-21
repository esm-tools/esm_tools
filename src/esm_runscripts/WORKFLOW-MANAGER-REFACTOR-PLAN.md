# New workflow design

**Project:** ESM-Tools+
**WP**: 3
**Deliverable**: Closes D9

## Requirements

- Flexible, enabling offline coupling complex workflows such as AWIESM2/3-PISM
- Tail recursion (resubmission of the next batch script/s from the current one)
- Prepare experiment and first run preparation happens by default in the login node
- Cli to run plans (already supported in the current version of the WF manager)

## Design choices

- Separate workflow definition from execution infrastructure
- In batch scripts, one plan/one line (no hidden plans triggered by the previous plan)
- Plan clustering into batch scripts is done through labelling plan objects (clustering of plans into batch scripts is as detached as possible from the DAG)

## Overall design

Add here more comments
