# FESOM STAC Catalog Strategies Comparison

This document compares the three different catalog organization strategies available for FESOM data.

## Overview

| Strategy | Structure | Best For |
|----------|-----------|----------|
| **Single Collection** | Catalog → Collection → Items | Small datasets, multi-variable analysis |
| **Per-Variable Collections** | Catalog → Variable Collections → Items | Single-experiment, variable-specific analysis |
| **Multi-Level** | Root → Experiments → Variable Collections → Items | Multiple experiments, scalable organization |

---

## 1. Single Collection Strategy

### Structure
```
fesom-catalog/
└── fesom-collection/
    ├── ssh.fesom.185001.01
    ├── ssh.fesom.185002.01
    ├── sst.fesom.185001.01
    ├── sst.fesom.185002.01
    └── ...
```

### Builder
- **File**: `builder.py`
- **Example**: `example.py`
- **Reading**: `reading_intake.py`

### Advantages
- ✅ Simplest structure (only one collection)
- ✅ Single temporal extent view
- ✅ Easy multi-variable queries
- ✅ Minimal metadata duplication

### Disadvantages
- ❌ Requires filtering by variable name
- ❌ Slower for large catalogs
- ❌ Less intuitive organization
- ❌ Generic metadata only

### Use When
- Single experiment with few files (< 1000)
- Frequent multi-variable analysis
- Simple structure preferred
- All variables have same temporal coverage

---

## 2. Per-Variable Collections Strategy

### Structure
```
fesom-catalog/
├── ssh-collection/
│   ├── ssh.fesom.185001.01
│   ├── ssh.fesom.185002.01
│   └── ssh.fesom.185003.01
├── sst-collection/
│   ├── sst.fesom.185001.01
│   └── sst.fesom.185002.01
└── ...
```

### Builder
- **File**: `builder_per_variable_collection.py`
- **Example**: `example_per_variable.py`
- **Reading**: `reading_intake_per_variable.py`

### Advantages
- ✅ Direct variable access (no filtering)
- ✅ Variable-specific metadata
- ✅ Accurate temporal extents per variable
- ✅ Better performance for large catalogs
- ✅ Intuitive for domain scientists

### Disadvantages
- ❌ More collections to manage
- ❌ Harder multi-variable queries
- ❌ Spatial metadata duplication
- ❌ Can proliferate with many variables

### Use When
- Single experiment, many variables
- Variable-specific time series analysis
- Users know which variable they need
- Performance is important
- **← Recommended for typical FESOM workflows**

---

## 3. Multi-Level Hierarchical Strategy

### Structure
```
fesom-experiments/
├── awiesm-basic-001/
│   ├── ssh-collection/
│   │   └── ssh.fesom.185001.01
│   ├── sst-collection/
│   │   └── sst.fesom.185001.01
│   └── ...
├── awiesm-basic-002/
│   ├── ssh-collection/
│   │   └── ssh.fesom.185002.01
│   └── ...
└── ...
```

### Builder
- **File**: `builder_multi_level.py`
- **Example**: `example_multi_level.py`
- **Reading**: `reading_multi_level.py`

### Advantages
- ✅ Experiment-level organization
- ✅ Combines benefits of per-variable strategy
- ✅ Highly scalable (many experiments)
- ✅ Rich metadata at multiple levels
- ✅ Flexible querying patterns
- ✅ Team-friendly (experiment ownership)
- ✅ Easy to add new experiments

### Disadvantages
- ❌ Most complex structure (3 levels)
- ❌ Deeper nesting (longer paths)
- ❌ Harder cross-experiment queries
- ❌ More metadata duplication
- ❌ Steeper learning curve

### Use When
- Multiple experiments to organize
- Experiments are logically separate
- Users work with one experiment at a time
- Need experiment-level metadata
- Team collaboration on different experiments
- Long-term scalability important
- **← Recommended for multi-experiment projects**

---

## Quick Decision Guide

### Choose Single Collection if:
- You have < 1000 total files
- Single experiment only
- Frequently analyze multiple variables together
- Simplicity is paramount

### Choose Per-Variable Collections if:
- You have one experiment with many variables
- Typical analysis focuses on one variable at a time
- Performance matters (large catalogs)
- Users are familiar with the variable names
- **This is the sweet spot for most FESOM users**

### Choose Multi-Level if:
- You have multiple experiments (or plan to)
- Experiments have different configurations/time periods
- Need to organize and compare across experiments
- Team members focus on specific experiments
- Want maximum flexibility and scalability

---

## Code Examples

### Single Collection
```python
cat = intake.open_stac_catalog("catalog.json")
collection = cat['fesom-collection']

# Filter for SSH
ssh_files = [item._stac_obj.assets['data'].href 
             for item_id in collection 
             if item_id.startswith('ssh.fesom.')]
```

### Per-Variable Collections
```python
cat = intake.open_stac_catalog("catalog.json")

# Direct access to SSH collection
ssh_collection = cat['ssh-collection']
ssh_files = [item._stac_obj.assets['data'].href 
             for item_id in ssh_collection]
```

### Multi-Level
```python
root_cat = intake.open_stac_catalog("catalog.json")

# Navigate: Root → Experiment → Variable
exp_cat = root_cat['awiesm-basic-001']
ssh_collection = exp_cat['ssh-collection']
ssh_files = [item._stac_obj.assets['data'].href 
             for item_id in ssh_collection]
```

---

## Migration Path

If you start with one strategy and want to switch:

1. **Single → Per-Variable**: Rebuild catalog with `builder_per_variable_collection.py`
2. **Single → Multi-Level**: Rebuild with `builder_multi_level.py`, organize experiments
3. **Per-Variable → Multi-Level**: Rebuild with `builder_multi_level.py`, add experiment level

All strategies use the same underlying STAC specification, so data remains accessible.

---

## Performance Comparison

| Strategy | Catalog Load Time | Query Time | Scalability |
|----------|------------------|------------|-------------|
| Single Collection | Fast (1 collection) | Slow (filtering) | Poor (> 1000 items) |
| Per-Variable | Medium (N collections) | Fast (direct) | Good (< 50 variables) |
| Multi-Level | Slow (N×M catalogs) | Fast (direct) | Excellent (unlimited) |

---

## Recommendations by Use Case

### Research Group with Multiple Projects
→ **Multi-Level**: Organize by experiment, easy collaboration

### Single Long Simulation
→ **Per-Variable**: Best performance for variable-specific analysis

### Quick Prototype/Testing
→ **Single Collection**: Fastest to set up

### Production Data Archive
→ **Multi-Level**: Most scalable and maintainable

### Teaching/Demonstrations
→ **Per-Variable**: Most intuitive for new users

---

## Summary

All three strategies are valid STAC catalogs and work with standard tools. Choose based on:
- Number of experiments
- Number of variables
- Typical analysis patterns
- Team structure
- Long-term scalability needs

For most FESOM users working with a single experiment, **Per-Variable Collections** is recommended.
For projects with multiple experiments, **Multi-Level** provides the best organization and scalability.
