"""Export catalog data to Parquet or JSON for interoperability and batch imports."""

import json
from pathlib import Path

from loguru import logger


def export_parquet(items: list[dict], output: Path):
    """Write *items* as a Parquet file to *output*.

    Each row has columns: id, collection, experiment, datetime, bbox, data (JSON string).
    Used as the staging format for SLURM array job scan batches.
    """
    import pyarrow as pa
    import pyarrow.parquet as pq

    output = Path(output)
    output.parent.mkdir(parents=True, exist_ok=True)

    ids = []
    collections = []
    experiments = []
    datetimes = []
    bboxes = []
    data_jsons = []

    for item in items:
        props = item.get("properties", {})
        ids.append(item["id"])
        collections.append(item.get("collection"))
        experiments.append(props.get("experiment"))
        datetimes.append(props.get("datetime") or props.get("start_datetime"))
        bbox = item.get("bbox")
        bboxes.append(bbox if bbox else [None, None, None, None])
        data_jsons.append(json.dumps(item))

    table = pa.table({
        "id": pa.array(ids, type=pa.string()),
        "collection": pa.array(collections, type=pa.string()),
        "experiment": pa.array(experiments, type=pa.string()),
        "datetime": pa.array(datetimes, type=pa.string()),
        "bbox": pa.array(bboxes, type=pa.list_(pa.float64())),
        "data": pa.array(data_jsons, type=pa.string()),
    })
    pq.write_table(table, str(output))
    logger.info("Exported {} items to {}", len(items), output)


def export_json(items: list[dict], output: Path):
    """Write *items* as a GeoJSON FeatureCollection to *output*."""
    output = Path(output)
    output.parent.mkdir(parents=True, exist_ok=True)

    feature_collection = {
        "type": "FeatureCollection",
        "features": items,
    }
    output.write_text(json.dumps(feature_collection, indent=2, default=str))
    logger.info("Exported {} items to {}", len(items), output)


def import_parquet(db, parquet_files: list[Path]):
    """Read Parquet staging files and insert all items into *db* (CatalogDB).

    This is the serial merge step that follows parallel SLURM scan jobs.
    """
    for pq_path in parquet_files:
        pq_path = Path(pq_path)
        logger.info("Importing {}", pq_path)
        _import_one(db, pq_path)


def _import_one(db, pq_path: Path):
    import pyarrow.parquet as pq

    table = pq.read_table(str(pq_path))
    data_col = table.column("data")

    for i in range(len(data_col)):
        item = json.loads(data_col[i].as_py())
        collection_id = item.get("collection")
        if collection_id and not db.collection_exists(collection_id):
            logger.warning(
                "Collection '{}' not found when importing item '{}'; "
                "run scan first to create collections.",
                collection_id,
                item["id"],
            )
        db.insert_item(item)
        if collection_id:
            db.update_collection_extent(collection_id, item)
            db.upsert_collection_item_props(collection_id, item)
