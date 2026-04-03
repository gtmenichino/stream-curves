from __future__ import annotations

import argparse
import pathlib
from typing import Any

import pandas as pd
import yaml


ROOT = pathlib.Path(__file__).resolve().parents[1]


def load_yaml(path: pathlib.Path) -> dict[str, Any]:
    with path.open("r", encoding="utf-8") as handle:
        return yaml.safe_load(handle) or {}


def pipe_join(values: list[Any] | tuple[Any, ...] | None) -> str | None:
    if not values:
        return None
    cleaned = [str(value).strip() for value in values if value is not None and str(value).strip()]
    return "|".join(cleaned) if cleaned else None


def pairwise_join(values: list[list[Any]] | None) -> str | None:
    if not values:
        return None
    pairs: list[str] = []
    for pair in values:
        if not pair or len(pair) != 2:
            raise ValueError(f"Invalid pairwise entry: {pair!r}")
        left = str(pair[0]).strip()
        right = str(pair[1]).strip()
        if not left or not right:
            raise ValueError(f"Invalid pairwise entry: {pair!r}")
        pairs.append(f"{left}~{right}")
    return "|".join(pairs) if pairs else None


def auto_pairwise(levels: list[str]) -> list[list[str]]:
    pairs: list[list[str]] = []
    for idx, left in enumerate(levels):
        for right in levels[idx + 1 :]:
            pairs.append([left, right])
    return pairs


def derive_predictor_fields(predictor_key: str, predictor_cfg: dict[str, Any]) -> tuple[str, str | None, float | None]:
    if not predictor_cfg.get("derived", False):
        return "none", None, None

    derivation = str(predictor_cfg.get("derivation", "")).strip()
    source_columns = predictor_cfg.get("source_columns") or []
    if derivation == "DA_mi2 * 2.58999":
        return "multiply_by_constant", pipe_join(source_columns), 2.58999
    if derivation == "Slope_per / 100":
        return "divide_by_constant", pipe_join(source_columns), 100.0
    if derivation == "DA_km2 * Slope_unitless":
        return "multiply", pipe_join(source_columns), None
    if derivation == "L_Bufferwidth + R_Bufferwidth":
        return "sum", pipe_join(source_columns), None

    raise ValueError(
        f"Unsupported derivation for predictor '{predictor_key}': {derivation!r}. "
        "Add an explicit mapping before regenerating the workbook."
    )


def build_metrics_tables(metric_registry: dict[str, Any]) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    metrics_rows: list[dict[str, Any]] = []
    metric_predictor_rows: list[dict[str, Any]] = []
    metric_strat_rows: list[dict[str, Any]] = []

    for metric_key, metric_cfg in metric_registry.items():
        metrics_rows.append(
            {
                "metric_key": metric_key,
                "display_name": metric_cfg.get("display_name"),
                "column_name": metric_cfg.get("column_name"),
                "units": metric_cfg.get("units"),
                "metric_family": metric_cfg.get("metric_family"),
                "higher_is_better": metric_cfg.get("higher_is_better"),
                "monotonic_linear": metric_cfg.get("monotonic_linear"),
                "preferred_transform": metric_cfg.get("preferred_transform"),
                "min_sample_size": metric_cfg.get("min_sample_size"),
                "best_subsets_allowed": metric_cfg.get("best_subsets_allowed"),
                "count_model": metric_cfg.get("count_model"),
                "stratification_mode": metric_cfg.get("stratification_mode"),
                "include_in_summary": metric_cfg.get("include_in_summary"),
                "missing_data_rule": metric_cfg.get("missing_data_rule"),
                "notes": metric_cfg.get("notes"),
            }
        )

        for idx, predictor_key in enumerate(metric_cfg.get("allowed_predictors") or [], start=1):
            metric_predictor_rows.append(
                {
                    "metric_key": metric_key,
                    "predictor_key": predictor_key,
                    "sort_order": idx,
                }
            )

        for idx, strat_key in enumerate(metric_cfg.get("allowed_stratifications") or [], start=1):
            metric_strat_rows.append(
                {
                    "metric_key": metric_key,
                    "strat_key": strat_key,
                    "sort_order": idx,
                }
            )

    return (
        pd.DataFrame(metrics_rows),
        pd.DataFrame(metric_predictor_rows),
        pd.DataFrame(metric_strat_rows),
    )


def build_stratification_tables(strat_registry: dict[str, Any]) -> tuple[pd.DataFrame, pd.DataFrame]:
    strat_rows: list[dict[str, Any]] = []

    for strat_key, strat_cfg in strat_registry.items():
        strat_type = strat_cfg.get("type")
        if strat_type == "single":
            strat_rows.append(
                {
                    "strat_key": strat_key,
                    "display_name": strat_cfg.get("display_name"),
                    "strat_type": "raw_single",
                    "source_column": strat_cfg.get("column_name"),
                    "source_data_type": "categorical",
                    "primary_strat_key": None,
                    "secondary_strat_key": None,
                    "derived_column_name": None,
                    "levels": pipe_join(strat_cfg.get("levels")),
                    "pairwise_comparisons": pairwise_join(strat_cfg.get("pairwise_comparisons")),
                    "min_group_size": strat_cfg.get("min_group_size"),
                    "notes": strat_cfg.get("notes"),
                }
            )
            continue

        if strat_type == "paired":
            strat_rows.append(
                {
                    "strat_key": strat_key,
                    "display_name": strat_cfg.get("display_name"),
                    "strat_type": "paired",
                    "source_column": None,
                    "source_data_type": None,
                    "primary_strat_key": strat_cfg.get("primary"),
                    "secondary_strat_key": strat_cfg.get("secondary"),
                    "derived_column_name": None,
                    "levels": None,
                    "pairwise_comparisons": None,
                    "min_group_size": strat_cfg.get("min_group_size"),
                    "notes": strat_cfg.get("notes"),
                }
            )
            continue

        raise ValueError(f"Unsupported stratification type for '{strat_key}': {strat_type!r}")

    strat_groups = pd.DataFrame(
        columns=["strat_key", "group_label", "sort_order", "source_values", "rule_expression"]
    )
    return pd.DataFrame(strat_rows), strat_groups


def normalize_ecoregion_metadata(stratifications_df: pd.DataFrame, data_df: pd.DataFrame) -> pd.DataFrame:
    mask = stratifications_df["strat_key"] == "Ecoregion"
    if not mask.any():
        return stratifications_df

    levels = sorted(data_df["Ecoregion"].dropna().astype(str).str.strip().unique().tolist())
    stratifications_df.loc[mask, "levels"] = pipe_join(levels)
    stratifications_df.loc[mask, "pairwise_comparisons"] = pairwise_join(auto_pairwise(levels))
    stratifications_df.loc[mask, "notes"] = "Raw ecoregion values from source data"
    return stratifications_df


def append_custom_stratifications(
    stratifications_df: pd.DataFrame,
    strat_groups_df: pd.DataFrame,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    custom_strat_rows = pd.DataFrame(
        [
            {
                "strat_key": "Ecoregion_grouped",
                "display_name": "Ecoregion (ECBP/HELP vs IP)",
                "strat_type": "custom_group",
                "source_column": "Ecoregion",
                "source_data_type": "categorical",
                "primary_strat_key": None,
                "secondary_strat_key": None,
                "derived_column_name": "Ecoregion_grouped",
                "levels": "ECBP/HELP|IP",
                "pairwise_comparisons": "ECBP/HELP~IP",
                "min_group_size": 5,
                "notes": "Custom grouping that combines ECBP and HELP against IP",
            },
            {
                "strat_key": "Slope_per_grouped",
                "display_name": "Slope (%) <=1 vs >1",
                "strat_type": "custom_group",
                "source_column": "Slope_per",
                "source_data_type": "continuous",
                "primary_strat_key": None,
                "secondary_strat_key": None,
                "derived_column_name": "Slope_per_grouped",
                "levels": "<=1|>1",
                "pairwise_comparisons": "<=1~>1",
                "min_group_size": 5,
                "notes": "Custom grouping that bins slope into <=1 and >1",
            },
        ]
    )
    custom_group_rows = pd.DataFrame(
        [
            {
                "strat_key": "Ecoregion_grouped",
                "group_label": "ECBP/HELP",
                "sort_order": 1,
                "source_values": "ECBP|HELP",
                "rule_expression": None,
            },
            {
                "strat_key": "Ecoregion_grouped",
                "group_label": "IP",
                "sort_order": 2,
                "source_values": "IP",
                "rule_expression": None,
            },
            {
                "strat_key": "Slope_per_grouped",
                "group_label": "<=1",
                "sort_order": 1,
                "source_values": None,
                "rule_expression": "<= 1",
            },
            {
                "strat_key": "Slope_per_grouped",
                "group_label": ">1",
                "sort_order": 2,
                "source_values": None,
                "rule_expression": "> 1",
            },
        ]
    )

    stratifications_df = pd.concat([stratifications_df, custom_strat_rows], ignore_index=True)
    strat_groups_df = pd.concat([strat_groups_df, custom_group_rows], ignore_index=True)
    return stratifications_df, strat_groups_df


def append_custom_metric_links(metric_strat_df: pd.DataFrame) -> pd.DataFrame:
    new_rows: list[dict[str, Any]] = []
    for metric_key, group_df in metric_strat_df.groupby("metric_key", sort=False):
        rows = group_df.sort_values("sort_order")
        inserted: list[dict[str, Any]] = []
        sort_order = 1
        for strat_key in rows["strat_key"].tolist():
            inserted.append(
                {
                    "metric_key": metric_key,
                    "strat_key": strat_key,
                    "sort_order": sort_order,
                }
            )
            sort_order += 1

            if strat_key == "Ecoregion":
                inserted.append(
                    {
                        "metric_key": metric_key,
                        "strat_key": "Ecoregion_grouped",
                        "sort_order": sort_order,
                    }
                )
                sort_order += 1

            if strat_key == "DACAT":
                inserted.append(
                    {
                        "metric_key": metric_key,
                        "strat_key": "Slope_per_grouped",
                        "sort_order": sort_order,
                    }
                )
                sort_order += 1

        new_rows.extend(inserted)

    return pd.DataFrame(new_rows)


def build_predictors_table(predictor_registry: dict[str, Any]) -> pd.DataFrame:
    rows: list[dict[str, Any]] = []

    for predictor_key, predictor_cfg in predictor_registry.items():
        derivation_method, source_columns, constant = derive_predictor_fields(predictor_key, predictor_cfg)
        expected_range = predictor_cfg.get("expected_range") or [None, None]
        if len(expected_range) != 2:
            raise ValueError(
                f"Predictor '{predictor_key}' has invalid expected_range: {expected_range!r}"
            )

        rows.append(
            {
                "predictor_key": predictor_key,
                "display_name": predictor_cfg.get("display_name"),
                "column_name": predictor_cfg.get("column_name"),
                "type": predictor_cfg.get("type"),
                "derived": predictor_cfg.get("derived", False),
                "derivation_method": derivation_method,
                "source_columns": source_columns,
                "constant": constant,
                "expected_min": expected_range[0],
                "expected_max": expected_range[1],
                "missing_data_rule": predictor_cfg.get("missing_data_rule"),
                "notes": predictor_cfg.get("notes"),
            }
        )

    return pd.DataFrame(rows)


def build_factor_recodes_table(factor_recode_registry: dict[str, Any]) -> pd.DataFrame:
    rows: list[dict[str, Any]] = []

    for recode_key, recode_cfg in factor_recode_registry.items():
        collapse_map = recode_cfg.get("collapse_map") or {}
        for target_level, source_values in collapse_map.items():
            rows.append(
                {
                    "recode_key": recode_key,
                    "source_column": recode_cfg.get("source_column"),
                    "target_column": recode_cfg.get("target_column"),
                    "target_level": target_level,
                    "source_values": pipe_join(source_values),
                    "notes": recode_cfg.get("notes"),
                }
            )

    return pd.DataFrame(rows)


def build_site_mask_tables(data_df: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    label_column = data_df.columns[0] if len(data_df.columns) > 0 else "site_label"
    site_masks_df = pd.DataFrame(columns=["masked_sites", "site_label"])
    site_mask_settings_df = pd.DataFrame([{"site_label_column": label_column}])
    return site_masks_df, site_mask_settings_df


def write_workbook(
    data_df: pd.DataFrame,
    metrics_df: pd.DataFrame,
    metric_predictors_df: pd.DataFrame,
    metric_strat_df: pd.DataFrame,
    stratifications_df: pd.DataFrame,
    strat_groups_df: pd.DataFrame,
    predictors_df: pd.DataFrame,
    factor_recodes_df: pd.DataFrame,
    site_masks_df: pd.DataFrame,
    site_mask_settings_df: pd.DataFrame,
    output_path: pathlib.Path,
) -> None:
    output_path.parent.mkdir(parents=True, exist_ok=True)

    with pd.ExcelWriter(output_path, engine="openpyxl") as writer:
        data_df.to_excel(writer, sheet_name="data", index=False)
        metrics_df.to_excel(writer, sheet_name="metrics", index=False)
        metric_predictors_df.to_excel(writer, sheet_name="metric_predictors", index=False)
        metric_strat_df.to_excel(writer, sheet_name="metric_stratifications", index=False)
        stratifications_df.to_excel(writer, sheet_name="stratifications", index=False)
        strat_groups_df.to_excel(writer, sheet_name="strat_groups", index=False)
        predictors_df.to_excel(writer, sheet_name="predictors", index=False)
        factor_recodes_df.to_excel(writer, sheet_name="factor_recodes", index=False)
        site_masks_df.to_excel(writer, sheet_name="site_masks", index=False)
        site_mask_settings_df.to_excel(writer, sheet_name="site_mask_settings", index=False)


def migrate_to_workbook(
    csv_path: pathlib.Path,
    metric_registry_path: pathlib.Path,
    strat_registry_path: pathlib.Path,
    predictor_registry_path: pathlib.Path,
    factor_recode_registry_path: pathlib.Path,
    output_path: pathlib.Path,
) -> None:
    data_df = pd.read_csv(csv_path, keep_default_na=True, na_values=["NA"])
    metric_registry = load_yaml(metric_registry_path)
    strat_registry = load_yaml(strat_registry_path)
    predictor_registry = load_yaml(predictor_registry_path)
    factor_recode_registry = load_yaml(factor_recode_registry_path)

    metrics_df, metric_predictors_df, metric_strat_df = build_metrics_tables(metric_registry)
    stratifications_df, strat_groups_df = build_stratification_tables(strat_registry)
    stratifications_df = normalize_ecoregion_metadata(stratifications_df, data_df)
    stratifications_df, strat_groups_df = append_custom_stratifications(stratifications_df, strat_groups_df)
    metric_strat_df = append_custom_metric_links(metric_strat_df)
    predictors_df = build_predictors_table(predictor_registry)
    factor_recodes_df = build_factor_recodes_table(factor_recode_registry)
    site_masks_df, site_mask_settings_df = build_site_mask_tables(data_df)

    write_workbook(
        data_df=data_df,
        metrics_df=metrics_df,
        metric_predictors_df=metric_predictors_df,
        metric_strat_df=metric_strat_df,
        stratifications_df=stratifications_df,
        strat_groups_df=strat_groups_df,
        predictors_df=predictors_df,
        factor_recodes_df=factor_recodes_df,
        site_masks_df=site_masks_df,
        site_mask_settings_df=site_mask_settings_df,
        output_path=output_path,
    )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Convert the source CSV + YAML registries into a workbook input."
    )
    parser.add_argument(
        "--csv",
        dest="csv_path",
        default=str(ROOT / "path/to/source.csv"),
        help="Path to the legacy source CSV.",
    )
    parser.add_argument(
        "--metric-registry",
        dest="metric_registry_path",
        default=str(ROOT / "config" / "metric_registry.yaml"),
        help="Path to metric registry YAML.",
    )
    parser.add_argument(
        "--strat-registry",
        dest="strat_registry_path",
        default=str(ROOT / "config" / "stratification_registry.yaml"),
        help="Path to stratification registry YAML.",
    )
    parser.add_argument(
        "--predictor-registry",
        dest="predictor_registry_path",
        default=str(ROOT / "config" / "predictor_registry.yaml"),
        help="Path to predictor registry YAML.",
    )
    parser.add_argument(
        "--factor-recode-registry",
        dest="factor_recode_registry_path",
        default=str(ROOT / "config" / "factor_recode_registry.yaml"),
        help="Path to factor recode registry YAML.",
    )
    parser.add_argument(
        "--output",
        dest="output_path",
        default=str(ROOT / ".local/test_workbook.xlsx"),
        help="Path for the generated workbook.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    migrate_to_workbook(
        csv_path=pathlib.Path(args.csv_path),
        metric_registry_path=pathlib.Path(args.metric_registry_path),
        strat_registry_path=pathlib.Path(args.strat_registry_path),
        predictor_registry_path=pathlib.Path(args.predictor_registry_path),
        factor_recode_registry_path=pathlib.Path(args.factor_recode_registry_path),
        output_path=pathlib.Path(args.output_path),
    )
    print(f"Wrote workbook: {args.output_path}")


if __name__ == "__main__":
    main()
