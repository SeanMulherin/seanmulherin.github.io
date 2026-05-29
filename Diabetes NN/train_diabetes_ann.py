#!/usr/bin/env python3
"""Train and export the diabetes-risk ANN used by neuralnetwork.html.

The exported JSON is intentionally framework-free so the static GitHub Pages
calculator can run the exact same preprocessing and network math in the browser.
"""

from __future__ import annotations

import argparse
import json
import math
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    accuracy_score,
    brier_score_loss,
    confusion_matrix,
    precision_score,
    recall_score,
    roc_auc_score,
)
from sklearn.model_selection import train_test_split
from sklearn.neural_network import MLPClassifier
from sklearn.preprocessing import StandardScaler


RANDOM_STATE = 42
SENSITIVITY_TARGET = 0.80
TARGET_COLUMN = "Diabetes_012"
FEATURE_COLUMNS = [
    "HighBP",
    "HighChol",
    "CholCheck",
    "BMI",
    "Smoker",
    "Stroke",
    "HeartDiseaseorAttack",
    "PhysActivity",
    "Fruits",
    "Veggies",
    "HvyAlcoholConsump",
    "AnyHealthcare",
    "NoDocbcCost",
    "GenHlth",
    "MentHlth",
    "PhysHlth",
    "DiffWalk",
    "Sex",
    "Age",
    "Education",
    "Income",
]


def make_binary_target(values: pd.Series | np.ndarray) -> np.ndarray:
    """Return 1 for prediabetes/diabetes and 0 for no diabetes."""
    return (np.asarray(values, dtype=float) > 0).astype(int)


def adult_age_to_cdc_category(age: int | float) -> int:
    """Map adult age to the CDC BRFSS 13-level age category used in the data."""
    age_value = int(age)
    if age_value < 18:
        raise ValueError("Age must be at least 18.")
    if age_value <= 24:
        return 1
    if age_value <= 29:
        return 2
    if age_value <= 34:
        return 3
    if age_value <= 39:
        return 4
    if age_value <= 44:
        return 5
    if age_value <= 49:
        return 6
    if age_value <= 54:
        return 7
    if age_value <= 59:
        return 8
    if age_value <= 64:
        return 9
    if age_value <= 69:
        return 10
    if age_value <= 74:
        return 11
    if age_value <= 79:
        return 12
    return 13


def load_training_data(csv_path: Path) -> tuple[pd.DataFrame, np.ndarray]:
    df = pd.read_csv(csv_path)
    missing = [column for column in [TARGET_COLUMN, *FEATURE_COLUMNS] if column not in df.columns]
    if missing:
        raise ValueError(f"Missing expected columns: {', '.join(missing)}")
    x = df[FEATURE_COLUMNS].astype(float)
    y = make_binary_target(df[TARGET_COLUMN])
    return x, y


def stratified_splits(
    x: pd.DataFrame,
    y: np.ndarray,
    random_state: int = RANDOM_STATE,
) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame, np.ndarray, np.ndarray, np.ndarray]:
    x_train_calib, x_test, y_train_calib, y_test = train_test_split(
        x,
        y,
        test_size=0.20,
        stratify=y,
        random_state=random_state,
    )
    x_train, x_calib, y_train, y_calib = train_test_split(
        x_train_calib,
        y_train_calib,
        test_size=0.25,
        stratify=y_train_calib,
        random_state=random_state,
    )
    return x_train, x_calib, x_test, y_train, y_calib, y_test


def oversample_minority(
    x: np.ndarray,
    y: np.ndarray,
    random_state: int = RANDOM_STATE,
) -> tuple[np.ndarray, np.ndarray]:
    """Balance the training split by oversampling the minority class only."""
    rng = np.random.default_rng(random_state)
    classes, counts = np.unique(y, return_counts=True)
    if len(classes) != 2:
        raise ValueError("Expected binary classes for oversampling.")

    max_count = int(counts.max())
    sampled_indices: list[np.ndarray] = []
    for klass in classes:
        class_indices = np.flatnonzero(y == klass)
        replace = len(class_indices) < max_count
        sampled_indices.append(rng.choice(class_indices, size=max_count, replace=replace))

    indices = np.concatenate(sampled_indices)
    rng.shuffle(indices)
    return x[indices], y[indices]


def sigmoid(values: np.ndarray | float) -> np.ndarray | float:
    clipped = np.clip(values, -709, 709)
    return 1.0 / (1.0 + np.exp(-clipped))


def logit(probabilities: np.ndarray) -> np.ndarray:
    clipped = np.clip(probabilities, 1e-6, 1.0 - 1e-6)
    return np.log(clipped / (1.0 - clipped))


def calibrate_probabilities(
    uncalibrated_probabilities: np.ndarray,
    slope: float,
    intercept: float,
) -> np.ndarray:
    return sigmoid(slope * logit(uncalibrated_probabilities) + intercept)


def fit_platt_scaler(uncalibrated_probabilities: np.ndarray, y_true: np.ndarray) -> tuple[float, float]:
    features = logit(uncalibrated_probabilities).reshape(-1, 1)
    model = LogisticRegression(solver="lbfgs", random_state=RANDOM_STATE)
    model.fit(features, y_true)
    return float(model.coef_[0][0]), float(model.intercept_[0])


def metrics_at_threshold(
    y_true: np.ndarray,
    probabilities: np.ndarray,
    threshold: float,
) -> dict[str, Any]:
    predictions = (probabilities >= threshold).astype(int)
    tn, fp, fn, tp = confusion_matrix(y_true, predictions, labels=[0, 1]).ravel()
    specificity = tn / (tn + fp) if (tn + fp) else 0.0
    return {
        "threshold": round_float(threshold),
        "accuracy": round_float(accuracy_score(y_true, predictions)),
        "sensitivity": round_float(recall_score(y_true, predictions, zero_division=0)),
        "recall": round_float(recall_score(y_true, predictions, zero_division=0)),
        "precision": round_float(precision_score(y_true, predictions, zero_division=0)),
        "specificity": round_float(specificity),
        "confusion_matrix": {
            "tn": int(tn),
            "fp": int(fp),
            "fn": int(fn),
            "tp": int(tp),
        },
    }


def select_threshold(
    y_true: np.ndarray,
    probabilities: np.ndarray,
    sensitivity_target: float = SENSITIVITY_TARGET,
) -> dict[str, Any]:
    thresholds = np.unique(np.concatenate(([0.0, 1.0], probabilities)))
    candidates: list[tuple[float, float, float, float]] = []
    fallback: list[tuple[float, float, float, float]] = []

    for threshold in thresholds:
        predictions = (probabilities >= threshold).astype(int)
        precision = precision_score(y_true, predictions, zero_division=0)
        sensitivity = recall_score(y_true, predictions, zero_division=0)
        beta = 2.0
        f2 = (
            (1 + beta**2) * precision * sensitivity / ((beta**2 * precision) + sensitivity)
            if precision + sensitivity
            else 0.0
        )
        fallback.append((f2, precision, sensitivity, float(threshold)))
        if sensitivity >= sensitivity_target:
            candidates.append((precision, sensitivity, f2, float(threshold)))

    if candidates:
        precision, sensitivity, f2, threshold = max(candidates, key=lambda item: (item[0], item[1], item[2]))
        strategy = "max_precision_at_or_above_sensitivity_target"
    else:
        f2, precision, sensitivity, threshold = max(fallback, key=lambda item: item[0])
        strategy = "best_f2_fallback_below_sensitivity_target"

    return {
        "value": round_float(threshold),
        "strategy": strategy,
        "sensitivity_target": sensitivity_target,
        "calibration_split_sensitivity": round_float(sensitivity),
        "calibration_split_precision": round_float(precision),
        "calibration_split_f2": round_float(f2),
    }


def exported_predict_probability(features: list[float], artifact: dict[str, Any]) -> float:
    values = np.asarray(features, dtype=float)
    mean = np.asarray(artifact["scaler"]["mean"], dtype=float)
    scale = np.asarray(artifact["scaler"]["scale"], dtype=float)
    activations = ((values - mean) / scale).reshape(1, -1)

    for index, (coefs, intercepts) in enumerate(
        zip(artifact["network"]["coefs"], artifact["network"]["intercepts"])
    ):
        activations = activations @ np.asarray(coefs, dtype=float) + np.asarray(intercepts, dtype=float)
        is_output_layer = index == len(artifact["network"]["coefs"]) - 1
        if is_output_layer:
            activations = sigmoid(activations)
        else:
            activations = np.maximum(activations, 0.0)

    raw_probability = float(activations[0, 0])
    calibration = artifact["calibration"]
    return float(calibrate_probabilities(raw_probability, calibration["slope"], calibration["intercept"]))


def round_float(value: float, digits: int = 6) -> float:
    if isinstance(value, (np.floating, np.integer)):
        value = float(value)
    if math.isnan(value) or math.isinf(value):
        return value
    return round(float(value), digits)


def build_artifact(
    scaler: StandardScaler,
    model: MLPClassifier,
    calibration: tuple[float, float],
    threshold: dict[str, Any],
    metrics: dict[str, Any],
) -> dict[str, Any]:
    slope, intercept = calibration
    return {
        "version": 1,
        "model_name": "CDC BRFSS binary diabetes-risk ANN",
        "created_by": "Diabetes NN/train_diabetes_ann.py",
        "random_state": RANDOM_STATE,
        "target": "Diabetes_012 > 0",
        "positive_class": "high risk",
        "negative_class": "low risk",
        "feature_columns": FEATURE_COLUMNS,
        "age_mapping": [
            {"category": 1, "range": "18-24"},
            {"category": 2, "range": "25-29"},
            {"category": 3, "range": "30-34"},
            {"category": 4, "range": "35-39"},
            {"category": 5, "range": "40-44"},
            {"category": 6, "range": "45-49"},
            {"category": 7, "range": "50-54"},
            {"category": 8, "range": "55-59"},
            {"category": 9, "range": "60-64"},
            {"category": 10, "range": "65-69"},
            {"category": 11, "range": "70-74"},
            {"category": 12, "range": "75-79"},
            {"category": 13, "range": "80+"},
        ],
        "scaler": {
            "mean": [round_float(value, 10) for value in scaler.mean_],
            "scale": [round_float(value, 10) for value in scaler.scale_],
        },
        "network": {
            "type": "MLPClassifier",
            "activation": "relu",
            "out_activation": "logistic",
            "hidden_layer_sizes": list(model.hidden_layer_sizes),
            "coefs": [
                [[round_float(value, 10) for value in row] for row in matrix.tolist()]
                for matrix in model.coefs_
            ],
            "intercepts": [
                [round_float(value, 10) for value in vector.tolist()]
                for vector in model.intercepts_
            ],
        },
        "calibration": {
            "method": "platt_logistic_on_uncalibrated_logit",
            "slope": round_float(slope, 10),
            "intercept": round_float(intercept, 10),
            "epsilon": 1e-6,
        },
        "threshold": threshold,
        "metrics": metrics,
    }


def train_and_export(
    csv_path: Path,
    output_path: Path,
    metrics_output_path: Path | None = None,
    max_iter: int = 45,
) -> dict[str, Any]:
    x, y = load_training_data(csv_path)
    x_train, x_calib, x_test, y_train, y_calib, y_test = stratified_splits(x, y)

    scaler = StandardScaler()
    x_train_scaled = scaler.fit_transform(x_train)
    x_calib_scaled = scaler.transform(x_calib)
    x_test_scaled = scaler.transform(x_test)

    x_train_balanced, y_train_balanced = oversample_minority(x_train_scaled, y_train)
    model = MLPClassifier(
        hidden_layer_sizes=(12, 6),
        activation="relu",
        solver="adam",
        alpha=0.0005,
        batch_size=2048,
        learning_rate_init=0.001,
        max_iter=max_iter,
        early_stopping=True,
        validation_fraction=0.12,
        n_iter_no_change=12,
        random_state=RANDOM_STATE,
    )
    model.fit(x_train_balanced, y_train_balanced)

    calib_raw = model.predict_proba(x_calib_scaled)[:, 1]
    slope, intercept = fit_platt_scaler(calib_raw, y_calib)
    calib_prob = calibrate_probabilities(calib_raw, slope, intercept)
    threshold = select_threshold(y_calib, calib_prob)

    test_raw = model.predict_proba(x_test_scaled)[:, 1]
    test_prob = calibrate_probabilities(test_raw, slope, intercept)
    baseline_accuracy = max(float(np.mean(y_test == 0)), float(np.mean(y_test == 1)))
    test_metrics = metrics_at_threshold(y_test, test_prob, threshold["value"])
    test_metrics.update(
        {
            "roc_auc": round_float(roc_auc_score(y_test, test_prob)),
            "brier_score": round_float(brier_score_loss(y_test, test_prob)),
            "baseline_accuracy": round_float(baseline_accuracy),
        }
    )

    calibration_metrics = metrics_at_threshold(y_calib, calib_prob, threshold["value"])
    calibration_metrics.update(
        {
            "roc_auc": round_float(roc_auc_score(y_calib, calib_prob)),
            "brier_score": round_float(brier_score_loss(y_calib, calib_prob)),
        }
    )

    metrics = {
        "split_sizes": {
            "train": int(len(y_train)),
            "calibration": int(len(y_calib)),
            "test": int(len(y_test)),
        },
        "class_balance": {
            "overall_high_risk_rate": round_float(float(np.mean(y))),
            "train_high_risk_rate": round_float(float(np.mean(y_train))),
            "calibration_high_risk_rate": round_float(float(np.mean(y_calib))),
            "test_high_risk_rate": round_float(float(np.mean(y_test))),
            "oversampled_train_high_risk_rate": round_float(float(np.mean(y_train_balanced))),
        },
        "calibration": calibration_metrics,
        "test": test_metrics,
        "model_training": {
            "n_iter": int(model.n_iter_),
            "loss": round_float(float(model.loss_)),
        },
    }

    artifact = build_artifact(scaler, model, (slope, intercept), threshold, metrics)
    output_path.write_text(json.dumps(artifact, indent=2), encoding="utf-8")
    if metrics_output_path:
        metrics_output_path.write_text(json.dumps(metrics, indent=2), encoding="utf-8")
    return artifact


def parse_args() -> argparse.Namespace:
    base_dir = Path(__file__).resolve().parent
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--data", type=Path, default=base_dir / "diabetes.csv")
    parser.add_argument("--output", type=Path, default=base_dir / "diabetes_ann_model.json")
    parser.add_argument("--metrics-output", type=Path, default=base_dir / "diabetes_ann_metrics.json")
    parser.add_argument("--max-iter", type=int, default=45)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    artifact = train_and_export(args.data, args.output, args.metrics_output, args.max_iter)
    test = artifact["metrics"]["test"]
    matrix = test["confusion_matrix"]
    print(f"Wrote {args.output}")
    print(f"Wrote {args.metrics_output}")
    print(
        "Test metrics: "
        f"accuracy={test['accuracy']}, "
        f"sensitivity={test['sensitivity']}, "
        f"precision={test['precision']}, "
        f"specificity={test['specificity']}, "
        f"roc_auc={test['roc_auc']}, "
        f"brier={test['brier_score']}, "
        f"baseline_accuracy={test['baseline_accuracy']}"
    )
    print(f"Confusion matrix: TN={matrix['tn']} FP={matrix['fp']} FN={matrix['fn']} TP={matrix['tp']}")


if __name__ == "__main__":
    main()
