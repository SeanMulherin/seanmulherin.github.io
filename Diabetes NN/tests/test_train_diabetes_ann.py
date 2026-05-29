import importlib.util
import unittest
from pathlib import Path

import numpy as np
import pandas as pd


MODULE_PATH = Path(__file__).resolve().parents[1] / "train_diabetes_ann.py"
SPEC = importlib.util.spec_from_file_location("train_diabetes_ann", MODULE_PATH)
train_diabetes_ann = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(train_diabetes_ann)


class DiabetesAnnTrainingTests(unittest.TestCase):
    def test_binary_target_marks_prediabetes_and_diabetes_as_high_risk(self):
        values = pd.Series([0.0, 1.0, 2.0])
        self.assertEqual(train_diabetes_ann.make_binary_target(values).tolist(), [0, 1, 1])

    def test_adult_age_to_cdc_category_boundaries(self):
        cases = {
            18: 1,
            24: 1,
            25: 2,
            44: 5,
            45: 6,
            64: 9,
            65: 10,
            79: 12,
            80: 13,
            100: 13,
        }
        for age, category in cases.items():
            with self.subTest(age=age):
                self.assertEqual(train_diabetes_ann.adult_age_to_cdc_category(age), category)

    def test_feature_order_matches_cdc_csv_order_without_target(self):
        expected = [
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
        self.assertEqual(train_diabetes_ann.FEATURE_COLUMNS, expected)

    def test_threshold_prefers_precision_while_meeting_sensitivity_target(self):
        y_true = np.array([0, 0, 0, 1, 1, 1])
        probabilities = np.array([0.05, 0.15, 0.35, 0.45, 0.8, 0.9])
        threshold = train_diabetes_ann.select_threshold(y_true, probabilities, sensitivity_target=0.65)
        metrics = train_diabetes_ann.metrics_at_threshold(y_true, probabilities, threshold["value"])
        self.assertGreaterEqual(metrics["sensitivity"], 0.65)
        self.assertEqual(threshold["strategy"], "max_precision_at_or_above_sensitivity_target")

    def test_exported_prediction_matches_manual_network_math(self):
        artifact = {
            "scaler": {"mean": [1.0, 2.0], "scale": [2.0, 4.0]},
            "network": {
                "coefs": [
                    [[0.5, -0.25], [0.25, 0.75]],
                    [[1.0], [-1.5]],
                ],
                "intercepts": [[0.1, -0.2], [0.05]],
            },
            "calibration": {"slope": 1.2, "intercept": -0.1, "epsilon": 1e-6},
        }
        features = [3.0, 6.0]
        probability = train_diabetes_ann.exported_predict_probability(features, artifact)

        scaled = np.array([[1.0, 1.0]])
        hidden = np.maximum(scaled @ np.array(artifact["network"]["coefs"][0]) + np.array([0.1, -0.2]), 0)
        raw = train_diabetes_ann.sigmoid(hidden @ np.array(artifact["network"]["coefs"][1]) + np.array([0.05]))
        expected = train_diabetes_ann.calibrate_probabilities(raw[0, 0], 1.2, -0.1)
        self.assertAlmostEqual(probability, expected, places=12)


if __name__ == "__main__":
    unittest.main()
