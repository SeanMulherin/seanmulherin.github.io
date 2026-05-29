(function () {
  "use strict";

  const MODEL_URL = "Diabetes%20NN/diabetes_ann_model.json";
  const BINARY_CONTROLS = {
    HighBP: { name: "hyp", yesId: "hypY" },
    HighChol: { name: "Hchol", yesId: "HcholY" },
    CholCheck: { name: "Cchol", yesId: "CcholY" },
    Smoker: { name: "smoke", yesId: "smokeY" },
    Stroke: { name: "stroke", yesId: "strokeY" },
    HeartDiseaseorAttack: { name: "heart", yesId: "heartY" },
    PhysActivity: { name: "physAt", yesId: "physAtY" },
    Fruits: { name: "fru", yesId: "fruY" },
    Veggies: { name: "veg", yesId: "vegY" },
    HvyAlcoholConsump: { name: "alc", yesId: "alcY" },
    AnyHealthcare: { name: "cov", yesId: "covY" },
    NoDocbcCost: { name: "cost", yesId: "costY" },
    DiffWalk: { name: "walk", yesId: "walkY" },
    Sex: { name: "sex", yesId: "male" }
  };

  let modelArtifact = null;

  function sigmoid(value) {
    const clipped = Math.max(-709, Math.min(709, value));
    return 1 / (1 + Math.exp(-clipped));
  }

  function logit(probability) {
    const epsilon = modelArtifact?.calibration?.epsilon || 1e-6;
    const clipped = Math.max(epsilon, Math.min(1 - epsilon, probability));
    return Math.log(clipped / (1 - clipped));
  }

  function dotLayer(inputs, weights, intercepts) {
    const output = new Array(intercepts.length).fill(0);
    for (let j = 0; j < intercepts.length; j += 1) {
      let sum = intercepts[j];
      for (let i = 0; i < inputs.length; i += 1) {
        sum += inputs[i] * weights[i][j];
      }
      output[j] = sum;
    }
    return output;
  }

  function adultAgeToCdcCategory(age) {
    if (age < 18) throw new Error("Age must be at least 18.");
    if (age <= 24) return 1;
    if (age <= 29) return 2;
    if (age <= 34) return 3;
    if (age <= 39) return 4;
    if (age <= 44) return 5;
    if (age <= 49) return 6;
    if (age <= 54) return 7;
    if (age <= 59) return 8;
    if (age <= 64) return 9;
    if (age <= 69) return 10;
    if (age <= 74) return 11;
    if (age <= 79) return 12;
    return 13;
  }

  function selectedBinaryValue(featureName) {
    const control = BINARY_CONTROLS[featureName];
    const selected = document.querySelector(`input[name="${control.name}"]:checked`);
    if (!selected) {
      throw new Error("Please answer every yes/no question before submitting.");
    }
    return selected.id === control.yesId ? 1 : 0;
  }

  function numericFieldValue(id, label, min, max) {
    const field = document.getElementById(id);
    const value = Number(field?.value);
    if (!Number.isFinite(value) || value < min || value > max) {
      throw new Error(`${label} must be between ${min} and ${max}.`);
    }
    return value;
  }

  function collectFeatures() {
    const featureMap = {};
    Object.keys(BINARY_CONTROLS).forEach((name) => {
      featureMap[name] = selectedBinaryValue(name);
    });

    featureMap.BMI = numericFieldValue("bmi", "BMI", 12, 98);
    featureMap.GenHlth = numericFieldValue("gen_health", "General health", 1, 5);
    featureMap.MentHlth = numericFieldValue("men_health", "Mental health days", 0, 30);
    featureMap.PhysHlth = numericFieldValue("phys_ill", "Physical health days", 0, 30);
    featureMap.Age = adultAgeToCdcCategory(numericFieldValue("age", "Age", 18, 100));
    featureMap.Education = numericFieldValue("edu", "Education", 1, 6);
    featureMap.Income = numericFieldValue("income", "Income", 1, 8);

    return modelArtifact.feature_columns.map((column) => featureMap[column]);
  }

  function predictProbability(features) {
    const mean = modelArtifact.scaler.mean;
    const scale = modelArtifact.scaler.scale;
    let activations = features.map((value, index) => (value - mean[index]) / scale[index]);

    modelArtifact.network.coefs.forEach((weights, index) => {
      const intercepts = modelArtifact.network.intercepts[index];
      activations = dotLayer(activations, weights, intercepts);
      const isOutputLayer = index === modelArtifact.network.coefs.length - 1;
      activations = isOutputLayer
        ? activations.map(sigmoid)
        : activations.map((value) => Math.max(0, value));
    });

    const rawProbability = activations[0];
    const calibrated = sigmoid(
      modelArtifact.calibration.slope * logit(rawProbability) + modelArtifact.calibration.intercept
    );
    return calibrated;
  }

  function formatPercent(value) {
    return `${(100 * value).toFixed(1)}%`;
  }

  function renderResult(message, isError) {
    const output = document.getElementById("output");
    output.className = isError ? "calculator-output calculator-output-error" : "calculator-output";
    output.innerHTML = message;
  }

  async function loadModel() {
    const response = await fetch(MODEL_URL);
    if (!response.ok) {
      throw new Error("The diabetes model could not be loaded.");
    }
    modelArtifact = await response.json();
  }

  function updateSliderValue(id) {
    const input = document.getElementById(id);
    const output = document.getElementById(`${id}V`);
    if (!input || !output) return;
    const update = () => {
      output.textContent = input.value;
    };
    input.addEventListener("input", update);
    update();
  }

  function onSubmit(event) {
    event.preventDefault();
    try {
      if (!modelArtifact) {
        throw new Error("The model is still loading. Try again in a moment.");
      }
      const probability = predictProbability(collectFeatures());
      const highRisk = probability >= modelArtifact.threshold.value;
      const label = highRisk ? "high" : "low";
      renderResult(
        `Estimated calibrated high-risk probability: <strong>${formatPercent(probability)}</strong>. ` +
          `Using the model's sensitivity-first threshold of ` +
          `<strong>${formatPercent(modelArtifact.threshold.value)}</strong>, this profile is classified as ` +
          `<strong>${label} risk</strong>.`,
        false
      );
    } catch (error) {
      renderResult(error.message, true);
    }
  }

  document.addEventListener("DOMContentLoaded", async () => {
    ["bmi", "income", "edu", "phys_ill", "gen_health", "men_health", "age"].forEach(updateSliderValue);
    document.getElementById("diabetesCalculator")?.addEventListener("submit", onSubmit);
    try {
      await loadModel();
    } catch (error) {
      renderResult(error.message, true);
    }
  });
})();
