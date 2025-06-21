# Predicting Cellulose Crystallinity Using IR Spectroscopy

## Overview
This project implements a machine learning-based approach for predicting the crystallinity index (CrI) of cellulose materials using infrared (IR) spectroscopy. The proposed framework demonstrates the feasibility of estimating CrI from spectral data without requiring X-ray diffraction (XRD) analysis, and provides insights into the molecular structure of cellulose.

## Data
The dataset includes FTIR spectra from various cellulose materials, including MCC, CNF, CNC, and pulp-derived samples. The target CrI values were derived using the Segal method.

## Key Features
- Spectral preprocessing: SG2 and L2 normalization
- Model types: PLSR, Decision Tree, Random Forest, MLP
- Repeated train-test splits (n = 50) with 3-fold CV
- Performance metrics: R², RMSE
- Validation: Y-scrambling test to verify for MLP model reliability

## Paper Information
This code supports the manuscript:
**"Estimation of Cellulose Crystallinity via Infrared Spectroscopy and Machine Learning"** (submitted to *Carbohydrate Polymers*).

## License
Distributed under the MIT License.
