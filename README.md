# Cellulose Crystallinity Estimation Using Infrared Spectroscopy and Machine Learning

This repository contains R scripts and data files for estimating the crystallinity index (CrI) of cellulose materials based on infrared (IR) spectroscopy combined with machine learning (ML) models. The study explores the use of Partial Least Squares Regression (PLSR), Decision Tree (DT), Random Forest (RF), and Multilayer Perceptron (MLP) algorithms to develop fast, cost-effective, and accurate methods for cellulose crystallinity prediction.

---

## Dataset

- **IR Summary.xlsx**  
  The main dataset contains IR spectra and corresponding crystallinity index (CrI) values for microcrystalline cellulose (MCC) samples, prepared by varying the ratio of amorphous and crystalline cellulose via ball milling and mixing.  
  Mixture samples were prepared using MCC combined with three different amorphous standards: 120-minute ball-milled MCC, xylan powder, and lignin powder.  
  The dataset covers a CrI range from 0% (fully amorphous) to ~82% (highly crystalline).

---

## Scripts

- **random_forest_cri.R**  
  Implements Random Forest regression to predict cellulose CrI using IR spectral data. Includes preprocessing (second-derivative and L2 normalization), hyperparameter tuning, model training over multiple random seeds, feature importance evaluation, and performance summary.

- **multi_layer_perceptron_cri.R**  
  Implements a Multilayer Perceptron (MLP) neural network for CrI prediction. Includes data preprocessing, hyperparameter tuning with stochastic gradient descent and Adam optimizers, model training, performance evaluation, and Y-scrambling to assess model robustness and avoid overfitting.

---

## Methodology Summary

- MCC samples were ball-milled to generate cellulose with varying crystallinity, mixed in controlled ratios with amorphous standards (ball-milled MCC, xylan, and lignin) to create a comprehensive dataset.
- IR spectra were collected in the 4000–600 cm⁻¹ range and preprocessed with Savitzky-Golay second derivatives and L2 normalization.
- The dataset was split by sample identity to prevent data leakage; training and testing were performed on distinct samples.
- PLSR, DT, RF, and MLP models were trained and evaluated using cross-validation and multiple random seeds for robustness.
- Feature importance analysis (Mean Decrease in Impurity for RF) was conducted to identify informative spectral regions.
- Models trained on selected spectral regions (e.g., 1400–900 cm⁻¹) showed improved performance and reduced computational cost.
- MLP demonstrated the best balance of accuracy and robustness across various cellulose materials, including lignocellulosic pulps, cellulose nanomaterials, and mixtures with xylan and lignin.

---

## Usage

1. Load the data from `IR Summary.xlsx`.
2. Run the RF or MLP script to train models and predict CrI values.
3. Use the provided functions for spectral preprocessing and model evaluation.
4. Apply trained models to new cellulose spectral data for crystallinity estimation.

---

## References

For detailed methodology, experimental design, and discussion, refer to the associated manuscript:

Lee, Y. J., Lee, D. Y., Lee, T.-J., & Kim, H. J. (2025). *Cellulose I crystallinity estimation using a combination of infrared spectroscopy and machine learning approaches*. Carbohydrate Polymers, (Submitted).

---

## Contact

For questions or data requests, please contact:

- Yong Ju Lee: paperlyj@gmail.com  
- Hyoung Jin Kim: hyjikim@kookmin.ac.kr
