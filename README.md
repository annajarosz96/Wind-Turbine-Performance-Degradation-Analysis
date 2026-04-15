# Wind-Turbine-Performance-Degradation-Analysis
This project involves a comprehensive statistical analysis of wind turbine performance and degradation using various regression techniques in R. By leveraging a dataset containing measurements of wind speed, active power, and electrical phase data, the project compares traditional frequentist models with Bayesian and machine learning approaches.

This repository contains a suite of R scripts designed to model the relationship between environmental factors (wind speed/direction) and electrical output (active power/voltage) in wind turbines. The project aims to identify patterns of power decrease and evaluate the predictive accuracy of various statistical frameworks.
🚀 Overview

Predicting power generation in wind turbines is critical for grid stability and maintenance scheduling. This project implements multiple modeling paradigms—from simple linear regressions to complex Bayesian Hierarchical models—to determine which method best captures the non-linear "power curve" and potential degradation of a turbine.
📊 Models Implemented

The project compares several modeling techniques across different script files:

    Frequentist Regressions:

        Linear & Quadratic: Standard baseline models for active power prediction.

        Poisson Regression: Applied to shifted and rounded power data to model output as count-based rates.

        GLM (Generalized Linear Models): Mapping the relationship between phase-specific voltage and power decrease using Gaussian identity links.

    Machine Learning:

        Random Forest: A non-parametric approach to capture complex interactions and assess variable importance (Mean Decrease in Gini).

    Bayesian & Hierarchical Modeling:

        MCMC (Stan/brms): Cubic and quadratic Bayesian regressions to provide uncertainty estimates via posterior distributions.

        HGLM (Hierarchical Generalized Linear Models): Incorporating "subject" level random effects to account for variations across different recording periods or turbines.

🛠️ Key Features

    Performance Metrics: Every model script calculates and compares AIC, BIC, Log-Likelihood, and WAIC to determine the best fit.

    Visualization: Detailed ggplot2 implementations including 2x2 grid comparisons, conditional effects plots, and variable importance histograms.

    Data Preprocessing: Includes scripts for feature scaling, handling missing values, and generating power decrease metrics derived from multi-phase current and voltage.

📈 Sample Visualizations

The project generates visualizations comparing the theoretical power curve against actual sensor data using GLM, GLMM, and Random Forest overlays.
📂 Repository Structure

    Linear regression model.R: Baseline linear analysis.

    Quadratic regression model.R: Captures the curvature of wind energy extraction.

    Weibull regression model.R: Advanced comparison of GLM vs. MCMC cubic models.

    MCMC brms model.R: Full Bayesian implementation for power decrease analysis.

    Random Forest model.R: Feature importance analysis for electrical parameters.

📋 Requirements

To run these scripts, you will need the following R libraries:
install.packages(c("ggplot2", "readxl", "gridExtra", "lme4", "randomForest", "rstanarm", "brms", "MASS"))

install.packages(c("ggplot2", "readxl", "gridExtra", "lme4", "randomForest", "rstanarm", "brms", "MASS"))
