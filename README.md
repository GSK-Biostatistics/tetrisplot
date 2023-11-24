# {tetrisplot}

## About
An effective way of representing the model uncertainty induced by variable selection is to consider the long run frequency of variables selected when applied to a sufficient number of bootstrap samples drawn from the original training-set. Strong (not necessarily real!) “outcome-predictor” effects will typically come through in the majority (or all) of the bootstrap fits. A number of known predictors might then be expected to appear “typically” in the final fit, but there are no guarantees. Bootstrap sampling can be used here to emulate the natural perturbations that might have occurred when originally collecting our data: summarising the impact of such perturbations on the final model can be a powerful and insightful way to explore this problem, particularly when conveying this message to our non-quantitative colleagues.

We introduce the the "Tetris" plot as a way of visualising this instability.

## Installation
You can install the development version of {tetrisplot} like so:
```
devtools::install_github("GSK-Biostatistics/tetrisplot")
```