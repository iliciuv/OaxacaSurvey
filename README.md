@author: Miguel Garcia-Duch 18-10-2023

# Package OaxacaSurvey 0.1

an r test package implementing the oaxaca-blinder decomposition for survey objects, that is, taking sample weights into consideration by means of svyglm superset of standard glm implemented in package *Survey*.

--------------------------------------------------
--------------------------------------------------

### Installation and usage

Install latest version from:

```r
devtools::install_github("iliciuv/OaxacaSurvey)
```

The only implemented method to perform the decomposition is *oaxaca_blinder_svy*. It performs Oaxaca-Blinder decomposition for 2 groups of controls from a survey with sampling weigths. The function uses the following parameters:

- **@param formula** *An object of class formula:* the model formula.
- **@param data** *A data frame:* containing the variables in the formula, group, and weights.
- **@param group** *A character string:* the name of the binary group variable in the data. 1 for the first group, and 0 for the second group.
- **@param weights** *A character string:* the name of the weights variable in the data.
- **@param R** *An integer:* the number of bootstrap replicates. Default is 1000.
- **@param conf.level** *A float less than 1:* Confidence level for estimated CI.

A working example can be run pointing to the following project path:

```r
source("tests/examples.R")
```

--------------------------------------------------
--------------------------------------------------

### Underlying logic

The Decomposition computed is the so-called "triple":

- Endowments
- Coefficients
- Interactions

$$ \Delta Y = E + C + I \ $$

Where:

- $\Delta Y$ is the mean difference in the outcome variable between the two groups.
- $E$ represents the endowments effect.
- $C$ represents the coefficients effect.
- $I$ represents the interaction effect.
The difference in mean outcomes between the two groups can be expressed as:

$$ \Delta \bar{Y} = (\bar{X}_1 - \bar{X}_2) \hat{\beta}_2 + \bar{X}_2 (\hat{\beta}_1 - \hat{\beta}_2) + (\bar{X}_1 - \bar{X}_2) (\hat{\beta}_1 - \hat{\beta}_2) $$

Where:

- $\Delta \bar{Y}$ is the difference in mean outcomes between group 1 and group 0.
- $\bar{X}_1$ and $\bar{X}_0$ are the mean values of the covariates for group 1 and group 0, respectively.
- $\hat{\beta}_1$ and $\hat{\beta}_0$ are the estimated coefficients for group 1 and group 0, respectively.

The three terms on the right side represent the contributions from differences in endowments (E), coefficients (C), and interaction effects (I).

--------------------------------------------------
--------------------------------------------------

## Pending implementations
