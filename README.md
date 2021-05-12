# anova1 operator

##### Description

The `anova1 operator` tests for a significant difference in the mean between any of the groups in the input data.

##### Usage

Input projection|.
---|---
`y-axis`        | numeric, y values, per cell 
`color`         | the sample names

Input parameters|.
---|---
`groupingType`      | Treat the ANOVA grouping variable as categorical (dft) or numerical (see above).

Output relations|.
---|---
`pFactor1`          | p-value for the probability that all group means are equal (groupingType = categorical) or the slope of the straight line fit is equal to zero                                 (groupingType = continuous). Hence, a low value indicates a significant effect.
`pFactor1`          | the significance as - 10 log(pFactor1)
`slope`:            | slope of the regression line. Only if the grouping type is continuous.
`intercept`         | intercept of the regression line. Only if the grouping type is continuous.

##### Details

1-way ANOVA is used for data with a single grouping factor, but with more than 2 groups. It test for a significant difference in the mean between any of the groups. As such, it is a generalization of the two sample t-test that works for two groups only.