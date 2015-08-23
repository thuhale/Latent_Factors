# Latent_Factors
Studying the latent factors underlying valued dyadic social network among students and teachers.

Bootstrap_Simulation: Perform low rank matrix completion on the interaction matrix, estimate variance, generate an error term, add that to the estimated values, and perform Canonical Correlation Analysis. 

Clustering: Clusters in students and teachers could result to bias in our estimates. This is to confirm that there aren’t two separate clusters in the school using regularized spectral clustering.

Cross_Validation: Compare LRMC with two-way anova model and mean model.


Data: math.csv/MATH.xlsx: interaction matrix with missing values, columns are teachers, and rows are students.

LRMC_Algorithm: General data analysis, fixed effects are accounted for.

Student_Regression: Nonparametric permutation test on regressions of student covariates on the estimated student latent factor.

SVD: Simulations, in which, underlying bipartite graph structure is maintained, random values replace observed, lrmc is applied.


Teacher_Regression: Regressions of teacher covariates on the estimated teacher latent factor.


Student_Simulation: Nonparametric permutation test on regressions of teacher covariates on the estimated teacher latent factor.

