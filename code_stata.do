******* Exam Replication ********

**** Name: Maria Luiza Dias Campos
**** Student ID Number: 202322096
**** Date: December 13 2023 

**** First Steps ****

capture log close
capture log using ./assignment_final.log, replace
clear all

**** Installing packages ****

ssc install gtools
net install binscatter2, from("https://raw.githubusercontent.com/mdroste/stata-binscatter2/master/")
ssc install rddensity
ssc install cmogram
net install lpdensity, from  (https://raw.githubusercontent.com/nppackages/lpdensity/master/stata/) replace
ssc install outreg2
ssc install binscatter
ssc install outreg2
ssc install estout
ssc install kdensity
use reg_panes

/* Question 1

Create the variables for cut off, and the quadratic term for the running variable and produce two histograms of income (one as a discrete variable, one as a continuous). Explain the differences of the histograms. Recenter income around the cutoff. */

/* Generate the treatment variable and quadratic term */

generate treat = 0
replace treat = 1 if ind_reest<0.02 & ind_reest~=. 
generate ind_reest_sq = ind_reest^2

/* Recenter the running variable */

replace ind_reest=ind_reest-0.02

/* Now, let's make the histograms. */

histogram ind_reest, discrete width(0.0001) ytitle(Frequency) xtitle(Predicted Income) xline(0)title(Replicating predicted income on Manacorda et al. (Figure A2)) subtitle(Proportion of households with different scores levels) note(Discrete histogram)

histogram ind_reest, width(0.0001) ytitle(Frequency) xtitle(Predicted Income) xline(0) title(Replicating predicted income on Manacorda et al. (Figure A2)) subtitle(Proportion of households with different scores levels) note(Continuous histogram)

/* The graphs show that "there is no indication that households just below the eligibility threshold are overrepresented relative to those just ineligible." (Manacorda et al., p. 14). 

Both graphs does not show much difference. 
When running the discrete command, you are treating "predicted income" as a categorical 
variable. Running the continuous command means that "predicted income" is continuous. 
The continuous command should be more suitable for predicted income scores. */

rddensity ind_reest, c(0) plot

/* H0: There is continuity.

We want to fail in rejecting the null. 
If p-value is high, than we cannot reject the hypothesis of having continuity 
without treatment. 

P-value = 0.4114 > 0.05.
We cannot reject the null. 

If I didn't have the treatment, data would be continuous. 
Data is DESCONTINUOUS because of treatment. 

Conclusion: we have evidence to presume that the descontinuity is due to PANES
eligibility. Being eligible to PANES has an effect on political support for the
government. Histograms show evidence that households are randomly assigned to each
side of the threshold. */

/* Question 2: come back. 

Running regressions on covariates (provided in .do file) to see if there is a jump in average values for each of these at the cutoff and explain the results. What can we say about covariates balance? */ 

reg bl_hhsize treat##c.ind_reest if ind_reest>=-.02 & ind_reest<=.02, robust
cmogram bl_hhsize ind_reest if ind_reest>=-.02 & ind_reest<=.02, cut(0) scatter line(0) lfitci
outreg2 using covariance_results.tex, replace

reg hhsize treat##c.ind_reest if ind_reest>=-.02 & ind_reest<=.02, robust /* With the variable 
"hhsize" we find the coefficient reported on the paper (-0.350), and it is negative and significant. */ 
describe
cmogram hhsize ind_reest if ind_reest>=-.02 & ind_reest<=.02, cut(0) scatter line(0) lfitci

/* Comparing both cmograns, visually, it is possible to argue that there is random
assignment around the cut-off for household size. Both variables look balanced around the cut-off. */

reg sexo treat##c.ind_reest if ind_reest>=-.02 & ind_reest<=.02 & sex != 999999, robust
cmogram sexo ind_reest if ind_reest>=-.02 & ind_reest<=.02, cut(0) scatter line(0) lfitci
outreg2 using covariance_results.tex, append

/* Treatment is not statistic significant, so it is not predicting sex. 
There is no jump. */ 

/* Main conclusion? Don't know. 
For household size, the p-values are low but when running the cmogram, data don't look 
descontinuited. The paper actually relies on that. Figure A1 of online appendix actually 
shows that. What looks like is that the authors judged as enough to only do a visualisation of
data to argue that there is no discontinuity regarding covariates. Plus, when adding them to the model, they don't change the results that much, what gives extra evidence to the random assignment 
around the cut-off. Is that enough? Don't know. Shouldn't they be running some tests? */

/* Doubts:
- Why running for covariates? 
In RDD analyses, regressions on covariates are used to control for the effects of variables that may be affecting the relationship between the treatment variable and the outcome variable. 

- What it means "covariates balance" and what those regressions are telling me about that? 

In statistics, covariate balance refers to the degree to which the distribution of covariates is similar across levels of the treatment variable. Covariates are variables that are associated with the outcome of interest, but are not the treatment itself. 
Covariate balance is important because it ensures that the treatment effect that is estimated is not confounded by the distribution of covariates. */


/* Question 3 
Produce main results of the paper (with our dataset) using political support as dependent variable.
A) Expand the window symmetricaly by 0.01.
B) Run the main results on 3 more dependent variables of your choice from the original paper. Explain which ones you choose, why and then subsequently present the results. */ 

/* Running the regression in the -0.02/+0.02 range. */

reg support_gov_2007 treat##c.ind_reest if ind_reest>=-.02 & ind_reest<=.02, robust 
outreg2 using results.tex, replace
reg support_gov_2007 bl_hhsize sexo treat##c.ind_reest if ind_reest>=-.02 & ind_reest<=.02 & sex != 999999, cluster(ind_reest) 
reg support_gov_2007 hhsize sexo treat##c.ind_reest if ind_reest>=-.02 & ind_reest<=.02 & sex != 999999, cluster(ind_reest) 
outreg2 using results_covariates.tex, replace

/* Being in the treatment group predicts the support for the government in 2007. 
Treatment group is equal 1 when your predict income score is less than 0.02.
This means that you are below the threshold, being eligible for PANES. 
The coefficient is positive, which means that support increases with being treated. */

reg support_gov_2007 treat##c.(ind_reest_sq) if ind_reest>=-0.02 & ind_reest<=.02, robust
outreg2 using results.tex, append
reg support_gov_2007 bl_hhsize sexo treat##c.(ind_reest_sq) if ind_reest>=-0.02 & ind_reest<=.02 & sex != 999999, cluster(ind_reest)
outreg2 using results_covariates.tex, append

/* Adjusting the model for a quadratic term gives the same result.
Being in the treatment group predicts support for the government in 2007 at 5% confidence level (p-valeu = 0.001 < 0.05). */ 


/* 3A) Expand the window symmetricaly by 0.01.
New range: -0.03 / + 0.03. */

summarize ind_reest

reg support_gov_2007 treat##c.ind_reest if ind_reest>=-.03 & ind_reest<=.03, robust

outreg2 using results_03.tex, replace

reg support_gov_2007 treat##c.(ind_reest_sq) if ind_reest>=-.03 & ind_reest<=.03, robust

outreg2 using results_03.tex, append

/* We don't have observations outside of the window of +0.02/-0.02. 
that's why this code does not change the x-axis. */

/* Let's run some graphs. */

cmogram support_gov_2007 ind_reest if ind_reest>=-.02 & ind_reest<=.02, cut(0) scatter line(0)

cmogram support_gov_2007 ind_reest if ind_reest>=-.02 & ind_reest<=.02, cut(0) scatter line(0) lfitci
/* lfitci adds a linear fit line to the scatter plot along with confidence intervals for the fitted line.*/ 

cmogram support_gov_2007 ind_reest if ind_reest>=-.02 & ind_reest<=.02, cut(0) scatter line(0) qfitci

/*qfitci adds a quadratic fit line to the scatter plot along with confidence intervals for the fitted curve.

The quadratic fit line is a curve that represents the best quadratic (second-degree polynomial) approximation of the relationship between the variables being plotted. The confidence intervals around the curve provide a graphical representation of the uncertainty associated with the estimated quadratic relationship. */ 
	
/* 3B) Run the main results on 3 more dependent variables. */

reg geo treat##c.ind_reest if ind_reest>=-.02 & ind_reest<=.02, robust
reg confidence_president treat##c.ind_reest if ind_reest>=-.02 & ind_reest<=.02, robust
outreg2 using results_extra.tex, replace

/* Having an income prediction score below the threshold predicts confidence in the president. 
Coefficient is positive (0.082) and significant at 5% (p-value = 0.022). */

cmogram confidence_president ind_reest if ind_reest>=-.02 & ind_reest<=.02, cut(0) scatter line(0) lfitci

reg pride treat##c.ind_reest if ind_reest>=-.02 & ind_reest<=.02, robust
outreg2 using results_extra.tex, append

/* Having an income prediction score below the threshold predicts pride. 
Coefficient is positive (0.053) and significant at 5% (p-value = 0.019). */

cmogram pride ind_reest if ind_reest>=-.02 & ind_reest<=.02, cut(0) scatter line(0) lfitci

reg interest_politics treat##c.ind_reest if ind_reest>=-.02 & ind_reest<=.02, robust
outreg2 using results_extra.tex, append

/* Having an income prediction score below the threshold predicts interest in politics. 
Coefficient is positive (0.056) and significant at 5% (p-value = 0.046). */ 

cmogram interest_politics ind_reest if ind_reest>=-.02 & ind_reest<=.02, cut(0) scatter line(0) lfitci

/* Question 4 

Replicate Q3 by running "donut hole regressions".  Explain why one might need a donut hole regression? */

/* To run a "donut hole regression" I have to DROP the units around the cutoff. */ 

gen donut = 0 
replace donut = 1 if ind_reest>-0.005 & ind_reest<0.005

reg support_gov_2007 treat##c.ind_reest if ind_reest>=-.02 & ind_reest<=.02 & donut==0, robust
outreg2 using results_donuthole.tex, replace

/* Doing a donut hole regression means that we are removing observations very close to the cut-off. 
We are doing this because it would be reasonable to assume that people have motivation to manipulate 
their income prediction score to be below of the threshold and benefit from the policy. 
However, the threshold was not public during the PANES program and households were not aware of
the calculation behind the prediction income score, so one could argue that households were enable 
to manipulate information to control the side they would fall in. 
Nevertheless, with the Donut Hole regression, the "treat" variable is still significant at 5% (p-value = 0.011 < 0.05). The coefficient is positive (0.1157).

Which colum of the table 1 should be this result? 
It is not column 2 since the 0.11 porcentage points should come from a polynomial regression. */

reg support_gov_2007 treat##c.(ind_reest_sq) if ind_reest>=-.02 & ind_reest<=.02 & donut==0, robust

/* Why we are not running this regression too? */


/* Question 5 come back 

Run local polynomials and explain why local polynomials might be needed after a donut hole. */

rdrobust support_gov_2007 ind_reest if donut==0, kernel(epanechnikov) masspoints(off) p(2) c(0)
outreg2 using results_poly_donut.tex, replace
rdrobust support_gov_2007 ind_reest, kernel(epanechnikov) masspoints(off) p(2) c(0)
outreg2 using results_poly.tex, replace

/* Kernel(epanechnikov): The term "Epanechnikov" in the context of kernel functions refers to a specific type of kernel function that was introduced by the statistician and mathematician Gideon Epanechnikov. Its formula reflects the quadratic shape of the kernel function. It gives higher weight to observations close to the point of interest and gradually decreases the weight as the distance from the point of interest increases. */ 

rdrobust support_gov_2007 ind_reest if donut==0, kernel(uniform) masspoints(off) p(2) c(0)
outreg2 using results_poly_donut.tex, append
rdrobust support_gov_2007 ind_reest, kernel(uniform) masspoints(off) p(2) c(0)
outreg2 using results_poly.tex, append

/* The uniform kernel gives equal weight to all observations within the bandwidth around the cutoff. By using different kernels and excluding data points in the "donut hole" in the second command, these analyses assess the robustness of the RD results to different specifications and potential data manipulation around the cutoff.*/

/* What is the command "masspoints"?

Masspoints is telling Stata what to do when there are multiple observations with the same value in the running variable. Turning it off ensures that the presence of multiple observations with the same value of the running variable (ind_reest) doesn't disproportionately affect the analysis.

How can I read these commands? I don't understand the table. 

- The coefficient is negative: the lower is your income predicted score, higher is your support for the government. Which makes sense with the paper: negative scores, which are the eligible ones for the policy, are more likely to support the current government. 

Why change the kernel function? Which one is more usually used? */

/*Doing some graphs representation */ 

/* Main graph: visual representation of the paper */

cmogram support_gov_2007 ind_reest if ind_reest>=-.02 & ind_reest<=.02 & donut==0, cut(0) scatter line(0) lfitci


/* How can I read this graph below? */ 

twoway (scatter support_gov_2007 ind_reest, sort) if ind_reest>=-.02 & ind_reest<=.02, ytitle(support_gov_2007) xtitle(ind_reest) xline(0) title(Scatter Plot) note(Cutoff is on 0)

binscatter support_gov_2007 ind_reest if ind_reest>=-.02 & ind_reest<=.02

/* Binscatter is a command that will plot the relationship between to variables.
The x-asis usually being a continuous variable divided into bins. 
The y-axis being the dependent variable presented in means. 
In this command in specific, the best line will be plot to visualisation of the relationship.
As we can see here, the relationship between fovernment support and income score is negative. 
Lower is your income score, higher is your support for the current government. */

binscatter support_gov_2007 ind_reest if ind_reest>=-.02 & ind_reest<=.02, line(qfit) by(treat)

/* With this command we can better visualize the descontinuity. 
It will plot a non-linear function to describe the relationship between variables, 
with the cut-off "treat". */


/* Question 6

Produce an RDplot and a cmogram with the original and expanded windows. Explain what the respective graphs show. */ 

/* Let's first plot with the usual window of -0.02/+0.02. */

cmogram support_gov_2007 ind_reest if ind_reest>=-.02 & ind_reest<=.02, cut(0) scatter line(0) lfitci 
* Plot the best flat line, with the treatment being below 0 and non-treatment above 0. 
* There ir no donut hole aqui, why? 

cmogram support_gov_2007 ind_reest if ind_reest>=-.02 & ind_reest<=.02, cut(0) scatter line(0) qfitci 
* Plot a non-linear curve, with the treatment being below 0 and non-treatment above 0. 
* This looks more adjusted to the data. 

/* Nevertheless, in both graphs we can see that lower predicted income scores are related to higher support for the government in 2007, which is the year of execution of the policy. Above the threshold, support decreases. */

rdplot support_gov_2007 ind_reest if ind_reest>=-.02 & ind_reest<=.02, binselect(qs) p(1) masspoints(off) c(0) graph_options(title(Government support and Predicted Income))

/* RD plot is a command for creating a regression-discontinuity plot.
- binselect(qs): This option specifies the method for selecting the number of bins in the RD plot. In this case, it's using the "quadratic-spline" method. 
- p(1): This option sets the order of the polynomial used for local linear fitting. In this case, it's using a first-degree polynomial.
- c(0): This option sets the continuity correction for local linear fitting to zero.*/

rdplot support_gov_2007 ind_reest if ind_reest>=-.02 & ind_reest<=.02, binselect(qs) p(2) masspoints(off) c(0) graph_options(title(Government support and Predicted Income))

* This command is fitting a curve to the data instead of a line. 

/* Main conclusion: in both commands (cmogram and rdplot) we can see that there is a discontinuity around the cut-off (support for the government decreases drastically with higher predicted income levels). Second-order polynomial appears to fit better the data than first-order polynomial. 
In this way, people who were eligi for PANES (with income levels below 0), are more likely to support the current government in 2007, year of the execution of the policy. */

* Let's now plot with the expanded window. 

cmogram support_gov_2007 ind_reest if ind_reest>=-.03 & ind_reest<=.03, cut(0) scatter line(0) lfitci 
count if ind_reest >= -0.03 & ind_reest <= 0.03
count if ind_reest >= -0.02 & ind_reest <= 0.02
cmogram support_gov_2007 ind_reest if ind_reest>=-.03 & ind_reest<=.03, cut(0) scatter line(0) qfitci

rdplot support_gov_2007 ind_reest if ind_reest>=-.03 & ind_reest<=.03, binselect(qs) p(1) masspoints(off) c(0) graph_options(title(Government support and Predicted Income))

rdplot support_gov_2007 ind_reest if ind_reest>=-.03 & ind_reest<=.03, binselect(qs) p(2) masspoints(off) c(0) graph_options(title(Government support and Predicted Income))

/* Q6: This basically prompts you to produce cmograms and RDplots for an asymmetric bandwidth. Any ideas why would the bandwidth be asymmetric in this case? */ 

* [-0.015, 0.02]

reg support_gov_2007 treat##c.ind_reest if ind_reest>=-.015 & ind_reest<=.02 & donut==0, robust
outreg2 using negative_narrowwind_015.tex, replace
reg support_gov_2007 treat##c.ind_reest if ind_reest>=-.015 & ind_reest<=.02, robust
outreg2 using positive_narrowwind_015.tex, append

* [-0.02, +0.015]

reg support_gov_2007 treat##c.ind_reest if ind_reest>=-.02 & ind_reest<=.015 & donut==0, robust
outreg2 using positive_narrowwind_015.tex, replace
reg support_gov_2007 treat##c.ind_reest if ind_reest>=-.02 & ind_reest<=.015, robust
outreg2 using positive_narrowwind_015.tex, append

* [-0.01, +0.02]

reg support_gov_2007 treat##c.ind_reest if ind_reest>=-.01 & ind_reest<=.02 & donut==0, robust
outreg2 using negative_narrowwind_01.tex, replace
reg support_gov_2007 treat##c.ind_reest if ind_reest>=-.01 & ind_reest<=.02, robust
outreg2 using negative_narrowwind_01.tex, append

* [-0.02, +0.01]

reg support_gov_2007 treat##c.ind_reest if ind_reest>=-.02 & ind_reest<=.01 & donut==0, robust
outreg2 using positive_narrowwind_01.tex, replace
reg support_gov_2007 treat##c.ind_reest if ind_reest>=-.02 & ind_reest<=.01, robust
outreg2 using positive_narrowwind_01.tex, append

log close
