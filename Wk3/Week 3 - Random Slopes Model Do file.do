* Week 3 - Random Slopes Models using Stata 15 *

* Open dataset hedon_RandomItercept.dta

* Study variables


* Centre continuous variables around their mean
egen age2 = mean(age)
gen centage = age - age2

egen meanincome = mean(income)
gen centincome = income - meanincome

egen meanedu = mean(eduyrs)
gen centedu = eduyrs - meanedu

*Run a variance component model
mixed hed|| country:, ml variance
estimates store vc

*Run a random intercepts model
mixed hed centage centincome centedu female || country: , ml variance
estimates store ri

* Run a random slopes model that allows the relationship between hedonism and income to differ across countries
mixed hed centage centincome centedu female || country: centincome , ml variance covariance(unstructured)
estimates store rs

estimates table vc ri rs
lrtest ri rs

*Answer to exercise 
mixed hed centage centincome centedu female || country: female , ml variance covariance(unstructured)

*Random slope on age
mixed hed centage centincome centedu female || country: centage, ml variance covariance(unstructured)

* new age variable
gen agesmall = age*0.01
egen meanagesmall=mean(agesmall)
gen agesmall2 = agesmall - meanagesmall
mixed hed agesmall2 centincome centedu female || country: agesmall2, ml variance covariance(unstructured)

*Random slopes on education
mixed hed centage centincome centedu female || country: centedu, ml variance cov(uns)

*Random slopes model on income and age
mixed hed centage centincome centedu female || country: centage centincome, ml variance cov(uns)

* Random slope on income
mixed hed centage centincome centedu female || country: centincome, ml variance cov(uns)

*Level 1 variance = var(Residual) = 0.788

* Calculate level-2 variance

*Plotting residuals and predictions
mixed hed centage centincome centedu female || country: centincome, ml variance cov(uns)

*To calculate the level 2 residuals:
predict r1 lev2, reffects

*Level-1 residuals
predict lev1, resid

sum r1 lev1 lev2

gen b0=_b[_cons] + r1 + lev2
bysort country: gen tolist = _n==1
list country b0 if tolist
graph dot (mean) b0, over(country, gap(country) sort(b0)) cw linetype(line) ytitle(Random Slopes model)

*Model diagnostics
kdensity lev1, normal
pnorm lev1
qnorm lev1
kdensity lev2, normal
pnorm lev2
qnorm lev2

* Exercise: Which country is the most hedonistic after controlling for age, income, education and gender and allowing for the relationship between hedonism and age to differ across countries? 
mixed hed centage centincome centedu female || country: centage, ml variance cov(uns)
predict res r2, reffects
gen b02=_b[_cons] + r2 + res
bysort country: gen tolist2 = _n==1
list country b02 if tolist2
graph dot (mean) b02, over(country, gap(country) sort(b02)) cw linetype(line) ytitle(Random Slopes model2)


*Testing for random slopes
mixed hed centage centincome centedu female || country: centincome, ml variance cov(uns)

*Examine intercept and slope residuals for countries
estat recov, corr
predict u1 u02, reffects
egen pickone=tag(country)
scatter u1 u02 if pickone==1, yline(0) xline(0) ///
	ytitle("Slope of income (u1j)") xtitle("Intercept (u0j)") 
predict predhed, fitted


*Add a random coefficient for gender 
mixed hed centage centincome centedu female || country: centincome female, ml variance cov(uns)
