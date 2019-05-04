* Joshua Alley
* Texas A&M University
* Replication and 

* Based on replication code provided by the authors
* Removed some code and added sensitivity analysis using poet (Chaudoin et al 2016)


* set working directory
capture cd "C:\Users\jkalley14\Dropbox\Research\Dissertation\arms-allies-review\replications" 
capture cd "C:\Users\Josh\Dropbox\Research\Dissertation\arms-allies-review\replications"


use AvsDA_replication, clear 

global NOREGIME DEMOC defense lnrivalmil atwar civilwar LNRGDP milcoor 
global MODEL DEMOC defense_dem defense_nodem lnrivalmil atwar civilwar LNRGDP milcoor 
global MODEL2 DEMOC LNdefense_MILEX_dem LNdefense_MILEX_nodem lnrivalmil atwar civilwar LNRGDP milcoor

***** Replications of results 
* TABLE 2 : Main model using dichotmous defense alliance with democracy + non-democracy: 
set more off 
xtset ccode year 
est clear 
xtpcse  LMILEX LMILEX1 $NOREGIME , p corr(psar)
	est store m0
xtpcse  LMILEX LMILEX1 $MODEL , p corr(psar)
	est store m1
xtpcse  LMILEX LMILEX1 $MODEL i.ccode , p corr(psar)
	est store m_cFE	
xtpcse  LMILEX LMILEX1 $MODEL i.year , het corr(psar)
	est store m_yFE
ivregress 2sls  LMILEX  $MODEL (LMILEX1 = L(1/2).(LNRGDP lnrivalmil)), vce(robust) first
	est store m2
est table m*, drop(i.ccode i.year) star stats(N)
	
	
// IV model robust to year and ccode FE	(noted in manuscript) 
qui ivregress 2sls  LMILEX  $MODEL (LMILEX1 = L(1/2).(LNRGDP lnrivalmil)) i.year, vce(robust) first
		eststo R1
qui ivregress 2sls  LMILEX  $MODEL (LMILEX1 = L(1/2).(LNRGDP lnrivalmil)) i.ccode, vce(robust) first
	eststo R2

	
	


***** sensivitiy analysis: how severe is selection on unobservables?
* The poet ado file is also in this folder, syntax is as follows:
* poet depvar [theor. relevant indepvars], treat(treatmentvar) other(other indepvars)

* Start with full regression specification
poet LMILEX LMILEX1 DEMOC defense_nodem lnrivalmil atwar civilwar LNRGDP milcoor, treat(defense_dem)

* remove lagged DV: this makes a huge difference
poet LMILEX DEMOC defense_nodem lnrivalmil atwar civilwar LNRGDP milcoor, treat(defense_dem) other(LMILEX1)

* remove military coordination dummy
poet LMILEX DEMOC defense_nodem lnrivalmil atwar civilwar LNRGDP, treat(defense_dem) other(LMILEX1 milcoor)

* remove defense with non-democracy dummy
poet LMILEX DEMOC lnrivalmil atwar civilwar LNRGDP, treat(defense_dem) other(LMILEX1 milcoor defense_nodem)








	