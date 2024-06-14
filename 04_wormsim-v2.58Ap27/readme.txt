wormsim-2 v2.58Ap27 (Oct 15, 2017)
1. Fixed bug in AggregatedDetailedOutputWriter that caused incomplete Z file output ; this was due to not flushing an OutPrintWriter
2. Added the -u option to not produce zipfiles

wormsim-2 v2.58Ap26 (Oct 5, 2017)
1. Added column for external FOI in standard output
2. Fixed bug that caused any other runs than the 1st run to attain zero prevalence after run-in period
   ( which was due to not resetting vector control to 1.0 at the start of each run)
3. Added mf5+ values 

wormsim-2 v2.58Ap25 (Feb 23, 2017)
Fixed bug in output on anywpos.
Corrections of previous results can be made by:
subtracting the M_wpairpos number from M_anywpos
subtracting the F_wpairpos number from F_anywpos

wormsim-2 v2.58Ap24 (Dec 7, 2016)

Added OV16 output (i.e. serology)
See the *Z.### and *Z.txt files.
Explanation of the header of the *Z.txt files:
year: time of survey
age: upper limit of age group
OV16[SEX][FLAGNR][-/+]: number of people of that SEX with FLAGNR positive (+) or negative (-)

OV16 FLAGNRs:

1: positive if number of prepatent worms > 0
2: positive if number of    patent worms > 0
3: positive if mf density > 0


Use the -d command-line option to enable detailed output.

wormsim-2 v2.58Ap23 (Sep 22, 2016)

The initial FOI:
<initial.foi duration="5" foi="10.0"/>
has been replaced by:
            <external.foi>
                <start year="1790" month="0" foi="4" exclusive="true"/>
                <start year="1797" month="6" foi="0" exclusive="false"/>
            </external.foi> 

wormsim-2 v2.58Ap22 (Sep 13, 2016)

Fixed bug that caused exposure / contribution interventions not to be reset at the start of the next simulation run 
(i.e. in practice from the 2nd run (runnr 1)).

wormsim-2 v2.58Ap21 (Sep 7, 2016)

Corrected code to comply with appendix to STH paper: Modeling of soil-transmitted helminths in Wormsim
Specifically
- redefined L1uptake (now just the total L1uptake of the population, i.e. no longer the mean)
- divided the FOI applied to each individual by dividing by the summed population exposure 

wormsim-2 v2.58Ap20 (Sep 5, 2016)

Moved "fraction.excluded" from the XML compliance element to the exposure and contribution interventions, 
and edited the code where necessary.

<exposure.interventions>
  <!-- note that effectivity below means the impact on individuals that are not excluded -->
  <moment year="1999" month="0" effectivity="0.75" fraction.excluded="0"/>
  <moment year="2002" month="6" effectivity="0.75" fraction.excluded="0"/>
</exposure.interventions>
<contribution.interventions>
  <!-- note that effectivity below means the impact on individuals that are not excluded -->
  <moment year="1999" month="0" effectivity="0.75" fraction.excluded="0"/>
  <moment year="2002" month="6" effectivity="0.75" fraction.excluded="0"/>
</contribution.interventions>

Also, to ensure consistency, also moved "fraction.excluded" from the XML compliance element for mass treatment rounds:
        <mass.treatment>
            <treatment.rounds>
                <treatment.round year="2000" month="0" coverage="0.5563910" delay="-1" fraction.excluded="0"/>
                <treatment.round year="2000" month="1" coverage="0.6390977" delay="-1" fraction.excluded="0"/>
                <treatment.round year="2000" month="7" coverage="0.5939850" delay="-1" fraction.excluded="0"/>
                <treatment.round year="2000" month="8" coverage="0.5413534" delay="-1" fraction.excluded="0"/>
            </treatment.rounds>
            <compliance fraction.malabsorption="0" compliance.model="0">
                <age.and.sex.specific.compliance age.limit="2" male.compliance="0" female.compliance="0"/>

Note that the default value of fraction.excluded equals zero.
Therefore, it is allowed to not specify fraction.excluded in the individual exposure and contribution interventions, 
and in the mass treatments rounds.
(As an aside, "fraction.excluded" is also defined for vector control moments but not used in the code.)


wormsim-2 v2.58Ap19 (Jul 27, 2016)

Changed the implementation of vector control (and exposure / contribution control).
Instead of defining a period of vector control, vector control is simply updated at specified moments:
    <vector.control>
        <moment year="1981" month="0" effectivity="0.41"/>
        <moment year="1981" month="3" effectivity="0.82"/>
        <moment year="1981" month="6" effectivity="0.47"/>
        <moment year="1982" month="0" effectivity="0.25"/>
        <moment year="1982" month="3" effectivity="0.00"/>
    </vector.control>
It is therefore necessary to reset vector control (e.g. to 0) at the end of a (set of) period(s) of vector control.
Note that the output on mbr shows the effect of vector control during the past month, 
as surveys are executed just before the end of the month (depending on delay which is usually set to -2 (hours)).



wormsim-2 v2.58Ap18 (Mar 2, 2016)
  Added support for multiple types of treatments.
  
  The inputfile now expects (see v258Ap18_lymfasim_india.xml for an example):
  <mass.treatments>
    <treatment.effect.variability dist.nr="0" mean="1.0"/>
    <mass.treatment>
      <treatment.rounds>
        .
        .
      </treatment.rounds>
      <compliance fraction.excluded="0.0" fraction.malabsorption="0.0">
        .
        .
      </compliance>
      <treatment.effects permanent.reduction.mf-production="0.01" period.of.recovery="0.01" shape.parameter.recovery.function="1.0" fraction.killed="0.5">
        <fraction.mf.surviving dist.nr="0" mean="0.01"/>
      </treatment.effects>
    </mass.treatment>
    <mass.treatment>
     .
     .
    </mass.treatment>
  </mass.treatments>
    
  In other words: 
  <treatment.effect.variability> is common to all mass treatments
  Multiple sets of <mass.treatment> are supported, each with its own treatment rounds, compliance, and treatment effects

  It is even possible to mix treatment types (i.e. in years 2000, 2002, 2005 treatment A and in 2001, 2003, 2004 treatment B).
  The number of different treatment types is unlimited (except for the imagination of the user).


wormsim-2 v2.58Ap17 (Feb 6, 2016)
  fixed bug that caused NaN in output (due to dividing 0/0 in AbstractHostCollection.getMeanL1Uptake())
wormsim-2 v2.58Ap16 (Feb 4, 2016)
  fixed bug that caused exposure and contribution interventions not to work
wormsim-2 v2.58Ap15 (Jan 31, 2016)
  additional output (by sex and age class) on any worm+, female worm+, and worm pair+
  for now, only in the X files, and for PATENT worms
wormsim-2 v2.58Ap14 (Jan 28, 2016)
  bednets and bug fix
  intermediate version - because of issues with coverage model 0 ....

wormsim-2 v2.58Ap13 (Jan 19, 2016)
  bednets etc
  intermediate version - because of issues with coverage model 0 ....

wormsim-2 v2.58Ap12 (Jan 12, 2016)
Fixed bug (forgot to regenerate XML classes with xjc resulting in 1.0 default for fraction mf surviving)
wormsim-2 v2.58Ap11 (Jan 12, 2016)
In the first xml element use "lymfasim" to enable exponential decay of mf survival:
<wormsim.inputfile xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
                   xsi:noNamespaceSchemaLocation="wormsim.xsd"
                   model="lymfasim">
and use mf-survival instead of mf-lifespan:
    <worm mf-survival="0.9" monthly.event.delay="+1">
to specify the fraction surviving to the next month

wormsim-2 v2.58Ap10 (Jan 6, 2016)

Implemented both anti-L3 and anti-worm (i.e. fecundity) immunity
Specify half-time (in years) of anti-L3 or anti-worm immunity with TH.L3 and TH.W
See further the formal description of Lymfasim.
Gamma is the strength of the immunity (either anti L3 or anti-worm)
and immunity.index is the individual variation in immune response

    <anti.L3.immunity TH.L3="1" gamma="0.1">
        <male>
            <immunity.index dist.nr="4" min="0" max="20" mean="1.0" p1="2"/>
        </male>
        <female>
            <immunity.index dist.nr="4" min="0" max="20" mean="1.0" p1="2"/>
        </female>
    </anti.L3.immunity>
    <anti.Worm.immunity TH.W="1" gamma="0.1">
        <male>
            <immunity.index dist.nr="4" min="0" max="20" mean="1.0" p1="2"/>
        </male>
        <female>
            <immunity.index dist.nr="4" min="0" max="20" mean="1.0" p1="2"/>
        </female>
    </anti.Worm.immunity>

A new additional parameter is success.ratio in 
    <fly transmission.probability="0.07345" success.ratio="0.00302">
This was necessary to boost anti-L3 immunity using mtp rather than the individual foi (without immune suppression)

wormsim-2 v2.58Ap9 (Mar 20, 2015)

Specify in the inputfile either:
    <skin.mf-density.per.worm fun.nr="1" a="7.6" c="-1"/>
or (see v258Ap9_STH_sat_fn.xml):
    <alt.skin.mf-density.per.worm>
            <a dist.nr="4" mean="1" p1="0.5"/>
            <b dist.nr="4" mean="1" p1="0.5"/>
            <c dist.nr="0" mean="1"/>
    </alt.skin.mf-density.per.worm>
In the latter case, the following formula is used to calculate sl(t) as a 
function of el(t), a, b and c:
 sl(t) = c * b * a * el(t) / (a*el(t)+b)

Note that the new saturating function for sl(t) is equivalent to the well-known equation 
Michaelis-Menten equation for enzyme kinetics: V = Vmax * [S] / (Km + [S])
(with Vmax = b*c and Km=b/a)

wormsim-2 v2.58Ap8 (Mar 19, 2015)
* modified as follows
  lu(m)_res=lu(m)_in+psi*(lu)(m-1)_res
  lr(m)    =lu(m)_res*zeta*v
* lines in mf log now contain: 
  seed year mf+ mf5+ w+ N aNmf aNmf20 N20 CMFL

wormsim-2 v2.58Ap7b (Mar 5, 2015)
* fixed bug in zeta and psi effects
  implemented:
  lu(m)_res=lu(m)_in+(1-zeta)*psi*(lu)(m-1)_res
  lr(m)    =lu(m)_res*zeta*v

wormsim-2 v2.58Ap7 (Mar 4, 2015)
* STH: added zeta and psi
* option to define skin snip categories
* see v258Ap7_test_1.xml

wormsim-2 v2.58Ap6 (Feb 25, 2015)
* lines in mf log now contain: seed year mf+ w+ N aNmf20 N20 CMFL

wormsim-2 v2.58Ap5 (Feb 18, 2015)
* STH extensions:

  renamed the <exposure> element to <exposure.and.contribution> and included a
  contribution function for age dependent contribution and an optional element 
  contribution index.

    <exposure.and.contribution>
        <initial.foi duration="7.5" foi="4.0"/>
        <male>
            <exposure.function fun.nr="1" a="0.05" c="1"/>
            <exposure.index dist.nr="4" min="0" max="20" p1="3.865"/>
            <contribution.function fun.nr="1" a="0.05" c="1"/>
        </male>
        <female>
            <exposure.function fun.nr="1" a="0.035" c="0.7"/>
            <exposure.index dist.nr="4" min="0" max="20" p1="3.865"/>
            <contribution.function fun.nr="1" a="0.035" c="1"/>
        </female>
    </exposure.and.contribution>

  add labda to age.dependent.mf-production to allow for density dependence of mf production
  the default value is 0 (also when labda attribute is omitted):

        <age.dependent.mf-production labda="0">
   

wormsim-2 v2.58Ap4 (Feb 12, 2015)
* added 'extra surveys' option
* added commandline option -n to suppress all output except the error log and mf log
* added mf log
* replaced definition of 'warm-up' by simulation start year

wormsim-2 v2.58Ap3 (Feb 13, 2014)
* fixed bug that caused FOI to become slightly negative resulting in dt<0
wormsim-2 v2.58Ap2 (Feb 13, 2014)
* fixed bug that caused the outputfile of the last succesful run to be replicated while averaging
* also fixed issue with the extent of outputfiles that occurred when run numbers do not start at 0
 
wormsim-2 v2.58Ap1 (September 25, 2013)
* fixed bug in OCP standardized output
wormsim-2 v2.58A (March 20, 2013)
* the detailed output of individual runs (*X.nnn and *Y.nnn files in the zip) are now 
  plain tab delimited text files
wormsim-2 v2.58 (Jan 7, 2012)
* added a new section to the inputfile to allow defining age classes for survey output 
  different from those used to specify the standard population
  see rbr75-exp677c13.xml and rbr75-exp677c14.xml and of course the XML schema wormsim.xsd
wormsim-2 v2.57 (Jan 5, 2012)
* modified implementation of ivermectin treatment effect to be compliant with Onchosim
  an ivermectin treatment can never lead to a recovery of a worm sooner than 
  the recovery from a previous treatment (this could happen due to heterogeneity in treatment effect and
  small interval between ivermectin treatments)
* corrected an error in the averaging procedure that caused error in zipping and deleting 
  output of individual runs when a range of runs did not start with 0

wormsim-2 v2.56 (Jan 4, 2012)
* corrected an error in the sequence of events triggered by the monthly event
  until now, L1uptake was based on mf load of the previous month
  in Onchosim, as now in Wormsim, the order is as follows:
  1. reproduction (i.e. insemination of female worms)
  2. mf production update
  3. calculcate FOI from L1uptake (or use clamped FOI during warmup period)
  4. distribute new worms

wormsim-2 v2.55 (Jan 3, 2012)
* corrected an error in the implementation of the delay of the monthly event
* inputfile rbr75-exp677c6.xml has the correct delays:
  reaper   		-4
  newborns 		-3   
  survey  		-2
  ivermectine 	-1
  monthly event +1
  
wormsim-2 v2.54 (Jan 2, 2012)
* included prepatent worms (both M and F) in ivermectin treatment ; this
  will cause F worms to have a lower mf production when becoming patent and inseminated 
  the first time


wormsim-2 v2.53 (Jan 2, 2012)
* modified defaults for optional delays; omitting the delays mentioned below will result in the default values specified below.

* modified default for optional delay attribute to survey start 		(default = -5 hours)
see:	<surveillance nr.skin-snips="2">
            <start year="2000" delay="-5"/>
            <stop year="2020"/>
            <interval years="5"/>
        </surveillance>

* modified default for optional delay attribute to treatment rounds		(default = -4 hours)
see:	<mass.treatment>
              <treatment.rounds>
                     <treatment.round year="2000" month="2" coverage="0.6" delay="-4"/>
                     <treatment.round year="2001" month="2" coverage="0.6" delay="-4"/>
                     <treatment.round year="2002" month="2" coverage="0.6" delay="-4"/>

* modified default for optional delay attribute to fertility table 		(default = -3 hours)
see:	<fertility.table delay="-3">

* modified default for optional delay attribute to the reaper   		(default = -2 hours)
see: 	<the.reaper max.population.size="440" reap="0.1" delay="-2"/>

* added optional monthly.event.delay attribute to worm					(default = -1 hour)
  this affects the monthly worm distribution
see		<worm mf-lifespan="9" monthly.event.delay="-1">

The allowed range for these attributes is +/- 12 (hours)

wormsim-2 v2.52 (Nov  30, 2011)
* included the Onchosim erroneous calculation Cw' = Cw + fc (instead of - as specified in the manual Cw' = Cw/(1-fc)) in the -o option
* added optional delay attribute to survey start 	(default = +0 hour)
* added optional delay attribute to the reaper   	(default = +1 hour)
* added optional delay attribute to fertility table (default = +2 hours)
* added optional delay attribute to treatment rounds(default = +3 hours)
  the allowed range for these attributes is +/- 12 (hours)
* allowed monthnr = 0 which is also the default for surveys ; yearnr=2000 and monthnr=0 is the same as yearnr=1999 monthnr=12


wormsim-2 v2.51 (Nov  9, 2011)
* added command-line option (-o) to reproduce Onchosim errors in births and exposure of newborns:
  with the -o option, newborns will be generated at the end of each year and added to the population in the past year (after the yearly survey)
  
wormsim-2 v2.50 (Oct 12, 2011)
* corrected error in getProduction() for Onchosim. The error was that recentlyInseminated() was NOT checked. 


wormsim-2 v0.01 (May 3, 2011)
* created a common code base for onchosim and schistosim
* checked events package: current version of event package will be common base
* see diff-oncho-schisto.txt file in package directory


Wormsim v0.96
1. Cosmetic change in Host.java: added method getSexRatio().
2. Modified FemaleWorm.recentlyInseminated() which did not produce the same result when 
   handling ReproductionEvent and MfProductionUpdateEvent. This may explain observed differences
   between Onchosim-97 and Wormsim-0.94 reported by Luc Coffeng.  

Wormsim v0.95
1. De random number generator wordt nu ook gebruikt voor poisson en neg binomiale verdelingen. 
   Dat betekent dat een run met dezelfde seed en inputfile altijd hetzelfde resultaat oplevert.
2. De malabsorptie factor is toegevoegd voor ivermectine behandelingen (random, niet consistent).
3. De leeftijdsafhankelijke mf productie moet nu in hetzelfde format als bij Onchosim worden opgegeven 
  (ipv de leeftijd vd worm moet nu het aantal jaar sinds patent worden gekoppeld aan een mf productie factor)
4. Een simulatierun duurt nu 4 sec ipv 20 sec (bij een maximale populatiegrootte van 440 op een enkele jaren oude laptop). 
 
