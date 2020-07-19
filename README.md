# Mair-et-al-2020


---
Supplementary Data for the Article "The MDD concept for establishing trust in non-significant results - a critical review"

---

This repository provides all R code for 
  
**Mair, MM, Kattwinkel, M, Jakoby, O and Hartig, F (2020) The MDD concept for establishing trust in non-significant results - a critical review**. 
    
      
*Abstract*  

Current regulatory guidelines for pesticide risk assessment recommend that non-significant results should be complemented by the minimum detectable difference (MDD), a statistical indicator that is used to decide if the experiment could have detected biologically relevant effects. Here, we review the statistical theory of the MDD and perform simulations to understand its properties and error rates. Most importantly, we compare the skill of the MDD in distinguishing between true and false negatives (i.e., type II errors) with two alternatives: the minimum detectable effect (MDE), an indicator based on a post-hoc power analysis common in medical studies; and confidence intervals (CIs). Our results demonstrate that MDD and MDE only differ in that the power of the MDD depends on the sample size. Moreover, while both MDD and MDE have some skill in distinguishing between false negatives and true absence of an effect, they do not perform as well as using CI upper bounds to establish trust in a non-significant result. The reason is that, unlike the CI, neither MDD nor MDE consider the estimated effect size in their calculation. We also show that MDD and MDE are no better than CIs in identifying larger effects among the false negatives. We conclude that, while MDDs are useful, CIs are preferable for deciding whether to treat a non-significant test result as a true negative, or for determining an upper bound for an unknown true effect.
 
 ---
  
The material is organized in two folders:

* [**Publication**](https://github.com/TheoreticalEcology/Mair-et-al-2020/tree/master/Publication): 

  Subfolder 'Code' contains the materials necessary to reproduce all simulations and analyses in Mair et al. (2020):
  + Simulation 1 belongs to the section "Can the MDD discriminate between true and false negative tests?"
  + Simulation 2 belongs to the section "Sensitivity of the MDD and other statistical filters to real effect size"

  Subfolder 'SupplementalData' contains the Supplemental Data file published along with the article. 
  The file 'MDD.R' contains the function used to calculate the minimum detectable difference (MDD) for the t-test

* [**MDDshiny**](https://github.com/TheoreticalEcology/Mair-et-al-2020/tree/master/MDDshiny): This contains an R Shiny application reproducing the experimental simulations (1 and 2). The app allows to specify experimental statistical parameters and produces figures similar to figures 5-7 in the published article. 

---

