# Mediation-analysis-for-disparity-research

Mediation analysis is used in many fields to differentiate effect between a predictor and an outcome through different middle variables (e.g. mediators or confounders). The R package mmabig is used to perform the mediation anlaysis. The mmabig package takes advantage of the Elastic Net regularized regression in the mediation analysis to identify significant mediators from a large dimension of potential middle variables and make inferences on their effects.

This  project aims at creating an interactive interface for the use of the R package: mmabig. We will then illustrate its use on a real data analysis.


#**Problem** 


Asthma is a chronic inflammatory respiratory illness characterized by coughing, wheezing, and shortness of breath and lower quality of life (CDC, 2011). It is often associated with familial, allergenic, socioeconomic, psychological, and environmental factors (CDC, 2003). Approximately, 20 million adults have asthma in the United States (US) (CDC, 2019). Self-reported current asthma prevalence among racial/ethnic minority populations ranged from 3.1% to 14.5%, compared with 7.6% among whites (CDC, 2004). Several factors have been associated with increased prevalence of asthma among adults. However, there has been little work done to date that explicitly evaluates the role played by factors such as alcohol, sociodemographic factors, depression, obesity and tobacco use in mediating observed disparities in asthma.

**Methods**

**Data**

We analyzed data from the 2018 Behavioral Risk Factor Surveillance System (BRFSS). The BRFSS comprises telephone surveys conducted by all 50 states, the District of Columbia, Puerto Rico, and Guam of the noninstitutionalized, civilian U.S. population aged >18 years. The survey collects information about modifiable risk factors for chronic diseases and other leading causes of death. 

**Outcome**

Current asthma was defined as a "yes" response to the same question and the question, "Do you still have asthma?"

**Predictor**

Race/ethnicity was defined as non-Hispanic White, non-Hispanic Black, non-Hispanic other and Hispanic.

**Study variables**

These variables included sex (Male/female), education level (less than high school/high school/college graduate), income, age, body mass index, and heavy alcohol drinking (Yes/No). Depression was assessed by the question “Have you ever told you had a depressive disorder” and classified as yes/no answers. Furthermore, those who responded “yes” to questions “Have you smoked at least 100 cigarettes in your entire life?” and “Have you ever used an ENDS…even just one time, in your entire life?” and “Have you ever used an smokeless tobacco products…even just one time, in your entire life?” and reported using these products “some days” or “every day” were classified as current users of cigarettes, electronic nicotine and delivery systems (ENDS) and smokeless tobacco (SLT), respectively. Finally, those who reported using marijuana at least one day in the past 30 days were categorized as current users.

![Up_Mediation Diagram](https://user-images.githubusercontent.com/86035332/123295649-edf73d00-d4e3-11eb-924e-e8107c3d3c38.png)




