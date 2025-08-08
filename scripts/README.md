## Climate Adaptation QNMs >> Scripts

Replicating Fisher et al. (2025)

**01_StatusQuo_Strat_x_HABhi** produces the output for section 3.1. *What HAB impacts are intensified / limited across adaptation strategies?* 
		- Input: QNMs as Dia objects from *data/dia*. 
		- Output: *QPress* simulation output as an .rds object in *data/HAB_sim_out*.
		- Figure: Produce Figure 2 using *Fig2.Rmd*.

**02_Sensitivity** identifies influential links according to boosted regression trees (Melbourne-Thomas et al. 2012) and edge strengths (Magel & Francis 2023; ultimately not used in paper). 

		- Input: *QPress* simulation output as an .rds object in *data/HAB_sim_out*. QNMs as Dia objects from *data/dia*. 
		- Intermediate output: GBM input matrix as an .rds object and in a .csv, in *data/sensitivity*.
		- Output: A matrix or dataframe containing relative influence values associated with each link for each variable, as a .csv object in *data/sensitivity*.
		- Figure: Supplemental figures are produced within script 02. 

**03_Influential_Link** produces the output for Results 3.2.1 *How do model assumptions alter the intensifying / reductive role of adaptation strategies? Altering an influential link.* 
		
		- Input: QNMs as Dia objects from *data/dia/influential_link*. 
		- Output: *QPress* simulation output in a list, saved as .rds objects in *data/HAB_sim_out*. There is one object per strategy.
		- Figure: Produce Figure 3 using *Fig3.Rmd*.

**04_Feedback.Rmd** produces the output for Results 3.2.2 *How do model assumptions alter the intensifying / reductive role of adaptation strategies? Completing a feedback.*
		
		- Input: QNMs as Dia objects from *data/dia/feedback*. 
		- Output: *QPress* simulation output in a list, as the .rds object *data/HAB_sim_out/Feedback_Infrastructure_x_HABhi_50k.rds*
		- Figure: Produce Figure 4 using *Fig4.Rmd*.




 The `**R**` folder contains custom functions that are called in the scripts listed above. Figures are saved into the `**drafts**` folder.
