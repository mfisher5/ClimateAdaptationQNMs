## Climate Adaptation QNMs >> Scripts

Replicating Fisher et al. (2025)

1. Produce Figure 1 using the `data/dia/figures/StatusQuo_HABhi_Fig1.dia` object. You can also find dia objects for Fig S.. in the `data/dia/figures` folder. These are formatted versions of the full dia objects that are read into R in (2)

2. Run `scripts/01_StatusQuo_Strat_x_HABhi` to produce the output for section 3.1. *What HAB impacts are intensified / limited across adaptation strategies?* 
		Input: QNMs as Dia objects from `data/dia`. 
		Output: *QPress* simulation output as an .rds object in `data/HAB_sim_out`.

3. Produce Figure 2 using `scripts/Fig2.Rmd`, which visualizes simulation output from (2).

4. Run `scripts/02_Influential_Link` to produce the output for section 3.2.1 *How do model assumptions alter the intensifying / reductive role of adaptation strategies? Altering an influential link.* 
		Input: *QPress* simulation output as an .rds object in `data/HAB_sim_out`. QNMs as Dia objects from `data/dia`. 
		Intermediate output: GBM input matrix as an .rds object and in a .csv, in `data/sensitivity`
		Output: .

5. Produce Figure 3 using `scripts/Fig3.Rmd`, which visualizes simulation output from (4)

6. Run `scripts/03_Feedback.Rmd' to produce the output for section 3.2.2 *How do model assumptions alter the intensifying / reductive role of adaptation strategies? Altering an influential link.*
		Input: QNMs as Dia objects from `data/dia/feedback`. 
		Output: *QPress* simulation output in a list, as the .rds object `data/HAB_sim_out/Feedback_Infrastructure_x_HABhi_50k.rds`

7. Produce Figure 4 using `scripts/Fig4.Rmd`, which visualizes simulation output from (6)

