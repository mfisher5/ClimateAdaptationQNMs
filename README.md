# Climate Adaptation QNMs

### Published as [Fisher et al. 2025 *Ecology & Society*](doi.org)

### Product of the Ocean Modeling Forum [Climate & Communities Working Group](https://oceanmodelingforum.org/working-groups/climate-and-communities/)


Adaptation to climate change can have trade-offs and unintended outcomes that may add to climate impacts. Identifying how these consequences may arise in local contexts is an important step in climate adaptation planning, but the tools for doing so are still evolving. We demonstrate how social-ecological Qualitative Network Models (QNMs) can be used to explore the consequences of climate adaptation in fisheries. Drawing on the dynamics of fishers’ participation in the U.S. West Coast Dungeness crab fishery, we simulate a climate-intensified harmful algal bloom in a model fishing community and compare outcomes for human well-being, with and without climate adaptation. We consider a range of climate adaptations, from coping mechanisms to transformational adaptation, based on actions identified during participatory scenario planning initiatives. We first use QNMs to identify how common trade-offs arise across adaptation strategies, specifically highlighting how diverse strategies focusing on material loss result in persistent negative outcomes for community relationships and culture. We then explore alternative configurations of model structure to understand how plausible diversity in a social-ecological system can contribute to unintended, inequitable outcomes from climate adaptation. In our QNMs, altering in-season flexibility - fishers’ capacity to increase effort in alternative fisheries not affected by a harmful algal bloom - greatly influenced the degree to which climate adaptation reduced or intensified HAB impacts on well-being. Based on this exercise, QNMs are a useful tool for climate adaptation planning because they can be used to explore common trade-offs across adaptation options; highlight potentially inequitable adaptation outcomes associated with system complexity and uncertainties; and direct future research and monitoring priorities to help early identification of unintended consequences.

#### Replicating Fisher et al. (2025)

1. Produce Figure 1 using the [`data/dia/figures/StatusQuo_HABhi_Fig1.dia`] object. You can also find dia objects for Fig S1-3. in the `data/dia/figures` folder. These are formatted versions of the full dia objects that are read into R in (2)

2. Run [`scripts/01_StatusQuo_Strat_x_HABhi`](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/scripts/01_StatusQuo_Strat_x_HABhi.Rmd) to produce the output for section 3.1. *What HAB impacts are intensified / limited across adaptation strategies?* 
		
		- Input: QNMs as Dia objects from `data/dia`. 
		- Output: *QPress* simulation output as an .rds object in `data/HAB_sim_out`.

3. Produce Figure 2 using [`scripts/Fig2.Rmd`](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/scripts/Fig2.Rmd), which visualizes simulation output from (2).

4. Run [`scripts/02_Sensitivity`](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/scripts/02_Sensitivity.Rmd) to identify influential links according to boosted regression trees (Melbourne-Thomas et al. 2012) and edge strengths (Magel & Francis 2023, ultimately not used in paper). 

5. Run [`scripts/03_Influential_Link`](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/scripts/03_Influential_Link.Rmd) to produce the output for section 3.2.1 *How do model assumptions alter the intensifying / reductive role of adaptation strategies? Altering an influential link.* 
		
		- Input: *QPress* simulation output as an .rds object in `data/HAB_sim_out`. QNMs as Dia objects from `data/dia`. 
		- Intermediate output: GBM input matrix as an .rds object and in a .csv, in `data/sensitivity`
		- Output: *QPress* simulation output as .rds objects in `data/HAB_sim_out`, one per strategy.

6. Produce Figure 3 using [`scripts/Fig3.Rmd`](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/scripts/Fig3.Rmd), which visualizes simulation output from (5)

7. Run [`scripts/04_Feedback.Rmd'](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/scripts/04_Feedback.Rmd) to produce the output for section 3.2.2 *How do model assumptions alter the intensifying / reductive role of adaptation strategies? Altering an influential link.*
		
		- Input: QNMs as Dia objects from `data/dia/feedback`. 
		- Output: *QPress* simulation output in a list, as the .rds object `data/HAB_sim_out/Feedback_Infrastructure_x_HABhi_50k.rds`

8. Produce Figure 4 using [`scripts/Fig4.Rmd`](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/scripts/Fig4.Rmd), which visualizes simulation output from (7)


The 'R' folder contains custom functions that are called in the scripts listed above. Figures are saved into the `drafts` folder. The `doc` folder contains a back-up of the publicly available documents / public-facing webpages from the Pacific Fishery Management Council's scenario planning initiative.
