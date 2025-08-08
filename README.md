# Climate Adaptation QNMs

#### Published as [Fisher et al. 2025 *Ecology & Society*](doi.org)

#### Product of the Ocean Modeling Forum [Climate & Communities Working Group](https://oceanmodelingforum.org/working-groups/climate-and-communities/)

#### Authors:
Mary C. Fisher, Laura K. Nelson, Tessa Francis, Phillip Levin, Jameal F. Samhouri, Chris Harvey, Laura Dee, Kristin N. Marshall, Steve J. Miller, Stephanie K. Moore, Michele L. Barnes, Curtis Cha, Joshua Eli Cinner, Steven A. Gray, Andre E. Punt, Corey Ridings, Franz W. Simon


### Abstract
Adaptation to climate change can have trade-offs and unintended outcomes that may add to climate impacts. Identifying how these consequences may arise in local contexts is an important step in climate adaptation planning, but the tools for doing so are still evolving. We demonstrate how social-ecological Qualitative Network Models (QNMs) can be used to explore the consequences of climate adaptation in fisheries. Drawing on the dynamics of fishers’ participation in the U.S. West Coast Dungeness crab fishery, we simulate a climate-intensified harmful algal bloom in a model fishing community and compare outcomes for human well-being, with and without climate adaptation. We consider a range of climate adaptations, from coping mechanisms to transformational adaptation, based on actions identified during participatory scenario planning initiatives. We first use QNMs to identify how common trade-offs arise across adaptation strategies, specifically highlighting how diverse strategies focusing on material loss result in persistent negative outcomes for community relationships and culture. We then explore alternative configurations of model structure to understand how plausible diversity in a social-ecological system can contribute to unintended, inequitable outcomes from climate adaptation. In our QNMs, altering in-season flexibility - fishers’ capacity to increase effort in alternative fisheries not affected by a harmful algal bloom - greatly influenced the degree to which climate adaptation reduced or intensified HAB impacts on well-being. Based on this exercise, QNMs are a useful tool for climate adaptation planning because they can be used to explore common trade-offs across adaptation options; highlight potentially inequitable adaptation outcomes associated with system complexity and uncertainties; and direct future research and monitoring priorities to help early identification of unintended consequences.

### Replicating Fisher et al. (2025)

**1.** The simplified Status Quo QNM structure in Figure 1 can be view using the [StatusQuo_HABhi_Fig1](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/data/dia/figures/StatusQuo_HABhi_Fig1.dia) and the [Fig1-StatusQuo-Fishing-Wellbeing](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/data/dia/figures/Fig1-StatusQuo-Fishing-Wellbeing.dia) Dia objects. You can also find Dia objects for the full Status Quo QNM [Fig S1](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/data/dia/figures/StatusQuo_HABhi_illustration_FigS1.dia) and the [Imposed](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/data/dia/figures/LivelihoodDiversify-1_HABhi_illustration.dia) / [Invested](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/data/dia/figures/LivelihoodDiversify-2_HABhi_illustration.dia) New Livelihoods QNMs (Fig S2-S3) in the `data/dia/figures` folder. These are nicely formatted versions of the dia objects that are read into R in Steps (2), (5), and (7), which are in the `data/dia` folder. *To get high-resolution versions of any Dia object, export the diagram as an SVG.*

**2.** Run script [01_StatusQuo_Strat_x_HABhi](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/scripts/01_StatusQuo_Strat_x_HABhi.Rmd) to reproduce Results 3.1. *What HAB impacts are intensified / limited across adaptation strategies?* 
		
		- Input: QNMs as Dia objects from *data/dia*. 
		- Output: *QPress* simulation output as an .rds object in *data/HAB_sim_out*.

**3.** Produce Figure 2 using script [Fig2.Rmd](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/scripts/Fig2.Rmd), which visualizes simulation output from (2).

**4.** Run script [02_Sensitivity](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/scripts/02_Sensitivity.Rmd) to identify influential links according to boosted regression trees (Melbourne-Thomas et al. 2012) and edge strengths (Magel & Francis 2023; ultimately not used in paper). 

		- Input: *QPress* simulation output as an .rds object in *data/HAB_sim_out*. QNMs as Dia objects from *data/dia*. 
		- Intermediate output: GBM input matrix as an .rds object and in a .csv, in *data/sensitivity*.
		- Output: A matrix or dataframe containing relative influence values associated with each link for each variable, as a .csv object in *data/sensitivity*.

**5.** Run script [03_Influential_Link](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/scripts/03_Influential_Link.Rmd) to produce the output for Results 3.2.1 *How do model assumptions alter the intensifying / reductive role of adaptation strategies? Altering an influential link.* 
		
		- Input: QNMs as Dia objects from *data/dia/influential_link*. 
		- Output: *QPress* simulation output in a list, saved as .rds objects in *data/HAB_sim_out*. There is one object per strategy.

**6.** Produce Figure 3 using [Fig3.Rmd](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/scripts/Fig3.Rmd), which visualizes simulation output from (5)

**7.** Run script [04_Feedback.Rmd](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/scripts/04_Feedback.Rmd) to produce the output for Results 3.2.2 *How do model assumptions alter the intensifying / reductive role of adaptation strategies? Completing a feedback.*
		
		- Input: QNMs as Dia objects from *data/dia/feedback*. 
		- Output: *QPress* simulation output in a list, as the .rds object *data/HAB_sim_out/Feedback_Infrastructure_x_HABhi_50k.rds*

**8.** Produce Figure 4 using [Fig4.Rmd](https://github.com/mfisher5/ClimateAdaptationQNMs/blob/main/scripts/Fig4.Rmd), which visualizes simulation output from (7)


All .Rmd scripts in the steps above can be found in the `**scripts**` folder. The `**R**` folder contains custom functions that are called in the scripts listed above. Figures are saved into the `**drafts**` folder. The `**doc**` folder contains a back-up of the publicly available documents / public-facing webpages from the Pacific Fishery Management Council's scenario planning initiative.

There is extensive supplementary material published with Fisher et al. to explain the rationale behind QNM variables and structure.

#### Acknowledgements
We are indebted to Dick Ogg, Garrett Dalan, and Mike Conroy for sharing their experiences and knowledge, which directed the questions we focused on and the variables we retained in the final model. We are similarly grateful to Sunny Jardine and Gway Kirchner, for their early reviews of the model structure. Jon Reum provided helpful feedback on an earlier draft of this paper. Genoa Sullaway provided organizational support for working meetings. Funding was provided by The David and Lucile Packard Foundation.
