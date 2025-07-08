## Climate Adaptation QNMs >> Data

`dia` objects include: 
- Status Quo and Strategy QNMs, formatted to be (a) read into R (`data/dia`), and (b) used as figures (`data/dia/figures`)
- Altered versions of the Status Quo and Strategy QNMs for exploring influential links () and novel feedbacks (`data/dia/feedback`)

Intermediate and final output from boosted regression trees are in the `sensitivity` folder. **These files can be large, so I have told git to ignore them.** As a result, they are not available online, but they are easily reproduced by running `scripts/02_Influential_Link.Rmd`. Or you can get them by contacting the authors.

The `HAB_sim_out` folder contains *QPress* simulation output saved as .rds objects. **These files can be large, so I have told git to ignore them.** As a result, they are not available online, but they are easily reproduced by running `scripts/01_StatusQuo_Strat_x_HABhi.Rmd`. Or you can get them by contacting the authors.

The two "variable_key" .csv files are used to create figures. 