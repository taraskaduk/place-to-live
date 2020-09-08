source("R/packages.R")
source("R/functions.R")
# source("R/plan.R")    

plan <- code_to_plan("R/script.R")
config <- drake_config(plan)
vis_drake_graph(config)

make(plan)

loadd(final_filtered_nogeo)
# unlink(tempdir(), recursive = TRUE)

final_filtered_nogeo %>% 
  select(name_short, weather_points, zhvi_points, 
         dest_points, walkscore_points, 
         preference_points, total_weighted) %>% View()
