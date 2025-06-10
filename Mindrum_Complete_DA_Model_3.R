#Mindrum DA model

# Holistic decision analysis model of agrosilvopastoral agroforestry system of Mindrum farm ####
# 14.9744 ha of two strip SRC operation measuring 200 x 20 m each 
# Scenario of changing silvoarable to silvopastoral is yet to be introduced, this will be first to introduce livestock elements
# Institutional support needs to reflect UK conditions


# Packages needed ####
#install.packages("decisionSupport")
library(decisionSupport)
library(ggplot2)
library(dplyr)
library(ggridges)

# Assign input table to object to easily use following "make_variables"-function
input_file <- read.csv("Input_table_Rev3_AM.csv") #, row.names = 1)


#Use make_variables function to test chunks of the function code (AF_benefit) during the development process
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)
}

generated_variables <- make_variables(as.estimate(input_file)) #Works by randomly selecting values from each variable of the input table and storing them in the global environment.These fixed values are not the ones used later in the Monte Carlo simulation but serve the sole purpose of allowing to run parts of the model and thereby testing is part for part

#herbal Grass Ley - herbal Grass Ley - herbal Grass Ley - Winter Wheat - Winter Wheat
#vs.
# herbal Grass Ley - herbal Grass Ley - Winter wheat - Winter CC - Spring Barley - Winter CC - Summer Beans - Winter Oats

Crop_rotation_1 <- c("Herbal_ley", "Herbal_ley", "Herbal_ley", "Winter_wheat", "Winter_wheat")
Crop_rotation_2 <- c("Herbal_ley", "Herbal_ley", "Winter_wheat", "Spring_barley", "Summer_beans", "Winter_oats")#Cover crops after Winter_wheat, Spring_barley and Summer_beans

n_years <- 60


total_one_time_funding <- 385 #This is to be taken from the UI reactive values. Hard coded for now, so the code works. 
total_annual_funding <- 500 #This is to be taken from the UI reactive values. Hard coded for now, so the code works.

treeless_system_crop_rotation_1 <- 0
treeless_system_crop_rotation_2 <- 1

agroforestry_system_1_crop_rotation_1 <- 0
agroforestry_system_1_crop_rotation_2 <- 1

agroforestry_system_2_crop_rotation_1 <- 0
agroforestry_system_2_crop_rotation_2 <- 1

include_animals_TL <- 1
include_animals_AF1 <- 0
include_animals_AF2 <- 0


# Function to get indices for a specific crop, repeating the rotation until n_years
Get_crop_indices <- function(Crop_rotation, Crop_name, n_years) {
  Rotation_length <- length(Crop_rotation)
  Full_rotation <- rep(Crop_rotation, length.out = n_years)
  
  # Find positions where the specified crop occurs
  Crop_indices <- which(Full_rotation == Crop_name)
  
  return(Crop_indices)
}


AF_benefit <- function(x, varnames) {
  
  #Variables, which are important for multiple crop rotation/AF-scenarios
  #Initial creation of the basic variables happens here, before conditional functions,
  #crop indices etc. are then added in the conditional functions, based on the scenario modelled
  Arable_area_AF1 <- arable_area - AF1_tree_row_area
  Arable_area_AF2 <- arable_area - AF2_tree_row_area
  
  Herbal_ley_yield <- rep(0, n_years)
  Herbal_ley_yield[1:n_years] <-
    vv(herbal_ley_yield, var_cv, length(n_years)) *  arable_area #[t]
  
  Winter_wheat_yield <- rep(0, n_years)
  Winter_wheat_yield[1:n_years] <-
   vv(winter_wheat_yield, var_cv, length(n_years)) * arable_area 
  
  Winter_cover_crop_yield <- rep(0, n_years)
  Winter_cover_crop_yield[1:n_years] <- vv(winter_cover_crop_yield, var_cv, length(n_years)) * arable_area #[t]
  
  #Costs
  Labour_costs <- rep(0, n_years) 
  Labour_costs <- vv(labour_costs, var_CV = var_cv, n_years) #[€/h]
  
  Herbal_ley_management_cost <- rep(0, n_years)
  Herbal_ley_management_cost[1:n_years] <- vv(herbal_ley_management, var_CV = var_cv, length(n_years))*arable_area
  
  Winter_wheat_management_cost <- rep(0, n_years)
  Winter_wheat_management_cost[1:n_years] <- vv(winter_wheat_management, var_CV = var_cv, length(n_years))*arable_area
  
  Herbal_ley_labour_cost <- rep(0, n_years)
  Herbal_ley_labour_cost[1:n_years] <- vv(herbal_ley_labour, var_CV = var_cv, length(n_years))*arable_area * Labour_costs
  
  Herbal_ley_grazing_labour_cost <- rep(0, n_years)
  Herbal_ley_grazing_labour_cost[1:n_years] <- vv(herbal_grazing_labour, var_cv, length(n_years))*arable_area * Labour_costs
  
  Winter_wheat_labour_cost <- rep(0, n_years)
  Winter_wheat_labour_cost[1:n_years] <- vv(winter_wheat_labour, var_CV = var_cv, length(n_years))*arable_area * Labour_costs
  
  
  #Livestock metrics: 
  # Convert fresh matter to dry matter
  Herbal_ley_dry_matter <- Herbal_ley_yield  #[tDM]
  
  Available_dry_matter_herbal_ley <- Herbal_ley_dry_matter * grazing_efficiency #[tDM]
  
  # Herbal ley cattle estimate live weight gain per hectare based grazing efficiency, daily DM intake
  Days_of_grazing_herbal_ley_cattle <- (Available_dry_matter_herbal_ley * cattle_intensity)/daily_dry_matter_intake_cattle #[d] #produces values over 365. Days of grazing means, how many days ONE cow could graze on the amount to forage available
  
  Total_saleable_beef_yield_herbal_ley <- Days_of_grazing_herbal_ley_cattle * daily_weight_gain_pasture_cattle #[t]
  
  # Herbal ley sheep estimate live weight gain per hectare based grazing efficiency, daily DM intake
  Days_of_grazing_herbal_ley_sheep <- (Available_dry_matter_herbal_ley * sheep_intensity)/daily_dry_matter_intake_sheep #[d] #produces values over 365. Days of grazing means, how many days ONE cow could graze on the amount to forage available
  
  Total_saleable_lamb_yield_herbal_ley <- Days_of_grazing_herbal_ley_sheep * daily_weight_gain_pasture_sheep #[t]
  
  # Winter crop fresh matter to dry matter
  Winter_cover_crop_dry_matter <- Winter_cover_crop_yield * winter_cover_crop_dry_matter  # [t]
  
  # Winter crop cattle estimate live weight gain per hectare based grazing efficiency, daily DM intake
  Days_of_grazing_winter_cover_crop_cattle <- (Winter_cover_crop_dry_matter * cattle_intensity)/daily_dry_matter_intake_cattle
  Winter_cover_crop_beef_yield <- Days_of_grazing_winter_cover_crop_cattle * daily_weight_gain_pasture_cattle #[t]
  
  # Winter crop sheep estimate live weight gain per hectare based grazing efficiency, daily DM intake
  Days_of_grazing_winter_cover_crop_sheep <- (Winter_cover_crop_dry_matter * sheep_intensity)/daily_dry_matter_intake_sheep
  Winter_cover_crop_lamb_yield <- Days_of_grazing_winter_cover_crop_sheep * daily_weight_gain_pasture_sheep #[t]
  
  
  #Crop indices
  
  #CR1:
  Herbal_ley_indices1 <- Get_crop_indices(Crop_rotation_1, "Herbal_ley", n_years)
  Winter_wheat_indices1 <- Get_crop_indices(Crop_rotation_1, "Winter_wheat", n_years)
  
  #CR2:
  Herbal_ley_indices2 <- Get_crop_indices(Crop_rotation_2, "Herbal_ley", n_years)
  Winter_wheat_indices2 <- Get_crop_indices(Crop_rotation_2, "Winter_wheat", n_years)
  Spring_barley_indices <- Get_crop_indices(Crop_rotation_2, "Spring_barley", n_years)
  Summer_beans_indices <- Get_crop_indices(Crop_rotation_2, "Summer_beans", n_years)
  Winter_oats_indices <- Get_crop_indices(Crop_rotation_2, "Winter_oats", n_years)
  
  Winter_cover_crop_indices <- sort(c(Winter_wheat_indices2, Spring_barley_indices))
  
  #Winter Crop
  Winter_cover_crop_yield[Winter_cover_crop_indices] <- vv(winter_cover_crop_yield, var_cv, length(Winter_cover_crop_indices)) *arable_area
  
  #Treeless system ###############################################################
  #Crop rotation 1:#####----------------------------------------------------------
    #Benefits:
  if(treeless_system_crop_rotation_1 ==1){  
    Herbal_ley_yield_CR1 <- rep(0, n_years)
    Herbal_ley_benefit_CR1 <- rep(0, n_years)
    Winter_wheat_yield_CR1 <- rep(0, n_years)
    
    #Crop rotation in AF system
    Herbal_ley_yield_CR1[Herbal_ley_indices1] <- Herbal_ley_yield[Herbal_ley_indices1]
    
    Winter_wheat_yield_CR1[Winter_wheat_indices1] <- Winter_wheat_yield[Winter_wheat_indices1] * herbal_effect #herbal ley fertilization effect increases wheat yield 
    
    #Benefits: 
    if (include_animals_TL == 0) {
      Herbal_ley_benefit_CR1 <-  vv(herbal_ley_value, var_cv, n_years) * Herbal_ley_yield_CR1
    }
    if (include_animals_TL == 1) { 
      Herbal_ley_benefit_CR1[Herbal_ley_indices1] <- (Total_saleable_beef_yield_herbal_ley[Herbal_ley_indices1] * beef_value*1000) + 
        (Total_saleable_lamb_yield_herbal_ley[Herbal_ley_indices1] * lamb_value * 1000)
    }
    
    Winter_wheat_benefit_CR1 <- vv(winter_wheat_value, var_cv, n_years) * Winter_wheat_yield_CR1
    
    Total_benefit_treeless_CR1 <- Herbal_ley_benefit_CR1 + Winter_wheat_benefit_CR1
    
    
    #Costs:
    #Management costs
    Herbal_ley_management_cost_CR1 <- rep(0, n_years) #includes: seed, insurance, fixed+variable machine cost (Values from GER available)
    Winter_wheat_management_cost_CR1 <- rep(0, n_years)
    
    Herbal_ley_management_cost_CR1[Herbal_ley_indices1] <- Herbal_ley_management_cost[Herbal_ley_indices1]
    
    Winter_wheat_management_cost_CR1[Winter_wheat_indices1] <- Winter_wheat_management_cost[Winter_wheat_indices1]
    
    #Labour costs
    Herbal_ley_labour_cost_CR1 <- rep(0, n_years)
    Winter_wheat_labour_cost_CR1 <- rep(0, n_years)
    Herbal_ley_grazing_labour_cost_CR1 <- rep(0, n_years)
    
    Herbal_ley_labour_cost_CR1[Herbal_ley_indices1] <- Herbal_ley_labour_cost[Herbal_ley_indices1]
    
    Herbal_ley_grazing_labour_cost_CR1[Herbal_ley_indices1] <- Herbal_ley_grazing_labour_cost[Herbal_ley_indices1]
    
    Winter_wheat_labour_cost_CR1[Winter_wheat_indices1] <- Winter_wheat_labour_cost[Winter_wheat_indices1]
    
    if(include_animals_TL == 1) {
      Total_herbal_ley_cost_CR1 <- Herbal_ley_management_cost_CR1 + Herbal_ley_labour_cost_CR1 + Herbal_ley_grazing_labour_cost_CR1
    }
    
    if(include_animals_TL == 0) {
      Total_herbal_ley_cost_CR1 <- Herbal_ley_management_cost_CR1 + Herbal_ley_labour_cost
    }
    
    
    Total_winter_wheat_cost_CR1 <- Winter_wheat_management_cost_CR1 + Winter_wheat_labour_cost
    
    
    Total_cost_treeless_CR1 <- Total_herbal_ley_cost_CR1 + Total_winter_wheat_cost_CR1
    
  #Bottom line treeless system:
    Treeless_bottom_line_benefit <- Total_benefit_treeless_CR1 - Total_cost_treeless_CR1
  }#will only be calculated, if User decides to check the box "Crop Rotation 1"
  
  #Crop_rotation_2:#####----------------------------------------------------------
  if(treeless_system_crop_rotation_2 ==1){  
    #Treeless system
    Herbal_ley_yield_CR2 <- rep(0, n_years)
    Winter_wheat_yield_CR2 <- rep(0, n_years)
    Spring_barley_yield <- rep(0, n_years)
    Summer_beans_yield <- rep(0, n_years)
    Winter_oats_yield <- rep(0, n_years)
    
    Winter_cover_crop_yield <- rep(0, n_years)
    
    Herbal_ley_benefit_CR2 <- rep(0, n_years)
    Winter_wheat_benefit_CR2 <- rep(0, n_years)
    
    #Crop rotation in treeless system
    Herbal_ley_yield_CR2[Herbal_ley_indices2] <- Herbal_ley_yield[Herbal_ley_indices2]
  
    Herbal_ley_benefit_CR2 <- vv(herbal_ley_value, var_cv, n_years) * Herbal_ley_yield_CR2
    
    Winter_wheat_yield_CR2[Winter_wheat_indices2] <- Winter_wheat_yield[Winter_wheat_indices2] * herbal_effect #herbal ley fertilization effect increases wheat yield 
    Winter_wheat_benefit_CR2 <- vv(winter_wheat_value, var_cv, n_years) * Winter_wheat_yield_CR2
    
    Spring_barley_yield[Spring_barley_indices] <-
      vv(spring_barley_yield, var_cv, length(Spring_barley_indices)) *  arable_area * winter_grazing_effect
    Spring_barley_benefit <- vv(spring_barley_value, var_cv, n_years) * Spring_barley_yield
    
    Summer_beans_yield[Summer_beans_indices] <- 
      vv(summer_beans_yield, var_cv, length(Summer_beans_indices)) *  arable_area * winter_grazing_effect
    Summer_beans_benefit <- vv(summer_beans_value, var_cv, n_years) * Summer_beans_yield
    
    Winter_oats_yield[Winter_oats_indices] <- 
      vv(winter_oats_yield, var_cv, length(Winter_oats_indices)) *  arable_area * summer_beans_effect 
    Winter_oat_benefit <- vv(winter_oats_value, var_cv, n_years) * Winter_oats_yield 
    
    Winter_cover_crop_yield[Winter_cover_crop_indices] <- vv(winter_cover_crop_yield, var_cv, length(Winter_cover_crop_indices)) *arable_area
    
    #Livestock metrics:
    Herbal_ley_livestock_yield_CR2 <- rep(0, n_years)
    Herbal_ley_livestock_yield_CR2[Herbal_ley_indices2] <- (Total_saleable_beef_yield_herbal_ley[Herbal_ley_indices2] * beef_value * 1000) + 
      (Total_saleable_lamb_yield_herbal_ley[Herbal_ley_indices2] * lamb_value * 1000)
    
    Winter_cover_crop_benefit <- rep(0, n_years)
    Winter_cover_crop_benefit[Winter_cover_crop_indices] <- (Winter_cover_crop_beef_yield[Winter_cover_crop_indices] * beef_value * 1000) + 
      (Winter_cover_crop_lamb_yield[Winter_cover_crop_indices] * lamb_value * 1000)
    
    Livestock_benefit_CR2 <- (Herbal_ley_livestock_yield_CR2 + Winter_cover_crop_benefit) 
    
    # Spring_barley_dry_matter <- Spring_barley_yield * barley_dry_matter_content # 
    # Spring_barley_beef_yield <- Spring_barley_dry_matter * cattle_feed_conversion_efficiency # kg live weight gain/ha
    # 
    # Summer_beans_dry_matter <- Summer_beans_yield * 1000 * beans_dry_matter_content # kg DM per ha
    # Summer_beans_beef_yield <- Summer_beans_dry_matter * cattle_feed_conversion_efficiency # kg live weight gain/ha
    # 
    # Winter_oats_dry_matter <- Winter_oats_yield * 1000 * oats_dry_matter_content # kg DM per ha
    # Winter_oats_beef_yield <- Winter_oats_dry_matter * cattle_feed_conversion_efficiency # kg live weight gain/ha
    
    if(include_animals_TL == 1){
      Total_benefit_treeless_CR2 <- Livestock_benefit_CR2 + Winter_wheat_benefit_CR2 + + Spring_barley_benefit + Summer_beans_benefit + Winter_oat_benefit
    }
    if(include_animals_TL == 0){
      Total_benefit_treeless_CR2 <- Herbal_ley_benefit_CR2 + Winter_wheat_benefit_CR2 + Spring_barley_benefit + Summer_beans_benefit + Winter_oat_benefit
    }
    
    #Costs:
    #Management costs
    Herbal_ley_management_cost_CR2 <- rep(0, n_years) #includes: seed, insurance, fixed+variable machine cost (Values from GER available)
    Winter_wheat_management_cost_CR2 <- rep(0, n_years)
    Spring_barley_management_cost <- rep(0, n_years)
    Summer_beans_management_cost <- rep(0, n_years)
    Winter_oats_management_cost <- rep(0, n_years)
    
    Winter_cover_crop_management_cost <- rep(0, n_years)
    
    Herbal_ley_management_cost_CR2[Herbal_ley_indices2] <- Herbal_ley_management_cost[Herbal_ley_indices2]
    Winter_wheat_management_cost_CR2[Winter_wheat_indices2] <- Winter_wheat_management_cost[Winter_wheat_indices2]
    Spring_barley_management_cost[Spring_barley_indices] <- vv(spring_barley_management, var_CV = var_cv, length(Spring_barley_indices))*arable_area
    Summer_beans_management_cost[Summer_beans_indices] <- vv(summer_beans_management, var_CV = var_cv, length(Summer_beans_indices))*arable_area
    Winter_oats_management_cost[Winter_oats_indices] <- vv(winter_oats_management, var_CV = var_cv, length(Winter_oats_indices))*arable_area
    
    Winter_cover_crop_management_cost[Winter_cover_crop_indices] <- vv(winter_cover_crop_management, var_CV = var_cv, length(Winter_cover_crop_indices))*arable_area
    
    #Labour costs
    Herbal_ley_labour_cost_CR2 <- rep(0, n_years)
    Winter_wheat_labour_cost_CR2 <- rep(0, n_years)
    Spring_barley_labour_cost <- rep(0, n_years)
    Summer_beans_labour_cost <- rep(0, n_years)
    Winter_oats_labour_cost <- rep(0, n_years)
    
    Winter_cover_crop_labour_cost <- rep(0, n_years)
    
    Herbal_ley_grazing_labour_cost_CR2 <- rep(0, n_years)
    Winter_CC_grazing_labour <- rep(0, n_years)
    
    
    Herbal_ley_labour_cost_CR2[Herbal_ley_indices2] <- Herbal_ley_labour_cost[Herbal_ley_indices2]
    Winter_wheat_labour_cost_CR2[Winter_wheat_indices2] <- Winter_wheat_labour_cost[Winter_wheat_indices2]
    Spring_barley_labour_cost[Spring_barley_indices] <- vv(spring_barley_labour, var_CV = var_cv, length(Spring_barley_indices))*arable_area * Labour_costs[Spring_barley_indices]
    Summer_beans_labour_cost[Summer_beans_indices] <- vv(summer_beans_labour, var_CV = var_cv, length(Summer_beans_indices))*arable_area * Labour_costs[Summer_beans_indices]
    Winter_oats_labour_cost[Winter_oats_indices] <- vv(winter_oats_labour, var_CV = var_cv, length(Winter_oats_indices))*arable_area * Labour_costs[Winter_oats_indices]
    
    Winter_cover_crop_labour_cost[Winter_cover_crop_indices] <- vv(winter_cover_crop_labour, var_CV = var_cv, length(Winter_cover_crop_indices))*arable_area * Labour_costs[Winter_cover_crop_indices]
    
    Herbal_ley_grazing_labour_cost_CR2[Herbal_ley_indices2] <- Herbal_ley_grazing_labour_cost[Herbal_ley_indices2]
    Winter_CC_grazing_labour[Winter_cover_crop_indices] <- vv(cover_crop_grazing_labour, var_cv, length (Winter_cover_crop_indices))*arable_area * Labour_costs[Winter_cover_crop_indices]
    
    Total_herbal_ley_cost_CR2 <- Herbal_ley_management_cost_CR2 + Herbal_ley_labour_cost_CR2
    Total_winter_wheat_cost_CR2 <- Winter_wheat_management_cost_CR2 + Winter_wheat_labour_cost_CR2
    Total_spring_barley_cost <- Spring_barley_management_cost + Spring_barley_labour_cost
    Total_summer_beans_cost <- Summer_beans_management_cost + Summer_beans_labour_cost
    Total_winter_oats_cost <- Winter_oats_management_cost + Winter_oats_labour_cost
    
    Total_winter_cover_crop_cost <- Winter_cover_crop_management_cost + Winter_cover_crop_labour_cost
    
    Total_grazing_cost <- Herbal_ley_grazing_labour_cost_CR2 + Winter_CC_grazing_labour
    
    if(include_animals_TL == 1){
      Total_cost_treeless_CR2 <- Total_herbal_ley_cost_CR2 + Total_winter_wheat_cost_CR2 + Total_spring_barley_cost + Total_summer_beans_cost + Total_winter_oats_cost + Total_winter_cover_crop_cost + Total_grazing_cost
    }
    if(include_animals_TL == 0){
      Total_cost_treeless_CR2 <- Total_herbal_ley_cost_CR2 + Total_winter_wheat_cost_CR2 + Total_spring_barley_cost + Total_summer_beans_cost + Total_winter_oats_cost + Total_winter_cover_crop_cost
    }
    
     
    #Bottom line treeless system:
    Treeless_bottom_line_benefit <- Total_benefit_treeless_CR2 - Total_cost_treeless_CR2
  }
  
  #AGROFORESTRY SYSTEM 1 - #######################################################
  

    
  #Implementation cost variables
  AF1_planning_cost <- rep(0, n_years) #FE;Invoice of service provider (planners/consultants), planning the AF system + measuring tree strips using GPS[€]
  AF1_tree_cost <- rep(0, n_years) #FE; Cost per tree [€]
  
  
  AF1_plant_tree_cost <- rep(0, n_years) #FE; Labour cost for planting one tree [€] -
  AF1_protect_cost <- rep(0, n_years) #FE; Material cost of tree protection mesh [€]
  AF1_field_prep_cost <- rep(0, n_years) #cost for subsoiling and harrowing [€/ha]
  AF1_weed_protect_cost <- rep(0, n_years) #Material cost of weed suppressing fleece [€]
  #AF1_compost_cost <- rep(0, n_years) #FE; Cost of compost used during planting [€]
  #AF1_irrigation_system_cost <- rep(0, n_years) #FE; Material and labour cost of installing a drip irrigation system in the tree rows [€]
  #AF1_irrigation_planting_cost <- rep(0, n_years) #FE; Cost for watering in newly planted trees [€]
  AF1_total_planting_cost <- rep(0, n_years)
  
  AF1_planning_cost[1] <- planning_consulting + (farmer_planning_time * Labour_costs[1])
  
  #Field prep
  AF1_tree_cost[1] <- SRC_cutting_price * AF1_num_trees
  AF1_field_prep_cost[1] <- SRC_field_prep * AF1_tree_row_area * Labour_costs[1]
  AF1_plant_tree_cost[1] <- SRC_machine_rent + SRC_planting * AF1_tree_row_area * Labour_costs[1]
  AF1_protect_cost[1] <- AF1_plant_protection * AF1_num_trees
  AF1_weed_protect_cost[1] <- weed_protection * AF1_tree_row_area * Labour_costs[1]
  #AF1_compost_cost[1] <- compost_planting_tree * compost_price * AF1_num_trees
  #AF1_irrigation_system_cost[1] <- irrigation_sys_install
  #AF1_irrigation_planting_cost[1] <- irrigation_planting_tree * water_price * AF1_num_trees
  
  AF1_total_planting_cost <- AF1_tree_cost + AF1_field_prep_cost + AF1_plant_tree_cost + AF1_protect_cost + AF1_weed_protect_cost
  
  AF1_total_investment_cost <- AF1_planning_cost + AF1_total_planting_cost #Investment cost of AF system implementation
  
  
  #Running cost variables
  AF1_subsidy_application <- rep(0, n_years) #FE; Time (regarded as labour cost) spent for application of Eco Scheme subsidy [€]
  AF1_annual_irrigation <- rep(0, n_years) #FE; Cost of annual irrigation of tree rows [€]
  AF1_timber_harvest <- rep(0, n_years)
  #create indices for harvest timing 
  Timber_harvest_indices <- round(seq(from = time_to_first_timber, to = n_years, by = round(harvest_interval_SRC)))
  
  AF1_subsidy_application <- vv(subsidy_application, var_cv, n_years) * Labour_costs #application subsidy has to be repeated annually 
  
  #AF1_annual_irrigation[1:3] <- vv(irrigation_123, var_CV = var_cv, 3)
  #AF1_annual_irrigation[4:n_years] <- vv(irrigation_annual, var_CV = var_cv, length(4:n_years))
  #AF1_annual_irrigation_cost <- AF1_annual_irrigation * water_price
  
  AF1_timber_harvest[Timber_harvest_indices] <-
    vv(timber_harvest, var_CV = var_cv, length(Timber_harvest_indices)) #Labour hours required to harvest SRC
  
  AF1_timber_harvest_cost <- AF1_timber_harvest * AF1_tree_row_area * Labour_costs
  
  AF1_total_running_cost <- AF1_subsidy_application + AF1_timber_harvest_cost #+ AF1_annual_irrigation_cost
  
  AF1_total_treerow_cost <- AF1_total_investment_cost + AF1_total_running_cost
  
  
  # account for yield reduction due to shading and competition from trees 
  AF1_perc_yield_reduction <- gompertz_yield(
    max_harvest = yield_reduc_max,
    time_to_first_yield_estimate = time_to_first_reduction,
    time_to_second_yield_estimate = time_to_second_reduction,
    first_yield_estimate_percent = perc_max_first_reduction,
    second_yield_estimate_percent = perc_max_second_reduction,
    n_years = n_years)
  
 
  # Calculate timber yield
  AF1_tot_timber_yield <- rep(0, n_years)  # Initialize a vector of zeros for each year
  AF1_tot_timber_yield[Timber_harvest_indices] <- AF1_tree_row_area * tree_yield_max * round(harvest_interval_SRC) #linear growth in SRC is assumed
  #annual biomass production (tree_yield_max) is multiplied by the number of years between the harvest years (harvest_interval_SRC)
  #more accurate growth dynamics would be appreciated
  
  #total benefit calculation
  AF1_biomass_timber_benefit <- 
    vv(biomass_timber_price, var_cv, n_years) * AF1_tot_timber_yield 
  
  #Subsidy in AF system
  #AF1_subsidy[1:n_years] <- AF1_subsidy * AF1_tree_row_area
  
  
  # Intangible benefits from ESS like soil erosion control, improved soil quality and biodiversity, change in microclimate, reduced impact of extreme events
  #carbon sequestration in T C/ha/yr
  
  AF1_GW_benefit <- rep(0, n_years)
  AF1_erosion_control_benefit <- rep(0, n_years)
  # groundwater storage and erosion control not realised in the first few years 
  #- can also make a variable and add vv fn.
  NMES_indices <- seq(from = 5, to = n_years)
  AF1_GW_benefit[NMES_indices] <-
    vv(pc_ground_water_recharge, var_cv, length(NMES_indices)) * arable_area
  
  AF1_erosion_control_benefit[NMES_indices] <- (vv(AF1_soil_loss_water, var_cv, length(NMES_indices)) + vv(AF1_soil_loss_wind, var_cv, length(NMES_indices))) * 
    vv(pc_soil_loss, var_cv, length(NMES_indices)) * arable_area  
  #pollinator_benefit yet to be added
  
  AF1_Nonmarket_ES_benefit <- AF1_GW_benefit + AF1_erosion_control_benefit 
  
  # from Porter et al. 2009
  #Nonmarket_ES_benefit <- vv(Nonmarket_ES_value, var_cv, n_years) * tree_row_area
  
  
  AF1_tree_benefit <- AF1_biomass_timber_benefit + AF1_Nonmarket_ES_benefit + total_one_time_funding + total_annual_funding
  
  AF1_farm_benefit <- AF1_biomass_timber_benefit + total_one_time_funding + total_annual_funding
  
  #woody benefit for livestock  
  AF1_total_woody_benefit <- rep(0, n_years) 
  AF1_total_woody_benefit <- vv(woody_benefit_shade, var_cv, n_years) + vv(woody_benefit_nutrition, var_cv, n_years)+
    vv(AF1_woody_benefit_windreduc, var_cv, n_years) #used in crop rotation part of code  
  
  #Crop rotation 1:#####----------------------------------------------------------
  if(agroforestry_system_1_crop_rotation_1 == 1) {
    
    #Annual arable crop component
    AF1_herbal_ley_yield_CR1 <- rep(0, n_years)
    AF1_winter_wheat_yield_CR1 <- rep(0, n_years)
    AF1_herbal_ley_beef_yield <- rep(0, n_years)
    
    #Crop rotation in AF system
    AF1_herbal_ley_yield_CR1[Herbal_ley_indices1] <- ((Herbal_ley_yield[Herbal_ley_indices1]/arable_area)*Arable_area_AF1) *(1 - AF1_perc_yield_reduction[Herbal_ley_indices1])
    
    # Convert fresh matter to dry matter
    AF1_Herbal_ley_dry_matter_CR1 <- AF1_herbal_ley_yield_CR1 #[tDM]
    
    AF1_Available_dry_matter_herbal_ley_CR1 <- AF1_Herbal_ley_dry_matter_CR1 * grazing_efficiency #[tDM]
    
    # Herbal ley cattle estimate live weight gain per hectare based grazing efficiency, daily DM intake
    Days_of_grazing_herbal_ley_cattle <- (AF1_Available_dry_matter_herbal_ley_CR1 * cattle_intensity)/daily_dry_matter_intake_cattle #[d] #produces values over 365. Days of grazing means, how many days ONE cow could graze on the amount to forage available
    
    AF1_Total_saleable_beef_yield_herbal_ley_CR1 <- Days_of_grazing_herbal_ley_cattle * daily_weight_gain_pasture_cattle #[t/ha]
    
    # Herbal ley sheep estimate live weight gain per hectare based grazing efficiency, daily DM intake
    Days_of_grazing_herbal_ley_sheep <- (AF1_Available_dry_matter_herbal_ley_CR1 * sheep_intensity)/daily_dry_matter_intake_sheep #[d] #produces values over 365. Days of grazing means, how many days ONE cow could graze on the amount to forage available
    
    AF1_Total_saleable_lamb_yield_herbal_ley_CR1 <- Days_of_grazing_herbal_ley_sheep * daily_weight_gain_pasture_sheep #[t/ha]
    
    
    #Livestock metrics: 
    # Estimate live weight gain per hectare based on FCE
    AF1_herbal_ley_livestock_yield <- rep(0, n_years) 
    AF1_herbal_ley_livestock_yield[Herbal_ley_indices1] <- (AF1_Total_saleable_beef_yield_herbal_ley_CR1[Herbal_ley_indices1] * beef_value*1000) + 
      (AF1_Total_saleable_lamb_yield_herbal_ley_CR1[Herbal_ley_indices1] * lamb_value*1000)
    
    AF1_winter_wheat_yield_CR1[Winter_wheat_indices1] <- (Winter_wheat_yield[Winter_wheat_indices1]/arable_area)*Arable_area_AF1* (1 - AF1_perc_yield_reduction[Winter_wheat_indices1])
    
    #Benefits: 
    if (include_animals_AF1 == 0) {
      AF1_herbal_ley_benefit_CR1 <-  vv(herbal_ley_value, var_cv, n_years) * AF1_herbal_ley_yield_CR1
    }
    if (include_animals_AF1 == 1) { 
      AF1_herbal_ley_benefit_CR1 <- AF1_herbal_ley_livestock_yield * (1 + AF1_total_woody_benefit) #AF1_herbal_ley_beef_yield is already in [€]
    }
    
    AF1_winter_wheat_benefit_CR1 <- vv(winter_wheat_value, var_cv, n_years) * AF1_winter_wheat_yield_CR1
    
    AF1_total_benefit_CR1 <- AF1_farm_benefit + AF1_herbal_ley_benefit_CR1 + AF1_winter_wheat_benefit_CR1
    
    
    #Costs:
    #Management costs
    AF1_herbal_ley_management_cost <- rep(0, n_years) #includes: seed, insurance, fixed+variable machine cost (Values from GER available)
    AF1_winter_wheat_management_cost <- rep(0, n_years)
    
    AF1_herbal_ley_management_cost[Herbal_ley_indices1] <- (Herbal_ley_management_cost[Herbal_ley_indices1]/arable_area)*Arable_area_AF1
    AF1_winter_wheat_management_cost[Winter_wheat_indices1] <- (Winter_wheat_management_cost[Winter_wheat_indices1]/arable_area)*Arable_area_AF1
    
    #Labour costs
    AF1_herbal_ley_labour_cost_CR1 <- rep(0, n_years)
    AF1_winter_wheat_labour_cost <- rep(0, n_years)
    
    if (include_animals_AF1 == 0) {
      AF1_herbal_ley_labour_cost_CR1[Herbal_ley_indices1] <- (Herbal_ley_labour_cost[Herbal_ley_indices1]/arable_area) * Arable_area_AF1 * af1_added_management_time_factor
    }
    if (include_animals_AF1 == 1) {
      AF1_herbal_ley_labour_cost_CR1[Herbal_ley_indices1] <- (Herbal_ley_grazing_labour_cost_CR1[Herbal_ley_indices1]/arable_area)* Arable_area_AF1 * af1_less_grazing_management_time_factor
    }
    
    AF1_winter_wheat_labour_cost <- (Winter_wheat_labour_cost_CR1/arable_area) * Arable_area_AF1 * af1_added_management_time_factor
    
    AF1_total_herbal_ley_cost_CR1 <- AF1_herbal_ley_management_cost + AF1_herbal_ley_labour_cost_CR1
    AF1_winter_wheat_cost_CR1 <- AF1_winter_wheat_management_cost + AF1_winter_wheat_labour_cost
    
    AF1_total_cost_CR1 <- AF1_total_treerow_cost + AF1_total_herbal_ley_cost_CR1 + AF1_winter_wheat_cost_CR1
    
    ##Bottom line AF system 1:
    AF1_bottom_line_benefit <- AF1_total_benefit_CR1 - AF1_total_cost_CR1
  }#Will only be calculated if user checks the box "Crop rotation 1" for AF 1
  
  
  
  #Crop rotation 2:#####----------------------------------------------------------
  #AF type 1, Crop rotation 2
  if(agroforestry_system_1_crop_rotation_2 == 1) {
    
    #Annual arable crop component
    AF1_herbal_ley_yield_CR2 <- rep(0, n_years)
    AF1_Winter_wheat_yield_CR2 <- rep(0, n_years)
    AF1_Spring_barley_yield <- rep(0, n_years)
    AF1_Summer_beans_yield <- rep(0, n_years)
    AF1_Winter_oats_yield <- rep(0, n_years)
    AF1_winter_crop_yield <- rep(0, n_years)
    
    #Crop rotation 2 in AF system 1
    AF1_herbal_ley_yield_CR2 <- (Herbal_ley_yield/arable_area) *  Arable_area_AF1 * (1 - AF1_perc_yield_reduction)
    AF1_herbal_ley_benefit_CR2 <- vv(herbal_ley_value, var_cv, n_years) * AF1_herbal_ley_yield_CR2 
    
    AF1_winter_wheat_yield_CR2 <- (Winter_wheat_yield_CR2/arable_area) * Arable_area_AF1 * (1 - AF1_perc_yield_reduction)  
    AF1_winter_wheat_benefit_CR2 <- vv(winter_wheat_value, var_cv, n_years) * AF1_Winter_wheat_yield_CR2
    
    AF1_spring_barley_yield <- (Spring_barley_yield/arable_area) * Arable_area_AF1 * (1 - AF1_perc_yield_reduction)
    AF1_spring_barley_benefit <- vv(spring_barley_value, var_cv, n_years) * AF1_Spring_barley_yield
    
    AF1_summer_beans_yield <- (Summer_beans_yield/arable_area) * Arable_area_AF1 * (1 - AF1_perc_yield_reduction)
    AF1_summer_beans_benefit <- vv(summer_beans_value, var_cv, n_years) * AF1_summer_beans_yield
    
    AF1_winter_oats_yield <- (Winter_oats_yield/arable_area) * Arable_area_AF1 * (1 - AF1_perc_yield_reduction)
    AF1_winter_oat_benefit <- vv(winter_oats_value, var_cv, n_years) * AF1_Winter_oats_yield 
    
    AF1_winter_crop_yield <- (Winter_cover_crop_yield/arable_area) *  Arable_area_AF1 * (1 - AF1_perc_yield_reduction)
    
    #Livestock metrics: 
    ### Herbal Ley
    AF1_herbal_ley_dry_matter_CR2 <- AF1_herbal_ley_yield_CR2 # [t]
    
    #Cattle metrics
    AF1_days_of_grazing_herbal_ley_cattle_CR2 <- (AF1_herbal_ley_dry_matter_CR2 * cattle_intensity)/daily_dry_matter_intake_cattle
    AF1_herbal_ley_beef_yield_CR2 <- AF1_days_of_grazing_herbal_ley_cattle_CR2 * daily_weight_gain_pasture_cattle #[t]
    
    #Sheep metrics
    AF1_days_of_grazing_herbal_ley_sheep_CR2 <- (AF1_herbal_ley_dry_matter_CR2 * sheep_intensity)/daily_dry_matter_intake_sheep
    AF1_herbal_ley_lamb_yield_CR2 <- AF1_days_of_grazing_herbal_ley_sheep_CR2 * daily_weight_gain_pasture_sheep #[t]
    
    
    ### Winter Crop
    AF1_winter_crop_dry_matter <- AF1_winter_crop_yield * winter_cover_crop_dry_matter  # [t]
    
    #Cattle metrics
    AF1_days_of_grazing_winter_crop_cattle <- (AF1_winter_crop_dry_matter * cattle_intensity)/daily_dry_matter_intake_cattle
    AF1_winter_crop_beef_yield <- AF1_days_of_grazing_winter_crop_cattle * daily_weight_gain_pasture_cattle #[t]
    
    #Sheep metrics
    AF1_days_of_grazing_winter_crop_sheep <- (AF1_winter_crop_dry_matter * sheep_intensity)/daily_dry_matter_intake_sheep
    AF1_winter_crop_lamb_yield <- AF1_days_of_grazing_winter_crop_sheep * daily_weight_gain_pasture_sheep #[t]
    
    # AF1_spring_barley_dry_matter <- AF1_spring_barley_yield * 1000 * barley_dry_matter_content # kg DM per ha
    # AF1_spring_barley_beef_yield <- AF1_spring_barley_dry_matter * cattle_feed_conversion_efficiency  # kg live weight gain/ha
    # 
    # AF1_summer_beans_dry_matter <- AF1_summer_beans_yield * 1000 * beans_dry_matter_content # kg DM per ha
    # AF1_summer_beans_beef_yield <- AF1_summer_beans_dry_matter * cattle_feed_conversion_efficiency  # kg live weight gain/ha
    # 
    # Af1_winter_oats_dry_matter <- AF1_winter_oats_yield * 1000 * oats_dry_matter_content # kg DM per ha
    # AF1_winter_oats_beef_yield <- Af1_winter_oats_dry_matter * cattle_feed_conversion_efficiency  # kg live weight gain/ha
    
    AF1_livestock_benefit_CR2 <- ((AF1_herbal_ley_beef_yield_CR2 + AF1_winter_crop_beef_yield) * beef_value * 1000 * (1 + AF1_total_woody_benefit)) +
      ((AF1_herbal_ley_lamb_yield_CR2 + AF1_winter_crop_lamb_yield) * lamb_value * 1000 * (1 + AF1_total_woody_benefit))
    
    
    if(include_animals_AF1 == 1){
      AF1_total_benefit_CR2 <- AF1_farm_benefit + AF1_livestock_benefit_CR2 + AF1_winter_wheat_benefit_CR2 + AF1_spring_barley_benefit + AF1_summer_beans_benefit + AF1_winter_oat_benefit
    }
    if(include_animals_AF1 == 0){
      AF1_total_benefit_CR2 <- AF1_farm_benefit + AF1_herbal_ley_benefit_CR2 + AF1_winter_wheat_benefit_CR2 + AF1_spring_barley_benefit + AF1_summer_beans_benefit + AF1_winter_oat_benefit
    }
    
    #Costs:
    #Management costs
    AF1_herbal_ley_management_cost_CR2 <- rep(0, n_years) #includes: seed, insurance, fixed+variable machine cost (Values from GER available)
    AF1_winter_wheat_management_cost_CR2 <- rep(0, n_years)
    AF1_spring_barley_management_cost <- rep(0, n_years)
    AF1_summer_beans_management_cost <- rep(0, n_years)
    AF1_winter_oats_management_cost <- rep(0, n_years)
    
    AF1_winter_cover_crop_management_cost <- rep(0, n_years)
    
    AF1_herbal_ley_management_cost_CR2[Herbal_ley_indices2] <- (Herbal_ley_management_cost_CR2[Herbal_ley_indices2]/arable_area)*Arable_area_AF1
    AF1_winter_wheat_management_cost_CR2[Winter_wheat_indices2] <- (Winter_wheat_management_cost_CR2[Winter_wheat_indices2]/arable_area)*Arable_area_AF1
    AF1_spring_barley_management_cost <- (Spring_barley_management_cost/arable_area)*Arable_area_AF1
    AF1_sumer_beans_management_cost <- (Summer_beans_management_cost/arable_area)*Arable_area_AF1
    AF1_winter_oats_management_cost <- (Winter_oats_management_cost/arable_area)*Arable_area_AF1
    
    AF1_winter_cover_crop_management_cost <- (Winter_cover_crop_management_cost/arable_area)*Arable_area_AF1
    
    
    #Labour costs
    AF1_herbal_ley_labour_cost_CR2 <- rep(0, n_years)
    AF1_herbal_ley_grazing_labour_cost_CR2 <- rep(0, n_years)
    AF1_winter_wheat_labour_cost_CR2 <- rep(0, n_years)
    AF1_sping_barley_labour_cost <- rep(0, n_years)
    AF1_summer_beans_labour_cost <- rep(0, n_years)
    AF1_winter_oats_labour_cost <- rep(0, n_years)
    
    AF1_winter_cover_crop_labour_cost <- rep(0, n_years)
    
    AF1_herbal_ley_labour_cost_CR2[Herbal_ley_indices2] <- (Herbal_ley_labour_cost_CR2[Herbal_ley_indices2]/arable_area) * Arable_area_AF1 * af1_added_management_time_factor
    AF1_winter_wheat_labour_cost_CR2[Winter_wheat_indices2] <- (Winter_wheat_labour_cost_CR2[Winter_wheat_indices2]/arable_area) * Arable_area_AF1 * af1_added_management_time_factor
    AF1_spring_barley_labour_cost <- (Spring_barley_labour_cost/arable_area) * Arable_area_AF1 * af1_added_management_time_factor
    AF1_summer_beans_labour_cost <- (Summer_beans_labour_cost/arable_area) * Arable_area_AF1 * af1_added_management_time_factor
    AF1_winter_oats_labour_cost <- (Winter_oats_labour_cost/arable_area) * Arable_area_AF1 * af1_added_management_time_factor
    
    AF1_winter_cover_crop_labour_cost <- (Winter_cover_crop_labour_cost/arable_area) * Arable_area_AF1 * af1_added_management_time_factor
    
    AF1_herbal_ley_grazing_labour_cost_CR2[Herbal_ley_indices2] <- (Herbal_ley_grazing_labour_cost_CR2[Herbal_ley_indices2]/arable_area) * Arable_area_AF1 * af1_less_grazing_management_time_factor
    AF1_winter_CC_grazing_labour <- (Winter_CC_grazing_labour/arable_area) * Arable_area_AF1 * af1_less_grazing_management_time_factor
    
    AF1_total_herbal_ley_cost_CR2 <- AF1_herbal_ley_management_cost_CR2 + AF1_herbal_ley_labour_cost_CR2
    AF1_total_winter_wheat_cost_CR2 <- AF1_winter_wheat_management_cost_CR2 + AF1_winter_wheat_labour_cost_CR2
    AF1_total_spring_barley_cost <- AF1_spring_barley_management_cost + AF1_spring_barley_labour_cost
    AF1_total_summer_beans_cost <- AF1_summer_beans_management_cost + AF1_summer_beans_labour_cost
    AF1_total_winter_oats_cost <- AF1_winter_oats_management_cost + AF1_winter_oats_labour_cost
    
    AF1_total_winter_cover_crop_cost <- AF1_winter_cover_crop_management_cost +  AF1_winter_cover_crop_labour_cost
    
    AF1_total_grazing_cost_CR2 <- AF1_herbal_ley_grazing_labour_cost_CR2 + AF1_winter_CC_grazing_labour
    
    if(include_animals_AF1 == 1){
      AF1_total_cost_CR2 <- AF1_total_treerow_cost + AF1_total_herbal_ley_cost_CR2 + AF1_total_winter_wheat_cost_CR2 + AF1_total_spring_barley_cost + AF1_total_summer_beans_cost + AF1_total_winter_cover_crop_cost + AF1_total_grazing_cost_CR2
    }
    if(include_animals_AF1 == 0){
      AF1_total_cost_CR2 <- AF1_total_treerow_cost + AF1_total_herbal_ley_cost_CR2 + AF1_total_winter_wheat_cost_CR2 + AF1_total_spring_barley_cost + AF1_total_summer_beans_cost + AF1_total_winter_cover_crop_cost
    }
    
    #Bottom line AF 1 system:
    AF1_bottom_line_benefit <- AF1_total_benefit_CR2 - AF1_total_cost_CR2
  }
  
  
  #AFGROFORESTRY SYSTEM 2 ########################################################
  
  # account for yield reduction due to shading and competition from trees 
  AF2_perc_yield_reduction <- gompertz_yield(
    max_harvest = yield_reduc_max,
    time_to_first_yield_estimate = time_to_first_reduction,
    time_to_second_yield_estimate = time_to_second_reduction,
    first_yield_estimate_percent = perc_max_first_reduction,
    second_yield_estimate_percent = perc_max_second_reduction,
    n_years = n_years)
  
  # account for yield increase in pasture due to protection from wind stress 
  AF2_perc_yield_increase <- gompertz_yield(
    max_harvest = yield_increase_max,
    time_to_first_yield_estimate = time_to_first_increase,
    time_to_second_yield_estimate = time_to_second_increase,
    first_yield_estimate_percent = perc_max_first_increase,
    second_yield_estimate_percent = perc_max_second_increase,
    n_years = n_years
  )
  
  
  #Tree row benefits
  #Woody yield
  #no actual yield, benefit to livestock considered for now in 'livestock component'
  #carbon sequestration in T C/ha/yr
  AF2_C_sequestration <- gompertz_yield(
    max_harvest = C_sequeter_max,
    time_to_first_yield_estimate = 10, #time_to_first_C_sequester,
    time_to_second_yield_estimate = 15, #time_to_second_C_sequester,
    first_yield_estimate_percent = C_sequester_first,
    second_yield_estimate_percent = C_sequester_second,
    n_years = n_years,
    var_CV = var_cv,
    no_yield_before_first_estimate = TRUE
  )
  
  AF2_C_benefit <- vv(C_price, var_cv, n_years) * AF2_C_sequestration * AF2_tree_row_area
  
  #Woody fruits and nuts benefit
  
  #Rowan
  
  AF_rowan_yield <- rep(0, n_years)
  AF2_rowan_yield <- gompertz_yield(max_harvest = rowan_yield_max,
                                    time_to_first_yield_estimate = time_to_first_rowan,
                                    time_to_second_yield_estimate = time_to_second_rowan,
                                    first_yield_estimate_percent = rowan_yield_first,
                                    second_yield_estimate_percent = rowan_yield_second,
                                    n_years=n_years,
                                    var_CV = var_cv,
                                    no_yield_before_first_estimate = TRUE)
  
  #Yield from all rowan fruit trees [kg] considering risks
  AF2_tot_rowan_yield <- AF2_rowan_yield * num_rowan_trees #* AF_Chance_perc_weather_fail * AF_chance_perc_crop_fail
  
  AF2_rowan_benefit <-  AF2_tot_rowan_yield * rowan_value 
  
  #Hazelnut
  
  AF2_hazel_yield <- rep(0, n_years)
  AF2_hazel_yield <- gompertz_yield(max_harvest = hazel_yield_max,
                                    time_to_first_yield_estimate = time_to_first_hazel,
                                    time_to_second_yield_estimate = time_to_second_hazel,
                                    first_yield_estimate_percent = hazel_yield_first,
                                    second_yield_estimate_percent = hazel_yield_second,
                                    n_years=n_years,
                                    var_CV = var_cv,
                                    no_yield_before_first_estimate = TRUE)
  
  #Yield from all hazelnut fruit trees [kg] considering risks
  AF2_tot_hazel_yield <- AF2_hazel_yield * num_hazel_trees #* AF_Chance_perc_weather_fail * AF_chance_perc_crop_fail
  
  AF2_hazel_benefit <-  AF2_tot_hazel_yield * hazel_value 
  
  #Yield of one damson tree in the tree row [kg/tree]
  AF2_damson_yield <- rep(0, n_years)
  AF2_damson_yield <- gompertz_yield(max_harvest = damson_yield_max,
                                     time_to_first_yield_estimate = time_to_first_damson,
                                     time_to_second_yield_estimate = time_to_second_damson,
                                     first_yield_estimate_percent = damson_yield_first,
                                     second_yield_estimate_percent = damson_yield_second,
                                     n_years=n_years,
                                     var_CV = var_cv,
                                     no_yield_before_first_estimate = TRUE)
  
  #Yield from all rowan fruit trees [kg] considering risks
  AF2_tot_damson_yield <- AF2_damson_yield * num_damson_trees #* AF_Chance_perc_weather_fail * AF_chance_perc_crop_fail
  
  AF2_damson_benefit <-  AF2_tot_damson_yield * damson_value
  
  #Ecosystem Service benefits 
  
  AF2_GW_benefit <- rep(0, n_years)
  AF2_erosion_control_benefit <- rep(0, n_years)
  # groundwater storage and erosion control not realised in the first few years 
  #- can also make a variable and add vv fn.
  NMES_indices <- seq(from = 5, to = n_years)
  AF2_GW_benefit[NMES_indices] <-
    vv(pc_ground_water_recharge, var_cv, length(NMES_indices)) * arable_area
  
  AF2_erosion_control_benefit[NMES_indices] <- (vv(AF2_soil_loss_water, var_cv, length(NMES_indices)) + vv(AF2_soil_loss_wind, var_cv, length(NMES_indices))) * 
    vv(pc_soil_loss, var_cv, length(NMES_indices)) * arable_area  
  #pollinator_benefit yet to be added
  
  AF2_Nonmarket_ES_benefit <- AF2_GW_benefit + AF2_erosion_control_benefit 
  
  # from Porter et al. 2009
  #Nonmarket_ES_benefit <- vv(Nonmarket_ES_value, var_cv, n_years) * tree_row_area
  
  #Subsidy in AF system
  #AF2_subsidy[1:n_years] <- AF2_subsidy * AF2_tree_row_area 
  
  AF2_tree_benefit <- AF2_C_benefit + AF2_rowan_benefit + AF2_hazel_benefit + AF2_damson_benefit + 
    AF2_Nonmarket_ES_benefit + total_annual_funding + total_one_time_funding
  
  AF2_farm_benefit <- AF2_C_benefit + AF2_rowan_benefit + AF2_hazel_benefit + AF2_damson_benefit + total_annual_funding + total_one_time_funding #plus woody benefit to livestock in 'crop rotation portion'
  
  #woody benefit for livestock  
  AF2_total_woody_benefit <- rep(0, n_years) 
  AF2_total_woody_benefit <- vv(woody_benefit_shade, var_cv, n_years) + vv(woody_benefit_nutrition, var_cv, n_years)+
    vv(AF2_woody_benefit_windreduc, var_cv, n_years) #to be used in crop rotation part of code
  
  
  
  #AF costs:
  #Implementation cost variables
  AF2_planning_cost <- rep(0, n_years) #FE;Invoice of service provider (planners/consultants), planning the AF system + measuring tree strips using GPS[€]
  AF2_shrub_cost <- rep(0, n_years) #FE; Cost per tree [€]
  AF2_tree_cost <- rep(0, n_years) #FE; Cost per tree [€]
  AF2_tree_planting <- rep(0, n_years) #FE; Labour cost for planting one tree [€] -
  AF2_protect_cost <- rep(0, n_years) #FE; Material cost of tree protection mesh [€]
  AF2_weed_protect_cost <- rep(0, n_years) #Material cost of weed suppressing fleece [€]
  AF2_compost_cost <- rep(0, n_years) #FE; Cost of compost used during planting [€]
  AF2_irrigation_system_cost <- rep(0, n_years) #FE; Material and labour cost of installing a drip irrigation system in the tree rows [€]
  AF2_irrigation_planting_cost <- rep(0, n_years) #FE; Cost for watering in newly planted trees [€]
  AF2_total_planting_cost <- rep(0, n_years)
  
  AF2_planning_cost[1] <-    planning_consulting + (farmer_planning_time * Labour_costs[1])
  #Field prep
  AF2_tree_planting[1] <- tree_planting * AF2_num_trees * Labour_costs[1]
  AF2_tree_cost[1] <- (oak_tree_cost * num_oak_trees) + (birch_tree_cost * num_birch_trees) + (rowan_tree_cost * num_rowan_trees) + 
    (hazel_tree_cost * num_hazel_trees) + (damson_tree_cost * num_damson_trees) + (bcherry_tree_cost * num_bcherry_trees)
  AF2_shrub_cost[1] <- shrub_price * AF2_num_shrubs #this includes all planting costs and labour
  AF2_protect_cost[1] <- AF2_plant_protection * (AF2_num_trees + AF2_num_shrubs)
  AF2_weed_protect_cost[1] <- weed_protection * AF2_tree_row_area * Labour_costs[1]
  AF2_compost_cost[1] <- (compost_planting_tree * compost_price * AF2_num_trees) + (compost_planting_shrub * compost_price * AF2_num_shrubs) 
  AF2_irrigation_system_cost[1] <- irrigation_sys_install
  AF2_irrigation_planting_cost[1] <- (irrigation_planting_tree * water_price * AF2_num_trees) + (irrigation_planting_shrub * water_price * AF2_num_shrubs)  
  
  AF2_total_planting_cost <- AF2_tree_planting + AF2_tree_cost + AF2_shrub_cost + AF2_protect_cost + AF2_weed_protect_cost  +  AF2_compost_cost +
    AF2_irrigation_system_cost + AF2_irrigation_planting_cost
  
  AF2_total_investment_cost <- AF2_planning_cost + AF2_total_planting_cost #Investment cost of AF system implementation
  
  #Running cost variables
  AF2_subsidy_application <- rep(0, n_years) #FE; Time (regarded as labour cost) spent for application of Eco Scheme subsidy [€]
  #AF2_annual_irrigation <- rep(0, n_years) #FE; Cost of annual irrigation of tree rows [€]
  
  AF2_rowan_harvest <- rep(0, n_years)
  AF2_hazel_harvest <- rep(0, n_years)
  AF2_damson_harvest <- rep(0, n_years)
  
  
  AF2_subsidy_application <- vv(subsidy_application, var_cv, n_years) * Labour_costs
  #AF2_annual_irrigation[1:3] <- vv(irrigation_123, var_CV = var_cv, 3)
  #AF2_annual_irrigation_cost <- AF2_annual_irrigation * water_price * AF2_num_trees
  
  AF2_rowan_harvest[time_to_first_rowan:n_years] <-
    vv(rowan_harvest_cost, var_CV = var_cv, length(time_to_first_rowan:n_years))*AF2_rowan_yield[time_to_first_rowan:n_years]*Labour_costs[time_to_first_rowan:n_years]
  AF2_hazel_harvest[time_to_first_hazel:n_years] <-
    vv(hazel_harvest_cost, var_CV = var_cv, length(time_to_first_hazel:n_years))*AF2_hazel_yield[time_to_first_hazel:n_years]*Labour_costs[time_to_first_hazel:n_years]
  AF2_damson_harvest[time_to_first_damson:n_years] <-
    vv(damson_harvest_cost, var_CV = var_cv, length(time_to_first_damson:n_years))*AF2_damson_yield[time_to_first_damson:n_years]*Labour_costs[time_to_first_damson:n_years]
  
  
  
  AF2_total_running_cost <- AF2_subsidy_application + AF2_rowan_harvest + 
    AF2_hazel_harvest + AF2_damson_harvest #+ AF2_annual_irrigation_cost
  
  AF2_total_treerow_cost <- AF2_total_investment_cost + AF2_total_running_cost
  
  #Crop rotation 1:#####----------------------------------------------------------
  if(agroforestry_system_2_crop_rotation_1 == 1){
    
    #Annual arable crop component
    AF2_herbal_ley_yield_CR1 <- rep(0, n_years)
    AF2_winter_wheat_yield_CR1 <- rep(0, n_years)
    
    #Crop rotation in AF system
    AF2_herbal_ley_yield_CR1[Herbal_ley_indices1] <- (Herbal_ley_yield[Herbal_ley_indices1]/arable_area)*Arable_area_AF2*(1+AF2_perc_yield_increase[Herbal_ley_indices1]) *
    (1 - AF2_perc_yield_reduction[Herbal_ley_indices1])
    
    #Livestock metrics: 
    # Estimate live weight gain per hectare based on FCE
    AF2_herbal_ley_dry_matter_CR1 <- AF2_herbal_ley_yield_CR1 # [t]
    
    #cattle
    AF2_days_of_grazing_herbal_ley_cattle <- (AF2_herbal_ley_dry_matter_CR1 * cattle_intensity)/daily_dry_matter_intake_cattle
    AF2_herbal_ley_beef_yield_CR1 <-  AF2_days_of_grazing_herbal_ley_cattle * daily_weight_gain_pasture_cattle #[t]
    
    #sheep
    AF2_days_of_grazing_herbal_ley_sheep <- (AF2_herbal_ley_dry_matter_CR1 * sheep_intensity)/daily_dry_matter_intake_sheep
    AF2_herbal_ley_sheep_yield_CR1 <-  AF2_days_of_grazing_herbal_ley_sheep * daily_weight_gain_pasture_sheep #[t]
    
    
    AF2_winter_wheat_yield_CR1[Winter_wheat_indices1] <- (Winter_wheat_yield[Winter_wheat_indices1]/arable_area)*Arable_area_AF2* (1 - AF2_perc_yield_reduction[Winter_wheat_indices1])
    
    #Benefits: 
    if (include_animals_AF2 == 0) {
      AF2_herbal_ley_benefit_CR1 <-  vv(herbal_ley_value, var_cv, n_years) * AF2_herbal_ley_yield_CR1
    }
    if (include_animals_AF2 == 1) { 
      AF2_herbal_ley_benefit_CR1 <- AF2_herbal_ley_beef_yield_CR1 * beef_value * 1000 * (1 + AF2_total_woody_benefit) + 
        AF2_herbal_ley_sheep_yield_CR1 * lamb_value * 1000 * (1 + AF2_total_woody_benefit)
    }
    
    AF2_winter_wheat_benefit_CR1 <- vv(winter_wheat_value, var_cv, n_years) * AF2_winter_wheat_yield_CR1
    
    AF2_total_benefit_CR1 <- AF2_farm_benefit + AF2_herbal_ley_benefit_CR1 + AF2_winter_wheat_benefit_CR1
    
    
    #Costs:
    #Arable field management costs
    AF2_herbal_ley_management_cost <- rep(0, n_years) #includes: seed, insurance, fixed+variable machine cost (Values from GER available)
    AF2_winter_wheat_management_cost <- rep(0, n_years)
    
    AF2_herbal_ley_management_cost[Herbal_ley_indices1] <- (Herbal_ley_management_cost[Herbal_ley_indices1]/arable_area)*Arable_area_AF2
    AF2_winter_wheat_management_cost[Winter_wheat_indices1] <- (Winter_wheat_management_cost[Winter_wheat_indices1]/arable_area)*Arable_area_AF2
    
    #Labour costs
    AF2_herbal_ley_labour_cost_CR1 <- rep(0, n_years)
    AF2_winter_wheat_labour_cost <- rep(0, n_years)
    
    if (include_animals_AF2 == 0) {
      AF2_herbal_ley_labour_cost_CR1[Herbal_ley_indices1] <- (Herbal_ley_labour_cost[Herbal_ley_indices1]/arable_area) * Arable_area_AF2 * af2_added_management_time_factor
    }
    if (include_animals_AF2 == 1) {
      AF2_herbal_ley_labour_cost_CR1[Herbal_ley_indices1] <- (Herbal_ley_grazing_labour_cost_CR1[Herbal_ley_indices1]/arable_area)* Arable_area_AF2 * af2_less_grazing_management_time_factor
    }
    
    AF2_winter_wheat_labour_cost[Winter_wheat_indices1] <- (Winter_wheat_labour_cost[Winter_wheat_indices1]/arable_area) * Arable_area_AF2 * af2_added_management_time_factor
    
    AF2_total_herbal_ley_cost_CR1 <- AF2_herbal_ley_management_cost + AF2_herbal_ley_labour_cost_CR1
    AF2_winter_wheat_cost_CR1 <- AF2_winter_wheat_management_cost + AF2_winter_wheat_labour_cost
    
    AF2_total_cost_CR1 <- AF2_total_treerow_cost + AF2_total_herbal_ley_cost_CR1 + AF2_winter_wheat_cost_CR1
    
    ##Bottom line AF system 1:
    AF2_bottom_line_benefit <- AF2_total_benefit_CR1 - AF2_total_cost_CR1
  }
  
  
  
  #Crop rotation 2:#####----------------------------------------------------------
  #AF type 2, Crop rotation 2
  if(agroforestry_system_2_crop_rotation_2 == 1){
    
    #Annual arable crop component
    AF2_herbal_ley_yield_CR2 <- rep(0, n_years)
    AF2_winter_wheat_yield_CR2 <- rep(0, n_years)
    AF2_Spring_barley_yield <- rep(0, n_years)
    AF2_Summer_beans_yield <- rep(0, n_years)
    AF2_Winter_oats_yield <- rep(0, n_years)
    AF2_winter_crop_yield <- rep(0, n_years)
    
    #Crop rotation 2 in AF system 1
    AF2_herbal_ley_yield_CR2[Herbal_ley_indices2] <- (Herbal_ley_yield[Herbal_ley_indices2]/arable_area) *  Arable_area_AF2 * (1+AF2_perc_yield_increase[Herbal_ley_indices2]) * 
      (1-AF2_perc_yield_reduction[Herbal_ley_indices2])
    AF2_herbal_ley_benefit_CR2 <- vv(herbal_ley_value, var_cv, n_years) * AF2_herbal_ley_yield_CR2 
    
    AF2_winter_wheat_yield_CR2[Winter_wheat_indices2] <- (Winter_wheat_yield[Winter_wheat_indices2]/arable_area) * Arable_area_AF2 * (1 - AF2_perc_yield_reduction[Winter_wheat_indices2])  
    AF2_winter_wheat_benefit_CR2 <- vv(winter_wheat_value, var_cv, n_years) * AF2_winter_wheat_yield_CR2
    
    AF2_spring_barley_yield <- (Spring_barley_yield/arable_area) * Arable_area_AF2 * (1 - AF2_perc_yield_reduction)
    AF2_spring_barley_benefit <- vv(spring_barley_value, var_cv, n_years) * AF2_Spring_barley_yield
    
    AF2_summer_beans_yield <- (Summer_beans_yield/arable_area) * Arable_area_AF2 * (1 - AF2_perc_yield_reduction)
    AF2_summer_beans_benefit <- vv(summer_beans_value, var_cv, n_years) * AF2_summer_beans_yield
    
    AF2_winter_oats_yield <- (Winter_oats_yield/arable_area) * Arable_area_AF2 * (1 - AF2_perc_yield_reduction)
    AF2_winter_oat_benefit <- vv(winter_oats_value, var_cv, n_years) * AF2_Winter_oats_yield 
    
    AF2_winter_crop_yield <- (Winter_cover_crop_yield/arable_area) *  Arable_area_AF2 * (1 - AF2_perc_yield_reduction)
    
    #Livestock metrics: 
    # Estimate live weight gain per hectare based on FCE
    AF2_herbal_ley_dry_matter_CR2 <- AF2_herbal_ley_yield_CR2 # [t]
    
    #cattle
    AF2_days_of_grazing_herbal_ley_cattle <- (AF2_herbal_ley_yield_CR2 * cattle_intensity)/daily_dry_matter_intake_cattle
    AF2_herbal_ley_beef_yield_CR2 <-  AF2_days_of_grazing_herbal_ley_cattle * daily_weight_gain_pasture_cattle #[t]
    
    #sheep
    AF2_days_of_grazing_herbal_ley_sheep <- (AF2_herbal_ley_yield_CR2 * sheep_intensity)/daily_dry_matter_intake_sheep
    AF2_herbal_ley_sheep_yield_CR2 <-  AF2_days_of_grazing_herbal_ley_sheep * daily_weight_gain_pasture_sheep #[t]
    
    ### Winter Crop
    AF2_winter_crop_dry_matter <- AF2_winter_crop_yield * winter_cover_crop_dry_matter  # [t]
    
    #Cattle metrics
    AF2_days_of_grazing_winter_crop_cattle <- (AF2_winter_crop_dry_matter * cattle_intensity)/daily_dry_matter_intake_cattle
    AF2_winter_crop_beef_yield <- AF2_days_of_grazing_winter_crop_cattle * daily_weight_gain_pasture_cattle #[t]
    
    #Sheep metrics
    AF2_days_of_grazing_winter_crop_sheep <- (AF2_winter_crop_dry_matter * sheep_intensity)/daily_dry_matter_intake_sheep
    AF2_winter_crop_sheep_yield <- AF2_days_of_grazing_winter_crop_sheep * daily_weight_gain_pasture_sheep #[t]
    
    # AF2_spring_barley_dry_matter <- AF2_spring_barley_yield * 1000 * barley_dry_matter_content # kg DM per ha
    # AF2_spring_barley_beef_yield <- AF2_spring_barley_dry_matter * cattle_feed_conversion_efficiency # kg live weight gain/ha
    # 
    # AF2_summer_beans_dry_matter <- AF2_summer_beans_yield * 1000 * beans_dry_matter_content # kg DM per ha
    # AF2_summer_beans_beef_yield <- AF2_summer_beans_dry_matter * cattle_feed_conversion_efficiency # kg live weight gain/ha
    # 
    # AF2_winter_oats_dry_matter <- AF2_winter_oats_yield * 1000 * oats_dry_matter_content # kg DM per ha
    # AF2_winter_oats_beef_yield <- AF2_winter_oats_dry_matter * cattle_feed_conversion_efficiency # kg live weight gain/ha
    # 
    
    AF2_livestock_benefit_CR2 <- (AF2_herbal_ley_beef_yield_CR2 + AF2_winter_crop_beef_yield) * beef_value * 1000 * (1 + AF2_total_woody_benefit) + 
      (AF2_herbal_ley_sheep_yield_CR2 + AF2_winter_crop_sheep_yield) * lamb_value * 1000 * (1 + AF2_total_woody_benefit)  #+ AF2_spring_barley_beef_yield + AF2_summer_beans_beef_yield + AF2_winter_oats_beef_yield) 
    
    if(include_animals_AF2 == 1){
      AF2_total_benefit_CR2 <- AF2_farm_benefit + AF2_livestock_benefit_CR2 + AF2_winter_wheat_benefit_CR2 + AF2_spring_barley_benefit + AF2_summer_beans_benefit + AF2_winter_oat_benefit
    }
    if(include_animals_AF2 == 0){
      AF2_total_benefit_CR2 <- AF2_farm_benefit + AF2_herbal_ley_benefit_CR2 + AF2_winter_wheat_benefit_CR2 + AF2_spring_barley_benefit + AF2_summer_beans_benefit + AF2_winter_oat_benefit
    }
    
    #Costs:
    #Management costs
    AF2_herbal_ley_management_cost_CR2 <- rep(0, n_years) #includes: seed, insurance, fixed+variable machine cost (Values from GER available)
    AF2_winter_wheat_management_cost_CR2 <- rep(0, n_years)
    AF2_spring_barley_management_cost <- rep(0, n_years)
    AF2_summer_beans_management_cost <- rep(0, n_years)
    AF2_winter_oats_management_cost <- rep(0, n_years)
    
    AF2_winter_cover_crop_management_cost <- rep(0, n_years)
    
    AF2_herbal_ley_management_cost_CR2[Herbal_ley_indices2] <- (Herbal_ley_management_cost[Herbal_ley_indices2]/arable_area)*Arable_area_AF2
    AF2_winter_wheat_management_cost_CR2[Winter_wheat_indices2] <- (Winter_wheat_management_cost[Winter_wheat_indices2]/arable_area)*Arable_area_AF2
    AF2_spring_barley_management_cost <- (Spring_barley_management_cost/arable_area)*Arable_area_AF2
    AF2_sumer_beans_management_cost <- (Summer_beans_management_cost/arable_area)*Arable_area_AF2
    AF2_winter_oats_management_cost <- (Winter_oats_management_cost/arable_area)*Arable_area_AF2
    
    AF2_winter_cover_crop_management_cost <- (Winter_cover_crop_management_cost/arable_area)*Arable_area_AF2
    
    
    #Labour costs
    AF2_herbal_ley_labour_cost_CR2 <- rep(0, n_years)
    AF2_winter_wheat_labour_cost_CR2 <- rep(0, n_years)
    AF2_sping_barley_labour_cost <- rep(0, n_years)
    AF2_summer_beans_labour_cost <- rep(0, n_years)
    AF2_winter_oats_labour_cost <- rep(0, n_years)
    
    AF2_winter_cover_crop_labour_cost <- rep(0, n_years)
    
    AF2_herbal_ley_labour_cost_CR2[Herbal_ley_indices2] <- (Herbal_ley_labour_cost[Herbal_ley_indices2]/arable_area) * Arable_area_AF2 * af2_added_management_time_factor
    AF2_winter_wheat_labour_cost_CR2[Winter_wheat_indices2] <- (Winter_wheat_labour_cost[Winter_wheat_indices2]/arable_area) * Arable_area_AF2 * af2_added_management_time_factor
    AF2_spring_barley_labour_cost <- (Spring_barley_labour_cost/arable_area) * Arable_area_AF2 * af2_added_management_time_factor
    AF2_summer_beans_labour_cost <- (Summer_beans_labour_cost/arable_area) * Arable_area_AF2 * af2_added_management_time_factor
    AF2_winter_oats_labour_cost <- (Winter_oats_labour_cost/arable_area) * Arable_area_AF2 * af2_added_management_time_factor
    
    AF2_winter_cover_crop_labour_cost <- (Winter_cover_crop_labour_cost/arable_area) * Arable_area_AF2 * af2_added_management_time_factor
    
    AF2_herbal_ley_grazing_labour_cost_CR2 <- (Herbal_ley_grazing_labour_cost_CR2/arable_area) * Arable_area_AF2 * af2_less_grazing_management_time_factor
    AF2_winter_CC_grazing_labour <- (Winter_CC_grazing_labour/arable_area) * Arable_area_AF2 * af2_less_grazing_management_time_factor
    
    AF2_total_herbal_ley_cost_CR2 <- AF2_herbal_ley_management_cost_CR2 + AF2_herbal_ley_labour_cost_CR2
    AF2_total_winter_wheat_cost_CR2 <- AF2_winter_wheat_management_cost_CR2 + AF2_winter_wheat_labour_cost_CR2
    AF2_total_spring_barley_cost <- AF2_spring_barley_management_cost + AF2_spring_barley_labour_cost
    AF2_total_summer_beans_cost <- AF2_summer_beans_management_cost + AF2_summer_beans_labour_cost
    AF2_total_winter_oats_cost <- AF2_winter_oats_management_cost + AF2_winter_oats_labour_cost
    
    AF2_total_winter_cover_crop_cost <- AF2_winter_cover_crop_management_cost +  AF2_winter_cover_crop_labour_cost
    
    AF2_total_grazing_cost_CR2 <- AF2_herbal_ley_grazing_labour_cost_CR2 + AF2_winter_CC_grazing_labour
    
    if(include_animals_AF2 == 1){
      AF2_total_cost_CR2 <- AF2_total_treerow_cost + AF2_total_herbal_ley_cost_CR2 + AF2_total_winter_wheat_cost_CR2 + AF2_total_spring_barley_cost + AF2_total_summer_beans_cost + AF2_total_winter_cover_crop_cost + AF2_total_grazing_cost_CR2
    }
    if(include_animals_AF2 == 0){
      AF2_total_cost_CR2 <- AF2_total_treerow_cost + AF2_total_herbal_ley_cost_CR2 + AF2_total_winter_wheat_cost_CR2 + AF2_total_spring_barley_cost + AF2_total_summer_beans_cost + AF2_total_winter_cover_crop_cost
    }
    
    #Bottom line AF 2 system:
    AF2_bottom_line_benefit <- AF2_total_benefit_CR2 - AF2_total_cost_CR2
  }
  
  #Calculating NPVs and Cash Flows####
  #Treeless system
  #Treeless system bottomline####
  
  NPV_treeless_system <- discount(Treeless_bottom_line_benefit, discount_rate = discount_rate,
                                  calculate_NPV = TRUE) #NVP of monoculture arable system 
  Treeless_cash_flow <- discount(Treeless_bottom_line_benefit, discount_rate = discount_rate,
                                 calculate_NPV = FALSE) #Cash flow of monoculture system
  Treeless_cum_cash_flow <- cumsum(Treeless_cash_flow) #Cumulative cash flow of monoculture system
  
  
  #AF System 1
  AF1_NPV <- discount(AF1_bottom_line_benefit, discount_rate=discount_rate,
                     calculate_NPV = TRUE)#NVP of AF system
  AF1_cash_flow <- discount(AF1_bottom_line_benefit,discount_rate=discount_rate,
                           calculate_NPV = FALSE)#Cash flow of AF system
  AF1_cum_cash_flow <- cumsum(AF1_cash_flow) #Cumulative cash flow of AF system
  
  #AF System 2
  AF2_NPV <- discount(AF2_bottom_line_benefit, discount_rate=discount_rate,
                      calculate_NPV = TRUE)#NVP of AF system
  AF2_cash_flow <- discount(AF2_bottom_line_benefit,discount_rate=discount_rate,
                            calculate_NPV = FALSE)#Cash flow of AF system
  AF2_cum_cash_flow <- cumsum(AF2_cash_flow) #Cumulative cash flow of AF system
  
  #Tradeoff (difference between AF system and treeless system)
  AF1_tradeoff_benefit <- AF1_bottom_line_benefit - Treeless_bottom_line_benefit
  #Tradeoff_benefit_farm <- AF_bottom_line_benefit_farm - Treeless_bottom_line_benefit
  
  AF1_NPV_tradeoff <- discount(AF1_tradeoff_benefit, discount_rate = discount_rate,
                           calculate_NPV = TRUE )
  # NPV_tradeoff_farm <- discount(Tradeoff_benefit_farm, discount_rate = discount_rate,
  #                               calculate_NPV = TRUE )
  
  AF2_tradeoff_benefit <- AF2_bottom_line_benefit - Treeless_bottom_line_benefit
  
  AF2_NPV_tradeoff <- discount(AF2_tradeoff_benefit, discount_rate = discount_rate,
                               calculate_NPV = TRUE )

  
  #Defining what output variables the following Monte Carlo Simulation should create #####
  return(list(NPV_Agroforestry_System1 = AF1_NPV,
              NPV_Agroforestry_System2 = AF2_NPV, 
              NPV_Treeless_System = NPV_treeless_system,
              NPVtrade_off_AF1 = AF1_NPV_tradeoff,
              NPVtrade_off_AF2 = AF2_NPV_tradeoff,
              Cashflow_AF1 = AF1_cash_flow,
              Cashflow_AF2 = AF2_cash_flow,
              Cumcashflow_AF1 = AF1_cum_cash_flow,
              Cumcashflow_AF2 = AF2_cum_cash_flow,
              Cashflow_treeless = Treeless_cash_flow,
              Cumcashflow_treeless = Treeless_cum_cash_flow
  ))
}



#-------------------------------------------------------------------------------

#Run the Monte Carlo analysis of the model
mcSimulation_results <- mcSimulation(
  estimate = estimate_read_csv(fileName = "Input_table_Rev3_AM.csv"),
  model_function = AF_benefit,
  numberOfModelRuns = 10000,
  functionSyntax = "plainNames")


Plot_mc_output <- function(mc_output, prefixes, total_area_value, plot_type) {
  # Extract the "y" element from the Monte Carlo output list
  y_data <- mc_output$y
  
  # Create an empty list to hold the selected columns by prefix
  selected_cols <- list()
  
  # Loop over each prefix
  for (prefix in prefixes) {
    # Find columns with the prefix in their names
    cols_with_prefix <- y_data[, grep(paste0("^", prefix), colnames(y_data)), drop = F]
    
    # Only add to the list if columns with the prefix are found
    if (ncol(cols_with_prefix) > 0) {
      selected_cols[[prefix]] <- cols_with_prefix
    }
  }
  
  # Combine all selected columns into one data frame
  filtered_data <- bind_cols(selected_cols)
  
  # Stack data so it can be used in ggplot2 functions
  stacked_data <- stack(filtered_data)
  # Find the value associated with "total_area" in the input_table
  # total_area_value <- input_table %>%
  #   filter(variable == "arable_area_treeless") %>%
  #   pull(lower)
  
  # Convert values in the stacked data
  # Divide values by 1000 and by the total area (K€/ha)
  stacked_data <- stacked_data %>%
    mutate(values = values / 1000) %>%
    mutate(values = values / total_area_value)
  
  # Calculate median values for each output group
  scenario_medians <- stacked_data %>%
    group_by(ind) %>%
    summarize(median_value = median(values, na.rm = TRUE))
  
  # Reorder 'ind' levels based on median values (from low to high)
  stacked_data$ind <- factor(stacked_data$ind, 
                             levels = scenario_medians$ind[order(scenario_medians$median_value, decreasing = FALSE)])
  
  # Generate unique colors for each 'ind' using a color palette
  unique_ind <- unique(stacked_data$ind)
  num_colors <- length(unique_ind)
  color_palette <- hcl.colors(num_colors, palette =  "Earth") # Adjust for up to 12 unique colors
  
  # Create the plot
  if(plot_type == "box"){
    plot <- ggplot(stacked_data, aes(x = ind, y = values, fill = ind)) +
      geom_boxplot(alpha = 0.4, outlier.size = 0.2, outlier.alpha = 0.2) + # Reduce outlier size and make them half transparent
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.ticks.y = element_line(), 
        legend.position = "none") +
      scale_fill_manual(values = color_palette) +
      labs(
        x = "Scenarios",
        y = "Outcome distribution [K€/ha]") +
      coord_flip() +
      stat_boxplot(geom = "errorbar", width = 0.2) +
      scale_y_continuous(breaks = seq(-75, max(stacked_data$values, na.rm = TRUE), by = 50)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
  }
  
  if(plot_type == "density"){
    plot <- ggplot(stacked_data, aes(x = values, fill = ind, color = ind)) +
      geom_density(alpha = 0.4, size = 0.7) + # Create density plot with transparency and line size
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "none") +
      scale_fill_manual(values = color_palette) +
      labs(
        x = "Outcome distribution [K€/ha]",
        y = "") +
      scale_x_continuous(breaks = seq(-75, max(stacked_data$values, na.rm = TRUE), by = 50)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.5)
  }
  
  if(plot_type =="ridge"){
    plot <- ggplot(stacked_data, aes(x = values, y = ind, fill = ind)) +
      geom_density_ridges(alpha = 0.4, scale = 1, rel_min_height = 0.001) +# Ridgeline plot with density
      scale_y_discrete(expand = c(0.01, 0)) +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_manual(values = color_palette) +
      labs(
        x = "Outcome distribution [K€/ha]",
        y = "Scenarios") +
      scale_x_continuous(breaks = seq(-75, max(stacked_data$values, na.rm = TRUE), by = 50)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.5)
  }
  # Return the plot
  return(plot)
}


MC_Box <- Plot_mc_output(mcSimulation_results, "NPVtrade_off", 14.9744, "box") 
MC_Box
MC_Density <- Plot_mc_output(mcSimulation_results, "NPVtrade_off_AF1", 14.9744, "density")
MC_Density
MC_Ridge <- Plot_mc_output(mcSimulation_results, "NPVtrade_off", 14.9744, "ridge")
MC_Ridge
#End of COde ####