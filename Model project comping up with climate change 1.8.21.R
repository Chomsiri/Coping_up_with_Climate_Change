#### 1. Preparation and defined values ####
# Package installation and loading to library.
install.packages(decisionSupport)
library(decisionSupport)
install.packages("ggplot2")
library(ggplot2)

# n is defined as 10 for all calculations since there are 10 years per decade. 
n = 10
# A is replacing posnorm distributions
A = "posnorm"
# B is replacing % yield loss
B = "% yield loss"
# C is replacing USD/ha
C = "USD/ha"
# D is replacing % of discount to profit
D = "% of discount to profit"
# E is replacing t_norm_0_1 distributions
E = "tnorm_0_1"


#### 2. Input table for values in the analyzed period ####
# Input table directly written in R code in order to be able to realize easy changes in values.
input_estimates <- data.frame(variable = c("drought_loss", "typhoon_loss", "flood_loss", "soil_quality_loss",
                                           "pests_loss", "weeds_loss", "pathogenes_loss", "rice_price", 
                                           "labour_cost", "irrigation_cost", "fertilizer_cost", "pesticide_cost", 
                                           "machinery_cost", "rice_yield_potential", "var_CV",
                                           "drought_risk", "typhoon_risk", "flood_risk", "tenant_cost", 
                                           "precipitation_loss", "temperature_loss", "discount_rate_event", "discount_rate_no_event",
                                           "potential_income_construction_work", "bonus_payment"),
                                            
                                            lower = c(0.10, 0.20, 0.10, 0.05,  0.03, 0.03, 0.03, 0.24,  175.00, 14.09, 98.200, 22.24,  26.00, 7680 , 25, 0.06,  0.20, 0.15, 369.6, 0.05,  0.04, 10.40, 1.04, 1877, 22),
                                            median = NA,
                                            upper = c(0.20, 0.25, 0.15, 0.10,  0.04, 0.04, 0.04, 0.36,  246.00, 22.59, 141.01, 26.40,  34.30, 9240 , 30, 0.15,  0.30, 0.20, 568.8, 0.10,  0.08, 15.61, 5.20, 3056, 64),
                                            
                                            distribution = c(E, E, E, E,  E, E, E, A,  A, A, A, A,  A, A, A, E,  E, E, A, E,  E, A, A, A, A),
                                            
                                            label = c(B, B, B, B, B, B, B, "USD/kg", C, C, C, C, C, "kg/ha", "Coefficient of variation", 
                                                      "% likelyhood drought", "% likelyhood typhoon", "% likelyhood flood", C, B, B, D, D, "$/season", "$/season"),
                                            
                                            Description = c("Yield loss due to too little rain (drought)", 
                                                            "Yield loss due to heavy wind events (Typhoon)", 
                                                            "Yield loss due to too mach rain (flood)", 
                                                            "Yield loss due to variation in soil quality", "Yield loss due to pest infestation",
                                                            "Yield loss due to weed infestation", "Yield los due to pathogene infestation", 
                                                            "Rice market price", "Labour market cost", "Price of irrigation",
                                                            "Price of fertilizer", "Price of pesticide", "Price of machinery",
                                                            "Rice yield potential in one growing season", "Coefficient of variation (measure of relative variability)",
                                                            "% chance of annual drought occurance", "% chance of annual typhoon occurance", 
                                                            "% chance of annual flood occurance", "Price of renting land", 
                                                            "Yield loss due to precipitation apart from typhoon or flood",
                                                            "Yiled loss due to temperature stress apart from drought", "Discount in case event happens",
                                                            "Discount in case event does not happen", "income as a construction worker without specific training in a 6 month period",
                                                            "bonus for construction worker in a 6 monthe period"))
input_estimates

# Income construction work Philippines:
# per hour: 120 PHP = 2.4 USD
# per year: 187105 - 304685 PHP = 3753 - 6112 USD
# per 6 month season: 93553 - 152343 PHP = 1877 - 3056 USD
# bonus payment annually average: 4300 PHP = 86 USD
# bonus payment seasonal (6 month average): 2150 PHP = 43 USD
# see also: https://www.salaryexpert.com/salary/job/construction-worker/philippines


#### 3. Model function ####
# Model function for the comparison of rice cultivation or construction working.

rice_function <- function(){
  
  # 3.1 Value varier function
  # Adding variation in time series to variable rice yield and rice price
  # because prices and yield fluctuate.
  # var_CV is the coefficient of variation in %. 
  yields <- vv(var_mean = rice_yield_potential, 
                var_CV = var_CV, 
                n)
  
  prices <- vv(var_mean = rice_price, 
                var_CV = var_CV, 
                n)
  
  # Income of construction worker is regular salary plus a bonus payment.
  # Due to uncertainties in the construction sector 50% variation assumed.
  income_construction_work <- vv(var_mean = potential_income_construction_work + bonus_payment,
                                var_CV = 50,
                                n)
  
  # 3.2 Chance event function
  # Effect of typhoon, drought and flood on the yield are expressed.
  # Chance describes the probability that the event happens (0-1).
  # cv_if and cv_if_no are coefficients of variation (%).
  # cv_if = 50 means 50% variation of the value_if for the 10 generated values.
  # value_if = 0 means that in case of the event no yield is assumed, = 1 means 100% of yield.
  typhoon_adjusted_yield <- chance_event(chance = typhoon_risk, 
                                          value_if = rice_yield_potential * (1 - typhoon_loss),
                                          value_if_not = rice_yield_potential,
                                          n,
                                          CV_if = 50,
                                          CV_if_not = 5)
  drought_adjusted_yield <- chance_event(chance = drought_risk,
                                          value_if = rice_yield_potential * (1 - drought_loss),
                                          value_if_not = rice_yield_potential,
                                          n,
                                          CV_if = 50,
                                          CV_if_not = 5)
  flood_adjusted_yield <- chance_event(chance = flood_risk,
                                        value_if = rice_yield_potential * (1 - flood_loss),
                                        value_if_not = rice_yield_potential,
                                        n,
                                        CV_if = 50,
                                        CV_if_not = 5)
  
  # 3.3 Additional yield loss and cost factors
  # Yield losses dependent on % yield loss due to soil quality, pests, weeds, pathogens, 
  # precipitation (apart from the typhoon/ flood/ drought) and temperature (apart from the typhoon/ flood/ drought).
  # Overall cost as sum of labor, irrigation, fertilizer, pesticide, machinery cost and rent for land.
  yield_loss <- soil_quality_loss + pests_loss + weeds_loss + pathogenes_loss + precipitation_loss + temperature_loss
  overall_costs <- labour_cost + irrigation_cost + fertilizer_cost + pesticide_cost + machinery_cost + tenant_cost
  
  # 3.4 Profit calculation for different weather scenarios
  # Calculates profit when there is a typhoon and when there is no typhoon.
  # Losses due to drought or flood are listed separately from other yield loss factors because they are involved in separate risk calculation.
  profit_typhoon <- ((typhoon_adjusted_yield * (1 - yield_loss - drought_loss - flood_loss)) * prices) - overall_costs
  profit_no_typhoon <- ((yields * (1 - yield_loss - drought_loss - flood_loss)) * prices) - overall_costs
  
  
  # Calculates profit when there is a drought and when there is no drought.
  # Losses due to typhoon or flood are listed separately from other yield loss factors because they are involved in separate risk calculation.
  profit_drought <- ((drought_adjusted_yield * (1 - yield_loss - typhoon_loss - flood_loss)) * prices) - overall_costs
  profit_no_drought <- ((yields * (1 - yield_loss - typhoon_loss - flood_loss)) * prices) - overall_costs
  
  
  # Calculates profit when there is a flood and when there is no flood.
  # Losses due to typhoon or drought are listed separately from other yield loss factors because they are involved in separate risk calculation.
  profit_flood <- ((flood_adjusted_yield * (1 - yield_loss - typhoon_loss - drought_loss)) * prices) - overall_costs
  profit_no_flood <- ((yields * (1 - yield_loss - typhoon_loss - drought_loss)) * prices) - overall_costs
  
  # 3.5 Discounting and Net Present value (NPV)
  # Calculate net present value (NPV) and discount for typhoon, drought and flood scenario.
  NPV_typhoon <- discount(profit_typhoon, discount_rate = discount_rate_event, calculate_NPV = TRUE)
  NPV_no_typhoon <- discount(profit_no_typhoon, discount_rate = discount_rate_no_event, calculate_NPV = TRUE)
  NPV_drought <- discount(profit_drought, discount_rate = discount_rate_event, calculate_NPV = TRUE)
  NPV_no_drought <- discount(profit_no_drought, discount_rate = discount_rate_no_event, calculate_NPV = TRUE)
  NPV_flood <- discount(profit_flood, discount_rate = discount_rate_event, calculate_NPV = TRUE)
  NPV_no_flood <- discount(profit_no_flood, discount_rate = discount_rate_no_event, calculate_NPV = TRUE)
  
  # 10% discount rate are assigned to the income from construction work.
  NPV_construction <- discount(income_construction_work, discount_rate = 10, calculate_NPV = TRUE)
  
  
  # Calculate the overall NPV a farmer can expect from his rice plantation in the future.
  # Summing up NPV of the 6 scenarios and dividing by 6 to get the average.
  NPV_rice <- (NPV_no_typhoon + NPV_no_drought + NPV_no_flood + NPV_typhoon + NPV_drought + NPV_flood) / 6
  
  # Out put for later visualization.
  # In addition to NPV_decade_1/2/3, NPV of different scenarios in different decades are also expressed.
  return(list(NPV_rice = NPV_rice,
              NPV_construction  = NPV_construction / 10))
}


#### 4. Monte Carlo simulation for model function ####
# Run the Monte Carlo simulation using the model function and data from input_estimates.
rice_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                   model_function = rice_function,
                                   numberOfModelRuns = 10000,
                                   functionSyntax = "plainNames")

rice_mc_simulation


#### 5. Projection to Latent Structures (PLS) analysis ####
# Shows the importance of different variables for the rice farming activity.
pls_result <- plsr.mcSimulation(object = rice_mc_simulation,
                                resultName = names(rice_mc_simulation$y)[1], ncomp = 1)


#### 6. Calculation of the expected cash flow ####
# Read output list from Monte Carlo simulation and filter for maximum and minimum values 
# of the variables NPV rice and NPV construction.
options(max.print = .Machine$integer.max)
options(digits=10)

typeof(rice_mc_simulation)

DF <- as.data.frame(rice_mc_simulation)

DF %>% select(y.NPV_rice)

max(DF %>% select(y.NPV_rice))
min(DF %>% select(y.NPV_rice))
max(DF %>% select(y.NPV_construction))
min(DF %>% select(y.NPV_construction))

# Additional input table, model function and Monte Carlo simulation for the cash flow calculation.
variable = c("revenue_option1_rice", "n_years","revenue_option2_construction")
distribution = c("norm", "const","norm")

# boundaries are coming from Monte Carlo simulation before. 
# They are the maximal and minimal values which were identified from the 10.000 runs.
lower = c(-4166.0295, 10, 455.290774)
upper = c(6197.73452, 10, 3713.12608)

costBenefitEstimate <- as.estimate(variable, distribution, lower, upper)

profit_options <- function(x) {
  
  cashflow_option1_rice <- vv(revenue_option1_rice, n = n_years, var_CV = 10)
  cashflow_option2_construction <- vv(revenue_option2_construction, n = n_years, var_CV = 5)
  
  return(list(Revenues_option1_rice = revenue_option1_rice,
              Revenues_option2_construction = revenue_option2_construction,
              Cashflow_option_one_rice = cashflow_option1_rice,
              Cashflow_option_two_construction = cashflow_option2_construction))
}

# Perform the Monte Carlo simulation:

prediction_profit <- mcSimulation(estimate = costBenefitEstimate,
                                  model_function = profit_options,
                                  numberOfModelRuns = 10000,
                                  functionSyntax = "plainNames")


#### 7. Visualization of the outputs and interpretation ####
# 7.1 Visualization and results net present value analysis as smooth simple overlay and as boxplot.
plot_distributions(mcSimulation_object = rice_mc_simulation, 
                   vars = c("NPV_rice", "NPV_construction"),
                   method = 'smooth_simple_overlay', 
                   base_size = 12,
                   colors = c("tomato4", "limegreen"),
                   x_axis_name = "Financial outcome in $ per ha in one growing season in a year / in $ per 6 month construction working")

plot_distributions(mcSimulation_object = rice_mc_simulation, 
                   vars = c("NPV_rice", "NPV_construction"),
                   method = 'boxplot', 
                   base_size = 12,
                   colors = c("tomato4", "limegreen"),
                   x_axis_name = "Financial outcome in $ per ha in one growing season in a year / in $ per 6 month construction working")


# 7.2 Visualization and results of projection to latent structures analysis.
plot_pls(pls_result, input_table = input_estimates, threshold = 0, base_size = 12)


# 7.3 Visualization and results cash flow analysis
plot_cashflow(mcSimulation_object = prediction_profit, 
              cashflow_var_name = c("Cashflow_option_one_rice", "Cashflow_option_two_construction"),
              x_axis_name = "Years",
              y_axis_name = "Annual cashflow in USD",
              color_25_75 = "green4", color_5_95 = "green1",
              color_median = "red", 
              facet_labels = c("Rice cultivation", "Construction work"))
