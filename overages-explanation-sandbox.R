##################################################
## Collin Edwards
## Wed Jan 15 08:16:45 2025
## Overages calculations for 5-year report
##################################################

library(tidyverse)
library(here)
## map fram stock and psc mus

psc.stocks = read_csv("PSCStocks.csv") |> 
  janitor::clean_names() |> 
  select(psc_stock_id, psc_stock_name)

psc.stocks.map = read_csv("PSCStockMap.csv")|> 
  janitor::clean_names() |> 
  left_join(psc.stocks)

##-----------------
# 
# year.initial = 2019
# year.final = 2022
##  Data setup ----------------
# psc_pst_full <- readr::read_csv(here("../PeriodicReportData/psc_postfram_1998_2022_temp.csv")) |> 
#   rename(`Catch Year` = RunYear)
# 
# psc_pst <- psc_pst_full |> 
#   filter(`Catch Year` >= year.initial) |> 
#   filter(`Catch Year` <= year.final)
# 
# psc_pre_full <- readr::read_csv(here("../PeriodicReportData/psc_prefram_2004_2023.csv")) |> 
#   rename(`Catch Year` = RunYear)
# 
# psc_pre <- psc_pre_full |> 
#   filter(`Catch Year` >= year.initial) |> 
#   filter(`Catch Year` <= year.final)
# 
# 
# ests.post = psc_pst_full |> 
#   distinct(`Catch Year`, PSC_Region, PSC_StockName, abund, low_mod_abund, mod_abd_abund) |> 
#   mutate(
#     PSC_Region = factor(PSC_Region, levels = c("Canada","US Inside","US Outside")),
#     abd = case_when(
#       abund < low_mod_abund ~ 1,
#       abund >= low_mod_abund & abund <= mod_abd_abund ~ 2,
#       abund > mod_abd_abund ~ 3,
#       is.na(low_mod_abund) ~ NA
#     )) |> 
#   filter(!is.na(abd))  |> 
#   select("Catch Year", PSC_Region, PSC_StockName, abd)
# 
# ests.pre = psc_pre_full |> 
#   distinct(`Catch Year`, PSC_Region, PSC_StockName, abund, low_mod_abund, mod_abd_abund) |> 
#   mutate(
#     PSC_Region = factor(PSC_Region, levels = c("Canada","US Inside","US Outside")),
#     abd = case_when(
#       abund < low_mod_abund ~ 1,
#       abund >= low_mod_abund & abund <= mod_abd_abund ~ 2,
#       abund > mod_abd_abund ~ 3,
#       is.na(low_mod_abund) ~ NA
#     )) |> 
#   filter(!is.na(abd))  |> 
#   select("Catch Year", PSC_Region, PSC_StockName, abd)
# 
# 
# ests.joint = inner_join(ests.post, ests.pre, by = c("Catch Year", "PSC_Region", "PSC_StockName"),
#                         suffix = c("_post", "_pre")) |> 
#   mutate(prediction = case_when(
#     abd_post == abd_pre ~ "No Change",
#     abd_post < abd_pre ~ "Too High",
#     abd_post > abd_pre ~ "Too Low"
#   )) |> 
#   count(`Catch Year`, PSC_Region, prediction) |> 
#   mutate(prediction = factor(prediction, c("Too High", "No Change", "Too Low")))
# 
# 
# er.limits = inner_join(ests.post, ests.pre, by = c("Catch Year", "PSC_Region", "PSC_StockName"),
#                        suffix = c("_post", "_pre")) |> 
#   filter(`Catch Year` >= year.initial) |> 
#   filter(`Catch Year` <= year.final) |> 
#   left_join(psc_pst |> select(low_er_cap, mod_er_cap, abd_er_cap, "Catch Year", PSC_StockName) |> distinct()) |> 
#   filter(PSC_Region == "US Inside") |> 
#   mutate(limit_post = case_when(
#     abd_post == 1 ~ low_er_cap,
#     abd_post == 2 ~ mod_er_cap,
#     abd_post == 3 ~ abd_er_cap)
#   ) |> 
#   mutate(limit_pre = case_when(
#     abd_pre == 1 ~ low_er_cap,
#     abd_pre == 2 ~ mod_er_cap,
#     abd_pre == 3 ~ abd_er_cap)
#   ) |> 
#   select(-low_er_cap, -mod_er_cap, -abd_er_cap)
# 
# er.vals = psc_pre |> 
#   select("Catch Year", PSC_StockName, er_pre =mort_sum_er) |> 
#   distinct() |> 
#   inner_join(
#     psc_pst |> 
#       select("Catch Year", PSC_StockName, er_post =mort_sum_er) |> 
#       distinct(),
#     by = c("Catch Year", "PSC_StockName")
#   )
# 
# er.comparison = er.limits |> 
#   left_join(er.vals,
#             by = c("Catch Year", "PSC_StockName")) |> 
#   mutate(preseason.residual = limit_pre - er_pre,
#          postseason.residual = limit_post - er_post, 
#          postseason.vs.target = limit_pre - er_post) |> 
#   select(-abd_post, -abd_pre) |> 
#   mutate(across(limit_post:postseason.vs.target, \(x) x*100))
# 
# er.comparison |> 
#   select("Catch Year", PSC_StockName, limit_pre, er_pre, preseason.residual,
#          limit_post, er_post, postseason.residual, postseason.vs.target) |> 
#   gt() |> 
#   fmt_number(columns = -"Catch Year", decimals = 1) |> 
#   cols_label(limit_pre = "Preseason Cap",
#              er_pre = "Preseason Model ER",
#              preseason.residual = "Preseason Unused",
#              limit_post = "Postseason Cap",
#              er_post = "Postseason Model ER",
#              postseason.residual = "Postseason Unused",
#              postseason.vs.target = "Postseason Unused Using Preseason Cap")
# 
# ## --------------------
# ## cases in which pre and post don't match
# er.comparison |> 
#   select("Catch Year", PSC_StockName, limit_pre, er_pre, preseason.residual,
#          limit_post, er_post, postseason.residual, postseason.vs.target) |> 
#   filter(limit_pre != limit_post) |> 
#   filter(limit_post < er_post) |> 
#   select(-ends_with("residual")) |> 
#   gt()

## UPDATE: THe above is NOT working right. This is probably the coastal thing

## ----------------------
## identifying fishery overages 

file.pre = "C:/Repos/PeriodicReportData/PSC_CoTC_Preseason_CohoFRAMDB_thru2024_06182024.mdb"
file.post = "C:/Repos/PeriodicReportData/PSC_CoTC_PostSeason_CohoFRAMDB_thru2022_03042024.mdb"

library(tidyverse)
library(framrsquared)

con.pre = connect_fram_db(file.pre)
con.post = connect_fram_db(file.post)

year.cur = 2019
cur.stock = "Skagit"

create_mort_comparison = function(year, stock){
  year.cur = year
  stock.cur = stock
  
  ## identify run
  id.pre = fetch_table(con.pre, "RunID") |> 
    filter(run_year == year.cur) |> 
    pull(run_id)
  
  id.post = fetch_table(con.post, "RunID") |> 
    filter(run_year == year.cur) |> 
    pull(run_id)
  
  # msf_mortalities(con.pre, run_id = id.pre) |> View()
  
  ## Skagit investigation:
  stock_id_list = psc.stocks.map |> 
    filter(psc_stock_name == cur.stock) |> 
    pull(fram_stock_id)
  
  morts.pre = fetch_table(con.pre, table_name = "Mortality") |> 
    filter(run_id == id.pre) |> 
    filter(stock_id %in% stock_id_list) |> 
    mutate(total_mort = landed_catch + non_retention + shaker + drop_off+
             msf_landed_catch + msf_non_retention + msf_shaker + msf_drop_off) |> 
    group_by(fishery_id, time_step) |> 
    summarize(total_mort_pre = sum(total_mort)) |> 
    ungroup()
  
  morts.post = fetch_table(con.post, table_name = "Mortality") |> 
    filter(run_id == id.post) |> 
    filter(stock_id %in% stock_id_list) |> 
    mutate(total_mort = landed_catch + non_retention + shaker + drop_off+
             msf_landed_catch + msf_non_retention + msf_shaker + msf_drop_off) |> 
    group_by(fishery_id, time_step) |> 
    summarize(total_mort_post = sum(total_mort)) |> 
    ungroup()
  
  mort_compare = morts.pre |> 
    left_join(morts.post, by = c("fishery_id", "time_step"))
  
  mort_compare |> 
    filter(total_mort_post > total_mort_pre) |> 
    left_join(framrosetta::fishery_coho_fram |> 
                select(fishery_id, fishery_title), by = "fishery_id") |> 
    arrange(desc(total_mort_post))
}

create_mort_comparison(year = 2019, stock = "Skagit") |> 
  gt()

# fetch_table(con.pre, table_name = "Cohort") |> 
#   filter(run_id == id.pre) |> 
#   filter(stock_id %in% stock_id_list) |> 
#   filter(time_step == 1) |> 
#   pull(working_cohort) |> 
#   sum()
#   

# recruits.pre = fetch_table(con.pre, table_name = "StockRecruit") |> 
#   filter(run_id == id.pre) |> 
#   filter(stock_id %in% stock_id_list) |> 
#   pull(recruit_cohort_size) |> 
#   sum()



## Also look at recruit scalers. Could multiply by base cohort size to get actual sizes.