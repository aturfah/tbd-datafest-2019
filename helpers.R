library(tidyverse)

load_UCSB <- function() {
  county_name <- function(name_col) {
    grepl(",", name_col)
  }
  fix_colnames <- function(df) {
    rename(df, Areaname=1,
           Edu_geq_hs_pct=4,
           Edu_geq_bachelors_pct=5,
           Edu_lt_hs_pct=6,
           resident_popn_total=7,
           resident_popn_med_age=8,
           resident_popn_lt_18_count=9,
           resident_popn_geq_18_count=10,
           resident_popn_geq_20_lt_24_count=11,
           resident_popn_geq_25_lt_29_count=12,
           resident_popn_geq_30_lt_34_count=13,
           resident_popn_geq_35_lt_44_count=14,
           resident_popn_geq_45_lt_54_count=15,
           resident_popn_geq_55_lt_59_count=16,
           resident_popn_geq_60_lt_64_count=17,
           resident_popn_geq_65=18,
           soc_sec_recipient_count=19,
           soc_sec_payments_1980=20,
           popn_sq_mile=21,
           popn_urban_count=22,
           popn_rural_count=23,
           male_popn_count=24,
           female_popn_count=25,
           white_popn_count=26,
           black_popn_count=27,
           asian_pfcisld_count=28,
           hispanic_count=29,
           males_geq_15_married_count=30,
           group_quarters_count=31,
           institutionalized_count=32,
           median_income_1979=33,
           hshld_income_lt_10_1979=34,
           hshld_income_geq_10_lt_15_1979=35,
           hshld_income_geq_15_lt_20_1979=36,
           hshld_income_geq_20_lt_25_1979=37,
           hshld_income_geq_25_lt_30_1979=38,
           hshld_income_geq_30_lt_35_1979=39,
           hshld_income_geq_35_lt_40_1979=40,
           hshld_income_geq_40_lt_50_1979=41,
           hshld_income_geq_50_lt_75_1979=42,
           hshld_income_geq_75_1979=43,
           expend_fed_govt=44,
           expend_local_govt=45,
           expend_local_govt_edu=46,
           expend_welfare=47,
           expend_hosp_health=48,
           expend_police_prot=49)
  }
  
  read.csv('ds_UCSB_csv.csv') %>%
    fix_colnames() %>%
    filter(Year==2010, STCOU != 0, county_name(Areaname)) %>%
    select(-popn_urban_count, -popn_rural_count) %>%
    mutate(Areaname=tolower(Areaname)) %>%
    separate(Areaname, c('County', 'State'), sep=', ') %>%
    mutate(Edu_geq_hs_pct=Edu_geq_hs_pct/100,
           Edu_geq_bachelors_pct=Edu_geq_bachelors_pct/100,
           Edu_lt_hs_pct=Edu_lt_hs_pct/100,
           total_households=hshld_income_lt_10_1979 + 
             hshld_income_geq_10_lt_15_1979 + 
             hshld_income_geq_15_lt_20_1979 + 
             hshld_income_geq_20_lt_25_1979 + 
             hshld_income_geq_25_lt_30_1979 + 
             hshld_income_geq_30_lt_35_1979 + 
             hshld_income_geq_35_lt_40_1979 + 
             hshld_income_geq_40_lt_50_1979 + 
             hshld_income_geq_50_lt_75_1979 + 
             hshld_income_geq_75_1979,
           resident_popn_per_household=resident_popn_total/total_households,
           resident_popn_lt_18_pct=resident_popn_lt_18_count/resident_popn_total,
           resident_popn_geq_18_pct=resident_popn_geq_18_count/resident_popn_total,
           resident_popn_geq_20_lt_24_pct=resident_popn_geq_20_lt_24_count/resident_popn_total,
           resident_popn_geq_25_lt_29_pct=resident_popn_geq_25_lt_29_count/resident_popn_total,
           resident_popn_geq_30_lt_34_pct=resident_popn_geq_30_lt_34_count/resident_popn_total,
           resident_popn_geq_35_lt_44_pct=resident_popn_geq_35_lt_44_count/resident_popn_total,
           resident_popn_geq_45_lt_54_pct=resident_popn_geq_45_lt_54_count/resident_popn_total,
           resident_popn_geq_55_lt_59_pct=resident_popn_geq_55_lt_59_count/resident_popn_total,
           resident_popn_geq_60_lt_64_pct=resident_popn_geq_60_lt_64_count/resident_popn_total,
           resident_popn_geq_65_pct=resident_popn_geq_65/resident_popn_total,
           male_popn_pct=male_popn_count/resident_popn_total,
           female_popn_pct=female_popn_count/resident_popn_total,
           white_popn_pct=white_popn_count/resident_popn_total,
           black_popn_pct=black_popn_count/resident_popn_total,
           asian_pfcisld_pct=asian_pfcisld_count/resident_popn_total,
           hispanic_pct=hispanic_count/resident_popn_total,
           hshld_income_lt_10_1979_pct=hshld_income_lt_10_1979/total_households,
           hshld_income_geq_10_lt_15_1979_pct=hshld_income_geq_10_lt_15_1979/total_households,
           hshld_income_geq_15_lt_20_1979_pct=hshld_income_geq_15_lt_20_1979/total_households,
           hshld_income_geq_20_lt_25_1979_pct=hshld_income_geq_20_lt_25_1979/total_households,
           hshld_income_geq_25_lt_30_1979_pct=hshld_income_geq_25_lt_30_1979/total_households,
           hshld_income_geq_30_lt_35_1979_pct=hshld_income_geq_30_lt_35_1979/total_households,
           hshld_income_geq_35_lt_40_1979_pct=hshld_income_geq_35_lt_40_1979/total_households,
           hshld_income_geq_40_lt_50_1979_pct=hshld_income_geq_40_lt_50_1979/total_households, 
           hshld_income_geq_50_lt_75_1979_pct=hshld_income_geq_50_lt_75_1979/total_households, 
           hshld_income_geq_75_1979_pct=hshld_income_geq_75_1979/total_households,
           expend_fed_govt_percapita=expend_fed_govt/resident_popn_total,
           expend_local_govt_percapita=expend_local_govt/resident_popn_total,
           expend_local_govt_edu_perstudent=expend_local_govt_edu/resident_popn_lt_18_count,
           expend_hosp_health_percapita=expend_hosp_health/resident_popn_total,
           expend_police_prot_percapita=expend_police_prot/resident_popn_total) %>%
    rename(county=County, state=State) %>%
    drop_na()
} 

load_elec_arda <- function() {
  read_csv("arda_elec.csv") %>%
    replace_na(list(Adventis.adherent=0,
                    Adventis.congreg=0,
                    Baptist.adherent=0,
                    Baptist.congreg=0,
                    Catholic.adherent=0,
                    Catholic.congreg=0,
                    Communal.adherent=0,
                    Communal.congreg=0,
                    Eastern.adherent=0,
                    Eastern.congreg=0,
                    European.adherent=0,
                    European.congreg=0,
                    Holiness.adherent=0,
                    Holiness.congreg=0,
                    Judaism.adherent=0,
                    Judaism.congreg=0,
                    Latter.d.adherent=0,
                    Latter.d.congreg=0,
                    Liberal.adherent=0,
                    Liberal.congreg=0,
                    Lutheran.adherent=0,
                    Lutheran.congreg=0,
                    Methodis.adherent=0,
                    Methodis.congreg=0,
                    Other.gr.adherent=0,
                    Other.gr.congreg=0,
                    Pentecos.adherent=0,
                    Pentecos.congreg=0,
                    Presbyte.adherent=0,
                    Presbyte.congreg=0,
                    Restorat.adherent=0,
                    Restorat.congreg=0)
               ) %>%
    mutate(total_congreg = Adventis.congreg + Baptist.congreg + Catholic.congreg + Communal.congreg
           + Eastern.congreg + European.congreg + Holiness.congreg + Judaism.congreg + Latter.d.congreg
           + Liberal.congreg + Lutheran.congreg + Methodis.congreg + Other.gr.congreg + Pentecos.congreg
           + Presbyte.congreg + Restorat.congreg,
           total_adherent = Adventis.adherent + Baptist.adherent + Catholic.adherent + Communal.adherent
           + Eastern.adherent + European.adherent + Holiness.adherent + Judaism.adherent + Latter.d.adherent
           + Liberal.adherent + Lutheran.adherent + Methodis.adherent + Other.gr.adherent + Pentecos.adherent
           + Presbyte.adherent + Restorat.adherent,
           adherent_pct=total_adherent/totpop,
           adherent_per_congreg=total_adherent/total_congreg,
           Adventis.congreg_pct=Adventis.congreg/total_congreg,
           Baptist.adherent_pct=Baptist.adherent/total_adherent,
           Baptist.congreg_pct=Baptist.congreg/total_congreg,
           Catholic.adherent_pct=Catholic.adherent/total_adherent,
           Catholic.congreg_pct=Catholic.congreg/total_congreg,
           Communal.adherent_pct=Communal.adherent/total_adherent,
           Communal.congreg_pct=Communal.congreg/total_congreg,
           Eastern.adherent_pct=Eastern.adherent/total_adherent,
           Eastern.congreg_pct=Eastern.congreg/total_congreg,
           European.adherent_pct=European.adherent/total_adherent,
           European.congreg_pct=European.congreg/total_congreg,
           Holiness.adherent_pct=Holiness.adherent/total_adherent,
           Holiness.congreg_pct=Holiness.congreg/total_congreg,
           Judaism.adherent_pct=Judaism.adherent/total_adherent,
           Judaism.congreg_pct=Judaism.congreg/total_congreg,
           Latter.d.adherent_pct=Latter.d.adherent/total_adherent,
           Latter.d.congreg_pct=Latter.d.congreg/total_congreg,
           Liberal.adherent_pct=Liberal.adherent/total_adherent,
           Liberal.congreg_pct=Liberal.congreg/total_congreg,
           Lutheran.adherent_pct=Lutheran.adherent/total_adherent,
           Lutheran.congreg_pct=Lutheran.congreg/total_congreg,
           Methodis.adherent_pct=Methodis.adherent/total_adherent,
           Methodis.congreg_pct=Methodis.congreg/total_congreg,
           Other.gr.adherent_pct=Other.gr.adherent/total_adherent,
           Other.gr.congreg_pct=Other.gr.congreg/total_congreg,
           Pentecos.adherent_pct=Pentecos.adherent/total_adherent,
           Pentecos.congreg_pct=Pentecos.congreg/total_congreg,
           Presbyte.adherent_pct=Presbyte.adherent/total_adherent,
           Presbyte.congreg_pct=Presbyte.congreg/total_congreg,
           Restorat.adherent_pct=Restorat.adherent/total_adherent,
           Restorat.congreg_pct=Restorat.congreg/total_congreg,
           pct_trump_rel_clint=trump/(trump + clinton),
           trump_binary=factor(trump>clinton)
           )
}

load_true_data <- function() {
  arda_elec_data <- load_elec_arda()
  ucsb_data <- load_UCSB()
  inner_join(ucsb_data, arda_elec_data, by=c('county'='county.x', 'state'='state_po')) %>%
    select(trump_binary, pct_trump_rel_clint, county, fips, state, resident_popn_total, popn_sq_mile,
           median_income_1979, expend_fed_govt, expend_local_govt, total_congreg, total_adherent,
           contains('_pct'), contains('_per')) %>%
    mutate(trump_binary=factor(trump_binary)) %>%
    drop_na()
}

tree_err_func <- function(rf, data, true_colname) {
  pred <- predict(rf, data, type="class")
  real <- data[, true_colname]
  matches <- pred == real
  
  # Incorrect Rows
  incorrect_rows <- cbind(data, pred, matches)
  
  # Confusion Matrix
  mispred_mat <- table(pred, real)
  
  # Mispredicton %
  total <- sum(mispred_mat)
  correct <- sum(diag(mispred_mat))
  list(matrix=mispred_mat,
       val=(total - correct)/total,
       rows=incorrect_rows)
}