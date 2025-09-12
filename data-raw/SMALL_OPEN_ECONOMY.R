equations <- "constot ~  gdpos + constot.L(1),
ifix ~ ifix.L(1),
extot1 ~ xworld + wkfreuro + extot1.L(1),
imtot1 ~  domdemoi + imtot1.L(1),
pconsp ~ wkfreuro + poilusd + pconsp.L(1),
srate ~ pconsp + srate_ge + srate.L(1),
gdpos ==  0.6*constot + 0.6*domdemoi + 0.5*extot1 - 0.4*imtot1,
domdemoi == 0.6*constot + 0.4*ifix"

exogenous_variables <- c("xworld", "wkfreuro", "poilusd", "srate_ge")

sys_eq <- koma::system_of_equations(equations, exogenous_variables)
endogenous_variables <- sys_eq$endogenous_variables
nominal_variables <- sys_eq$weight_variables

varfor <- c(
  endogenous_variables, exogenous_variables, nominal_variables
)
varfor <- unique(varfor)

keys <- paste0("ch.kof.modelinput.", varfor)

# con <- koftsdb::db_connection_create("kofdb",
#   host = "archivedb.kof.ethz.ch",
#   passwd = getPass::getPass()
# )
#
# ts_data <- koftsdb::db_ts_read(
#   con,
#   ts_keys = keys,
#   valid_on = "2025-01-01",
#   schema = "vja"
# )

small_open_economy <- list()
small_open_economy$consumption <- ts_data$ch.kof.modelinput.constot
small_open_economy$investment <- ts_data$ch.kof.modelinput.ifix
small_open_economy$exports <- ts_data$ch.kof.modelinput.extot1
small_open_economy$imports <- ts_data$ch.kof.modelinput.imtot1
small_open_economy$prices <- ts_data$ch.kof.modelinput.pconsp
small_open_economy$interest_rate <- ts_data$ch.kof.modelinput.srate
small_open_economy$gdp <- ts_data$ch.kof.modelinput.gdpos
small_open_economy$domestic_demand <- ts_data$ch.kof.modelinput.domdemoi
small_open_economy$world_gdp <- ts_data$ch.kof.modelinput.xworld
small_open_economy$exchange_rate <- ts_data$ch.kof.modelinput.wkfreuro
small_open_economy$oil_price <- ts_data$ch.kof.modelinput.poilusd
small_open_economy$interest_rate_germany <- ts_data$ch.kof.modelinput.srate_ge

small_open_economy <- lapply(small_open_economy, stats::window, end = c(2024, 4))

usethis::use_data(
  small_open_economy,
  overwrite = TRUE,
  internal = FALSE
)
