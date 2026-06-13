library(dplyr)
library(mice)

data <- read.csv("../synthetic_dm2_final.csv") 

# drop columns with >30% missingness ====
missing_prop <- colMeans(is.na(data))

drop_cols <- names(missing_prop[missing_prop > 0.3]) |> c("ID","follow_up_days")
drop_cols  # inspect which ones are getting dropped

data_clean <- data |> dplyr::select(-all_of(drop_cols))


# multiple imputation ====
set.seed(123)

imp <- mice(data_clean, m = 1, maxit = 5, seed = 123, method="cart")
imputated_data <- complete(imp,1)
saveRDS(imputated_data, file="../imputed.rds")


