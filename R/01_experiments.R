library(data.table)
library(portfolio.optimization)
library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
library(tidyr)
library(tibble)
library(dplyr)
library(ggplot2)
library(stringr)

# ---- Set parameters ----
min_ingreds <- 10
max_ingreds <- 20
breakfast_cals <- 4 * 40 + 9 * 17 + 4 * 165
n_meals <- 1
total_cal <- (3100 - breakfast_cals)/n_meals
vegetarian <- TRUE
targets <- list(protein=(150 - 40)/n_meals,
                fat = (70 - 17)/n_meals,
                carb = (470 - 165)/n_meals)

# ---- Process data ----
# Load base data
food_df_raw <- fread("https://query.data.world/s/42ntdhs6lhk3oaqgk5upef6caoux4y")

# Remove some unhelpful groups
food_df <- food_df_raw %>%
  mutate(uppercase = str_extract(Descrip, "\\b[A-Z]+\\b")) %>%
  filter(is.na(uppercase) | nchar(uppercase) < 2,
         !FoodGroup %in%c("Sweets", 
                          "Meals, Entrees, and Side Dishes", 
                          "American Indian/Alaska Native Foods",
                          "Spices and Herbs",
                          "Beverages",
                          "Baby Foods",
                          "Restaurant Foods",
                          "Fast Foods"),
         Descrip != "Cornstarch",
         !grepl("dried", tolower(Descrip)),
         !grepl("dry", tolower(Descrip)),
         !grepl("powder", tolower(Descrip)),
         !grepl("soy protein", tolower(Descrip)),
         !grepl("pork", tolower(Descrip)),
         !grepl("turtle", tolower(Descrip)),
         !grepl("glandless", tolower(Descrip)),
         !grepl("beef", tolower(Descrip)),
         !grepl("lamb", tolower(Descrip)),
         !grepl("veal", tolower(Descrip)),
         !grepl("amaranth", tolower(Descrip)),
         !grepl("arrowhead", tolower(Descrip)),
         !grepl("balsam-pear", tolower(Descrip)),
         !grepl("dehydrated", tolower(Descrip)),
         !grepl("drumstick leaves", tolower(Descrip)),
         !grepl("winged bean", tolower(Descrip)),
         !grepl("crude ", tolower(Descrip)),
         !grepl("flour", tolower(Descrip)),
         !grepl("meat extender", tolower(Descrip)),
         !grepl("bacon", tolower(Descrip)),
         !grepl("sausage", tolower(Descrip)),
         !grepl("meal", tolower(Descrip)))

# Dietary requirements
if (vegetarian) {
  food_df <- food_df %>%
    filter(!FoodGroup %in% c("Beef Products", 
                             "Lamb, Veal, and Game Products",
                             "Poultry Products",
                             "Pork Products",
                             "Sausages and Luncheon Meats"))
}

# Protein by food group?
food_df %>%
  group_by(FoodGroup) %>%
  summarise(protein = sum(Protein_g)) %>%
  ggplot(aes(x=reorder(FoodGroup, protein), y=protein)) +
  geom_col(fill="grey60", colour="grey40") +
  coord_flip()

# Explore some high protein foods
food_df %>%
  arrange(desc(Protein_g)) %>%
  dplyr::select(Descrip, FoodGroup, Energy_kcal, Protein_g) %>%
  slice(1:20)

# Select relevant cols
food_df_proc <- food_df %>%
  dplyr::select(ID, FoodGroup, Descrip, Energy_kcal:Sugar_g) %>%
  mutate(ingred_count = 1)

# ---- Specifiying the optimisation ----

# This approach applies weights to each of the *columns*, so we need to transpose.
# You also need to add a row of zeros to allow it to make a time series I think.
returns <- food_df_proc %>% dplyr::select(Protein_g) %>% t
names(returns) <- food_df_proc$ID
rownames(returns) <- c("2020-01-01")

# Create 'portfolio' object
pspec <- portfolio.spec(assets=unique(food_df_proc$ID))

# Add a constraint such that we pick ingredients up to a maximum weight constraint
pspec <- add.constraint(portfolio=pspec,
                        type="weight_sum",
                        min_sum=20,
                        max_sum=50)

# Add a 'balanced diet' constraint. Don't allow any single ingredient to be above a certain weight
pspec <- add.constraint(portfolio=pspec,
                        type="box",
                        min=0,
                        max=5)

# Add limit on total number of ingredients you can include
#pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=max_ingreds)

# Add a protein target constraint
pspec <- add.constraint(portfolio=pspec, type="return", return_target=targets$protein)

# Create constraints matrix
const_mat <- food_df_proc %>% dplyr::select(Energy_kcal, Fat_g, Carb_g) %>% as.matrix
rownames(const_mat) <- food_df_proc$ID

# Add constraint for carb and fat target
pspec <- add.constraint(portfolio=pspec, type="factor_exposure",
                        B=const_mat,
                        lower=c(total_cal * 0.95, targets$fat * 0.95, targets$carb * 0.95), 
                        upper=c(total_cal * 1.05, targets$fat * 1.05, targets$carb * 1.05))


# Add objective - i.e. maximise protein subject to constraints
pspec <- add.objective(portfolio=pspec,
                       type='return',
                       name='mean')

# ---- Optimise ----

opt <- optimize.portfolio(R=returns,
                          portfolio=pspec,
                          optimize_method="ROI",
                          trace=TRUE)

# Initialise recipes list
recipes <- list()

# See the ingredients
recipes[[1]] <- food_df_proc %>%
  mutate(weight_selected = opt$weights) %>%
  mutate_at(vars(Energy_kcal:Carb_g), function(x) round(x * .$weight_selected, 2)) %>%
  filter(weight_selected > 0)

# Show recipe and stats
recipes[[1]]
recipes[[1]] %>% 
  mutate(weight_selected = weight_selected * 100) %>%
  dplyr::select(Energy_kcal:weight_selected) %>% 
  colSums()
total_cal
targets
