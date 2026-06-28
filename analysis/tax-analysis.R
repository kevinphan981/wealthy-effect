library(tidyverse)
library(priceR)

# source(allagi-donations)
# requires df_hawaii

# A10300, which is total tax liability (what they owe). This is only federal.
df_hawaii |>
  ggplot(aes(x = agi_stub_str, y = taxes_per_return)) +
  geom_col()

#A10300 is total tax liabilities, 
# payments 10600 is what is taken out but could be adjusted with tax returns
df_hawaii |>
  ggplot(aes(x = agi_stub_str, y = A10300)) +
  geom_col()


# if we wanted to get state/local, we are going to have to add things up
df_hawaii |>
  ggplot(aes(x = agi_stub_str, y = A18500)) +
  geom_col()

