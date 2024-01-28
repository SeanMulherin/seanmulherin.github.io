coffee_shop <- data.frame(
  date = c("2024-01-01", "2024-01-01", "2024-01-02", "2024-01-02", "2024-01-03", "2024-01-03", "2024-01-04", "2024-01-04", "2024-01-05", "2024-01-05", "2024-01-06", "2024-01-06", "2024-01-07", "2024-01-07"),
  beverage = rep(c("latte", "mocha"), 7),
  quantity = c(20, 15, 25, 20, 40, 30, 41 ,40, 36, 34, 60, 65, 70, 62),
  price = rep(c(4.99, 5.99), 7)
)

coffee_shop <- read.table("~/Desktop/coffee_shop.tsv", header = TRUE)

filtered_coffee <- filter(coffee_shop, beverage == "latte")    # filter()
filtered_coffee2 <- filter(coffee_shop, quantity < 40)
grouped_coffee <- group_by(coffee_shop, date)                  # group_by()
only_latte <- select(coffee_shop, quantity)                            # select()
all_but_date <- select(coffee_shop, beverage:price)
summarised_coffee <- summarize(coffee_shop, weekly_sales = sum(quantity * price))                  # summarize()
mutated_coffee <- mutate(coffee_shop, sales = quantity * price)     # mutate()
arranged_coffee <- arrange(mutated_coffee, sales)    # arrange()
arranged_coffee_desc <- arrange(mutated_coffee, desc(sales))    
only_mocha <- filter(mutated_coffee, beverage == "mocha")


######## Pipelines ########
pipe_ex <- 1:10
pipe_ex |> sum()
pipe_ex %>% mean()
pipe_ex |> order(decreasing = TRUE)


coffee_shop |> 
  mutate(sales = quantity * price) |>
  filter(beverage == "latte")

coffee_shop |> 
  mutate(sales = quantity * price) |>
  filter(beverage == "latte") |>
  summarise(weekly_latte_sales = sum(sales))


##### starwars exercises ####
starwars |>
  filter(species == "Human") |>
  mutate(bmi = mass / (height / 100) ^ 2) |>
  select(name, bmi)
  



  
############### lubridate #################
class(coffee_shop$date)
coffee_shop$date <- as.Date(coffee_shop$date) # converting
year <- year(coffee_shop$date) # extracting
month <- month(coffee_shop$date)
day <- day(coffee_shop$date)

my_day <- as.Date("2024-01-25")
next_month <- my_day + months(1) # arithmetic
next_year <- my_day + years(1)

start_date <- ymd("2024-01-01")
end_date <- ymd("2024-01-31")
days_between <- as.numeric(end_date - start_date)

# format dates for display
formatted_date <- format(my_date, "%A, %B %d, %Y")





############### ggplot #################
ggplot(coffee_shop, aes(x = date, y = quantity)) +    # scatter plot
  geom_point()
  
ggplot(coffee_shop, aes(x = date, y = quantity)) +    # line graph
  geom_line()

ggplot(coffee_shop, aes(x = date, y = quantity)) +    # LOESS plot
  geom_smooth()


daily_sold <- coffee_shop |> 
                group_by(date) |>
                  summarise(quantity = sum(quantity))

ggplot(daily_sold, aes(x = date, y = quantity)) +     # bar plot
  geom_bar(stat = "identity")


ggplot(starwars, aes(height)) +     # histogram
  geom_histogram()


######### Customizing ######### 
line_graph <- ggplot(coffee_shop, aes(x = date, y = quantity)) +    
                geom_line(col = "salmon", linewidth = 2) +
                labs(x = "Date", y = "Quantity", title = "Total # of Beverages Sold Per Day")
line_graph

line_graph +
  annotate(geom="text", x = ymd("2024-01-04"), y = 50, label = "DataFest", col = 6, cex = 5)
line_graph + theme_bw()
line_graph + theme_dark()



bar_plot <- ggplot(daily_sold, aes(x = date, y = quantity)) +
              geom_bar(stat = "identity", 
                       fill = 1:7)

bar_plot
bar_plot + coord_flip()


  
# Checkpoint 2: Create a bar plot that showws total number of lattes sold each day
latte_by_day <- coffee_shop |>
  filter(beverage == "latte")

ggplot(latte_by_day, aes(x = date, y = quantity)) +
  geom_bar(stat = "identity", fill = 1:7, alpha = 0.7) +
  labs(x = "Day", y = "Lattes Sold", title = "Total # of Lattes Sold Per Day") + 
  theme_bw()


x1 = seq(-10, 10, 0.1)
y1 = 2*x^2 + 3*x - 5
y2 = sin(x) + x/2
sys_eq_df <- data.frame(x, y1, y2)
ysol <- y2[which(round(y1, 1) == round(y2, 1))]
xsol <- x[which(round(y1, 1) == round(y2, 1))]

ggplot(sys_eq_df, aes(x1)) +
  geom_line(aes(y = y1, col = "salmon"), linewidth = 1) +
  geom_line(aes(y = y2, col = "skyblue"), linewidth = 1) +
  theme_bw() + 
  ylim(-10, 10) +
  labs(x = "x", y = "y") +
  scale_color_discrete(name = "Lines", labels = c("y1", "y2")) +
  geom_point(x = xsol[1], y = ysol[1], col = "red", cex = 3) +
  geom_point(x = xsol[2], y = ysol[2], col = "red", cex = 3) +
  annotate(geom="text", x = 3.3, y = 0.3, label="(1.2, 1.53)") + 
  annotate(geom="text", x = -4.1, y = -3, label="(-2.2, -1.91)")
  


############### tidyr #################

######## gather() ######## 
gathered_df <- coffee_shop |>
                  gather(Cols, Elements, 
                         beverage:price)
gathered_df

######## spread() ######## 
spread_df <- coffee_shop |>
              spread(beverage, quantity)

spread_df |> drop_na()        # drop_na()
spread_df |> replace_na(list(latte = 0, mocha = 0))         # replace_na()

######## separate() ######## 
sep_df <- data.frame(Type = c("team.A", "team.B", "party.C", "crew.D", "party.E"))
sep_df

sep_df |> 
  separate(Type, c("Group Type", "Letter"))

sep_df |>
  separate(Type, c(NA, "Letter"))

######## unite() ######## 
names_df <- data.frame(First = c("Harry", "James", "Stephen", "Michael", "Taylor"),
                       Last = c("Styles", "Bond", "Hawking", "Buble", "Swift"),
                       Age = c(29, 37, 76, 48, 34))
names_df

united_df <- names_df |>
              unite("Full Name", First, Last, sep = " ")
united_df

names_df |>
  unite("Full Name", Last, First, sep = ", ")

# How would we reverse unite?
united_df |>
  separate("Full Name", c("First", "Last"), sep = " ")




############### readr #################
read_table("~/Desktop/Portfolio/dataCleaning/coffee_shop.tsv")
read_table(
  "~/Desktop/Portfolio/dataCleaning/coffee_shop.tsv",
  col_types = cols(
      date      = col_date(format = ""),
      beverage  = col_factor(levels = c("latte", "mocha")),
      quantity  = col_integer(),
      price     = col_double()
  )
)


################# purrr #################
double_func1 <- function(x) x * 2
double_func2 <- \(x) x * 2
double_func1(15)
double_func2(15)


coffee_shop |>                  # split()
  split(coffee_shop$beverage)

map(5, \(x) x + 2)        # map()
map(5, \(x) x^2)


## What if we wanted to predict the # of lattes and mochas sold Jan 8th?
coffee_shop |>
  split(coffee_shop$beverage) |>
  map(\(df) lm(quantity ~ date, data = df)) |>
  map(\(model) predict(model, newdata = data.frame(date = ymd("2024-01-08"))))



################# stringr #################
a_string <- "Hello! I hope you're enjoying this workshop."
str_sub(a_string, start = 1, end = 5)       
str_length(a_string)       # length
str_split(a_string, pattern = " ")       
str_count(a_string, "o")       
str_count(a_string, "[oab]")
str_detect(a_string, "joy")
str_locate(a_string, "you")
str_replace(a_string, "you", "we")
str_to_upper(a_string)

s <- str_split(a_string, pattern = " ")[[1]]
str_c(s, collapse = " ")

names(starwars)
starwars |>
  select(ends_with("color"))

################# Final Exercise #################
# Step 1: Read in data
eth_df <- read_csv("~/Desktop/eth.csv",
                   col_types = cols(Date = col_date(format = "%m/%d/%Y"))
                   )

# Step 2: Clean data
k <- str_detect(eth_df$Vol., "K")
m <- str_detect(eth_df$Vol., "M")

eth_df$Vol.[k] <- str_sub(eth_df$Vol.[k], start = 1, end = 6) |>
                  as.numeric() |>
                  map(\(x) x * 1000)

eth_df$Vol.[m] <- str_sub(eth_df$Vol.[m], start = 1, end = 4) |>
                  as.numeric() |>
                  map(\(x) x * 1000000)

eth_df$`Change %` <- eth_df$`Change %` |>
                     str_replace("%", "") |>
                     as.numeric()

# Step 3: Fit model and predict price
lm <- lm(Price ~ Date, data = eth_df)
days <- seq(ymd('2023-12-27'), ymd('2024-02-27'), by='1 day')
pred <- predict(lm, newdata = data.frame(Date = days))

# Step 4: Plot
eth_df[33:63] <- NA
ggplot(eth_df, aes(Date, Price)) +
  geom_line(linewidth = 1.5) +
  geom_line(aes(x = days, y = pred), col = "salmon", linewidth = 1.5) +
  theme_bw() +
  ggtitle("Linear Regression USD/ETH") +
  annotate(geom = "text", x = ymd('2024-02-25'), y = pred[63] + 20, label = "$2483.68") +
  geom_point(x = ymd('2024-02-27'), y = pred[63], col = "red", cex = 3)








