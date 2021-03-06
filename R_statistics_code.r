# R CODE

# LESSON 2

print("Hello World!")

5 + 5 # addition
5 - 5 # subtraction
5 * 2 # multiplication
5 / 2 # division (and addition)
5^2 # exponentiation, also "**" works, e.g. 2**5
5%%2 # modulo
5%/%2 # integer division

# This is a comment
1 + 1 # this is a comment as well

x <- 42 # assignment and variable creation
x # print the variable content
x <- 21 # reassign the variable
x # check the new values

my_apples <- 5
my_apples
my_oranges <- 6
my_apples + my_oranges # sum two variables
my_fruit <- my_apples + my_oranges
my_fruit

# What if a want to remove a variable?
rm(x)
x # now x is missing!

my_numeric <- 42 # numeric variable
my_integer <- 42L # integer variable
my_double <- 42.24 # numeric variable
my_character <- "universe" # character variable
my_logical <- FALSE # logical or boolean variable (n.b. T = TRUE; F = FALSE)

class(my_numeric) # check the variable type, using one of your first functions!
class(my_character)
class(my_logical)

numeric_vector <- c(1, 10, 49) # create a numeric vector
numeric_vector
character_vector <- c("a", "b", "c") # create a character vector
character_vector
character_vector <- c("a", "b", "c", my_numeric)
character_vector
boolean_vector <- c(TRUE, FALSE, T) # create a boolean vector
boolean_vector
apples_vector <- c(40, 50, 20, 120, 240)
apples_vector
oranges_vector <-  c(24, 50, 100, 350, 10)
oranges_vector
my_fruit_vector <- my_oranges_vector + my_apples_vector # if both vectors are the same size, the operation is "paired"
apples_vector <- c(40, 50, 20, 120)
oranges_vector <-  c(24, 50)

my_fruit_vector <- apples_vector + oranges_vector # if the vectors have different lengths, the shortest one is "recycled" until the longest ones ends (e.g. for a vector of length four and a vector of length two, it would be: first of A with first of B, second of A with second of B, third of A with first of B, fourth of A with second of B)
my_fruit_vector + 50 # in this case the operation is applied the same way to all vector's elements

1:10 # sequence of integer numbers from one to ten
10:1 # sequence of integer numbers from ten to one

rep(c("a", "b", "c"), 2) # this repeats the whole vector two times
rep(c("a", "b", "c"), times = 2) # this is the same as the above command
rep(c("a", "b", "c"), each = 2) # this repeats each vectors element twice before moving onto the next one




# LESSON 3

### How to get help in R ###

?mean
help("sum")

### Another way to create vectors (numeric only) ###

seq(1, 10, by = 1) # note that it's equal to "1:10"
seq(1, 10, by = 2) # note that the ending point "is not reached" as the sequence stops at 9, indeed 10 would be out of the sequence as 9 -> 10 has a step of 1 and not 2, whereas 11 is out of the given interval

### Other functions to work on vectors)

apples_vector <- c(20, 50, 30, 40, 100)
oranges_vector <- c(70, 70, 50, 20, 80)

names(apples_vector) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") # assign names to vector elements
apples_vector
names(oranges_vector) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
days_vector <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

names(apples_vector) <-  days_vector
apples_vector
names(oranges_vector) <- days_vector
oranges_vector

apples_vector + oranges_vector # note that names are "inherited" from apples_vector and oranges_vector

typeof(oranges_vector) # you can check the "type" of a given object with typeof (note that the given "classes" are slightly different to those printed by class() function)
is.numeric(oranges_vector) # with the is.___ functions you can test the types of objects
is.character(oranges_vector)
is.double(oranges_vector)
is.integer(oranges_vector)
as.character(oranges_vector) # with the as.___ functions you coerce data types into other data types
as.logical(oranges_vector)

typeof(days_vector)
is.character(days_vector)
is.logical(days_vector)
is.vector(days_vector)
as.logical(days_vector)
as.numeric(days_vector)

logical_vector <- c(T, T, F)
typeof(logical_vector)
is.logical(logical_vector)
is.numeric(logical_vector)
as.numeric(logical_vector)
as.character(logical_vector)

typeof(c(1.1, 2.3, 4))
length(oranges_vector)
length(1:3)

total_daily <- apples_vector + oranges_vector # sum two vectors
total_daily
total_apples <- sum(apples_vector) # sum the elements of a vector
total_apples
total_oranges <-  sum(oranges_vector)
total_week <- apples_vector + oranges_vector
total_week
total_apples > total_oranges # > major than

### Vectors indexing ###

apples_wednesday <- apples_vector["Wednesday"] # select elements by name
apples_wednesday
apples_vector[2:4] # select element by index/position
apples_vector[c("Monday", "Tuesday")]
selection_vector <- apples_vector > 100
selection_vector
apples_vector[selection_vector]
apples_vector[apples_vector < 100] # "<" means "minor than"
apples_vector[apples_vector < 100] # ">" means "minor than"
apples_vector[apples_vector == 120] # "==" means "is equal"
apples_vector[apples_vector != 120] # "!=" means "is not equal"
# "<=" means "minor or equal than"
# ">=" means "major or equal than"

### Matrices ###

matrix(1:9, nrow = 3) # your first matrix!
matrix(1:9, byrow = TRUE, nrow = 3) # by default byrow = FALSE, therefore the matrix is filled column-wise, by specifying byrow = TRUE the matrix is filled row-wise

# here we create our very first community matrix! A community matrix contains occurrences of species within sites. Typically, species are represented by columns and sites by rows. The value at the intersection between a species and a site tells us if the species was found at the site and in certain cases also the abundance of the species at the site
# in this case we create a matrix where values represent the number of individuals of a given species found in a given site (i.e. abundances). If we would have had only 1s and 0s, we would have called it a presence/absence community matrix (where 1 means we found the species and 0 means we didn't found the species)
garden <- c(10, 2) # in our garden site we found 10 individuals of the first species and two individuals of the second species
forest <- c(8, 3) # here we found 8 individuals of the first species and 3 of the second species
hedgerow <- c(1, 12) # see above
community_matrix <- matrix(c(garden, forest, hedgerow), nrow = 3, byrow = TRUE) # here we build our community matrix by providing a vector made by combining our previous 3 vectors and filling the matrix row-wise -> each row is a site and has two fields (i.e. columns)
species <- c("Parus major", "Erithacus rubecula") # here we create a vector for columns' names (i.e. species' names)
sites <- c("Garden", "Forest", "Hedgerow") # vector for rows' names (i.e. sites' names)
colnames(community_matrix) <- species # we assign names to the columns
rownames(community_matrix) <- sites # the same with rows
community_matrix # our nicely formatted community matrix

# we can also expand our matrix by "binding" other vectors or matrices to our original matrix

passer <- c(15, 2, 5) # we create a vector for another species we sampled but we previously forgot to add to the matrix
community_matrix <- cbind(community_matrix, passer) # here we add (bind) a new column (cbind -> column bind)
park <- c(10, 1, 8) # we also want to add another sampling site where we collected data on the three species we're studying
community_matrix <- rbind(community_matrix, park) # here we add a new row (rbind -> row bind)
community_matrix

colnames(community_matrix)[3] <- "Passer domesticus" # by using vector indexing we only modify the third column name
rownames(community_matrix)[4] <- "Park" # as above with the fourth row name
community_matrix




# LESSON 4

### Matrices ###

matrix(1:9, byrow = TRUE, nrow = 3) # default is byrow = FALSE

garden <- c(10, 2)
forest <- c(8, 3)
hedgerow <- c(1, 12)
community_matrix <- matrix(c(garden, forest, hedgerow), nrow = 3, byrow = TRUE)
species <- c("Parus major", "Erithacus rubecula")
sites <- c("Garden", "Forest", "Hedgerow")
colnames(community_matrix) <- species
rownames(community_matrix) <- sites
community_matrix

passer <- c(15, 2, 5)
community_matrix <- cbind(community_matrix, passer)
park <- c(10, 1, 8)
community_matrix <- rbind(community_matrix, park)
community_matrix

colnames(community_matrix)[3] <- "Passer domesticus"
rownames(community_matrix)[4] <- "Park"
community_matrix

dim(community_matrix) # check matrix's dimensions, first comes the number of rows, second comes the number of columns
nrow(community_matrix) # check the number of rows
ncol(community_matrix] # check the number of columns

colSums(community_matrix) # calculate column sums (this and the latter works for matrices, whereas for vectors you can use the sum() function)
rowSums(community_matrix) # calculate row sums

community_matrix + 2
community_matrix * 2 # note that this is NOT the standard matrix multiplications for which you should use %*% in R

community_matrix[, "Parus major"] # select the "Parus major" row
community_matrix["Park", ] # select the "Park" row
community_matrix[1, ] # this subset the first row of the matrix
community_matrix[, 2] # second column (note that as for the dim() function, the first number always refers to rows and the second to columns)
community_matrix[, 2, drop = FALSE] # keep the matrix structure by using the "drop = FALSE" argument
# indeed you can cross check it
class(community_matrix[, 2])
class(community_matrix[, 2, drop = FALSE])

community_matrix[2, 3] # select the element found at the intersection between the second row and the third column
community_matrix[1:2, ] # select the first two rows
community_matrix[, 2:ncol(community_matrix)] # select the second to the last columns by taking advantage of the ncol() function


### Factors ###

plants_vector <- c("Tree", "Grass", "Forb", "Grass", "Tree") # create a vector of names (or labels)
factor_plants_vector <- factor(plants_vector) # create the actual factor from the character vector
factor_plants_vector

temperature_vector <- c("High", "Low", "High","Low", "Medium")
factor_temperature_vector <- factor(temperature_vector, order = TRUE, levels = c("Low", "Medium", "High")) # note here we specified that the factor is an ordered one (factors can be ordered or not), as we specified it is ordered we have to specify
factor_temperature_vector
summary(factor_temperature_vector) # calculate the frequency of each element (i.e. a summary statistic, note that the summary() function can be used with several other data structures and objects)

substrate_vector <- c("L", "S", "S", "L", "L")
factor_substrate_vector <- factor(substrate_vector)
levels(factor_substrate_vector) <- c("Limestone", "Siliceous") # here we assign factor levels by means of levels() function
summary(factor_substrate_vector)

### Data frames ###

# the line below creates a 6 rows and 5 columns dataframe, note that different columns may contain different data types but a column can have only one data type. Moreover, all columns must have the same length
dat <- data.frame(
	id = 1:6,
	name = c("Eokochia saxicola", "Cytisus aeolicus", "Anthemis aeolica", "Centaurea aeolica", "Silene hicesiae", "Genista tyrrhena"),
	has_legumes = c(F, T, F, F, F, T),
	population_size = c(50, 2000, 5000, 10000, 200, 3000),
	growth_form = factor(c("Herb", "Tree", "Herb", "Shrub", "Herb", "Tree"))
	)




# LESSON 5

### Data frames ###

dat <- data.frame(
	id = 1:6,
	name = c("Eokochia saxicola", "Cytisus aeolicus", "Anthemis aeolica", "Centaurea aeolica", "Silene hicesiae", "Genista tyrrhena"),
	has_legumes = c(F, T, F, F, F, T),
	population_size = c(50, 2000, 5000, 10000, 200, 3000),
	growth_form = factor(c("Herb", "Tree", "Herb", "Shrub", "Herb", "Tree"))
	)

dat
dat$id # extracts a vector
dat[2, 3] # extracts a vector
dat[2, 3, drop = FALSE] # extracts a data frame thanks to the "drop = FALSE" argument
dat[, 3] # extracts a vector
dat[, 3, drop = FALSE] # extracts a data frame, this case and the three above explain also how a matrix behave, e.g. if you have a matrix "m <- matrix(1:9, ncol = 3)" if you do "m[2, 3]" or "m[2, ]" or "m[, 3]" you will end up with vectors, however if you do "m[2, 3, drop = F]" or "m[2, , drop = FALSE]" or "m[, 3, drop = FALSE]" you will end with data frames
dat[1, ] # however, even if you only extract just one row, you end up with a data frame, as different columns can host different data types, hence even a single row cannot be converted to a vector for sure without forcing coercion (unless all the columns host the same data type)
dat[2, 3:4] # extracts a data frame
dat[2:3, ] # extracts a data frame
dat["id"] # extracts a data frame
dat[["id"]] # extracts a vector	
class(dat["id"]) # in this way you can cross check the structure of your output
class(dat[["id"]]) # as above, AAA try to experiment with elements/components indexing/subsetting on differnt data types and see how the outputs change
dat[c("id", "name")]
dat[, c("id", "name")]
dat[1:2, c("id", "name")]
dat[dat$has_legumes == T, ]
dat[dat$population_size < 3000, ]
dat$growth_form <- NULL # by assigning NULL to a column you can erase it, this is equivalent to "dat <- dat[, -5]" and "dat <- dat[-5]"
dat


str(dat) # check the structure of dat (you can use dat on different data structure, not only data frames!)
summary(dat) # check summary statistics for each column
dim(dat) # check the number of rows and columns (as for the dim() function applied to a matrix)

class(dat)

cbind(dat, 6:1) # you can add a new column to a data frame with cbind
cbind(dat, id.2 = 6:1) # the same as above but providing a name to our new column
dat$id.2 <- 6:1 # this brings the same result as the previous line of code
new_data <- data.frame(
	id = 7,
	name = "Limonium minutiflorum",
	has_legumes = F,
	population_size = 2000,
	) # if you want to add one or more rows you have to build a data frame before with the same number of columns of the other data frame, their names and types have to match as well
rbind(dat, new_data) # now you can use rbind on the two "matched" data frames

### Lists ###

vec <- 1:10
mat <- matrix(1:9, ncol = 3)
dat <- mtcars[1:10, ]
my_list <- list(vec, mat, dat) # here we create a list providing the components as arguments of the list function
my_list
my_list <- list(my_vector = vec, my_matrix = mat, my_df = dat) # in this way you can create a list with named components
my_list
my_list[["my_vector"]] # to index a component of a list you need to use "[[" rather than "[", as with other data structures you can refer to it with its name or its index, also beware that when using "[[" you can extract only one component at timem
my_list[[1]]
my_list$my_vector
my_list[["my_vector"]][3] # to index an element of a component you can chain "[[" with "[", here indeed we first extract "my_vector" component and then we extract its third element
my_list[[1]][3]
my_list[["my_vector"]] <- 10:1 # in this way you can update a component or create one from scratch (to create one from scratch the new component has to have a name not used before, you can also do it with components' indices, e.g. "my_list[[4]] <- 1:2")
my_list
my_list[[3]] <- NULL # in this way you can remove a component from a list
my_list

my_list$my_other_vector <- 1:100
my_list

my_new_list <- list("A", 1:3, c(T, F, F), mat, dat)
names(my_new_list) <- c("First letter", "Integer vector", "Some logical", "A matrix...", "...and a data frame") # here you can name a list's components by using the names() function as you did with vectors
my_new_list

c(my_list, my_new_list)
str(c(my_list, my_new_list)) # notice that the list has been merged, i.e. the components of the list are now the components of a list
list(my_list, my_new_list) # notice you have now a list with two components, i.e. the two lists we provided to the list function, each composed by its components

length(my_list)

list(1, 2, 3)
unlist(list(1, 2, 3))




# LESSON 6

### The Null Object ###

NULL #NULL represents the null object in R. NULL is used mainly to represent the lists (and vectors) with zero length, and is often returned by expressions and functions whose value is undefined.
class(NULL)
typeof(NULL)
as.null(1:3)
is.null(NULL)
is.null(1:3)
is.null(pairlist())
is.null(vector())
as.null(1:3)
length(NULL)

### Other special values ###

# Missing values
NA # meaning not applicable or not available
class(NA)
typeof(NA)
vec <- c(1, NA, 4, 6, 10)
is.na(vec)
10 + NA
sqrt(NA) # sqrt(n) function calculates the square root of n (it is equivalent to "n^(1/2)")
sum(1:3) # this is the same as "1 + 2 + 3"
sum(c(1, 2, 3, NA)) # this is the same as "1 + 2 + 3 + NA"
sum(c(1, 2, 3, NA), na.rm = T) # this remove the NA value(s) before doing the sum of all values (indeed "na.rm" means "NA remove")
na.omit(c(1, 2, 3, NA))
FALSE & NA # special case, due to every state of NA matched with "FALSE &" will produce FALSE
TRUE | NA # special case, due to every state of NA matched with "TRUE |" will produce TRUE
length(NA)

# Not a number
NaN
class(NaN)
typeof(NaN)
0/0
0/0 == NaN
is.nan(0/0)
is.na(0/0)
length(NaN)

# Infinite
Inf
-Inf
class(Inf)
typeof(-Inf)
1/0
-1/0
length(Inf)

### Installing and loading packages ###
# always remember, you install a package once but load it every session (and the functions are install.package() and library(), respectively)

install.package("swirl")
library(swirl)
rm(list = ls())
swirl()

install.package("vegan")
library(vegan)
vignette(package = "vegan")
vignette("intro-vegan")
vignette("diversity-vegan")

# https://rstudio.github.io/learnr/examples.html
# https://rstudio.cloud/learn/primers
# https://www.dataquest.io/ here you can sign up for free and access the basic tutorials (advanced tutorials require paying, but the most basic are free! hence, it could be helpful to review)
# https://intro2r.com/ nice free book introducing R programming made by biostatisticians and ecoligists

### Pipes ###

# rather than putting one function call inside the other, or storing intermediate variables/objects, you can "pipe" something into a function and then "pipe" its output inside another function, and so on

# below an example showing you the logic of a pipe

x <- 42

log(x) # calculate the logarithm of x (by default it's the natural logarithm, but you can do it otherwise by specifying other arguments or using related functions, see ?log)

x |> log() # this is the same as the above command, although it is not much useful this way, it becomes handy when you have 2 or more passages you like to do in just one go, see the below example

# now let's calculate the logarithm of x and then the square root of the logarithm

sqrt(log(x)) # this is one way and it's achieved by nesting functions

log_x <- log(x) # this is another way which requires to (at least temporarily) store a variable and reused it in a second line of code, it's more readable than the above but uses more memory and typing
sqrt(log_x)

x |> log() |> sqrt() # this is the same, doesn't require storing intermediates and it's more readable than nesting functions as we can read the actual order of the functions from left to right rather than from the inside to the outside




# LESSON 7

### Writing functions ###

add_two <- function(x) {
	x_plus_two <- x + 2
	return(x_plus_two)
}
	
add_two(10)

cheer_me <- function(your_name) {
	cheer_string <- paste("Hello", your_name, sep = " ")
	print(cheer_string)
}

cheer_me("Piero")


# more on creating your functions at https://swcarpentry.github.io/r-novice-inflammation/02-func-R/

### Build your first R projects ###
# you can check https://r4ds.had.co.nz/workflow-projects.html
# create a project folder with a known position and inside create a subfolder called "data", inside of data you should put "dune.txt" and "dune_env.csv", beware that to correctly import the two datasets with the following lines the names of the files must be exactly "dune.txt" and "dune_env.csv"

### Import data ###

dune <- read.table("data/dune.txt")
dune.env <- read.csv("data/dune_env.csv")

# you can find these same datasets in vegan
# library(vegan)
# data(dune)
# data(dune.env)
# ?dune

### Explore data ###

?dune
str(dune) # also dim(dune)
str(dune.env) # also dim(dune.env)

# note that none of the columns within dune.env are factors, but they should (you can check it by "?dune" after calling the vegan package -> "library(vegan)")! We can fix this by using the factor function

dune.env$Management <- factor(dune.env$Management)
dune.env$Moisture <- factor(dune.env$Moisture, ordered = T)
dune.env$Manure <- factor(dune.env$Manure, ordered = T)
dune.env$Use <- factor(dune.env$Use, ordered = T, levels = c("Hayfield", "Haypastu", "Pasture"))
str(dune.env)

### exercises at https://www.w3schools.com/r/r_exercises.asp




# LESSON 8

### For loops - An iteration technique ###

for (i in 1:10) {
  print(i)
}

sr <- vector()
for (i in 1:nrow(dune)) {
  sr[i] <- sum(dune[i, ] > 0)
}
sr

calculate_sr <- function(df) {
  sr <- vector()
  for (i in 1:nrow(df)) {
    sr[i] <- sum(df[i, ] > 0)
  }
  return(sr)
}

calculate_sr(dune)

### Other examples not shown in class ###

week <- c('Sunday',
          'Monday',
          'Tuesday',
          'Wednesday',
          'Thursday',
          'Friday',
          'Saturday')
for (day in week) {
	print(day)
}

for (i in 1:10) {
  cat(i, "\n")
} # which is the difference between print() and cat()?


num <- 0
for(i in 1:3) {
  num <- num + 5
} # a loop to multiply 5 by 3 (not actually helpful as (5*3) is faster by almost an order of magnitude)


simb <- "*"
for(i in 1:5) {
  cat(simb)
  cat("\n")
  simb <- paste(simb, "*", sep = "")
} # a loop to print a right triangle whose height is 5 lines and is made by "*"


tri <- function(height, symbol) {
  simb <- symbol
  for(i in 1:height) {
    cat(simb)
    cat("\n")
    simb <- paste(simb, symbol, sep = "")
  }
} # a function taking advantage of a for loop to print a right triangle, the height and the symbol are provided by the user as function arguments


tri <- function(height = 5, symbol = "-") {
  simb <- symbol
  for(i in 1:height) {
    cat(simb)
    cat("\n")
    simb <- paste(simb, symbol, sep = "")
  }
} # the same as above with default values (as all default values, the user can override them, by specifying different values inside the function call)

# more on loops at https://swcarpentry.github.io/r-novice-inflammation/15-supp-loops-in-depth/index.html
# also here https://www.geeksforgeeks.org/loops-in-r-for-while-repeat/



### Back to our analysis ###

# First let's reload and fix our datasets (from the following line to the third str(dune.env))

library(vegan)

dune <- read.table("data/dune.txt")
dune.env <- read.csv("data/dune_env.csv",
                     stringsAsFactors = TRUE)

str(dune)
str(dune.env)

?dune

dune.env$Moisture <- factor(dune.env$Moisture, ordered = T)
dune.env$Manure <- factor(dune.env$Manure, ordered = T)
str(dune.env)

dune.env$Use <- factor(dune.env$Use, ordered = T, levels = c("Hayfield", "Haypastu", "Pasture"))
str(dune.env)

# Let's restart from here

summary(dune.env)

hist(dune.env$A1)
hist(dune.env$A1,
     xlab = "Thickness of soil A1 horizon (cm)",
     main = "",
     breaks = 10)

counts <- table(dune.env$Management)
counts
barplot(counts, 
        xlab = "Management Type")
# also barplot(table(dune.env$Management))

counts <- table(dune.env$Manure, 
                dune.env$Management)
counts
barplot(counts,
        main = "Plot Distribution by Manure and Management",
        xlab = "Management Type",
        ylab = "Manure Quantity",
        legend = rownames(counts),
        xlim = c(0, 6))
prop.table(counts)
prop.table(counts, 1)
round(prop.table(counts, 1), 2)
round(prop.table(counts, 2), 2)

boxplot(dune.env$A1 ~ dune.env$Use,
        xlab = "Use Type",
        ylab = "Thickness of soil A1 horizon (cm)")

specnumber(dune)
dune.env$sr <- specnumber(dune)
boxplot(sr ~ Management,
	data = dune.env)


     
     
# LESSON 9
     
### Conditionals ###

x <- 100
if(x > 10) {
	print(paste(x, "is greater than 10"))
}

x <- 5
if(x > 10) {
	print(paste(x, "is greater than 10"))
} else {
	print(paste(x, "is less than 10"))
}

is_odd <- function(x) {
	if(x %% 2 == 1) {
		print(TRUE)
	} else {
		print(FALSE)
	}
}

### Resuming from last lesson ###

library(vegan)

data("dune")
data("dune.env")

sr <- specnumber(dune)

dune.env$sr <- sr

boxplot(dune.env$sr ~ dune.env$Management)

boxplot(sr ~ Management,
	data = dune.env,
	xlab = "Use Type",
        ylab = "Thickness of soil A1 horizon (cm)")

plot(sr ~ A1,
     data = dune.env,
     xlab = "Thickness of soil A1 horizon (cm)",
     ylab = "Species richness")


### Export graphs ###

# Beware we're goind to export outputs to a new subfolder called "outputs"

dir.create("outputs") # in this way you can create your "outputs" subfolder inside your project folder directly in R, otherwise you can navigate to your project folder outside of R/RStudio e and manually create the folder

# you can erase the folder in R as well by running the following command: unlink("outputs", recursive = TRUE)

plot(sr ~ A1,
     data = dune.env,
     xlab = "Thickness of soil A1 horizon (cm)",
     ylab = "Species richness")

png("outputs/sr_vs_a1.png", width = 350, height = 350)
plot(sr ~ A1,
     data = dune.env,
     xlab = "Thickness of soil A1 horizon (cm)",
     ylab = "Species richness")
dev.off()


### Export data ###

dune_env_head <- head(dune.env)
write.table(dune_env_head,
            "outputs/dune_env_head.txt")

dune_complete <- cbind(dune.env,
                     dune)
write.csv(dune_complete, "outputs/dune_complete.csv", row.names = F)


### Transform the community matrix (with abundances now) into presence absence ###

dune_pa <- decostand(dune,
		      method = "pa") # note that decostand is a function from vegan, hence you need to load vegan to use it
dune_pa


### Association measures ###

specnumber(dune, MARGIN = 2) # MARGIN = 2 enables us to calculate species frequencies

species_by_use <- aggregate(dune_pa[c("Scorautu", "Trifrepe")],
          by = list(dune.env$Use),
          FUN = "sum") # here we calculate the frequencies of the two most common species across different Use categories

chisq.test(species_by_use[, -1]) # here we test if the two species are indipendently distributed across different Use categories. The null hypothesis is that there are no difference, as the p-value much larger than .01 or .05 (actually, it's almost 1!), we can't refuse the null hypothesis

install.packages("cramer")

library(cramer)

cramer.test(species_by_use[, 2],
	    [, 3])
	    
cor.test(dune.env$sr,
	 dune.env$A1)	    
	    
### How to export graphs from R http://www.sthda.com/english/wiki/creating-and-saving-graphs-r-base-graphs https://www.datamentor.io/r-programming/saving-plot/ https://intro2r.com/export-plots.html
### How to open csv in Excel https://www.copytrans.net/support/how-to-open-a-csv-file-in-excel/



     
# LESSON 10    
# 10/12/2022

library(vegan)
data(dune)
data(dune.env)

dune.env$sr <- specnumber(dune)

mat <- matrix(c(2, 9, 8, 1), ncol=2)
colnames(mat) <- c("sp1", "sp2")
rownames(mat) <- c("habitat1", "habitat2")
mat


# chi-square test
chisq.test(mat)
# p-value = 0,007001, very small non significant test for  alpha 5% or 1% -> two spp intependent
# if high p-value, grater than alpha (0.05 or 0.01), test significant and spp not independent

par(mfrow = c(1,2))
hist(dune.env$sr[dune.env$Management == "HF"])
hist(dune.env$sr[dune.env$Management != "HF"])
dev.off()


# t-test
# test if pop are different for a certain measurament
t.test(dune.env$sr[dune.env$Management == "HF"],
       dune.env$sr[dune.env$Management != "HF"],
       alternative = "greater"
       )
# p-value = 0.0001202 -> difference bw two management type is a associated with a diffeernce in sr
# you cannot say that management type is driving changes in sr -> there could be other parameteres
# you can just say the two things are associated


# correlation test
# we have 2 continuous variable, eg species richness (sr) correlated to Temperature? or to latitude?
# correlation can be negative or positive
plot(dune.env$A1,
     dune.env$sr)
cor.test(dune.env$A1,
         dune.env$sr,
         alternative = "less")
# cor.test has by depault alternative = "two.sided"
# p-value = 0.1782 -> not significant
# cor -0.2177732 -> negative non significant correlation


plot(mtcars$disp,
     mtcars$hp)
cor.test(mtcars$disp,
         mtcars$hp,
         alternative = "greater")
# p-value = 3.571e-08 = 3.571 * 10^(-8) -> very significant correlation
# 95 percent confidence interval
# cor 0.7909486 (stong significant positive correlation)


# correlation to test association bw two numerical variable for the same observation
# t-test to test differences in given values or if you have a continuous value and two groups, or tu test if pop > or < of given value


# regression model
mod <- lm(hp ~ disp, data = mtcars)
mod
# intercept + angular coefficient
summary(mod)
# residuals (that should be normally distributed) 
# + coefficients (intercept and disp) 
# + Pr(>|t|) which is p-value -> stars to resume significance
# + Adjusted R-squared

# plotting model
str(mod)
plot(mtcars$disp,
     mtcars$hp)
abline(a = mod$coefficients[1], 
       b = mod$coefficients[2]) # coefficients are intercept and slope
# closest the points from the line smallest R-squared

  
     
     
     
# LESSON 10
     
library(vegan)
data(dune)
data(dune.env)

dune.env$sr <- specnumber(dune)

### Association measures ###

mat <- matrix((c(2, 9, 8, 1)), ncol = 2) # here we create a matrix with frequencies of two species in two different habitats
colnames(mat) <- c("sp1", "sp2")
rownames(mat) <- c("habitat1", "habitat2")
mat
chisq.test(mat) # here we test if the two species are independently ditributed (p < .05 -> independent distribution)

# The same as above calculated on the two most frequent species of dune dataset, across the different "Use" categories

specnumber(dune, MARGIN = 2) # MARGIN = 2 enables us to calculate species frequencies

species_by_use <- aggregate(dune_pa[c("Scorautu", "Trifrepe")],
          by = list(dune.env$Use),
          FUN = "sum") # here we calculate the frequencies of the two most common species across different Use categories

chisq.test(species_by_use[, -1]) # here we test if the two species are indipendently distributed across different Use categories. 
# The null hypothesis is that there are no difference, as the p-value much larger than .01 or .05 (actually, it's almost 1!)
# we can't refuse the null hypothesis

install.packages("cramer")

library(cramer)

cramer.test(species_by_use[, 2],
	    [, 3])

### Two-sample t-test ###
	
t.test(dune.env$sr[dune.env$Management == "HF"],
       dune.env$sr[dune.env$Management != "HF"],
       alternative = "greater") # here we test the hypothesis that HF plots have higher species richness on average than the other plots
    
### Correlation test and linear regression ###
    
plot(dune.env$A1,
     dune.env$sr)
cor.test(dune.env$sr,
         dune.env$A1,
         alternative = "less") # here we test the hypothesis that species richness and soil thickness are negatively correlated	   

plot(mtcars$disp,
     mtcars$hp)
cor.test(mtcars$disp,
         mtcars$hp,
         alternative = "greater") # hypothesis -> positive correlation
mod <- lm(hp ~ disp, data = mtcars)
summary(mod)
str(mod, 1)
abline(mod$coefficients[1], mod$coefficients[2])

### Shapiro's normality test ###

hist(dune.env$sr)
shapiro.test(dune.env$sr)

hist(dune.env$A1)
shapiro.test(dune.env$A1)


     
     
     
# LESSON 11 -12     
     
######### BRYCE ##########
library(vegan)

# importing datasets
bryceveg <- read.csv("data/bryceveg.csv")
brycesite <- read.table("data/brycesite.txt", 
                        sep = " ", # using fill = T we eliminated the emmpty cell but everything shifted
                        header = T, # first row as column name
                        na.strings = c("NA", "")) # specifing the two types of missing values

View(brycesite)
View(bryceveg)

env_variable <- ncol(brycesite)
species <- ncol(bryceveg)
observation <- nrow(bryceveg)

str(brycesite)
str(bryceveg)

summary(brycesite)
# if you have NAs in summer means that we must manage it

index_na <- which(is.na(brycesite), arr.ind = T) # indices of missing values with coordinate
index_na <- index_na[, 1]

# remove NAs
brycesite <- na.omit(brycesite)# removing the entire line of brycesite and the same rows for bricesite
bryceveg <- bryceveg[-index_na, ]     

nrow(brycesite)
nrow(bryceveg)


brycesite$depth <- factor(brycesite$depth, 
                          levels = c("shallow", "deep"),
                          ordered = T)
unique(brycesite$depth) # shows possible values of that column

unique(brycesite$pos)
brycesite$pos <- factor(brycesite$pos, 
                        levels = c("bottom", "low_slope", "mid_slope", "up_slope", "ridge"),
                        ordered = T)

str(brycesite)

bryceveg <- decostand(bryceveg, method = "pa")
brycesite$sr <- specnumber(bryceveg) # MARGIN = 1 by default -> sum of the rows = species richness
# we stored the result as a new column in brycesite
specnumber(bryceveg, MARGIN = 2) # operation across the column, number of sites in which species appeard

sort(specnumber(bryceveg, MARGIN = 2))
sort(specnumber(bryceveg, MARGIN = 2), decreasing = T)
plot(sort(specnumber(bryceveg, MARGIN = 2), decreasing = T))

summary(brycesite$sr)

png("Figure1.png", res = 300, width = 4000, height = 2000)
hist(brycesite$sr, # sr distrubution
     main = "Species richness distribution",
     xlab = "Species richness per site")
dev.off()

barplot(table(brycesite$pos)) # cannot barplot directly pos bc it's a factor
barplot(table(brycesite$depth, brycesite$pos),
        legend = rownames(table(brycesite$depth, brycesite$pos)),
        xlim = c(0, 6))

boxplot(brycesite$sr ~ brycesite$pos,
        ylim = c(0, 30))

plot(brycesite$elev, brycesite$sr)
cor.test(brycesite$elev, brycesite$sr)
lm(sr ~ elev, data = brycesite)
summary(lm(sr ~ elev, data = brycesite))

     
     
     
     
     
     
     
     

