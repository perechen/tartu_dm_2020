
### 0. brief intro to R

# math operations
2+3
4-1
2*3
8/2
2^3
# defining variables
x = 2
x + 3
x
# character strings
chr = "this is a string"
chr
# functions 
my_vector = c(2,5,6) # combine values in a vector
mean(vector) # average function
seq(0, 100, by=10) # make a sequence of numbers
paste(chr, "and this will apear to the left", sep=" ") # paste strings together
# object indexing
my_vector[1] # access first element of "my_vector" variable
my_vector[c(1,3)] # access first & second element of "my_vector

my_table = data.frame(animals = c("cat", "cat", "dog"),
                      evaluation = c("good", "good", "very good!!"))

my_table
my_table$animals # use $ to access columns in a data frame
my_table$animals[1] # first element of column "animals"
my_table[1,] # access first row with all columns
my_table[1,2] # access value of first row and second column
my_table[,"evaluation"] # access columns by their names

### 1. install package "stylo"

install.packages("stylo")

### 2. load package in R
library(stylo)

### 3. Run stylo()!

stylo()

### 4. Reading output tables back to R

dist = read.table("distance_table_100mfw_0c.txt", sep=" ")
word = read.table("table_with_frequencies.txt", sep=" ")

### 5. Saving results to R directly

stylo_results = stylo()

summary(stylo_results) # check stylo object

### also check classify() and oppose()

?classify()
?oppose()


#### SOME DATASETS FOR INDIVIDUAL WORK

#From Computational Stylistics Group (Krakow): https://computationalstylistics.github.io/resources/
#From .txtLab (Quebec): https://txtlab.org/data-sets/ 
