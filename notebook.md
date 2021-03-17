
## Introduction
<p><img src="https://assets.datacamp.com/production/project_1174/img/trendlines.jpg" alt="Image of two trendlines over time."></p>
<p>It’s important to stay informed about trends in programming languages and technologies. Knowing what languages are growing or shrinking can help you decide where to invest. </p>
<p>An excellent source to gain a better understanding of popular technologies is <a href="https://stackoverflow.com/">Stack Overflow</a>. Stack overflow is an online question-and-answer site for coding topics. By looking at the number of questions about each technology, you can get an idea of how many people are using it.</p>
<p>You'll be working with a dataset with one observation for each tag in each year. The dataset was downloaded from the <a href="https://data.stackexchange.com/">Stack Exchange Data Explorer</a>. Below you can find an overview of the data that is available to you:<br><br></p>
<div style="background-color: #efebe4; color: #05192d; text-align:left; vertical-align: middle; padding: 15px 25px 15px 25px; line-height: 1.6;">
    <div style="font-size:20px"><b>datasets/stack_overflow_data.csv</b></div>
<ul>
    <li><b>year:</b> The year the question was asked.</li>
    <li><b>tag:</b> A word or phrase that describes the topic of the question.</li>
    <li><b>number:</b> The number of questions with a certain tag in that year.</li>
    <li><b>year_total:</b> The total number of questions asked in that year.</li>
</ul>
    </div>
<p>From here on out, it will be your task to explore and manipulate the existing data until you are able to answer the questions described in the instructions panel. Feel free to add as many cells as necessary. Finally, remember that you are only tested on your answer, not on the methods you use to arrive at the answer!</p>
<p><em><strong>Note:</strong> If you haven't completed a DataCamp project before you should check out the <a href="https://projects.datacamp.com/projects/41">Intro to Projects</a> first to learn about the interface. In this project, you also need to know your way around data manipulation and visualization in the Tidyverse and it's recommended that you take a look at the course <a href="https://www.datacamp.com/courses/introduction-to-the-tidyverse">Introduction to the Tidyverse</a>.</em></p>


```R
# Use this cell to begin your analysis, and add as many as you would like!
# import packages
library(dplyr)
library(readr)
library(ggplot2)
# read data
df <- read_csv('datasets/stack_overflow_data.csv')

# take a look of first 5 rows
head(df)

# glimpse the df
glimpse(df)
```

    Parsed with column specification:
    cols(
      year = [32mcol_double()[39m,
      tag = [31mcol_character()[39m,
      number = [32mcol_double()[39m,
      year_total = [32mcol_double()[39m
    )



<table>
<caption>A tibble: 6 x 4</caption>
<thead>
	<tr><th scope=col>year</th><th scope=col>tag</th><th scope=col>number</th><th scope=col>year_total</th></tr>
	<tr><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>2008</td><td>treeview       </td><td>69</td><td>168541</td></tr>
	<tr><td>2008</td><td>scheduled-tasks</td><td>30</td><td>168541</td></tr>
	<tr><td>2008</td><td>specifications </td><td>21</td><td>168541</td></tr>
	<tr><td>2008</td><td>rendering      </td><td>35</td><td>168541</td></tr>
	<tr><td>2008</td><td>http-post      </td><td> 6</td><td>168541</td></tr>
	<tr><td>2008</td><td>static-assert  </td><td> 1</td><td>168541</td></tr>
</tbody>
</table>



    Rows: 420,066
    Columns: 4
    $ year       [3m[38;5;246m<dbl>[39m[23m 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 2008, 20...
    $ tag        [3m[38;5;246m<chr>[39m[23m "treeview", "scheduled-tasks", "specifications", "render...
    $ number     [3m[38;5;246m<dbl>[39m[23m 69, 30, 21, 35, 6, 1, 159, 10, 4, 20, 11, 5, 19, 2, 19, ...
    $ year_total [3m[38;5;246m<dbl>[39m[23m 168541, 168541, 168541, 168541, 168541, 168541, 168541, ...



```R
#####
# What fraction of the total number of questions asked in 2019 had the R tag?
#####

r_percentage <- df %>%
    # filter year in 2019 and tag = r
    filter(year == 2019, tag == 'r') %>%
    summarise(r_percentage = number/year_total*100)

r_percentage
```


<table>
<caption>A tibble: 1 x 1</caption>
<thead>
	<tr><th scope=col>r_percentage</th></tr>
	<tr><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>0.9656728</td></tr>
</tbody>
</table>




```R
#####
# What were the five most asked-about tags in the last 5 years (2015-2020)?
#####
top_5 <- df %>%
    # filter year >= 2015
    filter(year >= 2015) %>%
    # group by tag
    group_by(tag) %>%
    #total number been asked during 5 years
    summarise(total_number = sum(number)) %>%
    # arrange by number in descending order
    arrange(desc(total_number)) %>%
    # top 5
    top_n(5, total_number)

highest_tags <- top_5$tag

highest_tags
```


<ol class=list-inline>
	<li>'javascript'</li>
	<li>'python'</li>
	<li>'java'</li>
	<li>'android'</li>
	<li>'c#'</li>
</ol>




```R
# trend analysis: top 3 tag over since 2015
bar_chart <- df %>%
    filter(year >= 2015) %>%
    group_by(year) %>%
    arrange(year) %>%
    top_n(3, number) %>%
    ggplot(aes(x = year, y = number, fill = tag)) +
        geom_bar(stat = "identity", position = 'fill') +
        ylab('Presentage') +
        xlab('Year') +
        ggtitle('Top 3 Tag Weight Change During 2015 - 2020')

bar_chart
```


![png](output_4_0.png)



```R
line_chart <- df %>%
#    filter(year >= 2015) %>%
    group_by(year) %>%
    arrange(year) %>%
    top_n(3, number) %>%
    ggplot(aes(x = year, y = number, color = tag)) +
        geom_line() +
        ylab('Number of Questions') +
        xlab('Year') +
        ggtitle('Top 3 Tag Weight Change During 2015 - 2020')

line_chart
```


![png](output_5_0.png)



```R
# These packages need to be loaded in the first @tests cell
library(testthat) 
library(IRkernel.testthat)
library(stringr)

# One or more tests of the student's code
# The @solution should pass the tests
# The purpose of the tests is to try to catch common errors and
# to give the student a hint on how to resolve these errors

# There are two tests in this cell. The first one tests that the
# correct package was loaded. The second one tests that the
# correct data were read in.

#/Users/sarabillen/Documents/GitHub/projects-functions-for-food-price-forecasts-r-unguided

run_tests({
    
    # Test that dplyr was loaded correctly
    test_that("the dplyr package was loaded correctly", {
        expect_true("dplyr" %in% .packages(), info = "Did you load the `dplyr` package? This package will help you solve the most common data manipulation challenges.")
    })
    
    # Question 1: Test that they created the fraction variable
    test_that("the new fraction variable was calculated correctly", {
        expect_false(as.numeric(r_percentage) == 52249, 
                     info = "`r_percentage` contains the wrong values. Did you create a new variable to calculate the fraction?")
    })
    
    # Question 1: Test that they saved the answer in the correct format
    test_that("the end result is calculated in percentage format", {
        expect_false(round(as.numeric(r_percentage), 4) == 0.0097, 
                     info = "`r_percentage` contains the wrong values. Did you multiply by 100 to save the answer in percentage format?")
    })
    
    # Question 1: Test that the answer is correct
    test_that("the answer to question 1 is correct", {
        expect_true(round(as.numeric(r_percentage), 2) == 0.97, 
                     info = "`r_percentage` contains the wrong value.")
    })
    
    # Question 2: Test that they filtered for period 2015-2020
    test_that("the highest tags are calculated for the 2015-2020 period", {
        expect_false("php" %in% highest_tags, 
                    info = "`highest_tags` contains the wrong values. Did you filter for the period 2015-2020?")
    })
    
    # Question 2: Test that the result is a character vector
    test_that("the result is a character vector", {
        expect_true(is.character(highest_tags),
                    info = "Did you save your answer to Question 2 as a character vector?")
    })
    
    # Question 2: Test that they only took the top 5
    test_that("results are returned only for the top 5 tags", {
        expect_true(length(highest_tags) == 5, 
                    info = "Did you save the top 5 most popular tags?")
    })
    
    # Question 2: Test that the answer is correct
    test_that("the answer to question 2 is correct", {
        expect_equal(str_sort(highest_tags), str_sort(c("javascript", "python", "java", "android", "c#")), 
                    info = "Did you save the correct tags to `highest_tags`?")
    })
    
})
```






    8/8 tests passed

