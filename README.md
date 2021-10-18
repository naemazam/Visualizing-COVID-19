# Visualizing-COVID-19
Within months, COVID-19 went from an epidemic to a pandemic. From the first identified case in December 2019, how did the virus spread so fast and widely? In this free R project, we will visualize data from the early months of the coronavirus outbreak to see how this virus grew to be a global pandemic.

## Authors
I am Very Proud to Be a Part of this Project, Thanks Datacamp
- [@naemazam](https://www.github.com/naemazam)
- [@Richie Cotton](https://www.datacamp.com/instructors/richie)

## 1. From epidemic to pandemic
<p><img style="float: left; margin:5px 20px 5px 1px; width:40%" src="https://www.nps.gov/aboutus/news/images/CDC-coronavirus-image-23311-for-web.jpg?maxwidth=650&autorotate=false"></p>
<p>In December 2019, COVID-19 coronavirus was first identified in the Wuhan region of China. By March 11, 2020, the World Health Organization (WHO) categorized the COVID-19 outbreak as a pandemic. A lot has happened in the months in between with major outbreaks in Iran, South Korea, and Italy. </p>
<p>We know that COVID-19 spreads through respiratory droplets, such as through coughing, sneezing, or speaking. But, how quickly did the virus spread across the globe? And, can we see any effect from country-wide policies, like shutdowns and quarantines? </p>
<p>Fortunately, organizations around the world have been collecting data so that governments can monitor and learn from this pandemic. Notably, the Johns Hopkins University Center for Systems Science and Engineering created a <a href="https://github.com/RamiKrispin/coronavirus">publicly available data repository</a> to consolidate this data from sources like the WHO, the Centers for Disease Control and Prevention (CDC), and the Ministry of Health from multiple countries.</p>
<p>In this notebook, you will visualize COVID-19 data from the first several weeks of the outbreak to see at what point this virus became a global pandemic.</p>
<p><em>Please note that information and data regarding COVID-19 is frequently being updated. The data used in this project was pulled on March 17, 2020, and should not be considered to be the most up to date data available.</em></p>


```R
# Load the readr, ggplot2, and dplyr packages
library(readr)
library(ggplot2)
library(dplyr)

# Read datasets/confirmed_cases_worldwide.csv into confirmed_cases_worldwide
confirmed_cases_worldwide <- read_csv("datasets/confirmed_cases_worldwide.csv")

# Print out confirmed_cases_worldwide
confirmed_cases_worldwide
```

    
    Attaching package: ‘dplyr’
    
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    
    
    [36m──[39m [1m[1mColumn specification[1m[22m [36m────────────────────────────────────────────────────────[39m
    cols(
      date = [34mcol_date(format = "")[39m,
      cum_cases = [32mcol_double()[39m
    )
    
    



<table class="dataframe">
<caption>A spec_tbl_df: 56 × 2</caption>
<thead>
	<tr><th scope=col>date</th><th scope=col>cum_cases</th></tr>
	<tr><th scope=col>&lt;date&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>2020-01-22</td><td>   555</td></tr>
	<tr><td>2020-01-23</td><td>   653</td></tr>
	<tr><td>2020-01-24</td><td>   941</td></tr>
	<tr><td>2020-01-25</td><td>  1434</td></tr>
	<tr><td>2020-01-26</td><td>  2118</td></tr>
	<tr><td>2020-01-27</td><td>  2927</td></tr>
	<tr><td>2020-01-28</td><td>  5578</td></tr>
	<tr><td>2020-01-29</td><td>  6166</td></tr>
	<tr><td>2020-01-30</td><td>  8234</td></tr>
	<tr><td>2020-01-31</td><td>  9927</td></tr>
	<tr><td>2020-02-01</td><td> 12038</td></tr>
	<tr><td>2020-02-02</td><td> 16787</td></tr>
	<tr><td>2020-02-03</td><td> 19881</td></tr>
	<tr><td>2020-02-04</td><td> 23892</td></tr>
	<tr><td>2020-02-05</td><td> 27635</td></tr>
	<tr><td>2020-02-06</td><td> 30817</td></tr>
	<tr><td>2020-02-07</td><td> 34391</td></tr>
	<tr><td>2020-02-08</td><td> 37120</td></tr>
	<tr><td>2020-02-09</td><td> 40150</td></tr>
	<tr><td>2020-02-10</td><td> 42762</td></tr>
	<tr><td>2020-02-11</td><td> 44802</td></tr>
	<tr><td>2020-02-12</td><td> 45221</td></tr>
	<tr><td>2020-02-13</td><td> 60368</td></tr>
	<tr><td>2020-02-14</td><td> 66885</td></tr>
	<tr><td>2020-02-15</td><td> 69030</td></tr>
	<tr><td>2020-02-16</td><td> 71224</td></tr>
	<tr><td>2020-02-17</td><td> 73258</td></tr>
	<tr><td>2020-02-18</td><td> 75136</td></tr>
	<tr><td>2020-02-19</td><td> 75639</td></tr>
	<tr><td>2020-02-20</td><td> 76197</td></tr>
	<tr><td>2020-02-21</td><td> 76823</td></tr>
	<tr><td>2020-02-22</td><td> 78579</td></tr>
	<tr><td>2020-02-23</td><td> 78965</td></tr>
	<tr><td>2020-02-24</td><td> 79568</td></tr>
	<tr><td>2020-02-25</td><td> 80413</td></tr>
	<tr><td>2020-02-26</td><td> 81395</td></tr>
	<tr><td>2020-02-27</td><td> 82754</td></tr>
	<tr><td>2020-02-28</td><td> 84120</td></tr>
	<tr><td>2020-02-29</td><td> 86011</td></tr>
	<tr><td>2020-03-01</td><td> 88369</td></tr>
	<tr><td>2020-03-02</td><td> 90306</td></tr>
	<tr><td>2020-03-03</td><td> 92840</td></tr>
	<tr><td>2020-03-04</td><td> 95120</td></tr>
	<tr><td>2020-03-05</td><td> 97882</td></tr>
	<tr><td>2020-03-06</td><td>101784</td></tr>
	<tr><td>2020-03-07</td><td>105821</td></tr>
	<tr><td>2020-03-08</td><td>109795</td></tr>
	<tr><td>2020-03-09</td><td>113561</td></tr>
	<tr><td>2020-03-10</td><td>118592</td></tr>
	<tr><td>2020-03-11</td><td>125865</td></tr>
	<tr><td>2020-03-12</td><td>128343</td></tr>
	<tr><td>2020-03-13</td><td>145193</td></tr>
	<tr><td>2020-03-14</td><td>156097</td></tr>
	<tr><td>2020-03-15</td><td>167449</td></tr>
	<tr><td>2020-03-16</td><td>181531</td></tr>
	<tr><td>2020-03-17</td><td>197146</td></tr>
</tbody>
</table>




```R
library(testthat) 
library(IRkernel.testthat)

soln_confirmed_cases_worldwide <- read_csv("datasets/confirmed_cases_worldwide.csv")

run_tests({
    test_that("readr is loaded", {
        expect_true(
            "readr" %in% .packages(), 
            info = "Did you load the `readr` package?"
        )
    })
    test_that("ggplot2 is loaded", {
        expect_true(
            "ggplot2" %in% .packages(), 
            info = "Did you load the `ggplot2` package?"
        )
    })
    test_that("dplyr is loaded", {
        expect_true(
            "dplyr" %in% .packages(), 
            info = "Did you load the `dplyr` package?"
        )
    })
    
    test_that("confirmed_cases_worldwide is a data.frame", {
        expect_s3_class(
            confirmed_cases_worldwide,
            "data.frame",
        )
    })
    test_that("confirmed_cases_worldwide has the correct column", {
        expect_identical(
            colnames(confirmed_cases_worldwide),
            colnames(soln_confirmed_cases_worldwide), 
            info = "The column names of the `confirmed_cases_worldwide` data frame do not correspond with the ones in the CSV file: `\"datasets/confirmed_cases_worldwide.csv\"`."
        ) 
    })
    test_that("has the correct data", {
        expect_equal(
            confirmed_cases_worldwide,
            soln_confirmed_cases_worldwide, 
            info = "The data of the `confirmed_cases_worldwide` data frame do not correspond with data in the CSV file: \"datasets/confirmed_cases_worldwide.csv\"."
        )
    })
})
```

    
    Attaching package: ‘testthat’
    
    
    The following object is masked from ‘package:dplyr’:
    
        matches
    
    
    
    [36m──[39m [1m[1mColumn specification[1m[22m [36m────────────────────────────────────────────────────────[39m
    cols(
      date = [34mcol_date(format = "")[39m,
      cum_cases = [32mcol_double()[39m
    )
    
    







    6/6 tests passed


## 2. Confirmed cases throughout the world
<p>The table above shows the cumulative confirmed cases of COVID-19 worldwide by date. Just reading numbers in a table makes it hard to get a sense of the scale and growth of the outbreak. Let's draw a line plot to visualize the confirmed cases worldwide.</p>


```R
# Draw a line plot of cumulative cases vs. date
# Label the y-axis
ggplot(confirmed_cases_worldwide, aes(date, cum_cases)) +
  geom_line() +
  ylab("Cumulative confirmed cases")
```


![png](output_4_0.png)



```R
run_tests({
    plot <- last_plot()
    test_that("the plot is created", {
        expect_false(
            is.null(plot),
            info = "Could not find a plot created with `ggplot()`."
        )
    })
    test_that("the plot uses the correct data", {
        expect_equal(
            plot$data,
            confirmed_cases_worldwide,
            info = "The dataset used in the last plot is not `confirmed_cases_worldwide`."
        )
    })
    test_that("the plot uses the correct x aesthetic", {
        expect_equal(
            quo_name(plot$mapping$x),
            "date",
            info = "The x aesthetic used in the last plot is not `date`."
        )
    })
    test_that("the plot uses the correct y aesthetic", {
        expect_equal(
            quo_name(plot$mapping$y),
            "cum_cases",
            info = "The y aesthetic used in the last plot is not `cum_cases`."
        )
    })
    test_that("the plot uses the correct geom", {
        expect_true(
            "GeomLine" %in% class(plot$layers[[1]]$geom),
            info = "The geom used in the last plot is not `geom_line()`."
        )
    })
    test_that("the plot uses the correct y label", {
        expect_true(
            grepl("[Cc]umulative\\s+[Cc]onfirmed\\s+[Cc]ases", plot$labels$y),
            info = "The y label used in the last plot is not `\"Cumulative confirmed cases\"`."
        )
    })
})
```






    6/6 tests passed


## 3. China compared to the rest of the world
<p>The y-axis in that plot is pretty scary, with the total number of confirmed cases around the world approaching 200,000. Beyond that, some weird things are happening: there is an odd jump in mid February, then the rate of new cases slows down for a while, then speeds up again in March. We need to dig deeper to see what is happening.</p>
<p>Early on in the outbreak, the COVID-19 cases were primarily centered in China. Let's plot confirmed COVID-19 cases in China and the rest of the world separately to see if it gives us any insight.</p>
<p><em>We'll build on this plot in future tasks. One thing that will be important for the following tasks is that you add aesthetics within the line geometry of your ggplot, rather than making them global aesthetics.</em></p>


```R
# Read in datasets/confirmed_cases_china_vs_world.csv
confirmed_cases_china_vs_world <- read_csv("datasets/confirmed_cases_china_vs_world.csv")

# See the result
glimpse(confirmed_cases_china_vs_world)

# Draw a line plot of cumulative cases vs. date, colored by is_china
# Define aesthetics within the line geom
plt_cum_confirmed_cases_china_vs_world <- ggplot(confirmed_cases_china_vs_world) +
  geom_line(aes(date, cum_cases, color = is_china)) +
  ylab("Cumulative confirmed cases")

# See the plot
plt_cum_confirmed_cases_china_vs_world
```

    
    [36m──[39m [1m[1mColumn specification[1m[22m [36m────────────────────────────────────────────────────────[39m
    cols(
      is_china = [31mcol_character()[39m,
      date = [34mcol_date(format = "")[39m,
      cases = [32mcol_double()[39m,
      cum_cases = [32mcol_double()[39m
    )
    
    


    Rows: 112
    Columns: 4
    $ is_china  [3m[90m<chr>[39m[23m "China", "China", "China", "China", "China", "China", "China…
    $ date      [3m[90m<date>[39m[23m 2020-01-22, 2020-01-23, 2020-01-24, 2020-01-25, 2020-01-26,…
    $ cases     [3m[90m<dbl>[39m[23m 548, 95, 277, 486, 669, 802, 2632, 578, 2054, 1661, 2089, 47…
    $ cum_cases [3m[90m<dbl>[39m[23m 548, 643, 920, 1406, 2075, 2877, 5509, 6087, 8141, 9802, 118…



![png](output_7_2.png)



```R
soln_confirmed_cases_china_vs_world <- read_csv("datasets/confirmed_cases_china_vs_world.csv")

run_tests({
    test_that("confirmed_cases_china_vs_world is a data.frame", {
        expect_s3_class(
            confirmed_cases_china_vs_world,
            "data.frame"
        )
    })
    test_that("confirmed_cases_china_vs_world has the correct column names", {
        expect_identical(
            colnames(confirmed_cases_china_vs_world),
            colnames(soln_confirmed_cases_china_vs_world), 
            info = "The column names of the `confirmed_cases_china_vs_world` data frame do not correspond with the ones in the CSV file: `\"datasets/confirmed_cases_china_vs_world.csv\"`."
        ) 
    })
    test_that("confirmed_cases_china_vs_world has the correct data", {
        expect_equal(
            confirmed_cases_china_vs_world,
            soln_confirmed_cases_china_vs_world, 
            info = "The data of the `confirmed_cases_china_vs_world` data frame do not correspond with data in the CSV file: \"datasets/confirmed_cases_china_vs_world.csv\"."
        )
    })
    # NOTE: glimpse is not tested. Can this be done?
    test_that("plt_cum_confirmed_cases_china_vs_world is not NULL", {
        expect_false(
            is.null(plt_cum_confirmed_cases_china_vs_world),
            info = "`plt_cum_confirmed_cases_china_vs_world` is NULL."
        )
    })
    test_that("plt_cum_confirmed_cases_china_vs_world is a plot", {
        expect_true(
            "ggplot" %in% class(plt_cum_confirmed_cases_china_vs_world),
            info = "`plt_cum_confirmed_cases_china_vs_world` is not a `ggplot()` object."
        )
    })
    test_that("plt_cum_confirmed_cases_china_vs_world uses the correct data", {
        expect_equal(
            plt_cum_confirmed_cases_china_vs_world$data,
            confirmed_cases_china_vs_world,
            info = "The dataset used in `plt_cum_confirmed_cases_china_vs_world` is not `confirmed_cases_china_vs_world`."
        )
    })
    layer <- plt_cum_confirmed_cases_china_vs_world$layers[[1]]
    test_that("plt_cum_confirmed_cases_china_vs_world uses uses the correct geom", {
        expect_false(
            is.null(layer),
            info = "The geom used in `plt_cum_confirmed_cases_china_vs_world` is not `geom_line()`."
        )
    })
    test_that("plt_cum_confirmed_cases_china_vs_world uses uses the correct geom", {
        expect_true(
            "GeomLine" %in% class(layer$geom),
            info = "The geom used in `plt_cum_confirmed_cases_china_vs_world` is not `geom_line()`."
        )
    })
    test_that("plt_cum_confirmed_cases_china_vs_world uses uses the correct x aesthetic", {
        expect_equal(
            quo_name(layer$mapping$x),
            "date",
            info = "The x aesthetic used in `plt_cum_confirmed_cases_china_vs_world` is not `date`."
        )
    })
    test_that("plt_cum_confirmed_cases_china_vs_world uses uses the correct y aesthetic", {
        expect_equal(
            quo_name(layer$mapping$y),
            "cum_cases",
            info = "The y aesthetic used in `plt_cum_confirmed_cases_china_vs_world` is not `cum_cases`."
        )
    })
    test_that("plt_cum_confirmed_cases_china_vs_world uses uses the correct color aesthetic", {
        expect_equal(
            quo_name(layer$mapping$colour),
            "is_china",
            info = "The color aesthetic used in `plt_cum_confirmed_cases_china_vs_world` is not `is_china`."
        )
    })
})
```

    
    [36m──[39m [1m[1mColumn specification[1m[22m [36m────────────────────────────────────────────────────────[39m
    cols(
      is_china = [31mcol_character()[39m,
      date = [34mcol_date(format = "")[39m,
      cases = [32mcol_double()[39m,
      cum_cases = [32mcol_double()[39m
    )
    
    







    11/11 tests passed


## 4. Let's annotate!
<p>Wow! The two lines have very different shapes. In February, the majority of cases were in China. That changed in March when it really became a global outbreak: around March 14, the total number of cases outside China overtook the cases inside China. This was days after the WHO declared a pandemic.</p>
<p>There were a couple of other landmark events that happened during the outbreak. For example, the huge jump in the China line on February 13, 2020 wasn't just a bad day regarding the outbreak; China changed the way it reported figures on that day (CT scans were accepted as evidence for COVID-19, rather than only lab tests).</p>
<p>By annotating events like this, we can better interpret changes in the plot.</p>


```R
who_events <- tribble(
  ~ date, ~ event,
  "2020-01-30", "Global health\nemergency declared",
  "2020-03-11", "Pandemic\ndeclared",
  "2020-02-13", "China reporting\nchange"
) %>%
  mutate(date = as.Date(date))

# Using who_events, add vertical dashed lines with an xintercept at date
# and text at date, labeled by event, and at 100000 on the y-axis
plt_cum_confirmed_cases_china_vs_world +
  geom_vline(aes(xintercept = date), data = who_events, linetype = "dashed") +
  geom_text(aes(date, label = event), data = who_events, y = 1e5)
```


![png](output_10_0.png)



```R
run_tests({
    plot <- last_plot()
    test_that("the plot got created", {
        expect_false(
            is.null(plot),
            info = "Could not find a plot created with `ggplot()`."
        )
    })
    layer1 <- plot$layers[[2]]
    layer2 <- plot$layers[[3]]
    test_that("the plot has both geoms", {
        expect_false(
            is.null(layer1) || is.null(layer2),
            info = "Could not fin `geom_vline()` and `geom_text()` in your last plot."
        )
    })
    test_that("the plot has both geoms", {
        expect_true(
            "GeomVline" %in% class(layer1$geom) && "GeomText" %in% class(layer2$geom) ||
            "GeomText" %in% class(layer1$geom) && "GeomVline" %in% class(layer2$geom),
            info = "Could not fin `geom_vline()` and `geom_text()` in your last plot."
        )
    })
    if ("GeomVline" %in% class(layer1$geom)) {
        vline <- layer1
        text <- layer2
    } else {
        vline <- layer2
        text <- layer1
    }
    test_that("the plot uses the correct data", {
        expect_equal(
            vline$data,
            who_events,
            info = "The dataset used in the `geom_vline()` is not `who_events`."
        )
    })
    test_that("the geom uses the correct xintercept aesthetic", {
        expect_equal(
            quo_name(vline$mapping$xintercept),
            "date",
            info = "The xintercept aesthetic used in the `geom_vline()` is not `date`."
        )
    })
    test_that("the geom uses the correct lintype parameter", {
        expect_equal(
            vline$aes_params$linetype,
            "dashed",
            info = "The linetype parameter used in the `geom_vline()` is not `\"dashed\"`."
        )
    })
    test_that("the geom uses the correct data", {
        expect_equal(
            text$data,
            who_events,
            info = "The dataset used in the `geom_text()` is not `who_events`."
        )
    })
    test_that("the geom uses the correct x aesthetic", {
        expect_equal(
            quo_name(text$mapping$x),
            "date",
            info = "The x aesthetic used in the `geom_text()` is not `date`."
        )
    })
    test_that("the geom uses the correct label aesthetic", {
        expect_equal(
            quo_name(text$mapping$label),
            "event",
            info = "The label aesthetic used in the `geom_text()` is not `event`."
        )
    })
    if(!is.null(text$aes_params$y)) {
        test_that("the geom uses the correct y parameter", {
            expect_equal(
                text$aes_params$y,
                100000
            )
        })
    } else if (!is.null(quo_name(text$mapping$y))) {
        test_that("the geom uses the correct y parameter", {
            expect_equal(
                quo_name(text$mapping$y),
                '1e+05'
            )
        })
    }
})
```






    10/10 tests passed


## 5. Adding a trend line to China
<p>When trying to assess how big future problems are going to be, we need a measure of how fast the number of cases is growing. A good starting point is to see if the cases are growing faster or slower than linearly.</p>
<p>There is a clear surge of cases around February 13, 2020, with the reporting change in China. However, a couple of days after, the growth of cases in China slows down. How can we describe COVID-19's growth in China after February 15, 2020?</p>


```R
# Filter for China, from Feb 15
china_after_feb15 <- confirmed_cases_china_vs_world %>%
  filter(is_china == "China", date >= "2020-02-15")

# Using china_after_feb15, draw a line plot cum_cases vs. date
# Add a smooth trend line using linear regression, no error bars
ggplot(china_after_feb15, aes(date, cum_cases)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Cumulative confirmed cases")
```

    `geom_smooth()` using formula 'y ~ x'
    



![png](output_13_1.png)



```R
run_tests({
    test_that("the data is filtered correctly", {
        soln_china_after_feb15 <- confirmed_cases_china_vs_world %>%
          filter(is_china == "China", date >= "2020-02-15")
        expect_equivalent(
            soln_china_after_feb15,
            china_after_feb15,
            info = "`china_after_feb15` has not been filtered correctly."
        )
    })
    plot <- last_plot()
    test_that("the plot is created", {
        expect_false(
            is.null(plot),
            info = "Could not find a plot created with `ggplot()`."
        )
    })
    test_that("the plot uses the correct data", {
        expect_equal(
            plot$data,
            china_after_feb15,
            info = "The dataset used in the last plot is not `soln_china_after_feb15`."
        )
    })
    test_that("the plot uses the correct x aesthetic", {
        expect_equal(
            quo_name(plot$mapping$x),
            "date",
            info = "The x aesthetic used in the last plot is not `date`."
        )
    })
    test_that("the plot uses the correct y aesthetic", {
        expect_equal(
            quo_name(plot$mapping$y),
            "cum_cases",
            info = "The y aesthetic used in the last plot is not `cum_cases`."
        )
    })
    layer1 <- plot$layers[[1]]
    layer2 <- plot$layers[[2]]
    test_that("the plot has the correct geoms", {
        expect_false(
            is.null(layer1) || is.null(layer2),
            info = "Could not fin `geom_line()` and `geom_smooth()` in your last plot."
        )
    })
    test_that("the plot has the correct geoms", {
        expect_true(
            "GeomLine" %in% class(layer1$geom) && "GeomSmooth" %in% class(layer2$geom) ||
            "GeomSmooth" %in% class(layer1$geom) && "GeomLine" %in% class(layer2$geom),
            info = "Could not fin `geom_line()` and `geom_smooth()` in your last plot."
        )
    })
    if ("GeomLine" %in% class(layer1$geom)) {
        line <- layer1
        smooth <- layer2
    } else {
        line <- layer2
        smooth <- layer1
    }
    test_that("the geom has the correct method parameter", {
        expect_equal(
            smooth$stat_params$method,
            "lm",
            info = "The method parameter used in the `geom_smooth()` is not `\"lm\"`."

        )
    })
    test_that("the geom has the correct se parameter", {
        expect_equal(
            smooth$stat_params$se,
            FALSE,
            info = "The se parameter used in the `geom_smooth()` is not `\"FALSE\"`."
        )
    })
})
```






    9/9 tests passed


## 6. And the rest of the world?
<p>From the plot above, the growth rate in China is slower than linear. That's great news because it indicates China has at least somewhat contained the virus in late February and early March.</p>
<p>How does the rest of the world compare to linear growth?</p>


```R
# Filter confirmed_cases_china_vs_world for not China
not_china <- confirmed_cases_china_vs_world %>%
  filter(is_china == "Not China")

# Using not_china, draw a line plot cum_cases vs. date
# Add a smooth trend line using linear regression, no error bars
plt_not_china_trend_lin <- ggplot(not_china, aes(date, cum_cases)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Cumulative confirmed cases")

# See the result
plt_not_china_trend_lin 
```

    `geom_smooth()` using formula 'y ~ x'
    



![png](output_16_1.png)



```R
run_tests({
    test_that("the data is filtered correctly", {
        soln_not_china <- confirmed_cases_china_vs_world %>%
          filter(is_china == "Not China")
        expect_equal(
            soln_not_china,
            not_china,
            info = "`not_china` has not been filtered correctly."
        )
    })
    plot <- last_plot()
    test_that("the plot is created", {
        expect_false(
            is.null(plot),
            info = "Could not find a plot created with `ggplot()`."
        )
    })
    test_that("the plot uses the correct data", {
        expect_equal(
            plot$data,
            not_china,
            info = "The dataset used in the last plot is not `not_china`."
        )
    })
    test_that("the plot uses the correct x aesthetic", {
        expect_equal(
            quo_name(plot$mapping$x),
            "date",
            info = "The x aesthetic used in the last plot is not `date`."
        )
    })
    test_that("the plot uses the correct y aesthetic", {
        expect_equal(
            quo_name(plot$mapping$y),
            "cum_cases",
            info = "The y aesthetic used in the last plot is not `cum_cases`."
        )
    })
    layer1 <- plot$layers[[1]]
    layer2 <- plot$layers[[2]]
    test_that("the plot uses the correct geoms", {
        expect_false(
            is.null(layer1) || is.null(layer2),
            info = "Could not fin `geom_line()` and `geom_smooth()` in your last plot."
        )
    })
    test_that("the plot uses the correct geoms", {
        expect_true(
            "GeomLine" %in% class(layer1$geom) && "GeomSmooth" %in% class(layer2$geom) ||
            "GeomSmooth" %in% class(layer1$geom) && "GeomLine" %in% class(layer2$geom),
            info = "Could not fin `geom_line()` and `geom_smooth()` in your last plot."
        )
    })
    if ("GeomLine" %in% class(layer1$geom)) {
        line <- layer1
        smooth <- layer2
    } else {
        line <- layer2
        smooth <- layer1
    }
    test_that("the geom uses the correct method parameter", {
        expect_equal(
            smooth$stat_params$method,
            "lm",
            info = "The method parameter used in the `geom_smooth()` is not `\"lm\"`."
        )
    })
    test_that("the geom uses the correct se parameter", {
        expect_equal(
            smooth$stat_params$se,
            FALSE,
            info = "The se parameter used in the `geom_smooth()` is not `\"FALSE\"`."
        )
    })
})
```






    9/9 tests passed


## 7. Adding a logarithmic scale
<p>From the plot above, we can see a straight line does not fit well at all, and the rest of the world is growing much faster than linearly. What if we added a logarithmic scale to the y-axis?</p>


```R
# Modify the plot to use a logarithmic scale on the y-axis
plt_not_china_trend_lin + 
  scale_y_log10()
```

    `geom_smooth()` using formula 'y ~ x'
    



![png](output_19_1.png)



```R
run_tests({
    plot <- last_plot()
    test_that("the plot is created", {
        expect_false(
            is.null(plot),
            info = "Could not find a plot created with `ggplot()`."
        )
    })
    scale <- plot$scales$get_scales(aes("y"))
    test_that("the plot has a scale", {
        expect_false(
            is.null(scale),
            info = "Could not find a scale in your last plot."
        )
    })
    test_that("the plot uses the correct scale", {
        expect_equal(
            scale$trans$name,
            "log-10",
            info = "Could not find a logarithmic y scale: `scale_y_log10()`."
        )
    })
})
```






    3/3 tests passed


## 8. Which countries outside of China have been hit hardest?
<p>With the logarithmic scale, we get a much closer fit to the data. From a data science point of view, a good fit is great news. Unfortunately, from a public health point of view, that means that cases of COVID-19 in the rest of the world are growing at an exponential rate, which is terrible news.</p>
<p>Not all countries are being affected by COVID-19 equally, and it would be helpful to know where in the world the problems are greatest. Let's find the countries outside of China with the most confirmed cases in our dataset.</p>


```R
# Run this to get the data for each country
confirmed_cases_by_country <- read_csv("datasets/confirmed_cases_by_country.csv")
glimpse(confirmed_cases_by_country)

# Group by country, summarize to calculate total cases, find the top 7
top_countries_by_total_cases <- confirmed_cases_by_country %>%
  group_by(country) %>%
  summarize(total_cases = max(cum_cases)) %>%
  top_n(7, total_cases)

# See the result
top_countries_by_total_cases
```

    
    [36m──[39m [1m[1mColumn specification[1m[22m [36m────────────────────────────────────────────────────────[39m
    cols(
      country = [31mcol_character()[39m,
      province = [31mcol_character()[39m,
      date = [34mcol_date(format = "")[39m,
      cases = [32mcol_double()[39m,
      cum_cases = [32mcol_double()[39m
    )
    
    


    Rows: 13,272
    Columns: 5
    $ country   [3m[90m<chr>[39m[23m "Afghanistan", "Albania", "Algeria", "Andorra", "Antigua and…
    $ province  [3m[90m<chr>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
    $ date      [3m[90m<date>[39m[23m 2020-01-22, 2020-01-22, 2020-01-22, 2020-01-22, 2020-01-22,…
    $ cases     [3m[90m<dbl>[39m[23m 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    $ cum_cases [3m[90m<dbl>[39m[23m 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …



<table class="dataframe">
<caption>A tibble: 7 × 2</caption>
<thead>
	<tr><th scope=col>country</th><th scope=col>total_cases</th></tr>
	<tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
	<tr><td>France      </td><td> 7699</td></tr>
	<tr><td>Germany     </td><td> 9257</td></tr>
	<tr><td>Iran        </td><td>16169</td></tr>
	<tr><td>Italy       </td><td>31506</td></tr>
	<tr><td>Korea, South</td><td> 8320</td></tr>
	<tr><td>Spain       </td><td>11748</td></tr>
	<tr><td>US          </td><td> 6421</td></tr>
</tbody>
</table>




```R
run_tests({
    test_that("the data is manipulated correctly", {
        soln_top_countries_by_total_cases <- confirmed_cases_by_country %>%
          group_by(country) %>%
          summarize(total_cases = max(cum_cases)) %>%
          top_n(7, total_cases)
        expect_equivalent(
            soln_top_countries_by_total_cases,
            top_countries_by_total_cases,
            info = "`top_countries_by_total_cases` has not been filtered correctly."
        )
    })
})
```






    1/1 tests passed


## 9. Plotting hardest hit countries as of Mid-March 2020
<p>Even though the outbreak was first identified in China, there is only one country from East Asia (South Korea) in the above table. Four of the listed countries (France, Germany, Italy, and Spain) are in Europe and share borders. To get more context, we can plot these countries' confirmed cases over time.</p>
<p>Finally, congratulations on getting to the last step! If you would like to continue making visualizations or find the hardest hit countries as of today, you can do your own analyses with the latest data available <a href="https://github.com/RamiKrispin/coronavirus">here</a>. </p>


```R
# Read in the dataset from datasets/confirmed_cases_top7_outside_china.csv
confirmed_cases_top7_outside_china <- read_csv("datasets/confirmed_cases_top7_outside_china.csv")

# Glimpse at the contents of confirmed_cases_top7_outside_china
glimpse(confirmed_cases_top7_outside_china)

# Using confirmed_cases_top7_outside_china, draw a line plot of
# cum_cases vs. date, colored by country
ggplot(confirmed_cases_top7_outside_china, aes(date, cum_cases, color = country)) +
  geom_line() +
  ylab("Cumulative confirmed cases")
```

    
    [36m──[39m [1m[1mColumn specification[1m[22m [36m────────────────────────────────────────────────────────[39m
    cols(
      country = [31mcol_character()[39m,
      date = [34mcol_date(format = "")[39m,
      cum_cases = [32mcol_double()[39m
    )
    
    


    Rows: 2,030
    Columns: 3
    $ country   [3m[90m<chr>[39m[23m "Germany", "Iran", "Italy", "Korea, South", "Spain", "US", "…
    $ date      [3m[90m<date>[39m[23m 2020-02-18, 2020-02-18, 2020-02-18, 2020-02-18, 2020-02-18,…
    $ cum_cases [3m[90m<dbl>[39m[23m 16, 0, 3, 31, 2, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,…



```R
soln_confirmed_cases_top7_outside_china <- read_csv("datasets/confirmed_cases_top7_outside_china.csv")

run_tests({
    test_that('confirmed_cases_top7_outside_china is a data.frame', {
        expect_s3_class(
            confirmed_cases_top7_outside_china,
            'data.frame'
        )
    })
    test_that('confirmed_cases_top7_outside_china had the correct column names', {
        expect_identical(
            colnames(confirmed_cases_top7_outside_china),
            colnames(soln_confirmed_cases_top7_outside_china), 
            info = "The column names of the `confirmed_cases_top7_outside_china` data frame do not correspond with the ones in the CSV file: `\"datasets/confirmed_cases_top7_outside_china.csv\"`."
        ) 
    })
    test_that('confirmed_cases_top7_outside_china had the correct data', {
        expect_equal(
            confirmed_cases_top7_outside_china,
            soln_confirmed_cases_top7_outside_china,
            info = "The data of the `confirmed_cases_top7_outside_china` data frame do not correspond with data in the CSV file: \"datasets/confirmed_cases_top7_outside_china.csv\"."
        )
    })
    # NOTE: glimpse is not tested. Can this be done?
    plot <- last_plot()
    test_that('the plot is created', {
        expect_false(
            is.null(plot),
            info = "Could not find a plot created with `ggplot()`."
        )
    })
    test_that('the plot uses the correct data', {
        expect_equal(
            plot$data,
            confirmed_cases_top7_outside_china,
            info = "The dataset used in the last plot is not `not_china`."
        )
    })
    line <- plot$layers[[1]]
    test_that('the plot uses the correct geom', {
        expect_false(
            is.null(line),
            info = "Could not fin `geom_line()` in your last plot."
        )
    })
    test_that('the plot uses the correct geom', {
        expect_true(
            'GeomLine' %in% class(line$geom),
            info = "Could not fin `geom_line()` in your last plot."
        )
    })
    mapping <- plot$mapping
    geom_mapping <- line$mapping
    test_that('the plot uses the correct x aesthetic', {
        expect_true(
            !is.null(mapping$x) && quo_name(mapping$x) == "date" ||
            !is.null(geom_mapping$x) && quo_name(geom_mapping$x) == "date",
            info = "The x aesthetic used in the last plot is not `date`."

        )
    })
    test_that('the plot uses the correct y aesthetic', {
        expect_true(
            !is.null(mapping$y) && quo_name(mapping$y) == "cum_cases" ||
            !is.null(geom_mapping$y) && quo_name(geom_mapping$y) == "cum_cases",
            info = "The y aesthetic used in the last plot is not `cum_cases`."
        )
    })
    test_that('the plot uses the correct color aesthetic', {
        expect_true(
            !is.null(mapping$colour) && quo_name(mapping$colour) == "country" ||
            !is.null(geom_mapping$colour) && quo_name(geom_mapping$colour) == "country",
            info = "The color aesthetic used in the last plot is not `country`."
        )
    })
})
```

## 🚀 About Me
I am Naem Azam. I'm Self-taught Python Programmer And an open-source enthusiast and maintainer.
- [@naemazam](https://www.github.com/naemazam)
[![portfolio](https://img.shields.io/badge/my_portfolio-000?style=for-the-badge&logo=ko-fi&logoColor=white)](http://naemazam.github.io/)
[![linkedin](https://img.shields.io/badge/linkedin-0A66C2?style=for-the-badge&logo=linkedin&logoColor=white)](https://www.linkedin.com/naem-azam)

  


## Contributing

Contributions are always welcome!

make `Pull Request` for ways to get started.

[![MIT License](https://img.shields.io/apm/l/atomic-design-ui.svg?)](https://github.com/tterb/atomic-design-ui/blob/master/LICENSEs)

