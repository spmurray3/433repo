HW1 (Faculty list assignment)
================
Sam Murray
9/22/2021

**Import libraries**

``` r
library(rvest)
```

    ## Warning: package 'rvest' was built under R version 3.6.2

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

**Read in html of faculty website from url**

``` r
html = read_html("https://guide.wisc.edu/faculty/")
html
```

    ## {html_document}
    ## <html class="no-js" xml:lang="en" lang="en" dir="ltr">
    ## [1] <head>\n<script>(function(H){H.className=H.className.replace(/\\bno- ...
    ## [2] <body>\n\n\n<!-- Google Tag Manager (noscript) -->\n<noscript><ifram ...

**Extract paragraphs of text from html and split by each**

``` r
names = html_nodes(html, 'p')
facultyList = strsplit(toString(names), "<p>")[[1]]

head(facultyList)
```

    ## [1] "<p class=\"p1\"><span class=\"s1\">This is a list of instructors, which includes all tenure/tenure track faculty, as well as academic staff who were listed as an instructor on a course at UW-Madison in Fall 2020 and Spring 2021.  The list is refreshed annually in March.</span></p>, <p class=\"p2\"><span class=\"s2\"><a href=\"http://www.wisc.edu/directories/\">Contact information for all faculty and staff</a></span></p>, "
    ## [2] "ABBOTT,DAVID H.<br>Professor<br>Obstetrics &amp; Gynecology<br>PHD 1979 University of Edinburgh<br></p>, "                                                                                                                                                                                                                                                                                                                                
    ## [3] "ABD-ELSAYED,ALAA A<br>Assoc Professor (Chs)<br>Anesthesiology<br>MD 2000 University of Assiut<br></p>, "                                                                                                                                                                                                                                                                                                                                  
    ## [4] "ABDUALLAH,FAISAL<br>Professor<br>Art<br>PHD 2012 Royal College of Art<br></p>, "                                                                                                                                                                                                                                                                                                                                                          
    ## [5] "ABRAHAM,OLUFUNMILOLA<br>Assistant Professor<br>Pharmacy<br>PHD 2013 Univ of Wisconsin-Madison<br></p>, "                                                                                                                                                                                                                                                                                                                                  
    ## [6] "ABRAMS,SAMANTHA<br>Assoc Lecturer<br>Information School<br>MA 2017 Univ of Wisconsin-Madison<br></p>, "

**Format each row into four variables and construct dataframe**

``` r
# create column vectors to fill in loop
Name = c()
Position = c()
Department = c()
Degree_Information = c()

for (row in facultyList){
  
  # if faculty member is in mulitple departments change "&amp;" to just "&"
  if (grepl(c('&'), row, fixed = TRUE)){
    i = gsub("amp;","",row)
  }
  
  # split row to get each data value to save in each vector
  currentRowList = strsplit(row, "<br>")[[1]]
  
  # save values in column vectors
  Name = c(Name, currentRowList[1])
  Position = c(Position, currentRowList[2])
  Department = c(Department, currentRowList[3])
  Degree_Information = c(Degree_Information, currentRowList[4])
  
}

# create dataframe from column vectors and remove observations that aren't faculty members, then reset the index
faculty_df = data.frame(Name, Position, Department, Degree_Information)
faculty_df = faculty_df[-1,]
faculty_df = head(faculty_df,-1)
row.names(faculty_df) <- seq(1:length(row.names(faculty_df)))
```

**Final dataframe of UW-Madison faculty**

``` r
print(str(faculty_df))
```

    ## 'data.frame':    3789 obs. of  4 variables:
    ##  $ Name              : Factor w/ 3788 levels "<p class=\"p1\"><span class=\"s1\">This is a list of instructors, which includes all tenure/tenure track facult"| __truncated__,..: 3 4 5 6 7 8 9 10 11 12 ...
    ##  $ Position          : Factor w/ 75 levels "Adjunct Assoc Prof",..: 56 18 56 10 14 56 53 56 10 10 ...
    ##  $ Department        : Factor w/ 203 levels "#N/A","A.C. Nielsen Ctr For Mkt Rsch",..: 154 22 26 164 115 173 9 32 141 32 ...
    ##  $ Degree_Information: Factor w/ 2502 levels "</p>, ","AA 1983 University of London",..: 1134 583 2176 2233 442 1124 448 1263 2008 2444 ...
    ## NULL

``` r
head(faculty_df, n=10)
```

    ##                      Name              Position
    ## 1         ABBOTT,DAVID H.             Professor
    ## 2      ABD-ELSAYED,ALAA A Assoc Professor (Chs)
    ## 3        ABDUALLAH,FAISAL             Professor
    ## 4    ABRAHAM,OLUFUNMILOLA   Assistant Professor
    ## 5         ABRAMS,SAMANTHA        Assoc Lecturer
    ## 6            ABRAMSON,LYN             Professor
    ## 7           ACKER,LINDSAY              Lecturer
    ## 8         ACKERMAN,STEVEN             Professor
    ## 9  ADAMCZYK,PETER GABRIEL   Assistant Professor
    ## 10 ADAMES-CORRALIZA,ANGEL   Assistant Professor
    ##                            Department
    ## 1         Obstetrics &amp; Gynecology
    ## 2                      Anesthesiology
    ## 3                                 Art
    ## 4                            Pharmacy
    ## 5                  Information School
    ## 6                          Psychology
    ## 7              Accting &amp; Info Sys
    ## 8  Atmospheric &amp; Oceanic Sciences
    ## 9              Mechanical Engineering
    ## 10 Atmospheric &amp; Oceanic Sciences
    ##                        Degree_Information
    ## 1        PHD 1979 University of Edinburgh
    ## 2            MD 2000 University of Assiut
    ## 3           PHD 2012 Royal College of Art
    ## 4      PHD 2013 Univ of Wisconsin-Madison
    ## 5       MA 2017 Univ of Wisconsin-Madison
    ## 6     PHD 1978 University of Pennsylvania
    ## 7     MACC 2005 Univ of Wisconsin-Madison
    ## 8      PHD 1987 Colorado State University
    ## 9  PHD 2008 Univ of Michigan at Ann Arbor
    ## 10      PHD 2018 University of Washington

**Show counts of position titles for all staff**

``` r
faculty_df %>% 
  group_by(Position) %>% 
  tally() %>% 
  arrange(desc(n))
```

    ## # A tibble: 75 x 2
    ##    Position                n
    ##    <fct>               <int>
    ##  1 Professor            1137
    ##  2 Assistant Professor   538
    ##  3 Associate Professor   413
    ##  4 Lecturer              297
    ##  5 Faculty Associate     188
    ##  6 Assoc Lecturer        166
    ##  7 Senior Lecturer       122
    ##  8 Assoc Faculty Assoc   116
    ##  9 Clinical Instructor    95
    ## 10 Clinical Asst Prof     81
    ## # … with 65 more rows

**Show counts of departments for all staff**

``` r
faculty_df %>% 
  group_by(Department) %>% 
  tally() %>% 
  arrange(desc(n))
```

    ## # A tibble: 203 x 2
    ##    Department            n
    ##    <fct>             <int>
    ##  1 Law School          123
    ##  2 Nursing              92
    ##  3 Mathematics          90
    ##  4 English              83
    ##  5 Medicine             83
    ##  6 Social Work          79
    ##  7 Pharmacy             73
    ##  8 Computer Sciences    65
    ##  9 Chemistry            62
    ## 10 History              60
    ## # … with 193 more rows
