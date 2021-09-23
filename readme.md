HW1
================
Sam Murray
9/22/2021

**Import libraries**
```{r library}
library(rvest)
library(dplyr)
```
**Read in html of faculty website from url**
```{r read-data}
html = read_html("https://guide.wisc.edu/faculty/")
html
```
**Extract paragraphs of text from html and split by each**
```{r parse-html}
names = html_nodes(html, 'p')
facultyList = strsplit(toString(names), "<p>")[[1]]

head(facultyList)
```
**Format each row into four variables and construct dataframe**
```{r fill-column-vectors}
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
```{r dataframe}
print(str(faculty_df))
head(faculty_df, n=10)
```

**Show counts of position titles for  all staff**
```{r counts}
faculty_df %>% 
  group_by(Position) %>% 
  tally() %>% 
  arrange(desc(n))
```
**Show counts of departments for  all staff**
```{r counts2}
faculty_df %>% 
  group_by(Department) %>% 
  tally() %>% 
  arrange(desc(n))
```