Minimal R shiny app demonstrating:

1. how to upload a CSV file into an R/shiny app
2. how to automatically populate list selectors based on column headers
3. how to use optional list selectors
4. how to populate a list selector with column names of numerical columns only
5. how to use an action button to trigger an event when you're ready to do so

---

Download and install:

* R
* RStudio
* Download the dummy csv file from this gist ( https://gist.github.com/psychemedia/9690079 )

In RStudio run:

`install.packages("shiny")`

then

`library(shiny)`

followed by

`runGist(9737637)`