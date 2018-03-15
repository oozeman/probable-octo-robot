This R Shiny application uses Home Mortgage Disclosure Act (HMDA) Loans data available for public access from the Consumer Finance Protection Bureau (CFPB) website. The Home Mortgage Disclosure Act (HMDA) requires many financial institutions to maintain, report, and publicly disclose information about mortgages. These public data are important because they help show whether lenders are serving the housing needs of their communities; they give public officials information that helps them make decisions and policies; and they shed light on lending patterns that could be discriminatory.

The files listed below power the Shiny app.
* demo_data.csv
This excel sheet contains information on 3680 geographies at the State, metro area, county and census tract level of the property. Some sample statistics available in this file include:
 - Population
 - Total number of families
 - Percentage of Males/Females
 - Median Family Income, etc.
These metrics appear in the ‘Demographic tab’ of the Shiny app when the user selects the required geography / Metropolitan statistical area. (MSA)
* app.r
This master script powers the R-Shiny application. It creates a data-frame of demographics by manipulating the ‘demo_data.csv’ and calculates statistics according to the user’s input at the lender level. Comparisons can be drawn between two lenders based on the number of loans approved for certain race/ethnicities, involvement in low income & minority communities, etc.