## Ambulance Quality Indicators - Shiny Apps <br/>
<!---#### UI for running the code with a click button interface
* Web checker: https://nhseiuecperformance.shinyapps.io/amb_checker_all_web/
* Folder checker: https://nhseiuecperformance.shinyapps.io/amb_checker_all_folder -->

##### The code performs 9 checks on the data:
* SummedFalse is “summed” columns for each trust compared against the England figure
* MeanedFalse is “meaned” columns for each trust compared against the England figure
* WeightedFalse is “weighted” columns for each trust compared against the England figure
* RegionSummedFalse is the same as SummedFalse except only for regions where there is more than one trust in the region comparing the total of the trusts to the region
* RegionMeanedFalse is the same as MeanedFalse except only for regions where there is more than one trust in the region comparing the total of the trusts to the region
* RegionWeightedFalse is the same as WeightedFalse except only for regions where there is more than one trust in the region comparing the total of the trusts to the region
* SingleFalse is checking for regions with only one trust that the trust figures match the region – no calculations just matching trust against region for all columns
* OtherValFalse is a custom check for ensuring metrics which are meant to add up do or that numerators are not larger than denominators
* Dashes is checking for dashes in the data for that period

#### AmbSys - Specific columns
##### 3 csv files loaded as data source at the beginning of the code
* meanlookup.csv - columns which are checks as mean - contains metric, numerator, denominator
* summedlookup.csv - columns which are checks as a sum - contains single column (heading = x)
* weightedlookup.csv - columns which are checks as weighted average against england figure - contains numerator, denominator
##### any changes to the included fields for ambsys can be changed in these files before deploying back to shinyapps.io

#### AmbCO Columns
##### AmbCO derives the classification based on the column name:
* Column name contains 50, 90 or m - weighted average numerator
* Match the weighted average numerator against other column names and find the one which is the same but has n instead of 50/90/m and use that for the denominator
* All other columns are checked as a sum

#### Example Output from web app:
![AmbwebOutput](https://github.com/LPulle/NHSE-Analytics/blob/master/AmbWebOutput.jpg)
#### Example Output from folder app:
![AmbwebOutput](https://github.com/LPulle/NHSE-Analytics/blob/master/AmbFileOutput.jpg)
