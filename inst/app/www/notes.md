
## Notes

The raw data for this project originates from XXX. For this project, the
raw data was scaled and standardized several ways. First, each variable
was assigned to a category where a high value equates to a high
opportunity (“higher value is better”), or where a high value equates to
a low opportunity (“lower is better”). <br>

### Z-score

The z-score value represents the number of standard deviations x is from
the mean. The z-score calculation is:

Where “higher is better”: <br>
![z \\, score = \\frac{x - mean}{standard\\,deviation}\\\\](https://latex.codecogs.com/png.latex?z%20%5C%2C%20score%20%3D%20%5Cfrac%7Bx%20-%20mean%7D%7Bstandard%5C%2Cdeviation%7D%5C%5C "z \, score = \frac{x - mean}{standard\,deviation}\\")

Where “lower is better”: <br>
![z \\, score = \\frac{x - mean}{standard\\,deviation}\\times (-1)\\\\](https://latex.codecogs.com/png.latex?z%20%5C%2C%20score%20%3D%20%5Cfrac%7Bx%20-%20mean%7D%7Bstandard%5C%2Cdeviation%7D%5Ctimes%20%28-1%29%5C%5C "z \, score = \frac{x - mean}{standard\,deviation}\times (-1)\\")

<!-- ### Weights nominal -->
<!-- The weights nominal value represents where x falls nominally in the range of values, on a 0-10 scale. The weights nominal calculation is:  -->
<!-- Where "higher is better":  <br> -->
<!-- $weights \, nominal = \frac{x - minimum\,value}{maximum\,value - minimum\,value}\times 10\\$ -->
<!-- Where "lower is better":  <br> -->
<!-- $weights \, nominal = 10 - \frac{x - minimum\,value}{maximum\,value - minimum\,value}\times 10\\$ -->
<!-- ### Weights standard score -->
<!-- The weights standard score normally distributes the z score of x on a 0-10 scale. **This is the primary variable mapped in this tool.** It is calculated according to: -->
<!-- Where "higher is better":  <br> -->
<!-- $weights \, standard\, score = (normal\, distribution\, of\, z\, score)\times 10\\$ -->
<!-- Where "lower is better":  <br> -->
<!-- $weights \, standard\, score = 10 - (normal\, distribution\, of\, z\, score)\times 10\\$ -->
<!-- ### Weights rank -->
<!-- The weights standard score normally distributes the z score of x on a 0-10 scale. It is calculated according to: -->
<!-- Where "higher is better":  <br> -->
<!-- $weights \, rank = \frac{rank\, of\, the\, nominal \,weight\,of\,x}{number \, of \,tracts\, with\,data \, on\,x}\times 10\\$ -->
<!-- Where "lower is better": <br> -->
<!-- $weights \, rank = \frac{rank\, of\, the\, nominal \,weight\,of\,x}{number \, of \,tracts\, with\,data \, on\,x}\times 10\\$ -->
