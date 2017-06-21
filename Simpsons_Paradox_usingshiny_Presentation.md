Simpsons Paradox using Shiny
========================================================
author: Sandeep Anand
date: 6/20/2017
autosize: true

First Slide
========================================================

For more details on the Simpsons Paradox, please check the link below
<https://sananand007.shinyapps.io/simpsonsparadox/> .

Every Simpson's paradox involves at least three variables:

+ the explained
+ the observed explanatory
+ the lurking explanatory
  If the effect of the observed explanatory variable on the explained variable changes directions when you account for the lurking explanatory variable, you've got a Simpson's Paradox.
+ The Simpson's Paradox is an idea that can explain and find faults in a number of data analysis by proper pooling of the data which can challenge reproducibility  

Second Slide
========================================================

For more details on the shiny app, please check the link below
<https://sananand007.shinyapps.io/simpsonsparadox/> .

- It shows the widgets being used to develop the Shiny app that is later shown as a Picture
- It also shows reactivity of graphs through the use of variable bar Plots
- It also shows how we can implement a rough version of Simpson's Paradox basic idea using a simple way

Code to Show the Data being Used
========================================================


```
$Admit
[1] "Admitted" "Rejected"

$Gender
[1] "Male"   "Female"

$Dept
[1] "A" "B" "C" "D" "E" "F"
```

```
  Decision Gender Category Count
1 Admitted   Male     Easy   865
2 Rejected   Male     Easy   520
3 Admitted Female     Easy   106
4 Rejected Female     Easy    27
5 Admitted   Male     Hard   333
6 Rejected   Male     Hard   973
7 Admitted Female     Hard   451
8 Rejected Female     Hard  1251
```

Slide With Plot
========================================================

![plot of chunk unnamed-chunk-1](./Capture-Output.png)
