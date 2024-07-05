File: calculate_xs_NN.R

This was my first attempt, where I considered a point along the centerline and drew lines from 1 to 180 degrees at 1-degree intervalsThen, I chose the line with the shortest width, and the output looked great, but the logic is not quite general. The problem here was that couldn't get the channel width (the two nearest intersections to the point on centerline) from the line with the same extent as maxd. Also, I didn't use the optimize function. This code takes about 3 minutes to run. 

File: test.R

In this script, I tried to find intersections with the bankline and then select the two closest intersections to the point, which seemed to work correctly. but when I combined this approach with my previous code, it ran without errors but resulted in null intersection outputs.

File: calcualate_xs_NN04

Actually, this one includes the optimize function combined with the test.R logic. However, I honestly don't understand much about what's happening in the calculate width function. It generates out put but the out put is not correct for all the points. 