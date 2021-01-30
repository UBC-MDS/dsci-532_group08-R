# Reflection 
### Authors: Mitchie Zhao, Jordon Lau, Kaicheng Tan, Daniel Ortiz


### What have we implemented

Our [mental health in tech dashboard (R version)](https://dsci532-viz-g8-r.herokuapp.com) currently consists of 4 plots and 4 filtering functionalities. Plots include a geographical map of the USA, a histogram for Age distribution, and bar charts for Interference with work, and Wellness program. Filtering options include a slider for age range selection and drop-down menus for features such as state, company size, and gender. All the plots and filtering functions mentioned above are on the "General Overview" tab, the "Company support" tab is reserved for further improvements and explorations in Milestone 4.


### What is not yet implemented 

After coding and deployment, there is not any obvious bug in our App. However, there are a few formatting issues that need to be fixed in the future: while the plots align well on our end, we have also found in certain browsers, the scale and mapping of the plots do not seem to be mapped properly in the full-screen mode, and this could be due to the bootstrap's self-adaptive layout feature. 


### Brief thoughts on using R for Dashboard

Compared to the Python `Altair` package, the R packages (`ggplot2` and `plotly`) seems to be more flexible in terms of coding different types of interactive plots. For example, `Altair` does not support pie chart graphs, while `ggplot2` does. The general process or workflow of creating dashboard elements (e.g. dropdown widgets, plots, maps) via R was very similar compared to Python. 
However, Dash does not seem to support R very well as it does for Python. We have noticed that when there are any bugs in the codes, we would not get any outputs saying where the problem occurs. This makes it incredibly more challenging to trace back and debug, especially when we work collaboratively. Moreover, the deployment seemed to be trickier than Python: the deployment with R language relies on Docker, and since it takes longer to run, we must be careful enough to avoid any typos and/or bugs. It is also harder to make any changes to the file `app.R` after pushing to heroku.

### Feedback and future plans

We have received some valuable feedback: in Milestone 1, our proposal included too much information and was too complicated to implement. We have addressed this issue and made our App simple in Milestone 2. The python `altair` version of the App had a bug of the drop-down menu (with repeating states), and this issue has already been fixed in our R-version App in Milestone 3.  In Milestone 2, there was some empty spaces between the plots, and we plan to further modify the layout and mapping to use the space more efficiently. Moreover, in Milestone 2, the x-axis/y-axis could change dynamically based on the user selections. The axis need to be fixed for better visualization and to reduce confusion in Milestone 4.