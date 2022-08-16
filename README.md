# BRERC_Modelling

An extension of the BRERC Modelling project started by Danielle Edwards.
Updating some of the code in the functions and adding a GUI.

# What each function does.

1.Run First
- to convert a dataset to a dataset which works with Darwin Standard. Fix any data issues before bringing it into R. Does not work with GUI, do it in RStudio instructions.

2. Shiny - Data Diagnostic
- tells you whether your data is statistically robust.
data diagnostics' looks at the species lists for a site over time
and use a liner model to tell you if there are any detectable changes in sampling intensity. You might
define a site as 'Oldbury Court' or ST6276; you could set the code so it looks at all records for that site
each year for the past 20 and tell you if there is a statistically significant difference in sampling
intensity.

- tells you if there is a significant change in the number or records and/or list lengths over time. If so you you need to do some more work with the data to use it in a statistical and robust way.


3. Telfer
 - compares taxa against each other across a set timespan to give a change index score. You need to have a large amount of data to do this (i.e + 100 lines per taxa in each time point)
*Error in is.data.frame(x) : object 'telfer_results' not found
- it does not work with this dummy dataset, get another.

My aim is to develop a workflow where you can ‘plug in’ a species or a number of species to see if there has been a
decline between two time periods. I can also quite easily do one to tell you if sampling intensity has significantly
increased or decreased between time periods / at specific sites if that is helpful too?

The Telfer function looks at range shift; is the given taxa range declining in the given geographical
area. It uses the 'site', 'year' and 'species' variables. The code looks for grid refs which have been
surveyed at set time points (frustratingly I can't access my code to see what I set without my laptop, I
think I set each decade as a time point) and discounts any sites which have only been visited during
one of those time points. It then uses pairwise comparisons to see how the composition of those sites
have changed between each time point based on presence/albescence. It puts this data into a linear
model using a logit transformation. The change index is the residuals from the linear model and the
numbers are the ranking of all of the taxa in the dataset against each other. Those with a '0' are likely
to be due to insufficient data. So, what the Telfer outputs can tell us is that some species are doing
better than others in terms of maintaining their range, but it does not tell us about relative abundance.

4. OccAssess Script
- seems to do a lot of different things, can probably seperate them into 4 buttons.

In addition, I have worked on the OccAssess code so that you can just run it with the headings already in your data.
Don’t try to run it with your whole dataset though, it will crash your computer and the plots will be unreadable!
I have worked up:
• Assess Record Number: simple plot of records per year
• Assess Species Number: how number of species has changed over time periods (I have put in decades, but it
is easy to change this in the section you are running)
• Assess Species ID: uses recording of species as a proxy for expertise. I.e. if something was only being
recorded to genus level in 1990, but suddenly species level in 2000 that implies an increase in expertise
• Assess Rarity Bias: regresses the number of records on range size and the residuals to give an index of how
over or under sampled something is. This didn’t work so well on what I looked at, but worth playing with on
different ‘cuts’ of the dataset.
Gmail - OccAssess https://mail.google.com/mail/u/0/?ik=b506349d21&view=pt&search=al...
1

Error: object 'taxBias' not found
Problem with assessRarity function in OccAssess
Plots only flash up momentarily, can probably fix this by making them into png files.

No video for this one.
