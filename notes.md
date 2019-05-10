## Assumptions:

- Some of the movies do not contain the year of release, thus assumed that it is alright to ignore these movies for all time plot.

- Movies with no genres listed are omitted from treemap and the timeplot

- Movies with multiple genres categories are duplicated for each genres in the timeplot and treemap.

- When parsing the data from the movies.csv, there were messy endings in some titles (ie. ", The"), the common ones are removed. Not all movies titles were reviewed by the time of submission

## Additional features

- For the barplot and the timeplot I implemented some selection features additional to the requirements

## Caveats

- The plotly package does not support converting treemaps into interactive mode. I wanted to create a visualization that showed the overview of all genres in the initial interface, then users would be able to access the treemaps of each individual genre but clicking on it. Thus, I compromized to having a general overview treemap on the side. Additionally I was not able to implement a tooltip that can show the movies represented by each blocks in the treemap, so I included a data table to show these information.


