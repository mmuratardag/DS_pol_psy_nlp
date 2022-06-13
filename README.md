# Text Analyses of Political Psychology Articles

Pulled a sample of 1382 academic article abstracts from two main political psychology journals

- [Political Psychology](https://onlinelibrary.wiley.com/journal/14679221)
- [Journal of Social & Political Psychology](https://jspp.psychopen.eu/index.php/jspp)

Applied the following techniques:

1) EDA consists of
	- Document clustering of article titles

![MDS plot](plots/00_mds_plot.png)

   - Bi-term topic modeling on article titles

![BTM](plots/02_BTM_t14.png)

2) Text map/landscape after parts of speech tagging for visualization

![text landscape](plots/03_txt_map.png)

3) Semi-supervised (keyword-assisted topic modeling) for content analysis

![word prob](plots/08_top_word_prob.png)