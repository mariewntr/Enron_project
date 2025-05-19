# Introduction

## A little history about Enron company

Enron is a natural-gas-transmission company founded in 1985 in the US. In the 1990s, the US Congress adopted a series of laws to deregulate the sale of natural gas. This caused Enron to lose its exclusivity rights on the natural gas pipeline. During this time, Jeffrey Skilling, who was initially a consultant and later became the company's chief operating officer, transformed Enron into a trader of energy derivatives, acting as an intermediary between natural-gas producers and their customers. Soon after, Enron became a leader in this market and made huge profits from its trades. This golden age for the company allowed them to recruit Andrew Fastow, who quickly became the chief financial officer. Moreover, they diversified their activities to include electricity, coal, paper, and steel.
However, success has its limits, and in the late 1990s, the company's profits began to shrink. Under pressure from shareholders, company executives started relying on dubious accounting practices, particularly using "mark-to-market accounting," which allowed the company to record unrealized future gains from some trading contracts as current income, thus giving the illusion of higher current profits. In August 2001, some people at the head of the company began to worry about a possible accounting scandal due to this practice. In October 2001, the Securities and Exchange Commission began investigating Enron's transactions. This was the starting event that led the company to bankruptcy, which officially began in December 2001.
 

Source [Britannica Enron scandal](https://www.britannica.com/event/Enron-scandal).

## Project aims 
The principal aim of this project is to explore the Enron's email data set for extracting insight about the fiscal fraud investigation and bankruptcy of the company in 2001. For that have 3 data sets:

- the employee list with their email address

- the emails exchange from 1999 to 2002

- the recipients of each emails (to, cc, bcc). 

Over this study we will investigate the email exchange by the side of the sender and the recipient. This will be made at 3 levels: 

- without a priori, meaning all the sender and recipient

- in function of the status

- for some person know to be imply in the fraud in the company as well as the person found to be the most active in the email exchange.

At each level we will look at the number of email send/received over the study period and analyze the subject and text of email send/received by focus on key words attached to some topics (meeting, business, and enron event). The analyze focus on the Enron's event. A long this prject you will discover the data, visualize the role of the different status as well as of worker know for being involved in the differents events.  

For that project we used several libraries listed here:
For data exploration, analysis and visualization:

* [tidyverse](https://www.tidyverse.org/packages/)

* [circlize](https://cran.r-project.org/web/packages/circlize/index.html)

* [wordcloud](https://cran.r-project.org/web/packages/wordcloud/wordcloud.pdf)

* [ggpubr](https://rpkgs.datanovia.com/ggpubr/)

* [patchwork](https://patchwork.data-imaginist.com/index.html)

* [ggbreak](https://cran.r-project.org/package=ggbreak)

* [gridExtra](https://cran.r-project.org/web/packages/gridExtra/index.html)

* [grid](https://bookdown.org/rdpeng/RProgDA/the-grid-package.html)

* [gtable](https://cran.r-project.org/web/packages/gtable/index.html)

To display the result into the Rmarkdown report:

* [knitr](https://cran.r-project.org/web/packages/knitr/index.html)

To create the shiny apps:

* [shiny](https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/)

The Rmarkdown describe all the step for cleaning, engineering and analyze the Enron data. A shiny app is available to navigate inside the key insigth of this project.
