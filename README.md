# ggESDA : An R package for Exploratory Symbolic data analysis

Symbolic data analysis (SDA) is an extension of standard data analysis where symbolic data tables are used as input and symbolic objects are made output as a result. The data units are called symbolic since they are more complex than standard ones, as they not only contain values or categories, but also include internal variation and structure.<a href="#ref1">[1]</a><a href="#ref2">[2]</a>

<b>ggESDA</b> is an extension of ggplot2 for visualizing the symbolic data based on exploratory data analysis (EDA).The package contains many useful functions for exploratory plots. Furthermore, the users can also transform the classical data into the symbolic data by the function in this package.

## Package installation

`devtools::install_github("kiangkiangkiang/ggESDA")`



## Creating

The example data is called <b>facedata</b> </a><a href="#ref3">[3]</a>. It will be the interval form with minimal and maximal, known as <b>interval-valued</b> data. However, most of the symbolic data are not exist at the beginning. Instead, they are usually aggregated by clustering algorithm from a classical data. Thus, we will use `classic2sym()` function to summarize classical data into symbolic data.


```{r library}
library(ggESDA)
```

```{r createData}
#aggregated by the variable Species in iris
iris_interval_var <- classic2sym(iris, groupby = Species)
iris_interval_kmeans <- classic2sym(iris, groupby = "kmeans")
iris_interval_hclust <- classic2sym(iris, groupby = "hclust")

d <- data.frame(minData = runif(100, 0, 10),
                maxData = runif(100, 50, 60))
                

d.sym <- classic2sym(d, groupby = "customize",
                    minData = d$minData,
                    maxData = d$maxData)
                    
#get interval-valued data
d.sym$intervalData
```

## Visualization

With the symbolic data generated, you can start to visualize the data by the following functions:

### ggInterval_index() for visualizing the interval of each observations

```{r ggInterval_index,eval=FALSE}
ggInterval_index(data = iris_interval, aes(x = 1:3, y = Sepal.Length))
```
It can get the preliminary understanding of the interval-valued data. 

You can also change `fill =` and `col =` to make the plot more visible, and set x or y axis to your variable will rotate the index line in figure.

### ggInterval_minmax() for visualizing the interval of each observations which is sorted by minimal value

```{r ggInterval_minmax,eval=FALSE}
ggInterval_minmax(iris_interval,aes(x=Petal.Length))
```

wait for edit...........

## References
<p id="ref1">
1. Diday, Edwin; Esposito, Floriana (December 2003). "An introduction to symbolic data analysis and the SODAS software".
</p>

<p id="ref2">
2. Lynne Billard; Edwin Diday (14 May 2012). Symbolic Data Analysis: Conceptual Statistics and Data Mining.
</p>

<p id="ref3">
 3. Billard L. and Diday E. (2006). Symbolic data analysis: Conceptual statistics and data mining. Wiley, Chichester.
 </p>
