# Develop Log

## hackmd

https://hackmd.io/TkH0xO_sRdKib_9bFDh-Gw?both

## TODO

- Coding Style (. -> _)
- 修正「uses deprecated versions of various functions」類似的warning messages
- as.tibble() -> as_tibble()
- 修正「load libraries in the middle」
- 修正「don’t use global variables! (e.g., code to Fig.6 and 7, and other) - you should instead create a variable inside the data table that you are visualizing」
- 把groupby = "customize"解釋好一點 in R help
- 修正ggInterval_scatter的warning msg（d$x1...）
- ggInterval_radar有很多warning msg (fig 12, 13)
- T -> TRUE
- unit test Authors: Jiang, testthat: Unit Testing for R: https://cran.r-project.org/web/ packages/testthat/index.html
- vignette 分段
- .R, .r統一
- tidyverse用到什麼再import 不要全進來
- 所有的變數命名用python style
- 




## DOING

## DONE

- 已回應read/write in ggESDA問題 (in .tex)

- 已回應why classic2sym (in .tex)

- 已完成重建套件pipeline
- 規劃修改進度
- 
- 
- 

## PROBLEM

- 不知如何下手
  1. when including the R code, try to include it so that it can be copied and pasted - try using the reprex package, then you wouldn’t have to include the code as a separate R-script
  2. Fig.7: - in the R-script, there is superassignment used, apparently to be able to use the function argument in the ’aes()’ mapping - this is not a correct way; the correct code would be something like the following:
ggInterval_minmax(facedata, aes(!!sym(x), size = 2))
please, check the {rlang} documentation (e.g., here: https://rlang.r-lib.org/
reference/injection-operator.html#injecting-expressions)
    3. Fig.10(b) - what does the ’col = "white"’ argument inside the ’aes()’ mapping mean? Are there specific ’aes’ arguments that ggESDA implements?
    4. ![](https://i.imgur.com/3OnSHLh.png)
    5. A comment about the code of the package itself: many functions use repetitive code patterns, such as:
     if(any(unlist(lapply(as.data.frame(data[,1:p]),FUN=identical,x=eval(this.x))))){ attr<-whi
     }else if(any(unlist(lapply(as.data.frame(data[,1:p]),FUN=identical,x=eval(this.y))))){ att
     }else
Avoid such repetitions of code by placing this code into an auxiliary function and calling it when required.


- 還不確定要怎麼改比較好
    1. the exported functions have a nice documentation, but all the other ones are not documented at all; comments in the code are scarce
    2. mean, sd, cov, cor跟RSDA應該是沒什麼不同，用標準的mean sd明明就已經複寫掉了＝＝，這部分看再怎麼去解釋
    3. 解釋一下scale內部詳細運算邏輯
    4. 解釋plotAll = F
    5. 



