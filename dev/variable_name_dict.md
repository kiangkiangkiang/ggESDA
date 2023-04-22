# 變數命名方式改變的對照表

## 已改動

### 共同

```yaml
rawData: raw_data
statisticsDF: statistics
intervalData: interval_data
clusterResult: cluster_result
testData: test_data_type
testXY: test_univariate
addFactor: add_factor_variables
this.x: aes_x
this.y: aes_y
p: num_of_variables
n: num_of_concepts
usermapping: origin_mapping
mymapping: converted_mapping
allmapping: plot_mapping
```

### cor, cov (RSDA寫的我就沒動)

```yaml
內部使用變數
```

### ggESDA

```yaml
invalidDataType: test_data_type_legal
isAllDF: is_all_data_frame
```

## ggInterval_2Dhist

```yaml
xBins: x_bins
yBins: y_bins
removeZero: is_zero_remove
addFreq: is_frequency_visible
freq.Rectangle: frequency_matrix
minX: x_minimum
minY: y_minimum
maxX: x_maximum
maxY: y_maximum
recX: x_interval
recY: y_interval
```


# 可能影響

1. github要去改
2. vignettes要改
3. paper要改
4. 版本更動說明要提我改了什麼以及可能影響的用法，並舉例
5. 要注意檔案之間的連動性，ex rawData很多檔案都會用到，要小心







