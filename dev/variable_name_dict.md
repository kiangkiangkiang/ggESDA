# 變數命名方式改變的對照表

## 已改動

### cor, cov (RSDA寫的我就沒動)

- 內部使用變數

### ggESDA

```yaml
rawData: raw_data
statisticsDF: statistics
intervalData: interval_data
clusterResult: cluster_result
invalidDataType: test_data_type_legal
isAllDF: is_all_data_frame
testData: test_data_type
testXY: test_univariate

```

# 可能影響

1. github要去改
2. vignettes要改
3. paper要改
4. 版本更動說明要提我改了什麼以及可能影響的用法，並舉例
5. 要注意檔案之間的連動性，ex rawData很多檔案都會用到，要小心







