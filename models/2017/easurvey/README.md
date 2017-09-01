## 2017 .impact Survey Analysis

Run the model and analysis:

```
cd ~/path/to/ea-data
R
```

```R
run("imsurvey2017")
```

-

Run the full data pipeline, assuming you possess the confidential un-anonymous CSVs in your `data` directory:

```R
source("models/2017/easurvey/ea_id.R")
source("models/2017/easurvey/convert_money.R")
run("2017/survey")
source("models/2017/easurvey/write_comments.R")
```
