# Bayesian-Analysis
This repo is what I've learned in STAT431 at University of Illinois at Urbana Champaign

1. Generate Test Data

To make the dataset, please run the following code

`python make_file.py --shop_num 10 --goods_num 1000`

--shop_num identifies the number of shops id
--goods_num identifies the number of goods id

The output file is saved as pos.csv under Data folder.

2. Aggregation

2.1. Total sales for each year, month and shop

`python sales_by_shops.py`

Output is saved as sales_by_shops.csv under Data folder.

2.2. Total sales for each year, month and goods

`python sales_by_goods.py`

Output is saved as sales_by_goods.csv under Data folder.

2.3. Unit Value for each day and goods

`python unit_value.py`

Output is saved as unit_value.csv under Data folder.
