-- Databricks notebook source
CREATE EXTERNAL TABLE if not exists accounts(
acct_num INT,
acct_created TIMESTAMP,
last_order TIMESTAMP,
first_name STRING,
last_name STRING,
address STRING,
city STRING, 
state STRING,
zipcode STRING,
phone_number STRING,
last_click TIMESTAMP,
last_logout TIMESTAMP
) ROW FORMAT DELIMITED
FIELDS TERMINATED BY ',' LOCATION '/FileStore/accounts';

-- COMMAND ----------

CREATE EXTERNAL TABLE if not exists accounts_with_areacode(
acct_num INT,
first_name STRING,
last_name STRING,
phone_number STRING,
areacode STRING
) ROW FORMAT DELIMITED
FIELDS TERMINATED BY ',' LOCATION '/FileStore/accounts_with_areacode';

-- COMMAND ----------

INSERT OVERWRITE TABLE accounts_with_areacode
SELECT 
  acct_num, 
  first_name, 
  last_name, 
  phone_number, 
  left(phone_number, 3) as areacode 
FROM accounts;

-- COMMAND ----------

CREATE EXTERNAL TABLE if not exists accounts_by_areacode ( 
  acct_num INT,
  first_name STRING, 
  last_name STRING,
  phone_number STRING
)
PARTITIONED BY (areacode STRING)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ',' LOCATION '/FileStore/accounts_by_areacode';

-- COMMAND ----------

-- MAGIC %python
-- MAGIC from pyspark import HiveContext
-- MAGIC sc = spark.sparkContext
-- MAGIC hiveContext = HiveContext(spark.sparkContext)
-- MAGIC hiveContext.setConf("hive.exec.dynamic.partition.mode", "nonstrict")

-- COMMAND ----------

INSERT OVERWRITE TABLE accounts_by_areacode
partition(areacode)
SELECT 
  acct_num, 
  first_name, 
  last_name, 
  phone_number, 
  areacode 
FROM accounts_with_areacode;

-- COMMAND ----------

SELECT * FROM accounts_by_areacode LIMIT 10;

-- COMMAND ----------

-- MAGIC %python
-- MAGIC dbutils.fs.ls('/FileStore/accounts_by_areacode')

-- COMMAND ----------

-- MAGIC %python
-- MAGIC print(len(dbutils.fs.ls('/FileStore/accounts_by_areacode')))
