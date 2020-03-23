-- Use T-SQL to retrieve data from
-- the Wide World Importers database
Use WideWorldImporters;
SELECT DISTINCT CityName, StateProvinceID
FROM Application.Cities
ORDER BY CityName;


-- Import the query results into Python, and
-- view the InputDataSet as a Pandas DataFrame
EXECUTE sp_execute_external_script @language = N'Python',
@script = N'
print(InputDataSet)
',
@input_data_1 = N'SELECT DISTINCT CityName, StateProvinceID FROM Application.Cities ORDER BY CityName;'


-- Retrieve a single row from the DataFrame
EXECUTE sp_execute_external_script @language = N'Python',
@script = N'
print (InputDataSet.iloc[[7]])
',
@input_data_1 = N'SELECT DISTINCT CityName, StateProvinceID FROM Application.Cities ORDER BY CityName;'


-- Retrieve a range of rows from the DataFrame
EXECUTE sp_execute_external_script @language = N'Python',
@script = N'
print (InputDataSet.iloc[5:15])
',
@input_data_1 = N'SELECT DISTINCT CityName, StateProvinceID FROM Application.Cities ORDER BY CityName;'


-- Retrieve the first ten rows from the DataFrame
EXECUTE sp_execute_external_script @language = N'Python',
@script = N'
print (InputDataSet.head(10))
',
@input_data_1 = N'SELECT DISTINCT CityName, StateProvinceID FROM Application.Cities ORDER BY CityName;'


-- Retrieve the column names from the DataFrame
EXECUTE sp_execute_external_script @language = N'Python',
@script = N'
print(InputDataSet.columns)
',
@input_data_1 = N'SELECT DISTINCT CityName, StateProvinceID FROM Application.Cities ORDER BY CityName;'


-- Retrieve the size of the DataFrame
EXECUTE sp_execute_external_script @language = N'Python',
@script = N'
print(InputDataSet.shape)
',
@input_data_1 = N'SELECT DISTINCT CityName, StateProvinceID FROM Application.Cities ORDER BY CityName;'