EXECUTE sp_execute_external_script @language = N'Python',
@script = N'
OutputDataSet = InputDataSet.iloc[5:10]
',
@input_data_1 = N'SELECT DISTINCT CityName, StateProvinceID FROM Application.Cities ORDER BY CityName;'
WITH RESULT SETS (([City Name] nvarchar(100), [State Province ID] int));