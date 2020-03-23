EXECUTE sp_execute_external_script @language = N'Python',
@script = N'
print(InputDataSet.iloc[5:10])
',
@input_data_1 = N'SELECT DISTINCT CityName, StateProvinceID FROM Application.Cities ORDER BY CityName;'