EXECUTE sp_execute_external_script
@language = N'Python',
@script = N'

',
@input_data_1 = N'SELECT TOP 10 CityID, CityName FROM Application.Cities'