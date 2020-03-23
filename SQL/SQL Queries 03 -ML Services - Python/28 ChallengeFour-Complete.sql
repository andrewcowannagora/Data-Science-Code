-- Create stored procedure with parameterized Python script
CREATE PROCEDURE MyFavoriteCities	@Quantity intASEXEC sp_execute_external_script@language = N'Python',@script = N'OutputDataSet = InputDataSet.sample(q)',@input_data_1 = N'SELECT CityID, CityName FROM Application.Cities',@params = N'@q int',@q = @QuantityWITH RESULT SETS (([CityID] int, [CityName] nvarchar(50)));


-- Execute the stored procedure
EXEC MyFavoriteCities @Quantity = 15