-- Save a Python script as a stored procedure with parameters
CREATE PROCEDURE BetterRandomNumbers	@Count intASEXECUTE sp_execute_external_script@language = N'Python',@script = N'import randomMyNumbers = []for x in range(0,n):  MyNumbers.append(random.randint(1,101))OutputDataSet = pandas.DataFrame(pandas.Series(MyNumbers))',@params = N'@n int',@n = @CountWITH RESULT SETS (([Random Numbers] int));

-- Execute the procedure with a parameter
EXEC BetterRandomNumbers @Count = 20