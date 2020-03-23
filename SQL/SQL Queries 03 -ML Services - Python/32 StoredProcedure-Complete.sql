-- Save a Python script as a stored procedure
CREATE PROCEDURE RandomNumbers
AS
EXECUTE sp_execute_external_script
@language = N'Python',
@script = N'
import random

MyNumbers = []
for x in range(0,10):
  MyNumbers.append(random.randint(1,101))

OutputDataSet = pandas.DataFrame(pandas.Series(MyNumbers))
'
WITH RESULT SETS (([Random Numbers] int));


-- Execute the procedure
EXEC RandomNumbers;