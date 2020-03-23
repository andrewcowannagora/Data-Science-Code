-- The basic Python script template
EXEC sp_execute_external_script
@language = N'Python',
@script = N'

'

-- Using the print function
EXEC sp_execute_external_script
@language = N'Python',
@script = N'
print(1+1)
'

-- Create a variable in T-SQL
DECLARE @A int;
SET @A = 1;


-- Add two variables in Python
EXEC sp_execute_external_script
@language = N'Python',
@script = N'
A = 1
B = 2
print(A + B)
'

-- Concatenate two text strings
EXEC sp_execute_external_script
@language = N'Python',
@script = N'
A = "Hello"
B = "World"
print(A + B)
'

-- Data types must match and are not converted
EXEC sp_execute_external_script
@language = N'Python',
@script = N'
A = "Hello"
B = 2
print(A + B)
'

-- Wrap integers in quotation marks to make strings
EXEC sp_execute_external_script
@language = N'Python',
@script = N'
A = "Hello"
B = "2"
print(A + B)
'

-- Or use the string function str()
EXEC sp_execute_external_script
@language = N'Python',
@script = N'
A = "Hello"
B = str(2)
print(A + B)
'