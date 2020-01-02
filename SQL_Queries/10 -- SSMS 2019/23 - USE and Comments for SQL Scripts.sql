USE AdventureWorks2017;
GO

-- Two hyphens adds comments
-- Add new line

SELECT FirstName, -- Add comment to line of query
	LastName
FROM Person.Person;

-- k + c to comment
-- k + u to uncomment

/* Block Comment
Line
Line
Line
*/

-- Use [] For multiple words
SELECT FirstName AS [Person First Name],
	LastName AS [Person Last Name]
FROM Person.Person;
