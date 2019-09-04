USE WideWorldImporters;

-- use a built in function to convert names into uppercase
SELECT LEFT(FirstName, 1), LEFT(LastName, 3)
FROM Guests;