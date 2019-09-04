-- Creates a nonclustered index on the LastName column of the Landon Hotel's Guests table.

USE LandonHotel;

CREATE NONCLUSTERED INDEX IX_Guests_LastName
ON dbo.Guests (LastName ASC);
