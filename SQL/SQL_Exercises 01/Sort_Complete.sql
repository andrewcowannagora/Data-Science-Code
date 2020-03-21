
CREATE TABLE Rooms (
	RoomID int IDENTITY(1, 1) NOT NULL,
	RoomNumber char(3) NOT NULL,
	BedType varchar(15) NOT NULL, 
	Rate smallmoney NOT NULL
);


INSERT INTO Rooms (RoomNumber, BedType, Rate)
	VALUES ('101','King',110),
		('102','Queen',100),
		('103','Two Doubles',90),
		('201','King',120),
		('202','King',120),
		('203','Two Doubles',90)
;

SELECT RoomID, RoomNumber, BedType, Rate
FROM dbo.Rooms
WHERE Rate >=120 AND BedType = 'King'
;

SELECT GuestID, FirstName, LastName, State
FROM dbo.Guests
WHERE State = 'CA'
ORDER BY FirstName DESC, LastName DESC
;

