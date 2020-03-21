CREATE TABLE Rooms (
	RoomID int IDENTITY(1, 1) NOT NULL,
	RoomNumber char(3) NOT NULL,
	BedType varchar(15) NOT NULL, 
	Rate smallmoney NOT NULL
);