SELECT * FROM Guests;
SELECT * FROM RoomReservations;
SELECT * FROM Rooms;


SELECT Guests.FirstName,
	Guests.LastName,
	RoomReservations.CheckInDate,
	RoomReservations.CheckOutDate,
	RoomReservations.RoomNumber,
	Rooms.BedType,
	Rooms.Rate
FROM Guests INNER JOIN RoomReservations ON Guests.GuestID = RoomReservations.GuestID
	INNER JOIN Rooms ON RoomReservations.RoomNumber = Rooms.RoomNumber
;