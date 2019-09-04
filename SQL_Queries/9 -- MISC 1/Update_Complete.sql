SELECT * FROM RoomReservations;

UPDATE RoomReservations
SET CheckInDate = '2019-05-13',
	CheckOutDate = '2019-05-14'
WHERE ReservationID = 1;