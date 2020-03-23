EXEC sp_execute_external_script
@language = N'Python',
@script = N'
A = 1
while A < 10:
    print (A)
    A = A + 1
'