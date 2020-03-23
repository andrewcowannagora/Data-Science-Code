-- A Python while loop
EXEC sp_execute_external_script
@language = N'Python',
@script = N'
A = 1
while A < 10:
    print (A)
    A = A + 1
'

-- a Python for loop with a range
EXEC sp_execute_external_script
@language = N'Python',
@script = N'
for x in range(0, 3):
    print("Hello World")
'

-- View the iterating variable's value
EXEC sp_execute_external_script
@language = N'Python',
@script = N'
for x in range(0, 3):
    print(x)
'

-- Iterate over letters in a text string
EXEC sp_execute_external_script
@language = N'Python',
@script = N'
for x in "Hello World":
  print (x)
'

-- Iterate over items in a list
EXEC sp_execute_external_script
@language = N'Python',
@script = N'
for x in ["red","blue","green","purple"]:
  print (x)
'