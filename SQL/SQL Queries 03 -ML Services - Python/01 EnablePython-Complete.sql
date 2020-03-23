-- Check server configuration status
sp_configure

-- Enable external script execution
EXEC sp_configure 'external scripts enabled', 1
RECONFIGURE WITH OVERRIDE

-- Verify server configuration status
sp_configure

-- Restart SQL Server instance

-- Execute a sample Python script
EXEC sp_execute_external_script
@language = N'Python',
@script = N'
print("Hello World")
'

-- Retrieve Python version
EXEC sp_execute_external_script
@language = N'Python',
@script = N'
import sys
print(sys.version)
'