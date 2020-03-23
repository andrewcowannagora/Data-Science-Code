-- View installed Python packages
EXECUTE sp_execute_external_script
@language = N'Python',
@script = N'
import pkg_resources
packages = [str(x) for x in pkg_resources.working_set]
packages.sort(key=str.lower)
OutputDataSet = pandas.DataFrame(packages)
'
WITH RESULT SETS(([Package] NVARCHAR(100)));