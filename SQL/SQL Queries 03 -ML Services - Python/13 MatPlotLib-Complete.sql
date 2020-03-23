-- Export a bar chart using pyplot
EXECUTE sp_execute_external_script
@language = N'Python',
@script = N'
import matplotlib.pyplot as plt
x = InputDataSet.StateProvinceID
y = InputDataSet.Cities
plt.bar(x, y)
plt.xlabel("State/Province ID")
plt.ylabel("Number of Cities")
plt.title("Cities per State/Province")
plt.savefig("..\MyFigure.png")
',
@input_data_1 = N'SELECT StateProvinceID, COUNT(CityID) AS Cities
FROM Application.Cities
GROUP BY StateProvinceID;';

-- Print Python's current working directory
EXECUTE sp_execute_external_script
@language = N'Python',
@script = N'
print(os.getcwd())
'