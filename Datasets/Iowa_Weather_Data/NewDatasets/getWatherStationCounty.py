path = "C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Liquor_Sales\\v1_Data_Processing"
os.chdir(path)
print(os.getcwd())

countyAPIurl = 'https://geo.fcc.gov/api/census/block/find'
with open('Weather Station Location.csv') as missingDataFile:
    missingData = csv.reader(missingDataFile, delimiter = ',')
    headers = next(missingData, None)
    for row in missingData:
        lat = row[1]
        lng = row[2]
        PARAMS = {
            'latitude' : lat,
            'longitude' : lng,
            'showall' : 'false',
            'format' : 'json',
        }
        response = requests.get(url = countyAPIurl, params = PARAMS)
        jsonReply = response.json()
        #print(jsonReply)
        if jsonReply['status'] == 'OK':
            print(jsonReply['County']['FIPS'] + "," + jsonReply['County']['name'])
        else:
            print(",")