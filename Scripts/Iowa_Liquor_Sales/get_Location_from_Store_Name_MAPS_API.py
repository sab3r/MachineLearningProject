#######################################################################
## GET ADDRESS, NAME AND LAT/LNG from the text query to the MAPS API ##
#######################################################################

import csv, os, urllib.parse, urllib.request, json, requests

path = "C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Liquor_Sales\\v1_Data_Processing"
os.chdir(path)
print(os.getcwd())

apiUrl = 'https://maps.googleapis.com/maps/api/place/findplacefromtext/json'

new_Store_Data_File = open("Data_From_Maps_API.csv", "w", encoding = "utf-8", newline = "")
csvwriter = csv.writer(new_Store_Data_File)

with open('Iowa_Liquor_Stores_With_Missing_County.csv') as missingDataFile:
	missingData = csv.reader(missingDataFile, delimiter = ',')
	headers = next(missingData, None)
	print(headers)
	for row in missingData:
		storeName = row[1]
		newRow = []
		newRow.append(storeName)
		storeName.replace('/', '')
		storeName.replace('&', 'and')
		storeName = storeName + " Iowa"
		#encodedStoreName = urllib.parse.quote(storeName)
		#searchParameter = encodedStoreName + " Iowa"
		PARAMS = {
			'input' : storeName,
			'inputtype' : 'textquery',
			'fields' : 'formatted_address,name,geometry',
			'key' : '{API Key removed for security purpose}',
		}
		response = requests.get(url = apiUrl, params = PARAMS)
		jsonReply = response.json()
		print(jsonReply['status'])
		if(jsonReply['status'] == 'OK'):
			#print(jsonReply['candidates'][0]['formatted_address'])
			newRow.append(jsonReply['candidates'][0]['formatted_address'])
			#print(jsonReply['candidates'][0]['geometry']['location']['lat'])
			newRow.append(jsonReply['candidates'][0]['geometry']['location']['lat'])
			#print(jsonReply['candidates'][0]['geometry']['location']['lng'])
			newRow.append(jsonReply['candidates'][0]['geometry']['location']['lng'])
			#print(jsonReply['candidates'][0]['name'])
			newRow.append(jsonReply['candidates'][0]['name'])
		else:
			newRow.append('')
			newRow.append('')
			newRow.append('')
			newRow.append('')
		csvwriter.writerow(newRow)

new_Store_Data_File.close()



################################################################
## GET COUNTY NAME AND COUNTY NUMBER FROM LAT/LNG INFORMATION ##
################################################################

path = "C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Liquor_Sales\\v1_Data_Processing"
os.chdir(path)
print(os.getcwd())

countyAPIurl = 'https://geo.fcc.gov/api/census/block/find'
with open('Data_From_Maps_API.csv') as missingDataFile:
    missingData = csv.reader(missingDataFile, delimiter = ',')
    headers = next(missingData, None)
    for row in missingData:
        lat = row[2]
        lng = row[3]
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