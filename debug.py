import requests
import uuid

patch_id = uuid.uuid4()
headers = {
    'Api-User': '',
    'Api-Key': ''
}

url = "https://evergreen.mongodb.com/api/rest/v2/patches/{}/configure".format(patch_id)
data = {
    'description': 'test patch',
    'variants': []
}
result = requests.post(url, headers=headers, data=data)
print(result.status_code)
print(result.json())
