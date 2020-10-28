import requests
import uuid
import os
import json

patch_id = uuid.uuid4()
headers = {
    'Api-User': os.environ['EVG_API_USER'],
    'Api-Key': os.environ['EVG_API_KEY']
}

url = "https://evergreen.mongodb.com/api/rest/v2/projects/{}/patches".format("mongo-rust-driver")
data = {
    'limit': 10
}
result = requests.get(url, headers=headers, data=data)
print(result.status_code)
results = result.json()[0]
sample = { k: results[k] for k in results if k != "patched_config" }
print(json.dumps(sample, indent=4))
