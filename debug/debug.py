import requests
import uuid
import os
import json

patch_id = uuid.uuid4()
headers = {
    'Api-User': os.environ['EVG_API_USER'],
    'Api-Key': os.environ['EVG_API_KEY'],
    'Content-Type': 'application/json'
}

graphql_url = "https://evergreen.mongodb.com/graphql/query"
# url = "https://evergreen.mongodb.com/api/rest/v2/versions/{}/builds".format("5f985f99e3c33102b2fd526a")
url = "https://evergreen.mongodb.com/api/rest/v2/tasks/{}".format("mongo_rust_driver_atlas_connect__async_runtime~async_std_os~macos_10.14_test_atlas_connectivity_patch_4eb171cd412a4d4227bd3c6690838a3a84b5158c_5fa0933732f4170e18899053_20_11_02_23_16_08")
# data = {
#     'limit': 10
# }
result = requests.get(url, headers=headers)
print(result.status_code)
results = result.json()
# sample = { k: results[k] for k in results if k != "patched_config" }
print(json.dumps(results, indent=4))

# data = { 'query': 'query { patch(id: "5f98b24fd6d80a586d0d1288") { project { variants { displayName,tasks }}}}'}
# result = requests.post(graphql_url, headers=headers, data=json.dumps(data))

### data = { 'variants': [ { 'name': 'lint', 'tasks': [ 'check-clippy' ] } ] }
### result = requests.post(url, headers=headers, data=json.dumps(data))
# print(result.status_code)
# print(json.dumps(result.json(), indent=4))
