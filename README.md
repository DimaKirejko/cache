
enter to run application:
*make run*

for insert:
curl -H "Content-Type: application/json" -X POST -d '{"action":"insert","key":"some_key", "value":[1,2,3], "ttl":10}' http://localhost:8080/api/cache_server

for lookup:
curl -H "Content-Type: application/json" -X POST -d '{"action":"lookup","key":"some_key"}' http://localhost:8080/api/cache_server

for search for a record within a time frame:
curl -H "Content-Type: application/json" -X POST -d '{"action":"lookup_by_date","date_from":"2015/1/1 00:00:00", "date_to": "2025/1/10 23:59:59"}' http://localhost:8080/api/cache_server
