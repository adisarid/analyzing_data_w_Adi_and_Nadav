# POST to localhost example

library(httr)

request_body <- '[{"year": 2022,"beds": 3,"baths": 2,"sqft": 1000,"county": "alameda"}, 
{"year": 2022,"beds": 1,"baths": 2,"sqft": 500,"county": "alameda"}]'

res <- httr::POST(url = "http://127.0.0.1:7491/predict_rent",
                  body = request_body)

unlist(content(res))
