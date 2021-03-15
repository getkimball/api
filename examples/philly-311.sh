set -ex

URL="https://phl.carto.com/api/v2/sql?filename=public_cases_fc&format=csv&skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20public_cases_fc%20WHERE%20requested_datetime%20%3E=%20%272020-01-01%27%20AND%20requested_datetime%20%3C%20%272021-01-01%27%20LIMIT%20%20100000"

time curl $URL -o data.csv

time csvcut -c 2,13 data.csv | ./load.py --event-prefix "zip:"
time csvcut -c 2,7 data.csv | ./load.py --event-prefix "agency:"

time csvcut -c 2,3  data.csv | ./load.py -g --event-prefix "status:" -g

echo 'Done!'
