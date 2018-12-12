# README.md for queryClient:

## Usage

python queryClient.py [-a] [-c category_name] [-d datasource_name] [-h api_url] [-p api_port] [-o output_path] [-u] [-v] [-x file_ext] query_filter [query_selectors, ...]

The basic format of a REST API web query is as follows: http://host:port/restapi/v1/category/datasource/query_filter/query_selector

-c is used to specify the category, which defaults to thesaurus
-d is used to specify the datasource, which defaults to hc (Harper Collins)

-h is used to specify the base api_url which defaults to http://localhost
-p is used to specify the port which defaults to 2001

-o is used to specify an output path and filename.
-x is used to override the default file extension of .json for the output file at output_path.
-v provides debugging output to the console.

-a is intended to include additional key information within the JSON response; but is not yet implemented.
-u is intended to specify a POST request; but is not yet implemented.

# README.md for queryServer:

queryServer.py is a Flask Web Application that serves the MachineVantage Web API.
To run the queryServer, simply enter the following at a command prompt:

## Usage

python runserver.py

## REST API Test Queries:

Test Case 1: thesaurus category, single entry, no selector
http://localhost:2001/restapi/v1/thesaurus/hc/in

Test Case 2: thesaurus category, single entry, single selector
http://localhost:2001/restapi/v1/thesaurus/hc/in/phraseSenses/synonyms

Test Case 3: thesaurus category, single entry, multiple selector
http://localhost:2001/restapi/v1/thesaurus/hc/in?projection={%22wordSenses.synonyms%22:1,%22phraseSenses.synonyms%22:1}

Test Case 4: thesaurus category, multiple entry, no selector
http://localhost:2001/restapi/v1/thesaurus/hc?filter={%22entry%22:{%22$regex%22:%22^inv%22}}

Test Case 5: thesaurus category, multiple entry, single selector
http://localhost:2001/restapi/v1/thesaurus/hc?filter={%22entry%22:{%22$regex%22:%22^inv%22}}&projection={%22wordSenses.synonyms%22:1}

Test Case 6: thesaurus category, multiple entry, multiple selector
http://localhost:2001/restapi/v1/thesaurus/hc?filter={%22entry%22:{%22$regex%22:%22^inv%22}}&projection={%22wordSenses.synonyms%22:1,%22phraseSenses.synonyms%22:1}

Test Case 7: songs category, songs by an artist
http://localhost:2001/restapi/v1/songs/mv/Ben%20E.%20King

Test Case 8: songs category, song by artist and title
http://localhost:2001/restapi/v1/songs/mv/Ben%20E.%20King/Spanish%20Harlem/lyric?flat

Test Case 9: songs category, rosa song metadata by artist and title
http://localhost:2001/restapi/v1/songs/rosa/beyonce/Be%20With%20You?flat

Test Case 10: dictionary category, single entry, single selector
http://localhost:2001/restapi/v1/dictionary/hc/abba/wordSenses/definitions

Test Case 11: dictionary category, multiple entry, no selector
http://localhost:2001/restapi/v1/dictionary/hc?filter={%22entry%22:{%22$regex%22:%22^inv%22}}

Test Case 12: pairs category, single entry, no selector
http://localhost:2001/restapi/v1/pairs/353?filter={%22wordset%22:[%22love%22,%22sex%22]}
