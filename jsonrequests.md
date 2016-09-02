# JSON request

- [createCase](#createCase)
- [deleteCase](#deletecase)
- [createVariant](#createVariant)
- [deleteVariant](#deleteVariant)
- [getData](#getData)
	- [table dm_queries](#dm_queries)
- [setKpiResult](#setKpiResult)
- [getKpiResult](#getKpiResult)
- [readDMQueries](#readDMQueries)
- [readDIQueries](#readDIQueries)
- [readDIObjectProperties](#readDIObjectProperties)


not implemented yet
- getGeojson


## createCase event on channel "data" <a name="createCase"></a>

	{
		"type": "request",
		"method": "createCase",
		"caseId": "<caseId>",
    	"userId": "<userId>",
    	"title": "<title of case>",
		"eventId": "data-to-dashboard"
	}

"eventId" is the optional name of the event to return the answer to, it defaults to `data-to-dashboard`.

The return event is

	{
		"type": "response",
    	"method": "createCase",
    	"caseId": "<caseId>",
    	"userId": "<userId>",
    	"status": "Success - schema already created before"
	}

The returned "status" field can be

- `In progress - creating schema`
- `Success - schema created`
- `Success - schema already created before`
- `failed - not supposed to have a variant id`
- `failed - no case id`


## deletecase event on channel "data" <a name="deletecase"></a>

	{
		
		"type": "request",
		"method": "deleteCase",
    	"caseId": "<caseId>",
    	"userId": "<userId>",
    	"eventId": "data-to-dashboard"
	}

"eventId" is the optional name of the event to return the answer to, it defaults to `data-to-dashboard`.

The return event is

	{
		"type": "response",
    	"method": "deleteCase",
    	"caseId": "<caseId>",
    	"userId": "<userId>",
    	"status": "Success - schema deleted"
	}

The returned "status" field can be

- `In progress - deleting cascading schemas`
- `Success - schema deleted`
- `Success - schema already deleted before`
- `failed - not supposed to have a variant id`
- `failed - no case id`


## createVariant event on channel "data" <a name="createVariant"></a>

	{
		"type": "request",
		"method": "createVariant",
		"caseId": "<caseId>",
		"variantId": "<variantId>",
		"userId": "<userId>",
		"name": "<variant-name>",
		"description": "<variant-description>",
		"eventId": "data-to-dashboard"
	}

"eventId" is the optional name of the event to return the answer to, it defaults to `data-to-dashboard`.

The return event is

	{
		"type": "response",
    	"method": "createVariant",
    	"caseId": "<caseId>",
		"variantId": "<variantId>",
    	"userId": "<userId>",
    	"status": "Success - variant created"
	}

The returned "status" field can be

- `In progress - creating variant`
- `Success - variant created`
- `Success - schema already created before`
- `failed - case schema does not exist`
- `failed - no variant id`
- `failed - no case id`


## deleteVariant event on channel "data" <a name="deleteVariant"></a>

	{
		"type": "request",
		"method": "deleteVariant",
		"caseId": "<caseId>",
		"variantId": "<variantId>",
		"userId": "<userId>",
		"eventId": "data-to-dashboard"
	}

"eventId" is the optional name of the event to return the answer to, it defaults to `data-to-dashboard`.

The return event is

	{
		"type": "response",
    	"method": "deleteVariant",
    	"caseId": "<caseId>",
		"variantId": "<variantId>",
    	"userId": "<userId>",
    	"status": "Success - variant deleted"
	}

The returned "status" field can be

- `In progress - deleting variant`
- `Success - variant deleted`
- `Success - variant already deleted before`
- `failed - no variant id`
- `failed - no case id`


## getData event on channel "data" <a name="getData"></a>

	{
		"type": "request",
		"method": "getData",
		"caseId": "<caseId>",
		"variantId": "<variantId>",
		"moduleId": "<moduleId>",
		"userId": "<userId>",
		"eventId": "data-to-dashboard"
	}

"eventId" is the optional name of the event to return the answer to, it defaults to `data-to-dashboard`.
"variantId" is optional; if ommited data is returned from the base case ("as is" situation).
"moduleId" is the id of the module that is used to retrieve a set of [queries](#dm_queries) from the database for to be executed to build a JSON object of data requested by the requesting module.

The return event is

	{
		"type": "response",
    	"method": "getData",
    	"caseId": "<caseId>",
		"variantId": "<variantId>",
    	"userId": "<userId>",
		"data": {<JSON object with returned data>},
    	"status": "Success"
	}

The returned "status" field can be

- `Success`
- `failed - no schema found for case and variant`
- `failed - no module id`
- `failed - no case id`

## table dm_queries <a name="dm_queries"></a>

The table dm_queries contains queries that are executed on request where the results are put together to form a response JSON object.

Per project there is a table dm_queries in the main projects schema. It is copied from the public schema on project creation but can be changed per project afterwards to conform to the used models and data structure for that project. Based on the moduleId in the getData request all matching queries are executed and results are combined to one JSON object returned in the response message.

### Table structure

columns

- `object_id` integer	the object_id must be unique integer
- `returntype` text		type of the result of the specified query, see below
- `request` text		name of the data returned
- `query` text			the query to be executed; it can contain `{case_id}` to be filled in with the schema name
- `module` text			the id of the module to be matched to the "moduleId" of the getData request, all matching queries are executed and combined to one getData result JSON object


the return type be can one of

- `INT`	the specified query returns 1 integer
- `FLOAT` the specified query returns 1 double
- `GEOJSON` the specified query returns a GEOJSON object
- `LIST` the specified query returns a list of values, the structure of the database is assumed to be citygml
- `TABLE` the specified query returns a table, the structure of the database is assumed to be a flat simple object model

## setKpiResult <a name="setKpiResult"></a>

Set values for KPIs.
...

	{
		...
		"kpiId":"<kpiId>"
		"kpiType":"<kpiType>"
		"kpiValueList":[{"type":"<type>","gml_id":"<gml_id>","kpiValue":"<kpiValue>"}, ...]
		...
	}

The returned "status" field can be

- `Success - data added to the database`
- `failed - no schema found for case and variant`
- `failed - no kpiId found in request`
- `failed - no module id`
- `failed - no case id`

## getKpiResult <a name="getKpiResult"></a>

Get values for KPIs.
...

	{
		...
		"kpiId":"<kpiId>"
		...
	}

The return event is

	{
		"type": "response",
    	"method": "getKpiResult",
    	"caseId": "<caseId>",
		"variantId": "<variantId>",
    	"userId": "<userId>",
		"kpiValue": [{"kpi_id":"<kpi_id>","gml_id":"<gml_id>","kpi_type":"<type>", "kpi_value":<value>}, ...],
    	"status": "Success"
	}

The returned "status" field can be

- `Success`
- `failed - no schema found for case and variant`
- `failed - no kpiId found in request`
- `failed - no module id`
- `failed - no case id`

## readDMQueries <a name="readDMQueries"></a>

Trigger the re-reading of the dm_queries table.

	{
		
		"type": "request",
		"method": "readDMQueries",
    	"caseId": "<caseId>",
    	"userId": "<userId>",
    	"eventId": "data-to-dashboard"
	}

"eventId" is the optional name of the event to return the answer to, it defaults to `data-to-dashboard`.

The return event is

	{
		"type": "response",
    	"method": "readDMQueries",
    	"caseId": "<caseId>",
    	"userId": "<userId>",
    	"status": "Success - read queries"
	}

The returned "status" field can be

- `Success - read queries`
- `failed - case not loaded to read dm-queries for or no queries found`

## readDIQueries <a name="readDIQueries"></a>

Trigger the re-reading of the di_queries table.

	{
		
		"type": "request",
		"method": "readDIQueries",
    	"caseId": "<caseId>",
    	"userId": "<userId>",
    	"eventId": "data-to-dashboard"
	}

"eventId" is the optional name of the event to return the answer to, it defaults to `data-to-dashboard`.

The return event is

	{
		"type": "response",
    	"method": "readDIQueries",
    	"caseId": "<caseId>",
    	"userId": "<userId>",
    	"status": "Success - read queries"
	}

The returned "status" field can be

- `Success - read queries`
- `failed - case not loaded to read di-queries for or no queries found`

## readDIObjectProperties <a name="readDIObjectProperties"></a>

Trigger the re-reading of the di_objectproperties table.

	{
		
		"type": "request",
		"method": "readDIObjectProperties",
    	"caseId": "<caseId>",
    	"userId": "<userId>",
    	"eventId": "data-to-dashboard"
	}

"eventId" is the optional name of the event to return the answer to, it defaults to `data-to-dashboard`.

The return event is

	{
		"type": "response",
    	"method": "readDIObjectProperties",
    	"caseId": "<caseId>",
    	"userId": "<userId>",
    	"status": "Success - read object properties"
	}

The returned "status" field can be

- `Success - read object properties`
- `failed - case not loaded to read di-object-properties for or no properties found`

