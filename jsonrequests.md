# JSON request

- [createCase](#createCase)
- [deleteCase](#deletecase)
- [createVariant](#createVariant)
- [deleteVariant](#deleteVariant)
- [getData](#getData)
	- [table public.dm_queries](#dm_queries)
- setKpiResult
- getKpiResult
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

## table public.dm_queries <a name="dm_queries"></a>

The table public.dm_queries contains queries that are executed on request where the results are put together to form a response JSON object.

