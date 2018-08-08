# JSHint Git-Hook Setup

## Configuration

We will be setting up a pre-commit git hook to make sure that the JavaScript style guide specified by the JSHint is followed strictly.  
If any .js file does not follow the defined JavaScript style rules, a commit won't be allowed.

---

## Setup Steps

* Make sure the `.jshintrc` file is present inside the `..\DesignModule\WebClient\` folder (Instructions to construct the file along with its contents have been specified in the [JsHint.md](JsHint.md) document).
* Go inside the `..\DesignModule\.git\hooks\` folder (`.git` is a hidden folder).
* Create a file - **pre-commit** (No extension).
* Inside the file add the following code:
    ```bat
    #!/bin/sh

    files=$(git diff --cached --name-only --diff-filter=ACM | grep "\.js$")
    if [ "$files" = "" ]; then 
        exit 0 
    fi

    pass=true

    echo "Validating JavaScript:"

    for file in ${files}; do
        result=$(jshint ${file} | egrep "errors?$")
        # echo -----------------
        # echo $(jshint ${file})
        # echo -----------------
        if [ "$result" == "" ]; then
            #echo result--$result
            echo "__JSHint Passed: ${file}__"
        else
            #echo result--$result
            echo "**JSHint Failed: ${file} | $result**"
            pass=false
        fi
    done

    echo "--JavaScript validation complete--"

    if ! $pass; then
        echo "COMMIT FAILED: Your commit contains file(s) that do not pass JSHint. Please fix the JSHint errors and try again."
        exit 1
    else
        echo "COMMIT SUCCEEDED"
    fi
    ```
* This file will be executed before every commit to make sure that the code being committed follows the rules defined by the JSHint.