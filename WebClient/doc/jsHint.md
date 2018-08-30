# JsHint Setup

## Configuration

We have decided to follow the airbnb javascript style for ES5. A configuration file has been provided by them in their resourses section - [Link](https://github.com/airbnb/javascript/tree/es5-deprecated/es5#resources)

A file with the name - `.jshintrc` is required for specifying the configurations.  

A list of all JSHint options - [Link](http://jshint.com/docs/options/)

---

## Setup Steps

* Use the `.jshintrc` file available inside the `..\DesignModule\WebClient\` folder.
* The configurations in this file are provided by airbnb for es5 - [Link](https://github.com/airbnb/javascript/blob/master/linters/.jshintrc)
* Following are some different configurations that we are using, compared to the airbnb standard:
    * `"strict" : false`
    * `"maxlen" : 128`

* Install Node.js and update the **Environment Variables**
    * Add the following values to the **Path** variable:
         - %AppData%\Roaming\npm
         - C:\Program Files\nodejs

### VS Code Setup

* Install the entension [jshint](https://marketplace.visualstudio.com/items?itemName=dbaeumer.jshint)

### Visual Studio 2017
* Open command prompt and run the command - `npm install -g jshint`
* Go to the path `%AppData%\npm\` and create a new file - `MyJSHint.cmd`
* Inside the `MyJSHint.cmd` file add the following snippet of code:

    ```bat
    :: call JSHint and reformat the output so that VS can jump to the correct line
    @jshint %* | "C:\Program Files\Git\usr\bin\sed.exe" -e "s/\(.*\): line \([0-9]\+\), col \([0-9]\+\), \(.*\)/\1(\2,\3): \4/"
    ```
* Make sure that the path `C:\Program Files\Git\usr\bin\sed.exe` exists on your system.
* Update the **Environment Variables** and add the following value to the **Path** variable:
    * C:\Program Files\Git\bin
* Open **Visual Studio 2017** and go to **Tools -> External Tools**
* Click on **Add** fill in the following information:
    ```
    Title: JSHint
    Command: %AppData%\npm\MyJSHint.cmd
    Arguments: $(ItemPath)
    Initial directory: $(ItemDir)

    - [x] Use Output window
    - [ ] Prompt for arguments
    - [ ] Treat output as Unicode
    ```
* Click **OK** to save the External Tools settings.
* Steps for setting up a keyboard shortcut to trigger JSHint
    * Go to **Tools -> Options**
    * Select **Keyboard** under the **Environment** tab.
    * In the textbox for "Show commands containing", input - **ExternalCommand**.
    * Select the **Tools.ExternalCommand#** that corresponds to the JSHint tool (depends upon the number of ExternalTools added before this).
    * Press the desired key combination inside the textbox for **Press shortcut keys:** and click on the **Assign** button (Suggested key combiation: *Ctrl+Shift+#*). 
    * Click **OK** to exit the **Options** dialog.