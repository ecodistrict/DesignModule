function showErrorMessage(aMessage, aTimeOut) {
    var emText = document.getElementById("errorMessage");
    if (emText) {
        emText.innerHTML = aMessage;
    }
    var emDiv = document.getElementById("errorMessages");
    if (emDiv) {
        emDiv.classList.remove("errorMessages-hidden");
        // start timer
        setTimeout(function (){ emDiv.classList.add("errorMessages-hidden"); }, aTimeOut ? aTimeOut : 3000); // default 3 seconds
    }
}