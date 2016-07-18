function showUserMessage(aMessage, aMessageType, aTimeOut) {
    var emText = document.getElementById('userMessage');
    if (emText) {
        emText.innerHTML = aMessage;
    }
    var emDiv = document.getElementById('userMessages');
    if (emDiv) {
        if (aMessageType) {
            switch (aMessageType) {
                case 1:
                    emDiv.classList.remove('userMessageError');
                    emDiv.classList.add('userMessageOk');
            }
        }
        else {
            emDiv.classList.remove('userMessageOk');
            emDiv.classList.add('userMessageError');
        }
        emDiv.classList.remove('userMessages-hidden');
        // start timer
        setTimeout(function (){ emDiv.classList.add('userMessages-hidden'); }, aTimeOut ? aTimeOut : 3000); // default 3 seconds
    }
}