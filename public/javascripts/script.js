if (window.console) {
    console.log("Hello, master debugger! I see you have discovered the js console. Welcome...");
}

var lastFocus;
window.setInterval(updateLastFocus, 1);

//i've written a terribly inefficient function. sorry
function updateLastFocus() {
    if (document.activeElement.tagName == "INPUT") {
        lastFocus = document.activeElement;
    }
    if (lastFocus === "undefined") {

    } else {
        var territoryList = document.getElementsByClassName("map-unit");
        if (lastFocus.getAttribute('name') == "terr" || lastFocus.getAttribute('name') == "otherTerr") {
            for (var i = 0; i < territoryList.length; i++) {
                territoryList[i].classList.remove("not-selectable");
            }
        } else {
            for (var i = 0; i < territoryList.length; i++) {
                territoryList[i].classList.add("not-selectable");
            }
        }
    }
}

function activateNeighbors(index) {
    var territoryList = document.getElementsByClassName("map-unit");

    if (index + 1 < 48 && index % 8 != 7) {
        territoryList[index + 1].classList.add("activated");
    }
    if (index - 1 >= 0 && index % 8 != 0) {
        territoryList[index - 1].classList.add("activated");
    }
    if (index + 8 < 48) {
        territoryList[index + 8].classList.add("activated");
    }
    if (index - 8 >= 0) {
        territoryList[index - 8].classList.add("activated");
    }
}

function deactivateNeighbors(index) {
    var territoryList = document.getElementsByClassName("map-unit");
    if (index + 1 < 48 && index % 8 != 7) {
        territoryList[index + 1].classList.remove("activated");
    }
    if (index - 1 >= 0 && index % 8 != 0) {
        territoryList[index - 1].classList.remove("activated");
    }
    if (index + 8 < 48) {
        territoryList[index + 8].classList.remove("activated");
    }
    if (index - 8 >= 0) {
        territoryList[index - 8].classList.remove("activated");
    }
}

function populateForm(index) {
    var formList = document.getElementsByTagName("INPUT");

    for (var i = 0; i < formList.length; i++) {
        if (formList[i].getAttribute('name') == "terr" && formList[i] == lastFocus) {
            formList[i].value = index;
        } else if (formList[i].getAttribute('name') == "otherTerr" &&
                    formList[i] == lastFocus) {
            formList[i].value = index;
        }
    }

}