var message = document.getElementById("message");
var currentLocation = document.getElementById("currentLocation");
var updateButtonSpan = document.getElementById("updateButton");

var defaultTimeZone = "America/Los_Angeles";

function getCurrentPosition() {
    request = new XMLHttpRequest();
    url = window.location.protocol + "//" + window.location.host + "/coordinates";
    request.open('GET', url, false);
    request.setRequestHeader('accept', 'text/plain');
    request.send();

    currentLocation.innerHTML = request.responseText;
}

function setupUpdates() {
    getCurrentPosition();

    if (navigator.geolocation) {
	button = document.createElement("button")
	button.innerHTML = "Update";
	button.id = "updateLocation";
	button.onclick = function() {
	    try {
		position = navigator.geolocation.getCurrentPosition(buildCoordinatesJSON);
		if (position == null) {
		    message.innerHTML = "An error occurred while updating location.";    
		}
	    } catch (e) {
		console.log(e);
		message.innerHTML = "An error occurred while updating location.";
	    }
	};
	updateButtonSpan.appendChild(button);
    } else {
	message.innerHTML = "Geolocation is unavailable; can't update position from this browser.";
    }
}        

function buildCoordinatesJSON(position) {
    coords = position.coords;
    altitude = coords.altitude;
    if (altitude == null) {
	altitude = 0;
    }
    c = {'latitude': coords.latitude, 'longitude': coords.longitude, 'altitude': altitude};
    j = JSON.stringify(c);
    sendCurrentLocation(j);
}

function sendCurrentLocation(coords) {
    request = new XMLHttpRequest();
    url = window.location.protocol + "//" + window.location.host + "/coordinates";
    console.log('sendCurrentLocation→'+url);
    request.open('POST', url, false);
    request.send(coords);
    message.innerHTML = "Location updated at " + Date.now();
    getCurrentPosition();
}

window.onload = setupUpdates;