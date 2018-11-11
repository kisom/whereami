var message = document.getElementById("message");
var currentLocation = document.getElementById("currentLocation");
var updateButtonSpan = document.getElementById("updateButton");

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

    timestamp = position.timestamp;
    if ((timestamp == 0) || (timestamp == null)) {
        timestamp = parseInt(Date.now().toFixed());
    }
	
	c = {
		'latitude': coords.latitude
		, 'longitude': coords.longitude
		, 'altitude': altitude
		, 'timestamp': timestamp
    };

	sendCurrentLocation(c);
}

// Thanks, Terin, for this useful tip.
var localeStringOptions = {
      "year":"numeric"
    , "month":"2-digit"
	, "day":"2-digit"
	, "hour":"2-digit"
	, "minute":"2-digit"
    , "second":"2-digit"
    , "hour12": false
};

function formatTimestamp(dt) {
    dts = dt.getFullYear() + "/" + (dt.getMonth() + 1) + "/" + dt.getDate();
}

function sendCurrentLocation(coords) {
    jsonCoordinates = JSON.stringify(coords);
    request = new XMLHttpRequest();
	url = window.location.protocol + "//" + window.location.host + "/coordinates";
	request.open('POST', url, false);
	request.send(jsonCoordinates);

    updatedAt = new Date(coords.timestamp);
    // South Africa is apparently the only en- locale that uses a sane time format?
	message.innerHTML = "Location updated at " + updatedAt.toLocaleString('en-ZA', localeStringOptions);
	getCurrentPosition();
}

window.onload = setupUpdates;