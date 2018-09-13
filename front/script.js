"use strict";

var selected = false;

function clickHandler(ev) {
    let t = $(this);

    if (selected) {
        selected.removeClass("selected");

        // Just unselect if same element is pressed twice in a row
        if (selected.is(t)) {
            selected = false;
            return;
        }
    }
    t.addClass("selected");

    selected = t;
    return;
}

function getDatestr () {
    return new Date().toISOString().slice(0, 10);
}

function gotoToday() {
	let datestr = getDatestr();
    document.getElementById(datestr).scrollIntoView();
    /*
     * This is to scroll to the top of the page.
     * This so that the dates are in view, which
     * are above by default since I want to line
     * up with the time bars.
     */
    window.scrollTo(window.scrollX, -20);
}

function highlightToday() {
	let datestr = getDatestr();

	let [year, month, day] = datestr.split("-");
	// TODO I know
	let yeasterday = year + "-" + month + "-" + (day - 1);

	$("#" + datestr)   .children(".meta").addClass("today");
	$("#" + yeasterday).children(".meta").removeClass("today");
}

$(document).ready(function () {
    $(".event").click(clickHandler);

	highlightToday();
	setInterval(highlightToday, 10 * 1000);
});

