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

$(document).ready(function () {
    $(".event").click(clickHandler);
});

