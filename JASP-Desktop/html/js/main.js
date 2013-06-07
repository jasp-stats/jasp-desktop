
$(document).ready(function() {

    var templatesInDOM = $("#templates")
    var templates = { }

    window.analysisChanged = function(analysis) {

        var id = "id-" + analysis.id
        var results = analysis.analyses

        var item = $("#" + id)

        if (item.length == 0) {
            item = $('<div id="' + id + '"></div>')
            $('body').append(item)
        }

        if (analysis.name === "Descriptives") {
        	item.frequencies( results )
        }
        else if (analysis.name === "TTestOneSample") {
            item.table( results.ttest )
        }

        $("html, body").animate({ scrollTop: item.offset().top }, { duration: 'slow', easing: 'swing'});
    }

})
