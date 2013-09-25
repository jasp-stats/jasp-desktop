
$(document).ready(function() {

    var analyses = []
    var selectedAnalysis = null

    window.select = function(id) {

        if (selectedAnalysis != null)
            $(selectedAnalysis).removeClass("selected")

        var analysis = $('#' + id)

        if (analysis.length === 0)
            return

        selectedAnalysis = analysis[0]

        analysis.addClass("selected").removeClass("unselected")
        $("body").addClass("selected")
    }

    window.unselect = function() {

        $(analyses).removeClass("unselected")
        $(selectedAnalysis).removeClass("selected")
        $("body").removeClass("selected")

        selectedAnalysis = null
    }

    var selectedHandler = function(event) {

        var id = $(event.currentTarget).attr("id")
        window.select(id)

        var idAsInt = parseInt(id.substring(3))

        jasp.analysisSelected(idAsInt);
    }

    window.analysisChanged = function(analysis) {

        if (selectedAnalysis != null)
            $(selectedAnalysis).removeClass("selected")

        var id = "id-" + analysis.id
        var results = analysis.results

        var item = $("#" + id)

        var newItem = $('<div id="' + id + '" class="jasp-analysis selected"></div>')
        newItem.click(selectedHandler)

        $(analyses).addClass("unselected")
        $("body").addClass("selected")

        selectedAnalysis = newItem[0]
        analyses.push(newItem[0])

        if (item.length !== 0)
        {
            analyses.filter(function(a) { return a !== item[0] })
            item.replaceWith(newItem);
        }
        else
        {
            var spacer = $("#spacer")
            spacer.before(newItem)
        }

        item = newItem

        if (analysis.name === "Descriptives") {
        
        	item.frequencies( results )
        }
        else if (analysis.name === "TTestOneSample" || analysis.name === "TTestBayesianOneSample" || analysis.name === "TTestPairedSamples") {

			var ts = [ results.ttest ]
			
            if (results.descriptives)
                ts.push(results.descriptives)

            item.tables( { tables : ts } )
        }
        else if (analysis.name === "TTestIndependentSamples") {

            var ts = [ results.ttest ]

            if (results.inequalityOfVariances)
                ts.push(results.inequalityOfVariances)
            if (results.descriptives)
                ts.push(results.descriptives)

            item.tables( { tables : ts } )
        }
        else if (analysis.name === "AnovaOneWay") {

            var ts = [ results.anova ]

            item.tables( { tables : ts } )
        }
        else if (analysis.name === "Anova" || analysis.name === "Ancova" || analysis.name === "AnovaBayesian" || analysis.name === "AnovaMultivariate"  || analysis.name === "AncovaMultivariate") {

            var ts = [ results.anova ]

            item.tables( { tables : ts } )
        }

        var itemTop = item.offset().top
        var itemBottom = itemTop + item.height() + parseInt(item.css('marginBottom')) + parseInt(item.css('marginTop'))
        var windowTop = document.body.scrollTop
        var windowBottom = windowTop + window.innerHeight

        //console.log(itemTop, itemBottom, windowTop, windowBottom)

        if (itemTop < windowTop && itemBottom < windowBottom)
            $("html, body").animate({ scrollTop: item.offset().top }, { duration: 'slow', easing: 'swing'});
        else if (itemBottom > windowBottom && item.height() < window.innerHeight)
            $("html, body").animate({ scrollTop: itemBottom - window.innerHeight + 10 }, { duration: 'slow', easing: 'swing'});
    }

    var display = function(name, results, element) {

        if ( ! _.has(displaydefs, name))
            return

        var displaydef = displaydefs[name]

        var constructor = $(element)[displaydef.ui]
        var options = $.jasp[displaydef.options]
        options = _.extend(options, results)

        element.empty()
        constructor(options)

    }

})
