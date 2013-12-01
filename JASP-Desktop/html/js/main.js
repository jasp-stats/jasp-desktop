
$(document).ready(function() {

    var analyses = []
	var selectedAnalysisId = -1;
    var selectedAnalysis = null

    window.select = function(id) {

        if (selectedAnalysis != null)
            $(selectedAnalysis).removeClass("selected")

        var analysis = $('#id-' + id)
        selectedAnalysisId = id

        if (analysis.length === 0)
            return

        selectedAnalysis = analysis[0]

        analysis.addClass("selected").removeClass("unselected")
        $("body").addClass("selected")
    }
    
    window.scrollIntoView = function(item) {
    
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

    window.unselect = function() {

        $(analyses).removeClass("unselected")
        $(selectedAnalysis).removeClass("selected")
        $("body").removeClass("selected")

		selectedAnalysisId = -1
        selectedAnalysis = null
    }

    window.remove = function(id) {

        window.unselect()

        var analysis = $('#id-' + id)
        console.log(analysis)
        analysis.remove()
    }

    window.unselectByClickingBody = function()
    {
        window.unselect()
        jasp.analysisUnselected()
    }

    var selectedHandler = function(event) {

        var id = $(event.currentTarget).attr("id")
        var idAsInt = parseInt(id.substring(3))

        window.select(idAsInt)
        jasp.analysisSelected(idAsInt)

        event.stopPropagation()
    }

    window.analysisChanged = function(analysis) {

        var id = "id-" + analysis.id
        var results = analysis.results

        var item = $("#" + id)

        var newItem = $('<div id="' + id + '" class="jasp-analysis"></div>')
        newItem.click(selectedHandler)
        
        if (analysis.id == selectedAnalysisId)
        {
        	if (selectedAnalysis != null)
        		$(selectedAnalysis).removeClass("selected")

        	$(analyses).addClass("unselected")
        	$("body").addClass("selected")

        	selectedAnalysis = newItem[0]
        	$(selectedAnalysis).addClass("selected")
        }
        
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
        else if (analysis.name == "RegressionLinear")
        {
            item.tables( { tables : results.regression } )
        }
        else if (analysis.name == "ContingencyTables")
        {
            item.tables( { tables : results.tables } )
        }
        else if (analysis.name == "Correlation")
        {
            item.tables( { tables : [ results.correlations ] } )
        }
        
        if (selectedAnalysisId == analysis.id)
        {
        	window.scrollIntoView(item);
        }
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

    $("body").click(window.unselectByClickingBody)

})
