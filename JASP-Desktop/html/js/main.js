
"use strict"

$(document).ready(function() {

	var analyses = []
	var selectedAnalysisId = -1;
	var selectedAnalysis = null

    var $intro = $("#intro")
    var introVisible = true
    var introHiding  = false
    var introHidingResultsWaiting = [ ]

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
		analysis.remove()
	}

	window.unselectByClickingBody = function(event) {

		if ($(event.target).is(".jasp-analysis *") == false) {
	
			window.unselect()
			jasp.analysisUnselected()
		}
	}

	var selectedHandler = function(event) {

		if ($(event.target).is(".toolbar") || $(event.target).is(".toolbar > *"))
			return

		var id = $(event.currentTarget).attr("id")
		var idAsInt = parseInt(id.substring(3))
		
		if (selectedAnalysisId == idAsInt) {
		
			window.unselect()
			jasp.analysisUnselected()
		}
		else {

			window.select(idAsInt)
			jasp.analysisSelected(idAsInt)
		}

	}
	
	var analysisChangedDownstreamHandler = function(event, data) {
	
		jasp.analysisChangedDownstream(data.id, JSON.stringify(data.options))
	
	}

	window.analysisChanged = function(analysis) {

		if (introVisible) {
			
			introHidingResultsWaiting.push(analysis)
			
			if (introHiding == false) {
			
				introHiding = true
			
				$intro.hide("slide", { direction : "up", easing : "easeOutCubic" }, function() {
				
					introHiding = false
					introVisible = false
					
					introHidingResultsWaiting.reverse()

					while (introHidingResultsWaiting.length > 0)
						window.analysisChanged(introHidingResultsWaiting.pop())
				})
			}
			
			return
		}

        var id = "id-" + analysis.id
		var results = analysis.results
		var status = analysis.status

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
		
		item.analysis( { id : analysis.id, results : results, status : status } )
		item.bind("analysisoptionschanged", analysisChangedDownstreamHandler)
		
		if (selectedAnalysisId == analysis.id)
			window.scrollIntoView(item);
	}

	$("body").click(window.unselectByClickingBody)

})

var stringify = function(element, tabs) {

	tabs = tabs || ""

	var text = ""
	var $el = $(element)
	
	if ($el.hasClass("do-not-copy"))
		return text

	var tag = $el.prop("tagName").toLowerCase()
	
	var attrs = ""
	var style = ""
	
	var css = $el.css( [ "border-collapse", "border-top-width", "border-bottom-width", "border-left-width", "border-right-width", "border-color", "border-style", "padding", "text-align" ] )

	if (tag === "td" || tag === "th") {
	
		if (css["border-top-width"])
			style += "border-width : " + css["border-top-width"] + " " + css["border-right-width"] + " " + css["border-bottom-width"] + " " + css["border-left-width"] + "; "
		if (css["border-color"])
			style += "border-color : " + css["border-color"] + "; "
		if (css["border-style"])
			style += "border-style : " + css["border-style"] + "; "
		if (css['text-align'])
			style += "text-align : " + css['text-align'] + "; "
		if (css['padding'])
			style += "padding : " + css['padding'] + "; "
		if ($el.prop("rowspan") && $el.prop("rowspan") != 1)
			attrs += 'rowspan="' + $el.prop("rowspan") + '" '
		if ($el.prop("colspan") && $el.prop("colspan") != 1)
			attrs += 'colspan="' + $el.prop("colspan") + '" '
	}

	if (tag === "table" && css['border-collapse'])
		style += "border-collapse : " + css['border-collapse'] + "; "
	
	if (style)
		text = tabs + '<' + tag + ' style="' + style + '" ' + attrs + '>'
	else
		text = tabs + '<' + tag + ' ' + attrs + '>'
	
	var contents = $el.contents()

	if (contents.length > 0) {
		
		for (var i = 0; i < contents.length; i++) {
			var node = contents[i]
			if (node.nodeType === 3) {
				var value = $(node).text()
				if (value)
					text += "\n" + tabs + value + "\n"
			}
			else {
				text += "\n" + stringify(contents[i], tabs + "\t")
			}
		}
		
		text += tabs + '</' + tag + '>\n'
	}
	else {
	
		text += '</' + tag + '>\n'	
	}
	
	return text
}

var pushToClipboard = function(element) {

	var $el = $(element)
	
	jasp.pushToClipboard(stringify($el, "\t\t"))

}
