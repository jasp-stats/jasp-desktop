
"use strict"

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

    window.analysisChanged = function(renderer, analysis) {

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

        renderer.render(item, results)
        
        if (selectedAnalysisId == analysis.id)
        	window.scrollIntoView(item);
    }

    $("body").click(window.unselectByClickingBody)

})

var stringify = function(element, tabs) {

	tabs = tabs || ""

	var text = ""
	var $el = $(element)
	
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
