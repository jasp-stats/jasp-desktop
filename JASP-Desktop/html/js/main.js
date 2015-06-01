
'use strict'

window.getPPI = function () {

	return 96 * window.devicePixelRatio
}



$(document).ready(function () {
	var ua = navigator.userAgent.toLowerCase();

	if (ua.indexOf("windows") !== -1)
		$("body").addClass("windows")

	var analysesViews = []
	var selectedAnalysisId = -1;
	var selectedAnalysis = null

	var $intro = $("#intro")
	var introVisible = true
	var introHiding = false
	var introHidingResultsWaiting = []

	var $instructions = $("#instructions")
	var showInstructions = false;

	window.select = function (id) {

		if (selectedAnalysis != null)
			selectedAnalysis.unselect()

		var jaspWidget = _.find(analysesViews, function (cv) { return cv.model.get("id") === id; });
		if (typeof jaspWidget !== 'undefined') {
			selectedAnalysisId = id;
			selectedAnalysis = jaspWidget;
			selectedAnalysis.select();
			$("body").addClass("selected")
		}
	}

	window.showInstructions = function () {

		showInstructions = true
	}

	window.hideInstructions = function () {

		showInstructions = false

		$instructions.animate({ opacity: 0 }, 400, "easeOutCubic", function () {
			$instructions.slideUp()
		})
	}

	window.scrollIntoView = function (item) {

		var itemTop = item.offset().top
		var itemBottom = itemTop + item.height() + parseInt(item.css('marginBottom')) + parseInt(item.css('marginTop'))
		var windowTop = document.body.scrollTop
		var windowBottom = windowTop + window.innerHeight

		if (item.height() < window.innerHeight) {

			if (itemTop < windowTop)
				$("html, body").animate({ scrollTop: item.offset().top }, { duration: 'slow', easing: 'swing' });
			else if (itemBottom > windowBottom)
				$("html, body").animate({ scrollTop: itemBottom - window.innerHeight + 10 }, { duration: 'slow', easing: 'swing' });
		}
		else {
			if (itemTop > windowTop)
				$("html, body").animate({ scrollTop: item.offset().top }, { duration: 'slow', easing: 'swing' });
			else if (itemBottom < windowBottom)
				$("html, body").animate({ scrollTop: itemBottom - window.innerHeight + 10 }, { duration: 'slow', easing: 'swing' });
		}

	}

	window.unselect = function () {

		_.invoke(analysesViews, "unselect");

		$("body").removeClass("selected")

		selectedAnalysisId = -1
		selectedAnalysis = null

		if (showInstructions)
			hideInstructions()
	}

	window.remove = function (id) {

		window.unselect()

		var analysis = $('#id-' + id)

		analysis.animate({ opacity: 0 }, 400, "easeOutCubic", function () {

			analysis.slideUp(400)
		})

		var jaspWidget = _.find(analysesViews, function (cv) { return cv.model.get("id") === id; });
		if (typeof jaspWidget !== 'undefined') {
			jaspWidget.close();
			analysesViews = _.without(analysesViews, jaspWidget);
		}

		if (showInstructions)
			hideInstructions()
	}

	window.unselectByClickingBody = function (event) {

		if (selectedAnalysisId !== -1 && $(event.target).is(".jasp-analysis *") == false) {

			window.unselect()
			jasp.analysisUnselected()

			if (showInstructions)
				hideInstructions()
		}
	}

	var selectedHandler = function (event) {

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

	var analysisChangedDownstreamHandler = function (event, data) {

		jasp.analysisChangedDownstream(data.id, JSON.stringify(data.model))

	}



	window.analysisChanged = function (analysis) {

		if (introVisible) {

			introHidingResultsWaiting.push(analysis)

			if (introHiding == false) {

				introHiding = true

				$intro.hide("slide", { direction: "up", easing: "easeOutCubic" }, function () {

					introHiding = false
					introVisible = false

					introHidingResultsWaiting.reverse()

					while (introHidingResultsWaiting.length > 0)
						window.analysisChanged(introHidingResultsWaiting.pop())
				})
			}

			return
		}

		if (showInstructions)
			$instructions.fadeIn(400, "easeOutCubic")

		var id = "id-" + analysis.id

		var jaspWidget = _.find(analysesViews, function (cv) { return cv.model.get("id") === analysis.id; });
		if (typeof jaspWidget == 'undefined') {
			jaspWidget = new JASPWidgets.AnalysisView({ id: id, className: "jasp-analysis", model: new JASPWidgets.Analysis(analysis) });

			var newItem = jaspWidget.$el;

			newItem.click(selectedHandler)

			if (selectedAnalysis != null)
				selectedAnalysis.unselect()

			$("body").addClass("selected")

			selectedAnalysis = jaspWidget;
			jaspWidget.select();

			analysesViews.push(jaspWidget)

			var spacer = $("#spacer")
			newItem.css("opacity", 0)
			spacer.before(newItem)
			newItem.animate({ "opacity": 1 }, 400, "easeOutCubic")

			jaspWidget.on("optionschanged", function (id, options) {

				jasp.analysisChangedDownstream(id, JSON.stringify(options))

			});
		}
		else
			jaspWidget.model.set(analysis);

		jaspWidget.render();

		window.scrollIntoView(jaspWidget.$el);
	}

	$("body").click(window.unselectByClickingBody)

})

var stringify = function (element, tabs) {

	tabs = tabs || ""

	var text = ""
	var $el = $(element)

	if ($el.hasClass("do-not-copy"))
		return text

	var tag = $el.prop("tagName").toLowerCase()

	var attrs = ""
	var style = ""

	var css = $el.css(["border-collapse", "border-top-width", "border-bottom-width", "border-left-width", "border-right-width", "border-color", "border-style", "padding", "text-align"])

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
				if (value) {

					value = value
						.replace(/&/g, '&amp;')
						.replace(/"/g, '&quot;')
						.replace(/'/g, '&#39;')
						.replace(/</g, '&lt;')
						.replace(/>/g, '&gt;')
						.replace(/\u2212/g, '-')

					text += "\n" + tabs + value + "\n"
				}
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

var pushToClipboard = function (element) {

	var $el = $(element)

	jasp.pushToClipboard("text/html", stringify($el, "\t\t"))

}

var pushTextToClipboard = function (str) {

	jasp.pushToClipboard("text/plain", str)
}

