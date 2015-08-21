
'use strict'

window.getPPI = function () {

	return 96 * window.devicePixelRatio
}



$(document).ready(function () {
	var ua = navigator.userAgent.toLowerCase();

	if (ua.indexOf("windows") !== -1)
		$("body").addClass("windows")

	var analysesViews = []
	//var menuObject = null;
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

		selectedAnalysisId = id;

		var jaspWidget = _.find(analysesViews, function (cv) { return cv.model.get("id") === id; });
		if (jaspWidget !== undefined) {
			selectedAnalysis = jaspWidget;
			selectedAnalysis.select();
			$("body").addClass("selected")
		}
	}

	window.setAppVersion = function (version) {
		$(".app-version").text("Version " + version);
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

	window.copyMenuClicked = function () {
		if (window.menuObject.copyMenuClicked | window.menuObject.copyMenuClicked())
			window.menuObject.toolbar.displayMessage("Copied to clipboard");

		window.menuObject = null;
	}

	window.citeMenuClicked = function () {
		if (window.menuObject.citeMenuClicked | window.menuObject.citeMenuClicked())
			window.menuObject.toolbar.displayMessage("Citations copied to clipboard");

		window.menuObject = null;
	}

	window.notesMenuClicked = function (noteType, visibility) {
		if (window.menuObject.notesMenuClicked | window.menuObject.notesMenuClicked(noteType, visibility))
			window.menuObject.toolbar.displayMessage();

		window.menuObject = null;
	}

	window.removeMenuClicked = function () {
		if (window.menuObject.removeMenuClicked)
			window.menuObject.removeMenuClicked();

		window.menuObject = null;
	}

	window.analysisMenuHidden = function () {
		if (window.menuObject !== undefined && window.menuObject !== null) {
			window.menuObject.toolbar.completeEvent();
		}
	}

	window.exportHTML = function (filename) {
		var exportObject = {
			views: analysesViews,
			exportComplete: function (exportParams, exportContent) {
				if (exportParams.error) {

				}

				if (exportParams.process === JASPWidgets.ExportProperties.process.save)
					jasp.saveTextToFile(filename, wrapHTML(exportContent.html, exportParams));
			},
			getStyleAttr: function () {
				return "style='display: block;'";
			}
		};

		var exportParams = new JASPWidgets.Exporter.params();
		exportParams.format = JASPWidgets.ExportProperties.format.formattedHTML;
		exportParams.process = JASPWidgets.ExportProperties.process.save;
		exportParams.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.embedded;

		if (filename === "%PREVIEW%") {
			exportParams.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.resource;
		}

		JASPWidgets.Exporter.begin(exportObject, exportParams, true, "margin: .7em; padding: 1em;")
	}

	window.getAnalysesNotes = function () {
		var notes = [];
		for (var i = 0; i < analysesViews.length; i++) {
			notes.push(analysesViews[i].getAnalysisNotes());
		}
		return JSON.stringify(notes)
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

			analysis.slideUp(400, function () {
				var jaspWidget = _.find(analysesViews, function (cv) { return cv.model.get("id") === id; });
				if (jaspWidget !== undefined) {
					jaspWidget.close();
					analysesViews = _.without(analysesViews, jaspWidget);
				}
			});


		})


		if (showInstructions)
			hideInstructions()
	}

	window.unselectByClickingBody = function (event) {

		var target = event.target || event.srcElement;
		if (selectedAnalysisId !== -1 && $(target).is(".jasp-analysis *") == false) {

			window.unselect()
			jasp.analysisUnselected()

			if (showInstructions)
				hideInstructions()
		}
	}

	var selectedHandler = function (event) {

		var target = event.target || event.srcElement;
		if ($(target).is(".jasp-resize, .jasp-notes, .jasp-notes *, .toolbar-clickable, .toolbar-clickable *"))
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
		if (jaspWidget == undefined) {
			jaspWidget = new JASPWidgets.AnalysisView({ id: id, className: "jasp-analysis", model: new JASPWidgets.Analysis(analysis) });

			var newItem = jaspWidget.$el;

			newItem.click(selectedHandler)

			if (selectedAnalysisId === analysis.id) {
				if (selectedAnalysis != null)
					selectedAnalysis.unselect()

				$("body").addClass("selected")

				selectedAnalysis = jaspWidget;
				jaspWidget.select();
			}

			analysesViews.push(jaspWidget)

			var spacer = $("#spacer")
			newItem.css("opacity", 0)
			spacer.before(newItem)
			newItem.animate({ "opacity": 1 }, 400, "easeOutCubic")

			jaspWidget.on("optionschanged", function (id, options) {

				jasp.analysisChangedDownstream(id, JSON.stringify(options))

			});

			jaspWidget.on("toolbar:showMenu", function (obj, options) {

				jasp.showAnalysesMenu(JSON.stringify(options));
				window.menuObject = obj;
			});

			jaspWidget.on("analysis:remove", function (id) {
				jasp.removeAnalysisRequest(id);
			});

			jaspWidget.on("analysis:noteChanged", function (id, key) {
				jasp.updateNote(id, key);
			});
		}
		else
			jaspWidget.model.set(analysis);

		jaspWidget.render();

		if (selectedAnalysisId === analysis.id)
			window.scrollIntoView(jaspWidget.$el);
	}

	$("body").click(window.unselectByClickingBody)


})

var wrapHTML = function (html, exportParams) {
	var completehtml = "<!DOCTYPE HTML>\n"
	completehtml += "<html>\n"
	completehtml += "	<head>\n"
	completehtml += "		<meta http-equiv='Content-Type' content='text/html; charset=utf-8' />\n"
	completehtml += "		<title>JASP</title>"
	completehtml += "	</head>\n"

	var styles = "";
	if (exportParams.isFormatted())
		styles = JASPWidgets.Exporter.getStyles($("body"), ["font-family", "display", "font-size", "padding", "margin"]);
	else
		styles = JASPWidgets.Exporter.getStyles($("body"), ["display", "padding", "margin"]);

	completehtml += "	<body " + styles + ">\n";
	completehtml += html;
	completehtml += "	</body>\n"
	completehtml += "</html>";
	return completehtml;
};

var pushHTMLToClipboard = function (exportContent, exportParams) {

	jasp.pushToClipboard("text/html", "", wrapHTML(exportContent.html, exportParams));
}

var pushTextToClipboard = function (exportContent, exportParams) {

	jasp.pushToClipboard("text/plain", exportContent.raw, wrapHTML(exportContent.html, exportParams))
}

var pushImageToClipboard = function (exportContent, exportParams) {

	jasp.pushImageToClipboard(exportContent.raw, wrapHTML(exportContent.html, exportParams))
}


var savingId = 0;
var savingImages = {};

var saveImageBegin = function (path, base64, callback, context) {
	var index = savingId;
	savingId += 1;
	savingImages[index] = {
		callback: callback,
		context: context
	}
	jasp.saveTempImage(index, path, base64);
}

window.imageSaved = function (args) {
	var callbackData = savingImages[args.id];
	callbackData.callback.call(callbackData.context, args.fullPath);
	delete savingImages.savingId;
}