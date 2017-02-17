
'use strict'

window.getPPI = function () {

	return 96 * window.devicePixelRatio
}



$(document).ready(function () {
	var ua = navigator.userAgent.toLowerCase();

	if (ua.indexOf("windows") !== -1)
		$("body").addClass("windows")

	var selectedAnalysisId = -1;
	var selectedAnalysis = null

	var $intro = $("#intro")
	var introVisible = true
	var introHiding = false
	var introHidingResultsWaiting = []

	var $instructions = $("#instructions")
	var showInstructions = false;
	
	var analyses = new JASPWidgets.Analyses({ className: "jasp-report" });

	window.select = function (id) {

		if (selectedAnalysis != null)
			selectedAnalysis.unselect()

		selectedAnalysisId = id;

		var jaspWidget = analyses.getAnalysis(id);
		if (jaspWidget !== undefined) {
			selectedAnalysis = jaspWidget;
			selectedAnalysis.select();
			$("body").addClass("selected")
		}
	}

	window.setAppYear = function () {
		var d = new Date();
		var year = d.getFullYear();
		$(".app-year").text(year);
	}

	window.setAppVersion = function (version) {
		$(".app-version").text("Version " + version);
	}

    window.setAppBuildDate = function(builddate){
       $(".app-builddate").text(builddate);
    }

    window.noInstructions = function () {
        $('#instructions').text("");
    }

    window.noPatchinfo = function () {
        $('#patchinfo').text("");
    }


	window.setTextHeight = function (height) {
		$('body').css('font-size', height + 'px')
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

	window.collapseMenuClicked = function () {
		if (window.menuObject.collapseMenuClicked)
			window.menuObject.collapseMenuClicked();

		window.menuObject = null;
	}

	window.editTitleMenuClicked = function () {
		if (window.menuObject.editTitleClicked)
			window.menuObject.editTitleClicked()

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

		var exportParams = new JASPWidgets.Exporter.params();
		exportParams.format = JASPWidgets.ExportProperties.format.formattedHTML;
		exportParams.process = JASPWidgets.ExportProperties.process.save;
		exportParams.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.embedded;
		exportParams.includeNotes = true;

		if (filename === "%PREVIEW%") {
			exportParams.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.resource;
		}

		analyses.exportBegin(exportParams, function (exportParams, exportContent) {
			if (exportParams.error) {

			}

			if (exportParams.process === JASPWidgets.ExportProperties.process.save)
				jasp.saveTextToFile(filename, wrapHTML(exportContent.html, exportParams));
		})
	}

	window.getAllUserData = function () {
		var userData = analyses.getAllUserData();
		return JSON.stringify(userData)
	}

	window.getResultsMeta = function () {

		var meta = analyses.getResultsMeta();

		return JSON.stringify(meta)
	}

	window.setResultsMeta = function (resultsMeta) {

		analyses.setResultsMeta(resultsMeta);
	}

	window.scrollIntoView = function (item, complete) {

		var itemTop = item.offset().top
		var itemBottom = itemTop + item.height() + parseInt(item.css('marginBottom')) + parseInt(item.css('marginTop'))
		var windowTop = document.body.scrollTop
		var windowBottom = windowTop + window.innerHeight

		if (item.height() < window.innerHeight) {

			if (itemTop < windowTop)
				$("html, body").animate({ scrollTop: item.offset().top }, { duration: 'slow', easing: 'swing', complete: complete });
			else if (itemBottom > windowBottom)
				$("html, body").animate({ scrollTop: itemBottom - window.innerHeight + 10 }, { duration: 'slow', easing: 'swing', complete: complete });
			else if (complete !== undefined)
				complete.call(item);
		}
		else {
			if (itemTop > windowTop)
				$("html, body").animate({ scrollTop: item.offset().top }, { duration: 'slow', easing: 'swing', complete: complete });
			else if (itemBottom < windowBottom)
				$("html, body").animate({ scrollTop: itemBottom - window.innerHeight + 10 }, { duration: 'slow', easing: 'swing', complete: complete });
			else if (complete !== undefined)
				complete.call(item);
		}

	}

	window.slideAlpha = function (item, time, cssProperties, targetAlphas, divisions, clearStyleOnZero, completeCallback) {

		var params = {
			item: item,
			cssProperties: cssProperties,
			cssValues: item.css(cssProperties),
			colors: [],
			alphas: [],
			rates: [],
			targetAlphas: targetAlphas,
			baseTime: 0,
			waitTime: time / divisions,
			clearStyleOnZero: clearStyleOnZero,
			callback: completeCallback,
			divisions: divisions,
			count: 0
		}

		var propertiesLeft = 0;

		for (var i = 0; i < cssProperties.length; i++) {

			var cssValue = params.cssValues[cssProperties[i]];

			var alpha = 1;
			var pre = cssValue.substring(0, 4);
			if (pre === 'rgba') {
				var color = cssValue.substring(5, cssValue.length - 1);
				var lastComma = color.lastIndexOf(",");
				alpha = parseFloat(color.substring(lastComma + 1, color.length));
				params.colors.push(color.substring(0, lastComma));
			}
			else if (pre === 'rgb(')
				params.colors.push(cssValue.substring(4, cssValue.length - 1));
			else {
				cssValue = parseInt(cssValue);
				alpha = (cssValue >> 24) & 255;
				params.colors.push((cssValue & 255) + ', ' + ((cssValue >> 8) & 255) + ', ' + ((cssValue >> 16) & 255));
			}

			var diff = targetAlphas[i] - alpha;
			if (diff !== 0) {
				params.rates.push(diff / divisions);
				propertiesLeft += 1;
			}
			else
				params.rates.push(0);

			params.alphas.push(alpha);
		}

		if (propertiesLeft > 0) {
			var totalWaitTime = params.waitTime;
			for (var f = 0; f < divisions; f++) {
				setTimeout(window.nudgeAlpha, totalWaitTime - params.baseTime, params);
				totalWaitTime += params.waitTime;
			}
		}
	}

	window.nudgeAlpha = function (params) {

		params.count += 1;

		var changes = false;
		for (var i = 0; i < params.cssProperties.length; i++) {
			var rate = params.rates[i];
			if (rate === 0)
				continue;

			var color = params.colors[i];
			var alpha = params.alphas[i] + rate;
			var targetAlpha = params.targetAlphas[i];

			alpha = Math.round(alpha * 1000) / 1000;

			if ((alpha >= targetAlpha && rate > 0) || (alpha <= targetAlpha && rate < 0))
				alpha = targetAlpha;

			if (params.clearStyleOnZero && (params.count === params.divisions || alpha === targetAlpha))
				params.cssValues[params.cssProperties[i]] = "";
			else
				params.cssValues[params.cssProperties[i]] = 'rgba(' + color + ', ' + alpha + ')';

			if (params.count < params.divisions && alpha !== targetAlpha) 
				params.alphas[i] = alpha;
			else
				params.rates[i] = 0;

			changes = true;
		}

		if (changes)
			params.item.css(params.cssValues);

		params.baseTime += params.waitTime;

		if (params.count === params.divisions)
			params.callback();			
	}


	window.unselect = function () {

		analyses.unselectAllAnalyses();

		$("body").removeClass("selected")

		selectedAnalysisId = -1
		selectedAnalysis = null

		if (showInstructions)
			hideInstructions()
	}

	window.remove = function (id) {

		window.unselect()

		analyses.removeAnalysisId(id);

		if (showInstructions)
			hideInstructions()
	}

	window.unselectByClickingBody = function (event) {

		var target = event.target || event.srcElement;

		var noteClicked = $(target).is(".jasp-notes, .jasp-notes *");
		var ignoreSelectionProcess = wasLastClickNote === true && noteClicked === false;
		wasLastClickNote = noteClicked;
		if (ignoreSelectionProcess)
			return;

		if (selectedAnalysisId !== -1 && $(target).is(".jasp-analysis *, .etch-editor-panel, .etch-editor-panel *") == false) {

			window.unselect()
			jasp.analysisUnselected()

			if (showInstructions)
				hideInstructions()
		}
	}

	var wasLastClickNote = false;

	var selectedHandler = function (event) {

		var target = event.target || event.srcElement;
		var noteClicked = $(target).is(".jasp-notes, .jasp-notes *");

		var ignoreSelectionProcess = wasLastClickNote === true && noteClicked === false;

		wasLastClickNote = noteClicked;

		if (ignoreSelectionProcess || $(target).is(".jasp-resize, .toolbar-clickable, .toolbar-clickable *"))
			return;

		var id = $(event.currentTarget).attr("id")
		var idAsInt = parseInt(id.substring(3))

		if (selectedAnalysisId == idAsInt && noteClicked === false) {
				window.unselect()
				jasp.analysisUnselected()
		}
		else if (selectedAnalysisId !== idAsInt && noteClicked === true) {
			if (selectedAnalysisId !== -1) {
				window.unselect()
				jasp.analysisUnselected()
			}
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

		var spacer = $("#spacer")

		if (analyses.initialised === undefined) {

			analyses.initialised = true;

			analyses.on("toolbar:showMenu", function (obj, options) {

				jasp.showAnalysesMenu(JSON.stringify(options));
				window.menuObject = obj;
			});

			analyses.on("analyses:userDataChanged", function (key) {
				jasp.updateUserData(-1, key);
			});

			analyses.render();

			analyses.$el.css("opacity", 0)
			spacer.before(analyses.$el);
			analyses.$el.animate({ "opacity": 1 }, 400, "easeOutCubic")
		}

		var jaspWidget = analyses.getAnalysis(analysis.id);
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

			analyses.addAnalysis(jaspWidget);

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

			jaspWidget.on("analysis:userDataChanged", function (id, key) {
				jasp.updateUserData(id, key);
			});
		}
		else
			jaspWidget.model.set(analysis);

		jaspWidget.render();

		if (selectedAnalysisId === analysis.id)
			window.scrollIntoView(jaspWidget.$el);
	}
	
	$("#results").on("click", ".stack-trace-selector", function(e) {
		$(this).next(".stack-trace").slideToggle(function() {
			var $selectedInner = $(this).parent().siblings(".jasp-analysis");
			var errorBoxHeight = $(this).parent(".analysis-error").outerHeight(true);
			if ($(this).next(".stack-trace").is(":hidden")) {
				$selectedInner.css("height", "");
			}
			if ($selectedInner.height() < errorBoxHeight) {
				$selectedInner.height(errorBoxHeight)
			}
		}.bind(this))
	});

	$("body").click(window.unselectByClickingBody)


})

var wrapHTML = function (html, exportParams) {
	var completehtml = "<!DOCTYPE HTML>\n"
	completehtml += "<html>\n"
	completehtml += "	<head>\n"
	completehtml += "		<meta http-equiv='Content-Type' content='text/html; charset=utf-8' />\n"
	completehtml += "		<title>JASP</title>"
	completehtml += "		<style>"
	completehtml += "			p {margin-top:1em; margin-bottom:1em;}"
	if (exportParams.isFormatted())
		completehtml += "			body {font-family: sans-serif;}"
	completehtml += "		</style>"
	completehtml += "	</head>\n"

	var styles = JASPWidgets.Exporter.getStyles($("body"), ["display", "padding", "margin"]);

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

var simulateClick = function (x, y, count) {

	jasp.simulatedMouseClick(x, y, count);
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

window.resultsDocumentChanged = function () {
	jasp.resultsDocumentChanged();
}

window.displayWarningMessage = function (msg) {
	jasp.displayMessageFromResults(msg);
}
