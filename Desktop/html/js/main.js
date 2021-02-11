'use strict'

var jasp = null;
var scrollAtAll = true;

$(document).ready(function () {
	var d		= new Date();
	var month	= d.getMonth();
	var day		= d.getDate();

	if ((month == 11 && day >= 19) || (month == 0 && day <= 5))
		$("#note").css("background-image", "url('img/snow.gif')");

	if (typeof qt !== "undefined")
		var ch = new QWebChannel(qt.webChannelTransport, function (channel) { jasp = channel.objects.jasp; });

	var ua = navigator.userAgent.toLowerCase();

	if (ua.indexOf("windows") !== -1)
		$("body").addClass("windows")

	// Global settings for analysis output. Add here if making new setting.
	window.globSet = {
		"pExact" :		false,
		"decimals":		"",
		"tempFolder":	""
	}

	var selectedAnalysisId	= -1;
	var selectedAnalysis	= null
	var $instructions		= $("#instructions")
	var showInstructions	= false;
	var wasLastClickNote	= false;
	var analyses			= new JASPWidgets.Analyses({ className: "jasp-report" });

	window.setZoom			= function (zoom)			{ document.body.style.zoom = "" + Math.floor(zoom * 100) + "%";	}
	window.reRenderAnalyses = function ()				{ analyses.reRender();											}
	window.moveAnalyses		= function (fromId, toId)	{ analyses.move(fromId, toId);									}

	window.refreshEditedImage = function(id, imageEditResults) {
		var analysis = analyses.getAnalysis(id);
		if (analysis === undefined) return;

		if (imageEditResults.error && imageEditResults.resized)	analysis.undoImageResize();
		else													analysis.insertNewImage(imageEditResults);
	}

	window.cancelImageEdit = function(id) {
		var analysis = analyses.getAnalysis(id);
		if (analysis !== undefined)
			analysis.undoImageResize();
	}

	window.select = function (id, fromQML) {

		if (selectedAnalysis != null)
			selectedAnalysis.unselect()

		selectedAnalysisId = id;

		var jaspWidget = analyses.getAnalysis(id);
		if (jaspWidget === undefined) return;

		selectedAnalysis = jaspWidget;
		selectedAnalysis.select();
		$("body").addClass("selected")

		if(fromQML)
			window.scrollToTopView(selectedAnalysis.$el); //Only when selected in QML should we be scrollin'
	}
	
	window.setStatus = function(id, status) {
		var analysis = analyses.getAnalysis(id);
		if (analysis === undefined) 
			return;

		analysis.toolbar.setStatus(status);
		analysis.toolbar.render();
	}

	window.changeTitle = function(id, title) {
		var analysis = analyses.getAnalysis(id);
		if (analysis === undefined) 
			return;

		analysis.toolbar.setTitle(title);
		analysis.toolbar.render();
	}

	window.overwriteUserdata = function(id, userData) {
		var analysis = analyses.getAnalysis(id);
		if (analysis === undefined) 
			return;

		analysis.overwriteUserData(userData)
	}

	window.setAnalysesTitle = function(newTitle) { analyses.setTitle(newTitle); }



	window.setAppVersion	= function(version) { $(".app-version").text("Version " + version);	}
	window.noInstructions	= function()		{ $('#instructions').text("");					}
	window.noPatchinfo		= function()		{ $('#patchinfo').text("");						}
	window.setTextHeight	= function(height)	{ $('body').css('font-size', height + 'px');	}
	window.showInstructions = function()		{ showInstructions = true;						}

	window.hideInstructions = function () {

		showInstructions = false

		$instructions.animate({ opacity: 0 }, 400, "easeOutCubic", function () { $instructions.slideUp() })
	}

	window.setSelection = function(value) {
		if (window.menuObject != undefined)
			window.menuObject.toolbar.setSelected(value);
	}

	window.copyMenuClicked = function () {
		if (window.menuObject.copyMenuClicked && window.menuObject.copyMenuClicked())
			window.menuObject.toolbar.displayMessage("Copied to clipboard");

		setSelection(false);
		window.menuObject = null;
	}

	window.menuObjectFunctionCaller = function(func, msg) {
		if(func && func() && msg !== undefined)
			window.menuObject.toolbar.displayMessage(msg);

		setSelection(false);
		window.menuObject = null;
	}

	//Ok we .bind() the menuObject to the function because otherwise javascript decides that "this" is window and not a plot/analysis/table/whatever...
	window.saveImageClicked			= function () { window.menuObjectFunctionCaller( window.menuObject.saveImageClicked			.bind(window.menuObject) ); }
	window.editImageClicked			= function () { window.menuObjectFunctionCaller( window.menuObject.editImageClicked			.bind(window.menuObject) ); }
	window.editTitleMenuClicked		= function () { window.menuObjectFunctionCaller( window.menuObject.editTitleClicked			.bind(window.menuObject) ); }
	window.collapseMenuClicked		= function () { window.menuObjectFunctionCaller( window.menuObject.collapseMenuClicked		.bind(window.menuObject) ); }
	window.showDependenciesClicked	= function () { window.menuObjectFunctionCaller( window.menuObject.showDependenciesClicked	.bind(window.menuObject) ); }
	window.duplicateMenuClicked		= function () { window.menuObjectFunctionCaller( window.menuObject.duplicateMenuClicked		.bind(window.menuObject) ); }
	window.removeMenuClicked		= function () { window.menuObjectFunctionCaller( window.menuObject.removeMenuClicked		.bind(window.menuObject) ); }
	window.citeMenuClicked			= function () { window.menuObjectFunctionCaller( window.menuObject.citeMenuClicked			.bind(window.menuObject),	"Citations copied to clipboard"	); }
	window.latexCodeMenuClicked		= function () { window.menuObjectFunctionCaller( window.menuObject.latexCodeMenuClicked		.bind(window.menuObject),	"LaTeX code copied to clipboard"); }

	window.notesMenuClicked = function (noteType, visibility) {
		if (window.menuObject.notesMenuClicked && window.menuObject.notesMenuClicked(noteType, visibility))
			window.menuObject.toolbar.displayMessage();

		setSelection(false);
		window.menuObject = null;
	}



	window.analysisMenuHidden = function () {
		if (window.menuObject !== undefined && window.menuObject !== null) {
			window.menuObject.toolbar.completeEvent();
		}
	}

	window.exportHTML = function (filename) {

		var exportParams				= new JASPWidgets.Exporter.params();
		exportParams.format				= JASPWidgets.ExportProperties.format.formattedHTML;
		exportParams.process			= JASPWidgets.ExportProperties.process.save;
		exportParams.htmlImageFormat	= JASPWidgets.ExportProperties.htmlImageFormat.embedded;
		exportParams.includeNotes		= true;

		if (filename === "%PREVIEW%") { exportParams.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.resource; }

		analyses.exportBegin(exportParams, function (exportParams, exportContent) {

			if (exportParams.process === JASPWidgets.ExportProperties.process.save)
				jasp.saveTextToFile(filename, wrapHTML(exportContent.html, exportParams));
		})
	}

	window.getAllUserData = function ()				{ jasp.setAllUserDataFromJavascript(JSON.stringify(analyses.getAllUserData()))	}
	window.getResultsMeta = function ()				{ jasp.setResultsMetaFromJavascript(JSON.stringify(analyses.getResultsMeta()))	}
	window.setResultsMeta = function (resultsMeta)	{ analyses.setResultsMeta(resultsMeta);											}
	window.setScrollAtAll = function (scrollOrNot)  { scrollAtAll = scrollOrNot;													}

	window.scrollIntoView = function (item, complete) {

		if(!scrollAtAll)
			return;

		var itemTop			= item.offset().top
		var itemBottom		= itemTop + item.height() + parseInt(item.css('marginBottom')) + parseInt(item.css('marginTop'))
		var windowTop		= document.body.scrollTop
		var windowBottom	= windowTop + window.innerHeight

		if (item.height() < window.innerHeight)
		{
			if (itemTop < windowTop)				$("html, body").animate({ scrollTop: item.offset().top }, { duration: 'slow', easing: 'swing', complete: complete });
			else if (itemBottom > windowBottom)		$("html, body").animate({ scrollTop: itemBottom - window.innerHeight + 10 }, { duration: 'slow', easing: 'swing', complete: complete });
			else if (complete !== undefined)		complete.call(item);
		}
		else
		{
			if (itemTop > windowTop)				$("html, body").animate({ scrollTop: item.offset().top }, { duration: 'slow', easing: 'swing', complete: complete });
			else if (itemBottom < windowBottom)		$("html, body").animate({ scrollTop: itemBottom - window.innerHeight + 10 }, { duration: 'slow', easing: 'swing', complete: complete });
			else if (complete !== undefined)		complete.call(item);
		}
	}

	window.scrollToTopView = function (item)
	{
		//console.log("window.scrollToTopView called and scrollAtAll: " + (scrollAtAll === undefined ? "undefined" : scrollAtAll ? "true" : "false"))

		if(!scrollAtAll)
			return;

		$("html, body").animate({ scrollTop: item.offset().top  }, { duration: 'slow', easing: 'swing' });
	}

	window.pageDown = function ()	{ window.scrollBy(0,  window.innerHeight ); }
	window.pageUp   = function ()	{ window.scrollBy(0, -window.innerHeight); }

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

	window.removeAllAnalyses = function () {
		window.unselect();
		analyses.close();
		// Initialize view to defaults and re-render - Clears titles, notebox, etc.
		analyses = new JASPWidgets.Analyses({ className: "jasp-report" });
	}

	var ignoreSelectionProcess = function(target) {

		var stacktraceClicked = $(target).is(".stack-trace-span, .stack-trace-arrow, .stack-trace-selector");
		var noteClicked       = $(target).is(".jasp-notes, .jasp-notes *");
		var toolbarClicked    = $(target).is(".jasp-resize, .toolbar-clickable, .toolbar-clickable *");

		var ignoreSelection   = (wasLastClickNote === true && noteClicked === false) ||		// save the modified note
								stacktraceClicked === true ||								// toggle the stack trace
								toolbarClicked    === true;									// click on an analysis toolbar

		wasLastClickNote = noteClicked;

		return ignoreSelection;
	}

	window.unselectByClickingBody = function (event) {

		var target = event.target || event.srcElement;

		if (ignoreSelectionProcess(target) || selectedAnalysisId === -1 || $(target).is(".jasp-analysis *, .etch-editor-panel, .etch-editor-panel *") === true)
			return;

		window.unselect()
		jasp.analysisUnselected()

		if (showInstructions)
			hideInstructions()
	}

	var selectedHandler = function (event) {

		var target = event.target || event.srcElement;

		if (ignoreSelectionProcess(target) || (selectedAnalysisId === -1 && wasLastClickNote === true))
			return;

		var id = $(event.currentTarget).attr("id")
		var idAsInt = parseInt(id.substring(3))

		if (selectedAnalysisId !== idAsInt)
		{
			if (wasLastClickNote !== true) {
				window.select(idAsInt, false)
				jasp.analysisSelected(idAsInt)
			} else {
				window.unselect()
				jasp.analysisUnselected()
			}
		}
	}

	var analysisChangedDownstreamHandler = function (event, data) {

		jasp.analysisChangedDownstream(data.id, JSON.stringify(data.model))

	}

	window.analysisChanged = function (analysis) {

		if (showInstructions)
			$instructions.fadeIn(400, "easeOutCubic")

		// probably change something here
		var id = "id-" + analysis.id

		var spacer = $("#spacer")

		if (analyses.initialised === undefined) {

			analyses.initialised = true;

			analyses.on("toolbar:showMenu", function (obj, options) {

				jasp.showAnalysesMenu(JSON.stringify(options));

				window.menuObject = obj;
			});

			analyses.on("analyses:userDataChanged", function () {
				window.getResultsMeta()
			});

			analyses.render();

			analyses.$el.css("opacity", 0)
			spacer.before(analyses.$el);
			analyses.$el.animate({ "opacity": 1 }, 400, "easeOutCubic")
		}

		var jaspWidget = analyses.getAnalysis(analysis.id);
		if (jaspWidget == undefined)
		{
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

			jaspWidget.on("optionschanged",				function (id, options)	{ jasp.analysisChangedDownstream(id, JSON.stringify(options))	});
			jaspWidget.on("saveimage",					function (id, options)	{ jasp.analysisSaveImage(id, JSON.stringify(options))			});
			jaspWidget.on("editimage",					function (id, options)	{ jasp.analysisEditImage(id, JSON.stringify(options))			});
			jaspWidget.on("showDependencies",			function (id, optName)	{ jasp.showDependenciesInAnalysis(id, optName);					});
			jaspWidget.on("analysis:remove",			function (id)			{ jasp.removeAnalysisRequest(id);								});
			jaspWidget.on("analysis:duplicate",			function (id)			{ jasp.duplicateAnalysis(id);									});
			jaspWidget.on("analysis:userDataChanged",	function ()				{ window.getAllUserData();										});

			jaspWidget.on("toolbar:showMenu", function (obj, options) {

				jasp.showAnalysesMenu(JSON.stringify(options));
				window.menuObject = obj;
			});
		}
		else
			jaspWidget.model.set(analysis);

		jaspWidget.render();
	}

	$("#results").on("click", ".stack-trace-selector", function()
	{
		$(this).next(".stack-trace").slideToggle(function()
		{
			var $selectedInner = $(this).parent().siblings(".jasp-analysis");
			var errorBoxHeight = $(this).parent(".analysis-error-message").outerHeight();

			if ($(this).next(".stack-trace").is(":hidden"))
				$selectedInner.css("height", "");

			if ($selectedInner.height() < errorBoxHeight)
				$selectedInner.height(errorBoxHeight);

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
	delete savingImages[args.id];
}

var convertToBase64Id = 0;
var convertToBase64Images = {};

var convertToBase64Begin = function (path, callback, context) {
	var index = convertToBase64Id;
	convertToBase64Id += 1;
	convertToBase64Images[index] = {
		callback: callback,
		context: context
	}
	jasp.getImageInBase64(index, path);
}

window.convertToBase64Done = function (args) {
	var callbackData = convertToBase64Images[args.id];
	callbackData.callback.call(callbackData.context, args.result);
	delete convertToBase64Images[args.id];
}


window.resultsDocumentChanged = function () {
	jasp.resultsDocumentChanged();
}

window.displayWarningMessage = function (msg) {
	jasp.displayMessageFromResults(msg);
}

window.setTheme = function(themeName) {
	document.getElementById("style").setAttribute("href", "css/" + themeName + "-jasp.css");
}

window.setFontFamily = function(fontFamily) {
	document.body.style.fontFamily = fontFamily;
}
