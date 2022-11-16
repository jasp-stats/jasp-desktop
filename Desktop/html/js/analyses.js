
JASPWidgets.Analyses = JASPWidgets.View.extend({

	initialize: function () {

		this.analyses = [];
		this.views = [];

		this.toolbar = new JASPWidgets.Toolbar({ className: "jasp-toolbar jasp-title-toolbar jasp_top_level" })
		this.toolbar.setParent(this);

		this.toolbar.titleDefault	= "Results";
		this.toolbar.title			= this.toolbar.titleDefault;
		this.toolbar.titleTag		= "h1";

		this.note = new JASPWidgets.Note();
		this.noteBox = new JASPWidgets.NoteBox({ className: "jasp-notes jasp-main-note jasp_top_level", model: this.note });

		this.listenTo(this.noteBox, "NoteBox:textChanged", function () {
			this.trigger("analyses:userDataChanged");
		});

		this.views.push(this.noteBox);
	},

	setTitle: function(newTitle) {
		//console.log("analyses.setTitle("+newTitle+") was called and default title= " +this.toolbar.titleDefault + " while the old title is: " + this.toolbar.title);
		var wasDefault = this.toolbar.title === this.toolbar.titleDefault;
		this.toolbar.titleDefault = newTitle;

		if(wasDefault)
			this.toolbar.title = this.toolbar.titleDefault
	},

	menuName: 'All',

	close: function () {
		// Based on https://stackoverflow.com/questions/7379263/disposing-of-view-and-model-objects-in-backbone-js/7383008#7383008
		// this.$el.empty();
		this.remove();
		// this.unbind();
	},

	noteOptions: function () {
		var visible = this.noteBox.visible;
		var options = { key: 'main', visible: visible };

		return [options];
	},

	notesMenuClicked: function (noteType, visibility) {

		this.noteBox.setVisibilityAnimate(visibility);
	},

	copyMenuClicked: function () {
		var exportParams = new JASPWidgets.Exporter.params();
		exportParams.format = JASPWidgets.ExportProperties.format.html;
		exportParams.process = JASPWidgets.ExportProperties.process.copy;
		exportParams.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.temporary;
		exportParams.includeNotes = true;

		this.exportBegin(exportParams);

		return true;
	},

	editTitleClicked: function () {
		this.toolbar.startEdit(function(){ window.getResultsMeta(); });
	},

	events: {
		'mouseenter': '_hoveringStart',
		'mouseleave': '_hoveringEnd',
		'click': '_mouseClicked',
	},

	_hoveringStart: function (e) {
		this.toolbar.setVisibility(true);
	},

	_hoveringEnd: function (e) {
		this.toolbar.setVisibility(false);
	},

	_mouseClicked: function (e) {},

	addAnalysis: function(analysis) {
		this.analyses.push(analysis);
		this.views.push(analysis);

		analysis.$el.css("opacity", 0)
		this.$el.append(analysis.$el);
		analysis.$el.animate({ "opacity": 1 }, 400, "easeOutCubic")

		window.scrollToTopView(analysis.$el);
	},

	removeAnalysis: function (analysis) {

		analysis.$el.animate({ opacity: 0 }, 400, "easeOutCubic", function () {
			analysis.$el.slideUp(400, function () {
				// TODO: See why analysis is NULL here.
				// analysis.close();
				this.analyses = _.without(this.analyses, analysis);
				this.views = _.without(this.analyses, analysis);
			});
		});
	},

	removeAnalysisId: function (analysisId) {

		var analysis = this.getAnalysis(analysisId);
		if (analysis !== undefined) {
			this.removeAnalysis(analysis);
		}
	},

	getAnalysis: function(id) {
		return _.find(this.analyses, function (cv) { return cv.model.get("id") === id; });
	},

	reRender: function() {
		this.analyses.forEach(function(analysis) {analysis.render();});
	},

	move: function(fromId, toId) {
		var fromIndex = -1, toIndex = -1
		for (var i = 0; i < this.analyses.length; i++) {
			if (this.analyses[i].model.get("id") === fromId)
				fromIndex = i
			if (this.analyses[i].model.get("id") === toId)
				toIndex = i
		}
		if (fromIndex >= 0 && toIndex >= 0 && fromIndex != toIndex) {
			var fromAnalysis = this.analyses[fromIndex]
			var toAnalysis = this.analyses[toIndex]
			this.analyses.splice(toIndex, 0, this.analyses.splice(fromIndex, 1)[0])
			this.views.splice(toIndex + 1, 0, this.views.splice(fromIndex + 1, 1)[0])

			fromAnalysis.$el.slideUp(200, function () {
				if (toIndex > fromIndex)
					toAnalysis.$el.after(fromAnalysis.$el)
				else
					toAnalysis.$el.before(fromAnalysis.$el)
				fromAnalysis.$el.slideDown(200)
			})
		}
	},

	getAllUserData: function () {
		var notes = [];
		for (var i = 0; i < this.analyses.length; i++) {
			notes.push(this.analyses[i].getAllUserData());
		}
		return notes;
	},

	getResultsMeta: function () {

        // text: Mrkdwn.fromHtmlText(this.note.get('text')),
        // format: 'markdown',

		return {
			title:	this.toolbar.title,
			notes: {
				first: {
					text:			this.note.get('text'),
					format:			this.note.get('format'),
					visible:		this.noteBox.visible,
					delta:			this.note.get('delta'),
					deltaAvailable: this.note.get('deltaAvailable'),
				}
			}
		};
	},

	_toSubSubHtml: function(instr) {
		var out = "";
		var supsub = "";
		for (var x = 0; x < instr.length; x++)
		{
			var c = instr.charAt(x);
			switch (c)
			{
			case '<':
				supsub=instr.substring(x+1, x+5);
				if (supsub === "sup>" || supsub === "/sup" || supsub === "sub>" || supsub === "/sub")
					out+=c;
				else
					out+="&lt;";
				break;
			case '>':
				supsub=instr.substring(x-4, x);
				if (supsub === "<sup" || supsub === "/sup" || supsub === "<sub" || supsub === "/sub")
					out+=c;
				else
					out+="&gt;";
				break;
			default:
				out+=c;
				break;
			}
		}
		return out;
	},


	setResultsMeta: function (resultsNotes)
	{
		if(resultsNotes === null || !insideJASP)
			return; //null is default value in Analyses class

		var notes = resultsNotes['notes'];
		var title = resultsNotes['title'];
		var first = notes['first'];
		first['text'] = this._toSubSubHtml(first['text']);

		this.note.set(first);
		this.noteBox.setVisibility(first['visible']);
		this.toolbar.title = title;
		this.toolbar.render();
	},

	unselectAllAnalyses: function() {
		_.invoke(this.analyses, "unselect");
	},

	exportWrapper: function(html) {
		return '<div style="display:inline-block">' + html + '</div>';
	},

	exportBegin: function (exportParams, completedCallback) {

		if (exportParams == undefined)
			exportParams = new JASPWidgets.Exporter.params();
		else if (exportParams.error)
			return false;

		var callback = this.exportComplete;
		if (completedCallback !== undefined)
			callback = completedCallback;

		return JASPWidgets.Exporter.begin(this, exportParams, callback, true);
	},

	exportComplete: function (exportParams, exportContent) {
		if (!exportParams.error)
			pushHTMLToClipboard(exportContent, exportParams);
	},

	render: function () {

		//var $titleSpace = $('<div class="jasp-report-title"><div>');

		this.toolbar.render();
		this.$el.append(this.toolbar.$el);

		this.noteBox.render();
		this.$el.append(this.noteBox.$el);

		//this.$el.append($titleSpace);
	},

	onClose: function () {
		this.toolbar.close();
		this.noteBox.close();
	}
});
