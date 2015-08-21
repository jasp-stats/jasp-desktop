JASPWidgets.Analysis = Backbone.Model.extend({
	defaults: {
		id: -1,
		results: {},
		status: 'waiting',
		optionschanged: []
	}
});


JASPWidgets.Analyses = Backbone.Collection.extend({
	model: JASPWidgets.Analysis
});


JASPWidgets.AnalysisView = JASPWidgets.View.extend({
	views: [],
	volatileViews: [],

	initialize: function () {

		this.toolbar = new JASPWidgets.Toolbar({ className: "jasp-toolbar" })
		var self = this;
		this.toolbar.selectionElement = function () {
			return self.$el.find('.jasp-analysis-inner');
		};

		this.notes = this.model.get('notes');


		if (this.notes == null || this.notes.first === null)
			this.firstNote = new JASPWidgets.Note();
		else {
			this.firstNote = new JASPWidgets.Note(this.notes.first);
		}

		this.firstNoteBox = new JASPWidgets.NoteBox({ className: "jasp-notes jasp-first-note", model: this.firstNote })

		if (this.notes == null || this.notes.last === null)
			this.lastNote = new JASPWidgets.Note();
		else
			this.lastNote = new JASPWidgets.Note(this.notes.last);

		this.lastNoteBox = new JASPWidgets.NoteBox({ className: "jasp-notes jasp-last-note", model: this.lastNote })

		this.listenTo(this.firstNoteBox, "NoteBox:textChanged", function () {
			this.trigger("analysis:noteChanged", this.model.get('id'), 'first');
		});
		this.listenTo(this.lastNoteBox, "NoteBox:textChanged", function () {
			this.trigger("analysis:noteChanged", this.model.get('id'), 'last');
		});

		this.toolbar.setParent(this);
	},

	events: {
		'mouseenter': '_hoveringStart',
		'mouseleave': '_hoveringEnd',
	},

	getAnalysisNotes: function () {
		return {
			id: this.model.get('id'),
			notes: {
				first: { text: Mrkdwn.fromHtmlText(this.firstNote.get('text')), format: 'markdown', visible: this.firstNoteBox.visible },
				last: { text: Mrkdwn.fromHtmlText(this.lastNote.get('text')), format: 'markdown', visible: this.lastNoteBox.visible }
			}
		};
	},

	_hoveringStart: function (e) {
		this.toolbar.setVisibility(true);
	},

	_hoveringEnd: function (e) {
		this.toolbar.setVisibility(false);
	},

	notesMenuClicked: function (noteType, visibility) {

		var noteBox = this[noteType + 'NoteBox'];
		if (noteBox !== undefined) {
			noteBox.$el.css("opacity", visibility ? 0 : 1);

			if (visibility === true) {
				noteBox.$el.slideDown(200, function () {
					noteBox.setVisibility(visibility);
					if (visibility === true)
						noteBox.$el.animate({ "opacity": 1 }, 200, "easeOutCubic");
				});
			}
			else {
				noteBox.$el.slideUp(200, function () {
					noteBox.setVisibility(visibility);
				});
			}

			if (visibility === false)
				noteBox.clear();
		}

		return true;
	},

	noteOptions: function() {
		return [{ key: 'first', menuText: 'Before Analysis', visible: this.firstNoteBox.visible },
			{ key: 'last', menuText: 'After Analysis', visible: this.lastNoteBox.visible }];
	},

	copyMenuClicked: function () {
		
		var exportParams = new JASPWidgets.Exporter.params();
		exportParams.format = JASPWidgets.ExportProperties.format.html;
		exportParams.process = JASPWidgets.ExportProperties.process.copy;
		exportParams.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.temporary;

		return this.exportBegin(exportParams);
	},

	exportMenuClicked: function () {

		var exportParams = new JASPWidgets.Exporter.params();
		exportParams.format = JASPWidgets.ExportProperties.format.html;
		exportParams.process = JASPWidgets.ExportProperties.process.save;
		exportParams.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.embedded;

		return this.exportBegin(exportParams);
	},

	exportBegin: function (exportParams) {
		if (exportParams == undefined)
			exportParams = new JASPWidgets.Exporter.params();
		else if (exportParams.error)
			return false;

		return JASPWidgets.Exporter.begin(this, exportParams, true);
	},

	exportComplete: function (exportParams, exportContent) {

		if (!exportParams.error)
			pushHTMLToClipboard(exportContent, exportParams);
	},

	removeMenuClicked: function () {
		this.trigger("analysis:remove", this.model.get('id'));
	},

	menuName: "Analysis",

	renderChildren: function ($element, result, status, metaEntry) {

		if (metaEntry.type == "title") {

			this.toolbar.titleTag = "h1";
			this.toolbar.title = result;
			this.toolbar.render();
			$element.append(this.toolbar.$el);

			this.firstNoteBox.render();
			$element.append(this.firstNoteBox.$el);
		}
		else if (metaEntry.type == "h1") {

			$element.append("<h2>" + result + "</h2>")
		}
		else if (metaEntry.type == "h2") {

			$element.append("<h3>" + result + "</h3>")
		}
		else {

			if (!_.has(result, "status"))
				result.status = status;

			var item = new JASPWidgets[metaEntry.type](result);

			item.on("CustomOptions:changed", function (options) {

				this.trigger("optionschanged", this.model.get("id"), options)

			}, this);

			var itemView;
			if (_.isArray(result)) {

				item.each(function (subItem) {
					var status = subItem.get("status");
					if (status === null || status === undefined)
						subItem.set("status", this.status);
				}, result);
				itemView = new JASPWidgets[metaEntry.type + "View"]({ className: "jasp-" + metaEntry.type + " jasp-view", collection: item });
			}
			else
				itemView = new JASPWidgets[metaEntry.type + "View"]({ className: "jasp-" + metaEntry.type + " jasp-view", model: item });

			this.listenTo(itemView, "toolbar:showMenu", function (obj, options) {

				this.trigger("toolbar:showMenu", obj, options);
			});

			this.views.push(itemView);
			this.volatileViews.push(itemView);

			itemView.render();
			$element.append(itemView.$el);
		}

	},

	render: function () {

		this.destroyViews();

		this.toolbar.$el.detach();
		this.firstNoteBox.$el.detach();
		this.lastNoteBox.$el.detach();

		this.views.push(this.firstNoteBox);

		this.$el.empty();

		var $innerElement = $('<div class="jasp-analysis-inner"></div>')

		var results = this.model.get("results");
		if (results.error) {

			var error = results.errorMessage

			error = error.replace(/\n/g, '<br>')
			error = error.replace(/  /g, '&nbsp;&nbsp;')

			$innerElement.append('<div class="error-message-box ui-state-error"><span class="ui-icon ui-icon-alert" style="float: left; margin-right: .3em;"></span>' + error + '</div>')

		}
		else if (results[".meta"]) {

			var meta = results[".meta"]

			for (var i = 0; i < meta.length; i++) {

				if (_.has(results, meta[i].name))
					this.renderChildren($innerElement, results[meta[i].name], this.model.get("status"), meta[i])
			}

		}

		this.lastNoteBox.render();
		$innerElement.append(this.lastNoteBox.$el);

		this.$el.append($innerElement)

		this.views.push(this.lastNoteBox);

		return this;
	},

	unselect: function () {
		this.$el.removeClass("selected");
		//this.$el.addClass("unselected")
	},

	select: function () {
		this.$el.addClass("selected")
		//this.$el.removeClass("unselected");
	},

	destroyViews: function() {
		for (var i = 0; i < this.volatileViews.length; i++)
			this.volatileViews[i].close();

		this.volatileViews = [];
		this.views = [];
	},

	onClose: function () {
		this.destroyViews();
		this.toolbar.close();
		this.firstNoteBox.close();
		this.lastNoteBox.close();
	}
});