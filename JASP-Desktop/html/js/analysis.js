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

		this.viewNotes = { list: [] };

		this.toolbar = new JASPWidgets.Toolbar({ className: "jasp-toolbar" })
		//var self = this;
		//this.toolbar.selectionElement = function () {
		//	return self.$el.find('.jasp-analysis-inner');
		//};

		this.notes = this.model.get('notes');

		var path = ['first'];
		var firstNoteBox = this.createNoteBox(path, this.getNoteFromPath(path, true), true);
		
		path = ['last'];
		var lastNoteBox = this.createNoteBox(path, this.getNoteFromPath(path, true), true);

		this.toolbar.setParent(this);
	},

	events: {
		'mouseenter': '_hoveringStart',
		'mouseleave': '_hoveringEnd',
	},
	
	detachNotes: function() {
		for (var i = 0; i < this.viewNotes.list.length; i++)
			this.viewNotes.list[i].widget.detach();
	},

	createNoteBox: function (path, note, isRootNote) {

		if (note === null || note === undefined)
			note = new JASPWidgets.Note();

		if (isRootNote === undefined)
			isRootNote = false;

		var key = path.join('-');
		var widget = new JASPWidgets.NoteBox({ className: "jasp-notes jasp-" + key + "-note", model: note });
		this.viewNotes[key + 'NoteBox'] = widget;
		this.viewNotes.list.push({ path: path, key: key, widget: widget, note: note, isRootNote: isRootNote });

		this.listenTo(widget, "NoteBox:textChanged", function () {
			this.trigger("analysis:noteChanged", this.model.get('id'), key);
		});

		return widget;
	},

	getNextObject: function(obj, name) {
		if (Array.isArray(obj)) {
			var next = _.find(obj, function (cv) { return cv.name === name; });
			if (next === null)
				name = undefined;
		}
		else
			return obj[name];
	},

	getAnalysisNotes: function () {
		var notes = {
			id: this.model.get('id'),
			notes: {}
		};

		var results = this.model.get("results");
		for (var i = 0; i < this.viewNotes.list.length; i++) {
			var noteBoxData = this.viewNotes.list[i];
			var obj = notes.notes;
			if (noteBoxData.isRootNote === false) {
				
				if (notes.notes.others === undefined)
					notes.notes.others = {};

				obj = notes.notes.others;
			}

			var resultObj = results;
			for (j = 0; j < noteBoxData.path.length; j++) {
				var levelName = noteBoxData.path[j];

				if (j < noteBoxData.path.length - 1) {
					resultObj = this.getNextObject(resultObj, levelName)
					if (resultObj === undefined)
						break;
				}

				if (j === noteBoxData.path.length - 1)
					obj[levelName] = { text: Mrkdwn.fromHtmlText(noteBoxData.note.get('text')), format: 'markdown', visible: noteBoxData.widget.visible };
				else {
					var nextLevel = obj[levelName];
					if (nextLevel === undefined) {
						nextLevel = {};
						obj[levelName] = nextLevel
					}
					obj = nextLevel;
				}
			}
		}

		return notes;
	},

	getNoteFromPath: function (path, isRootNote) {

		if (this.notes === null)
			return null;

		var obj = this.notes;
		if (isRootNote === false)
			obj = this.notes.others;

		if (obj === undefined)
			return null;

		for (var i = 0; i < path.length; i++) {
			if (i === path.length - 1) {
				return new JASPWidgets.Note(obj[path[i]]);
			}

			obj = obj[path[i]];
			if (obj === undefined)
				return null;
		}
	},

	passNoteObjToView: function (path, itemView) {

		if (itemView.views !== undefined) {
			for (var i = 0; i < itemView.views.length; i++) {
				var subView = itemView.views[i]
				var name = subView.model.get('name');
				if (name !== null)
					this.passNoteObjToView(path.concat([name]), subView);
				else 
					throw "there must be a name parameter."
			}
		}

		if (itemView.hasNotes === undefined || itemView.hasNotes() === false)
			return;

		var key = path.join('-');

		var noteBox = this.viewNotes[key + 'NoteBox'];
		if (noteBox === undefined || noteBox === null) {

			noteBox = this.createNoteBox(path, this.getNoteFromPath(path, false), false)
			//noteBox.ghostText = 'Write notes here - ' + itemView.menuName;
		}

		itemView.setNoteBox(key, noteBox);
	},

	_hoveringStart: function (e) {
		this.toolbar.setVisibility(true);
	},

	_hoveringEnd: function (e) {
		this.toolbar.setVisibility(false);
	},

	notesMenuClicked: function (noteType, visibility) {

		var noteBox = this.viewNotes[noteType + 'NoteBox'];
		if (noteBox !== undefined) {
			noteBox.setVisibilityAnimate(visibility);
			return true;
		}
		return false;
	},

	noteOptions: function () {
		var firstOpt = { key: 'first', menuText: 'Add Note Before', visible: this.viewNotes.firstNoteBox.visible };
		if (this.viewNotes.firstNoteBox.visible)
			firstOpt.menuText = 'Remove Note Before';

		var lastOpt = { key: 'last', menuText: 'Add Note After', visible: this.viewNotes.lastNoteBox.visible };
		if (this.viewNotes.lastNoteBox.visible)
			lastOpt.menuText = 'Remove Note After';

		return [firstOpt, lastOpt];
	},

	copyMenuClicked: function () {
		
		var exportParams = new JASPWidgets.Exporter.params();
		exportParams.format = JASPWidgets.ExportProperties.format.html;
		exportParams.process = JASPWidgets.ExportProperties.process.copy;
		exportParams.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.temporary;
		exportParams.includeNotes = true;

		return this.exportBegin(exportParams);
	},

	exportMenuClicked: function () {

		var exportParams = new JASPWidgets.Exporter.params();
		exportParams.format = JASPWidgets.ExportProperties.format.html;
		exportParams.process = JASPWidgets.ExportProperties.process.save;
		exportParams.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.embedded;
		exportParams.includeNotes = true;

		return this.exportBegin(exportParams);
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

	removeMenuClicked: function () {
		this.trigger("analysis:remove", this.model.get('id'));
	},

	menuName: "Analysis",

	renderChildren: function ($element, result, status, metaEntry) {

		if (metaEntry.type == "title") {

			this.toolbar.titleTag = "h2";
			this.toolbar.title = result;
			this.toolbar.render();
			$element.append(this.toolbar.$el);

			this.viewNotes.firstNoteBox.render();
			$element.append(this.viewNotes.firstNoteBox.$el);
		}
		else if (metaEntry.type == "h1") {

			$element.append("<h3>" + result + "</h3>")
		}
		else if (metaEntry.type == "h2") {

			$element.append("<h4>" + result + "</h4>")
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

			this.passNoteObjToView([metaEntry.name], itemView);

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

		this.toolbar.$el.detach();
		this.detachNotes();

		this.destroyViews();

		this.views.push(this.viewNotes.firstNoteBox);

		this.$el.empty();

		var $innerElement = this.$el;//$('<div class="jasp-analysis-inner"></div>')

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

		this.viewNotes.lastNoteBox.render();
		$innerElement.append(this.viewNotes.lastNoteBox.$el);

		//this.$el.append($innerElement)

		this.views.push(this.viewNotes.lastNoteBox);

		return this;
	},

	unselect: function () {
		this.$el.removeClass("selected");
	},

	select: function () {
		this.$el.addClass("selected")
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

		for (var i = 0; i < this.viewNotes.list.length; i++)
			this.viewNotes.list[i].widget.close();
	}
});