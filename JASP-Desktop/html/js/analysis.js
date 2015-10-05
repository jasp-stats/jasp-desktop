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


JASPWidgets.NoteDetails = function (noteKey, path) {
	this.path = path === undefined ? [] : path;
	this.isRoot = this.path.length === 0;
	this.noteKey = noteKey;
	this.level = this.path.length;

	this.GetFullKey = function () {
		if (this.fullKey === undefined) {
			if (this.path.length === 0)
				this.fullKey = this.noteKey;
			else {
				this.fullKey = this.path.join('-');
				if (this.noteKey !== '')
					this.fullKey = this.fullKey + '-' + this.noteKey;
			}
		}

		return this.fullKey;
	};

	this.GetFullKeyArray = function () {

		if (this.fullKeyArray === undefined) {
			this.fullKeyArray = [];

			for (var i = 0; i < this.path.length; i++) {
				this.fullKeyArray.push(this.path[i]);
			}

			if (this.noteKey !== '')
				this.fullKeyArray.push(this.noteKey);
		}

		return this.fullKeyArray;
	};
}

JASPWidgets.AnalysisView = JASPWidgets.View.extend({
	views: [],
	volatileViews: [],

	initialize: function () {

		this.viewNotes = { list: [] };

		this.toolbar = new JASPWidgets.Toolbar({ className: "jasp-toolbar" })

		this.notes = this.model.get('notes');

		var firstNoteDetails = new JASPWidgets.NoteDetails('first');
		var firstNoteBox = this.getNoteBox(firstNoteDetails);
		
		var lastNoteDetails = new JASPWidgets.NoteDetails('last');
		var lastNoteBox = this.getNoteBox(lastNoteDetails);

		this.toolbar.setParent(this);

		

		this.model.on("CustomOptions:changed", function (options) {

			this.trigger("optionschanged", this.model.get("id"), options)
		}, this);
	},

	_setTitle: function (title, format) {

		this.viewNotes.lastNoteBox.ghostText = title + ' Conclusion - ' + this.viewNotes.lastNoteBox.ghostTextDefault;

		this.toolbar.title = title;
		this.toolbar.titleTag = format;
	},

	events: {
		'mouseenter': '_hoveringStart',
		'mouseleave': '_hoveringEnd',
	},
	
	detachNotes: function() {
		for (var i = 0; i < this.viewNotes.list.length; i++)
			this.viewNotes.list[i].widget.detach();
	},

	getNoteData: function (noteDetails) {

		var noteData = null;
		var obj = this.notes;
		if (obj !== null) {

			var noteData = null;
			var keyPath = noteDetails.GetFullKeyArray();
			for (var i = 0; i < keyPath.length; i++) {
				if (i < noteDetails.level) {
					obj = obj.others;
					if (obj === undefined)
						break;
				}

				if (i === keyPath.length - 1) {
					noteData = new JASPWidgets.Note(obj[keyPath[i]]);
				}
				else {
					obj = obj[keyPath[i]];
					if (obj === undefined)
						break;	
				}
			}
		}

		if (noteData === null)
			noteData = new JASPWidgets.Note();

		return noteData;
	},

	getNoteBox: function (noteDetails) {

		var noteData = this.getNoteData(noteDetails);

		var key = noteDetails.GetFullKey();

		var widget = this.viewNotes[key + 'NoteBox'];
		if (widget === undefined || widget === null) {
			widget = new JASPWidgets.NoteBox({ className: "jasp-notes jasp-" + key + "-note", model: noteData });
			this.viewNotes[key + 'NoteBox'] = widget;
			this.viewNotes.list.push({ noteDetails: noteDetails, widget: widget, note: noteData });

			this.listenTo(widget, "NoteBox:textChanged", function () {
				this.trigger("analysis:noteChanged", this.model.get('id'), key);
			});
		}

		return widget;
	},

	getNextView: function (view, name) {

		var next;
		if (view.views) {
			next = _.find(view.views, function (cv) { return cv.model.get('name') === name; });
			if (next === null)
				next = undefined;
		}

		return next;
	},

	getAnalysisNotes: function () {
		var notes = {
			id: this.model.get('id'),
			notes: {}
		};

		var results = this.model.get("results");
		for (var i = 0; i < this.viewNotes.list.length; i++) {
			var noteBoxData = this.viewNotes.list[i];
			var noteDetails = noteBoxData.noteDetails;

			if (noteBoxData.widget.visible === false)
				continue;

			var obj = notes.notes;

			var view = this;
			var fullKeyPath = noteDetails.GetFullKeyArray();
			for (j = 0; j < fullKeyPath.length; j++) {

				if (j < noteDetails.level) {

					if (obj.others === undefined)
						obj.others = {};

					obj = obj.others;
				}

				var levelName = fullKeyPath[j];

				if (j < fullKeyPath.length - 1) {
					view = this.getNextView(view, levelName)
					if (view === undefined)
						break;
				}

				var nextLevel = obj[levelName];
				if (nextLevel === undefined) {
					nextLevel = {};
					obj[levelName] = nextLevel
				}

				if (j === fullKeyPath.length - 1) {
					if (noteBoxData.widget.isTextboxEmpty())
						nextLevel.text = '';
					else
						nextLevel.text = Mrkdwn.fromHtmlText(noteBoxData.note.get('text'));
					nextLevel.format = 'markdown';
					nextLevel.visible = noteBoxData.widget.visible;
				}
				else 
					obj = nextLevel;
			}
		}

		return notes;
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

		var noteKeys = [''];
		if (itemView.avaliableNoteKeys)
			noteKeys = itemView.avaliableNoteKeys();

		for (var i = 0; i < noteKeys.length; i++) {
			var noteDetails = new JASPWidgets.NoteDetails(noteKeys[i], path)
			var noteBox = this.getNoteBox(noteDetails)
			itemView.setNoteBox(noteDetails.GetFullKey(), noteDetails.noteKey, noteBox);
		}
	},

	_hoveringStart: function (e) {
		this.toolbar.setVisibility(true);
	},

	_hoveringEnd: function (e) {
		this.toolbar.setVisibility(false);
	},

	notesMenuClicked: function (noteType, visibility) {

		var scrollIntoView = true;
		for (var i = 0; i < this.viewNotes.list.length; i++) {
			var noteBoxData = this.viewNotes.list[i];
			if (noteBoxData.noteDetails.level === 0) {
				var noteBox = noteBoxData.widget;
				if (noteBox.visible !== visibility) {
					noteBox.setVisibilityAnimate(visibility, scrollIntoView);
					scrollIntoView = false;
				}
			}
		}

		return true;
	},

	noteOptions: function () {
		var firstOpt = { key: 'all', menuText: 'Add Notes', visible: this.viewNotes.firstNoteBox.visible && this.viewNotes.lastNoteBox.visible };

		return [firstOpt];
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

	createChild: function (result, status, metaEntry) {

		var itemView = null;

//backwards compatibility//////////////////
		if (metaEntry.type == "title") {
			this.titleRequest = { title: result, titleFormat: 'h2' };
			this.labelRequest = null;
		}
		else if (metaEntry.type == "h1")
			this.labelRequest = { title: result, titleFormat: 'h3' };
		else if (metaEntry.type == "h2")
			this.labelRequest = { title: result, titleFormat: 'h4' };
		else {
		
			if (_.isArray(result)) {

				result = { collection: result };
				if (this.labelRequest) {
					result.title = this.labelRequest.title;
					result.titleFormat = this.labelRequest.titleFormat;
				}
				if (metaEntry.type === 'tables')
					metaEntry.meta = 'table';
				else if (metaEntry.type === 'images')
					metaEntry.meta = 'image';

				metaEntry.type = 'collection'
			}
			this.labelRequest = null;
///////////////////////////////////////////

			itemView = JASPWidgets.objectConstructor.call(this, result, { meta: metaEntry, status: status, childOfCollection: false, embeddedLevel: 1 }, false);
		}

		return itemView;
	},

	render: function () {

		this.toolbar.$el.detach();
		this.detachNotes();

		var $innerElement = this.$el;

		var $tempClone = $innerElement.clone();
		this.$el.before($tempClone).detach(); 

		this.destroyViews();

		this.views.push(this.viewNotes.firstNoteBox);

		$innerElement.empty();

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

				var name = meta[i].name;
				if (_.has(results, name)) {
					var itemView = this.createChild(results[name], this.model.get("status"), meta[i])
					if (itemView !== null) {
						this.passNoteObjToView([name], itemView);

						this.views.push(itemView);
						this.volatileViews.push(itemView);

						itemView.render();
						$innerElement.append(itemView.$el);
					}
				}
			}

		}

		if (this.titleRequest)
			this._setTitle(this.titleRequest.title, this.titleRequest.titleFormat);
		else
			this._setTitle(results.title, 'h2');

		this.viewNotes.lastNoteBox.render();
		$innerElement.append(this.viewNotes.lastNoteBox.$el);

		this.viewNotes.firstNoteBox.render();
		$innerElement.prepend(this.viewNotes.firstNoteBox.$el);

		this.toolbar.render();
		$innerElement.prepend(this.toolbar.$el);

		this.views.push(this.viewNotes.lastNoteBox);

		$tempClone.replaceWith($innerElement);
		$tempClone.empty();

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