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


JASPWidgets.DataDetails = function (dataKey, path) {
	this.path = path === undefined ? [] : path;
	this.isRoot = this.path.length === 0;
	this.dataKey = dataKey;
	this.level = this.path.length;

	this.GetFullKey = function () {
		if (this.fullKey === undefined) {
			if (this.path.length === 0)
				this.fullKey = this.dataKey;
			else {
				this.fullKey = this.path.join('-');
				if (this.dataKey !== '')
					this.fullKey = this.fullKey + '-' + this.dataKey;
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

			if (this.dataKey !== '')
				this.fullKeyArray.push(this.dataKey);
		}

		return this.fullKeyArray;
	};

	this.GetExtended = function (dataKey) {

		var extendedDetails = new JASPWidgets.DataDetails(dataKey, this.path)

		var fullKey = this.GetFullKey();
		if (fullKey === '') 
			fullKey = dataKey;
		else
			fullKey = fullKey + '-' + dataKey;

		extendedDetails.fullKey = fullKey;

		return extendedDetails;
	};
}

JASPWidgets.AnalysisView = JASPWidgets.View.extend({
	views: [],
	volatileViews: [],

	initialize: function () {

		this.viewNotes = { list: [] };

		this.toolbar = new JASPWidgets.Toolbar({ className: "jasp-toolbar" })

		this.userdata = this.model.get('userdata');
		if (this.userdata === undefined || this.userdata === null)
			this.userdata = {};

		var firstNoteDetails = new JASPWidgets.DataDetails('firstNote');
		var firstNoteBox = this.getNoteBox(firstNoteDetails);
		
		var lastNoteDetails = new JASPWidgets.DataDetails('lastNote');
		var lastNoteBox = this.getNoteBox(lastNoteDetails);

		this.toolbar.setParent(this);	

		this.model.on("CustomOptions:changed", function (options) {

			this.trigger("optionschanged", this.model.get("id"), options)
		}, this);

		this.$el.on("changed:userData", this, this.onUserDataChanged);
	},

	onUserDataChanged: function (event, details, dataValues) {
		var self = event.data;
		if (self === null)
			return;

		if (dataValues !== undefined) {
			for (var i = 0; i < dataValues.length; i++) {
				var pair = dataValues[i];
				self.setData(details, pair.key, pair.value, self.userdata);
			}
		}
		self.trigger("analysis:userDataChanged", self.model.get('id'));
	},

	_setTitle: function (title, format) {

		this.viewNotes.lastNoteNoteBox.ghostText = title + ' Conclusion - ' + this.viewNotes.lastNoteNoteBox.ghostTextDefault;

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

	setData: function (dataDetails, key, value, rootDataNode) {
		if (rootDataNode === null)
			return;

		var dataObject = this.getData(dataDetails, rootDataNode);
		if (dataObject === null) {

			var keyPath = dataDetails.GetFullKeyArray();
			dataObject = rootDataNode;
			for (var i = 0; i < keyPath.length; i++) {
				if (i < dataDetails.level) {

					if (dataObject.children === undefined)
						dataObject.children = {};
					dataObject = dataObject.children;
				}

				var name = keyPath[i];

				var node = dataObject[name];
				if (node === undefined)
					dataObject[name] = {};

				dataObject = dataObject[name];
			}
		}

		dataObject[key] = value;
	},

	getData: function (dataDetails, rootDataNode) {

		var data = null;
		if (rootDataNode !== null) {

			var keyPath = dataDetails.GetFullKeyArray();
			for (var i = 0; i < keyPath.length; i++) {
				if (i < dataDetails.level) {
					rootDataNode = rootDataNode.children;
					if (rootDataNode === undefined)
						break;
				}

				if (i === keyPath.length - 1) {
					data = rootDataNode[keyPath[i]];
				}
				else {
					rootDataNode = rootDataNode[keyPath[i]];
					if (rootDataNode === undefined)
						break;	
				}
			}
		}

		if (data === undefined)
			return null;

		return data;
	},

	getNoteBox: function (noteDetails) {

		var noteData = this.getData(noteDetails, this.userdata);

		if (noteData === null)
			noteData = new JASPWidgets.Note();

		noteData = new JASPWidgets.Note(noteData);

		var key = noteDetails.GetFullKey();

		var widget = this.viewNotes[key + 'NoteBox'];
		if (widget === undefined || widget === null) {
			widget = new JASPWidgets.NoteBox({ className: "jasp-display-primative jasp-notes jasp-" + key + "-note", model: noteData });
			this.viewNotes[key + 'NoteBox'] = widget;
			this.viewNotes.list.push({ noteDetails: noteDetails, widget: widget, note: noteData });

			this.listenTo(widget, "NoteBox:textChanged", function () {
				this.trigger("analysis:userDataChanged", this.model.get('id'), key);
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

	getAllUserData: function () {

		var getUserData = function (item) {

			var hasData = true;

			var userData = null;

			if (item.getLocalUserData)
				userData = item.getLocalUserData();

			if (userData === null) {
				userData = {};
				hasData = false;
			}

			if (item.views) {
				for (var i = 0; i < item.views.length; i++) {
					var child = item.views[i];
					var childData = getUserData(child);
					if (childData !== null) {
						var name = child.model.get('name');

						if (userData.children === undefined)
							userData.children = {};

						userData.children[name] = childData;
						hasData = true;
					}
				}
			}

			if (hasData)
				return userData;

			return null
		}

		var data = {
			id: this.model.get('id'),
			userdata: getUserData(this)
		};

		return data;
	},

	getLocalUserData: function () {

		var hasData = false;

		var userData = {};

		if (this.viewNotes.firstNoteNoteBox.visible) {

			var firstNoteData = {};

			if (this.viewNotes.firstNoteNoteBox.isTextboxEmpty())
				firstNoteData.text = '';
			else
				firstNoteData.text = Mrkdwn.fromHtmlText(this.viewNotes.firstNoteNoteBox.model.get('text'));
			firstNoteData.format = 'markdown';
			firstNoteData.visible = this.viewNotes.firstNoteNoteBox.visible;

			userData.firstNote = firstNoteData;

			hasData = true;
		}

		if (this.viewNotes.lastNoteNoteBox.visible) {

			var lastNoteData = {};

			if (this.viewNotes.lastNoteNoteBox.isTextboxEmpty())
				lastNoteData.text = '';
			else
				lastNoteData.text = Mrkdwn.fromHtmlText(this.viewNotes.lastNoteNoteBox.model.get('text'));
			lastNoteData.format = 'markdown';
			lastNoteData.visible = this.viewNotes.lastNoteNoteBox.visible;

			userData.lastNote = lastNoteData;

			hasData = true;
		}

		if (hasData)
			return userData;
		else
			return null;
	},

	passUserDataToView: function (path, itemView) {

		if (itemView.views !== undefined) {
			for (var i = 0; i < itemView.views.length; i++) {
				var subView = itemView.views[i]
				var name = subView.model.get('name');
				if (name !== null)
					this.passUserDataToView(path.concat([name]), subView);
				else 
					throw "there must be a name parameter."
			}
		}

		var dataDetails = new JASPWidgets.DataDetails("", path);
		if (itemView.setUserData)
			itemView.setUserData(dataDetails, this.getData(dataDetails, this.userdata));

		if (itemView.hasNotes === undefined || itemView.hasNotes() === false)
			return;

		var noteKeys = ['note'];
		if (itemView.avaliableNoteKeys)
			noteKeys = itemView.avaliableNoteKeys();

		for (var i = 0; i < noteKeys.length; i++) {
			var noteKey = noteKeys[i];
			var noteDetails = dataDetails.GetExtended(noteKey)
			var noteBox = this.getNoteBox(noteDetails)
			itemView.setNoteBox(noteDetails.GetFullKey(), noteKey, noteBox);
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
		var firstOpt = { key: 'all', menuText: 'Add Notes', visible: this.viewNotes.firstNoteNoteBox.visible && this.viewNotes.lastNoteNoteBox.visible };

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

			itemView = JASPWidgets.objectConstructor.call(this, result, { meta: metaEntry, status: status, childOfCollection: false, embeddedLevel: 1, indent: false }, false);
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

		this.views.push(this.viewNotes.firstNoteNoteBox);

        $innerElement.empty();

		var results = this.model.get("results");
        if (results.error) {

            var status = this.model.get("status");
			var error = results.errorMessage

			error = error.replace(/\n/g, '<br>')
			error = error.replace(/  /g, '&nbsp;&nbsp;')

            $innerElement.append($tempClone.clone());
            $innerElement.find('.analysis-error').remove();
            $innerElement.addClass('error-state');
            if (status === "exception") $innerElement.addClass("exception");
            $innerElement.find(".status").removeClass("waiting");

            $innerElement.append('<div class="analysis-error error-message-box ui-state-error"><span class="ui-icon ui-icon-' + (status === "exception" ? 'alert' : 'info') + '" style="float: left; margin-right: .3em;"></span>' + error + '</div>')
            if ($innerElement.find('.jasp-display-item').length > 3) {
                $innerElement.find('.analysis-error').addClass('analysis-error-top-max');
            }

		}
        else
        {
            $innerElement.removeClass("error-state");
            if (results[".meta"]) {

                var meta = results[".meta"]

                for (var i = 0; i < meta.length; i++) {

                    var name = meta[i].name;
                    if (_.has(results, name)) {
                        var itemView = this.createChild(results[name], this.model.get("status"), meta[i])
                        if (itemView !== null) {
                            this.passUserDataToView([name], itemView);

                            this.views.push(itemView);
                            this.volatileViews.push(itemView);

                            itemView.render();
                            $innerElement.append(itemView.$el);
                        }
                    }
                }
            }

		}

		if (this.titleRequest)
			this._setTitle(this.titleRequest.title, this.titleRequest.titleFormat);
		else
			this._setTitle(results.title, 'h2');

		this.viewNotes.lastNoteNoteBox.render();
		$innerElement.append(this.viewNotes.lastNoteNoteBox.$el);

		this.viewNotes.firstNoteNoteBox.render();
		$innerElement.prepend(this.viewNotes.firstNoteNoteBox.$el);

		this.toolbar.render();
		$innerElement.prepend(this.toolbar.$el);

		this.views.push(this.viewNotes.lastNoteNoteBox);

		$tempClone.replaceWith($innerElement);
		$tempClone.empty();
		
		var errorBoxHeight = $innerElement.find(".analysis-error").outerHeight(true);
		var $selectedAnalysis = $innerElement.find(".jasp-analysis");
		if ($selectedAnalysis.height() < errorBoxHeight) {
			$selectedAnalysis.height(errorBoxHeight);
		}

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
