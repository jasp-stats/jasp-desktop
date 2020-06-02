JASPWidgets.Analysis = Backbone.Model.extend({
	defaults: {
		id: -1,
		progress: -1,
		results: {},
		status: 'waiting',
		optionschanged: [],
		saveimage: [],
		editimage: []
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

		this.toolbar = new JASPWidgets.Toolbar({ className: "jasp-toolbar  jasp-title-toolbar" })

		var progressbarModel = new JASPWidgets.Progressbar({analysis: this});
		this.progressbar = new JASPWidgets.ProgressbarView({ model: progressbarModel });

		this.imageBeingEdited = null;


		this.userdata = this.model.get('userdata');
		if (this.userdata === undefined || this.userdata === null)
			this.userdata = {};

		var firstNoteDetails	= new JASPWidgets.DataDetails('firstNote');
		var firstNoteBox		= this.getNoteBox(firstNoteDetails);
		var lastNoteDetails		= new JASPWidgets.DataDetails('lastNote');
		var lastNoteBox			= this.getNoteBox(lastNoteDetails);

		this.toolbar.setParent(this);

		this.model.on("analysis:resizeStarted",		function (image)			{ this.imageBeingEdited = image	},																					this);
		this.model.on("CustomOptions:changed",		function (options)			{											this.trigger("optionschanged",		this.model.get("id"), options)	},	this);
		this.model.on("SaveImage:clicked",			function (options)			{											this.trigger("saveimage",			this.model.get("id"), options)	},	this);
		this.model.on("EditImage:clicked",			function (image, options)	{ this.imageBeingEdited = image;			this.trigger("editimage",			this.model.get("id"), options)	},	this);
		this.model.on("ShowDependencies:clicked",	function (optName)			{											this.trigger("showDependencies",	this.model.get("id"), optName)	},	this);

		this.$el.on("changed:userData",	this, this.onUserDataChanged);
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

		self.trigger("analysis:userDataChanged");
	},

	_setTitle: function (title, format) {

		this.viewNotes.firstNoteNoteBox.ghostText = title + ' - Introduction: ' + this.viewNotes.firstNoteNoteBox.ghostTextDefault;
		this.viewNotes.lastNoteNoteBox.ghostText  = title + ' - Conclusion: '   + this.viewNotes.lastNoteNoteBox.ghostTextDefault;

		this.toolbar.title = title;
		this.toolbar.titleTag = format;
	},

	events: {
		'mouseenter': '_hoveringStart',
		'mouseleave': '_hoveringEnd',
	},

	undoImageResize: function() {
		if (this.imageBeingEdited !== null)
			this.imageBeingEdited.restoreSize();
	},

	insertNewImage: function(imageEditResults) {
		if (this.imageBeingEdited !== null) {
			if ("revision" in imageEditResults)
				this.imageBeingEdited.setRevision(imageEditResults["revision"]);

			this.imageBeingEdited.reRender();
		}
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
			widget = new JASPWidgets.NoteBox({ className: "jasp-display-primitive jasp-notes jasp-" + key + "-note", model: noteData });
			this.viewNotes[key + 'NoteBox'] = widget;
			this.viewNotes.list.push({ noteDetails: noteDetails, widget: widget, note: noteData });

			this.listenTo(widget, "NoteBox:textChanged", function () {
				this.trigger("analysis:userDataChanged");
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
				firstNoteData.text = this.viewNotes.firstNoteNoteBox.model.get('text');

			firstNoteData.format = 'html';
			firstNoteData.deltaAvailable = this.viewNotes.firstNoteNoteBox.model.get('deltaAvailable');
			firstNoteData.delta = this.viewNotes.firstNoteNoteBox.model.get('delta');

			firstNoteData.visible = this.viewNotes.firstNoteNoteBox.visible;

			userData.firstNote = firstNoteData;

			hasData = true;
		}

		if (this.viewNotes.lastNoteNoteBox.visible) {

			var lastNoteData = {};

			if (this.viewNotes.lastNoteNoteBox.isTextboxEmpty())
				lastNoteData.text = '';
			else
				lastNoteData.text = this.viewNotes.lastNoteNoteBox.model.get('text');
				// lastNoteData.text = Mrkdwn.fromHtmlText(this.viewNotes.lastNoteNoteBox.model.get('text'));

				lastNoteData.format = 'html';
			lastNoteData.visible = this.viewNotes.lastNoteNoteBox.visible;
			lastNoteData.deltaAvailable = this.viewNotes.lastNoteNoteBox.model.get('deltaAvailable');
			lastNoteData.delta = this.viewNotes.lastNoteNoteBox.model.get('delta');

			userData.lastNote = lastNoteData;

			hasData = true;
		}

		if (hasData)
			return userData;
		else
			return null;
	},

   hasCitation: function () {

	   var optCitation = this.model.get("citation");

	   if(optCitation !== null)
		   return true;

		if (item.views)
			for (var i = 0; i < item.views.length; i++)
			{
				var child = item.views[i];
				if(child.model.get('citation') !== null)
						return true;
			}

		return false;
   },

   citeMenuClicked: function () {
	   var exportParams = new JASPWidgets.Exporter.params();
	   exportParams.format = JASPWidgets.ExportProperties.format.html;
	   exportParams.process = JASPWidgets.ExportProperties.process.copy;
	   exportParams.htmlImageFormat = JASPWidgets.ExportProperties.htmlImageFormat.temporary;
	   exportParams.includeNotes = false;

	   var resultsModel = this.model.get("results");
	   var optCitation = resultsModel === undefined ? undefined : resultsModel.citation;

	   if(optCitation === null || optCitation === undefined)
		   optCitation = []

	   if (this.views)
		   for (var i = 0; i < this.views.length; i++)
				if(this.views[i].getCitations)
					optCitation = optCitation.concat(this.views[i].getCitations())

	   var htmlCite = '<p>' + optCitation.join("</p><p>") + '</p>';

	   var exportContent = new JASPWidgets.Exporter.data(optCitation.join("\n\n"), htmlCite);

	   pushTextToClipboard(exportContent, exportParams);
	   return true;
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
		//if (itemView.avaliableNoteKeys) //Commented out because it wasn't defined anywhere so it probably isn't functional...
		//	noteKeys = itemView.avaliableNoteKeys();

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
		var firstOpt = { key: 'all', visible: this.viewNotes.firstNoteNoteBox.visible && this.viewNotes.lastNoteNoteBox.visible };

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

   duplicateMenuClicked: function () {
	   this.trigger("analysis:duplicate", this.model.get('id'));
   },

	menuName: "Analysis",

	createResultsViewFromMeta: function (results, resultsMeta, $result) {
		for (let i = 0; i < resultsMeta.length; i++) {

			let meta = resultsMeta[i];
			let name = meta.name;

			if (!_.has(results, name))
				continue
			let data = results[name];

			/*if (meta.type == 'collection' && data.title == "") {  // remove collections without a title from view
				let collectionMeta = meta.meta;
				if (Array.isArray(collectionMeta)) { // the meta comes from a jaspResult analysis
					this.createResultsViewFromMeta(data["collection"], collectionMeta, $result);
					continue;
				}
			}*/

			let itemView = this.createChild(data, this.model.get("status"), meta);
			if (itemView === null)
				continue;

			this.passUserDataToView([name], itemView);

			this.views.push(itemView);
			this.volatileViews.push(itemView);

			itemView.render();
			$result.append(itemView.$el);

		}
	},

	setErrorOnPreviousResults: function (errorMessage, status, $lastResult, $result) {
		if (errorMessage == null) // parser.parse() in the engine was unable to parse the R error message
			errorMessage = "An unknown error occurred.";

		errorMessage = errorMessage.replace(/\n/g, '<br>');
		errorMessage = errorMessage.replace(/  /g, '&nbsp;&nbsp;');

		$lastResult.removeClass("unselected selected");

		if ($lastResult.hasClass("error-state"))
			$result.append($lastResult.find(".jasp-analysis").not(".error-state").clone())
		else
			$result.append($lastResult.clone());

		$result.find(".status").removeClass("waiting running");
		$result.find(".error-message-box").remove();
		$result.addClass('error-state');

		$result.append('<div class="' + status + ' analysis-error-message error-message-box ui-state-error"><span class="ui-icon ui-icon-' + (status === "fatalError" ? 'alert' : 'info') + '" style="float: left; margin-right: .3em;"></span>' + errorMessage + '</div>');
	},

	setHeightErroredAnalysis: function ($result) {
		// the error box has an absolute position and unknown height, we need to manually verify the container height
		var $selectedAnalysis = $result.find(".jasp-analysis");
		var errorBoxHeight = $result.find(".analysis-error-message").outerHeight();
		if ($selectedAnalysis.height() < errorBoxHeight)
			$selectedAnalysis.height(errorBoxHeight);
	},

	updateProgressbarInResults: function() {
		this.progressbar.render();
		this.$el.find(".jasp-progressbar-container").replaceWith(this.progressbar.$el);
	},

	editTitleClicked: function () {
		var id = this.model.get("id");
		this.toolbar.startEdit(function(title) { jasp.analysisTitleChangedInResults(id, title); } );
	},

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

	overwriteUserData: function(userdata) {
		this.userdata = userdata;
		console.log("New userdata: %o", this.userdata);
		var firstNote = this.viewNotes.firstNoteNoteBox
		var lastNote = this.viewNotes.lastNoteNoteBox
		var newList = []
		for (var i = 0; i < this.viewNotes.list.length; i++) {
			if (this.viewNotes.list.widget === firstNote || this.viewNotes.list.widget === lastNote)
				newList.push(this.viewNotes.list[i])
		}

		this.viewNotes = { list: newList, firstNoteNoteBox: firstNote, lastNoteNoteBox: lastNote };
	},

	render: function () {

		var results = this.model.get("results");

		// once everything becomes jaspResults this is always an object and the following can be removed
		var progress = this.model.get("progress")
		if (typeof progress == "number")
			this.model.set("progress", { value: progress, label: "" })
		else if (!progress)
			this.model.set("progress", { value: -1, label: "" })
		// up to here

		if (results == "" || results == null) {
			progress = this.model.get("progress");
			if (progress.value > -1)
				this.updateProgressbarInResults();
			return this;
		}

		this.imageBeingEdited = null;

		this.toolbar.$el.detach();
		this.detachNotes();

		var $innerElement = this.$el;
		$innerElement.find(".jasp-progressbar-container").remove();

		var $tempClone = $innerElement.clone();
		this.$el.before($tempClone).detach();

		this.destroyViews();

		this.views.push(this.viewNotes.firstNoteNoteBox);

		$innerElement.empty();

		if (!results.error) {
			$innerElement.removeClass("error-state");
			meta = results[".meta"]
			if (meta)
				this.createResultsViewFromMeta(results, meta, $innerElement);
		} else {
			var status = this.model.get("status");
			var errorMessage = results.errorMessage
			this.setErrorOnPreviousResults(errorMessage, status, $tempClone, $innerElement);
		}

		if (this.titleRequest)
			this._setTitle(this.titleRequest.title, this.titleRequest.titleFormat);
		else
			this._setTitle(results.title, 'h2');

		this.progressbar.render();
		$innerElement.prepend(this.progressbar.$el);

		this.viewNotes.lastNoteNoteBox.render();
		$innerElement.append(this.viewNotes.lastNoteNoteBox.$el);

		this.viewNotes.firstNoteNoteBox.render();
		$innerElement.prepend(this.viewNotes.firstNoteNoteBox.$el);

		this.toolbar.setStatus(this.model.get("status"));
		this.toolbar.render();
		$innerElement.prepend(this.toolbar.$el);

		this.views.push(this.viewNotes.lastNoteNoteBox);

		$tempClone.replaceWith($innerElement);
		$tempClone.empty();

		if (results.error)
			this.setHeightErroredAnalysis($innerElement);

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
