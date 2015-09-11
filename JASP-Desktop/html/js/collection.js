JASPWidgets.collection = Backbone.Model.extend({
	defaults: {
		title: '',
		titleFormat: 'h3',
		collection: [],
	}
});

JASPWidgets.collectionView = JASPWidgets.View.extend({

	/** Initialises the base class property and creates the views from the model collection. */
	initialize: function () {
		this._collectionViewBase = JASPWidgets.View.prototype;
		this._collectionViewBase.initialize.call(this);

		this.toolbar = new JASPWidgets.Toolbar({ className: "jasp-toolbar" })
		this.toolbar.setParent(this);

		this.views = [];
		this.localViews = [];
	},

	setNoteBox: function (key, localKey, noteBox) {
		this.noteBox = noteBox;
		this.noteBoxKey = key;
		this.views.unshift(noteBox);
	},

	notesMenuClicked: function (noteType, visibility) {

		this.noteBox.setVisibilityAnimate(visibility);
		return true;
	},

	noteOptions: function () {
		var options = { key: this.noteBoxKey, menuText: 'Add Note', visible: this.noteBox.visible };

		return [options];
	},

	hasNotes: function () {
		return this.model.get('name') !== null;
	},

	events: {
		'mouseenter': '_hoveringStart',
		'mouseleave': '_hoveringEnd',
	},

	_hoveringStart: function (e) {
		this.toolbar.setVisibility(true);
	},

	_hoveringEnd: function (e) {
		this.toolbar.setVisibility(false);
	},


	eventEcho: function (eventName, arg1, arg2, arg3, arg4, arg5, arg6, arg7) {
		this.trigger(eventName, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
	},


	setObjectConstructor: function (constructor, data) {

		this.collection = this.model.get('collection');

		_.each(this.collection, function (itemResults) {
			var itemView = constructor.call(this, itemResults, data, true);
			if (itemView !== null) {
				itemView.inCollection = true;

				if (itemView.resizer)
					this.listenTo(itemView.resizer, "ResizeableView:resizeStart", this.onResizingStart);

				this.listenTo(itemView, "all", this.eventEcho)
				this.views.push(itemView);
				this.localViews.push(itemView);
			}
		}, this);
	},

	hasViews: function () {
		return this.localViews.length > 0;
	},

	onResizingStart: function (w, h) {
		for (var i = 0; i < this.localViews.length; i++) {
			var itemView = this.localViews[i];
			if (itemView.resizer) {
				if (itemView.resizer.isMouseResizing()) {
					this.listenTo(itemView.resizer, "ResizeableView:resized", this.onResized);
					this.listenTo(itemView.resizer, "ResizeableView:viewResized", this.onViewResized);
				}
				else
					itemView.resizer.resizeStart(w, h, true);
			}
		}
	},

	onViewResized: function (w, h) {
		for (var i = 0; i < this.localViews.length; i++) {
			var itemView = this.localViews[i];
			if (itemView.resizer) {
				if (!itemView.resizer.isMouseResizing())
					itemView.resizer.resizeView(w, h);
			}
		}
	},

	onResized: function (w, h) {
		for (var i = 0; i < this.localViews.length; i++) {
			var itemView = this.localViews[i];
			if (itemView.resizer) {
				if (itemView.resizer.isMouseResizing()) {
					this.stopListening(itemView.resizer, "ResizeableView:resized", this.onResized);
					this.stopListening(itemView.resizer, "ResizeableView:viewResized", this.onViewResized);
				}
				else
					itemView.resizer.resizeStop(w, h);
			}
		}
	},

	/**
	* Renders an item view.
	* @param {Backbone.View} itemView - The view to be rendered.
	*/
	onItemRender: function (itemView) {
		itemView.render();
	},

	/** Renders the collection view. */
	render: function () {

		var title = this.model.get("title")
		var titleFormat = this.model.get("titleFormat")
		if (title) {
			this.toolbar.title = title;
			if (titleFormat)
				titleFormat = "h3";
			this.toolbar.titleTag = titleFormat;
		}

		this.toolbar.render();
		this.$el.append(this.toolbar.$el);

		for (var i = 0; i < this.views.length; i++) {
			var itemView = this.views[i];
			this.onItemRender(itemView);
			this.$el.append(itemView.$el);
		}


	},

	/** Cleans up views when collection is closed. */
	onClose: function () {
		for (var i = 0; i < this.localViews.length; i++) {
			this.localViews[i].close();
		}
		this.views = [];
		this.localViews = [];

		this.toolbar.close();
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

	exportUseNBSPDefault: true,

	exportBegin: function (exportParams, completedCallback) {
		if (exportParams == undefined)
			exportParams = new JASPWidgets.Exporter.params();
		else if (exportParams.error)
			return false;

		var callback = this.exportComplete;
		if (completedCallback !== undefined)
			callback = completedCallback;

		if (this.views.length > 0)
			JASPWidgets.Exporter.begin(this, exportParams, callback, this.exportUseNBSPDefault);
		else
			callback.call(this, exportParams, new JASPWidgets.Exporter.data(null, ""));

		return true;
	},

	exportComplete: function (exportParams, exportContent) {
		if (!exportParams.error)
			pushHTMLToClipboard(exportContent, exportParams);
	}
});