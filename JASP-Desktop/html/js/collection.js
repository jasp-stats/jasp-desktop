JASPWidgets.collection = Backbone.Model.extend({
	defaults: {
		title:			'',
		titleFormat:	'h3',
		collection:		[],
		name:			"",
		initCollapsed:	false
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

		this.listenTo(this.model, 'change:collapsed', this.onCollapsedChange);
	},

	setNoteBox: function (key, localKey, noteBox) {
		this.noteBox = noteBox;
		if (this.indentChildren)
			noteBox.$el.addClass('jasp-indent');
		this.noteBoxKey = key;
		this.noteBoxLocalKey = localKey;
		this.views.unshift(noteBox);
	},

	setUserData: function (details, data) {
		this.userDataDetails = details;
		this.settingUserData = true;
		if (data !== null) {
			if (data.collapsed !== undefined)
				this.model.set("collapsed", data.collapsed);
		}
		this.settingUserData = false;
	},

	getLocalUserData: function () {

		var hasData = false;

		var userData = {};

		if (this.$el.hasClass('jasp-collapsed')) {
			userData.collapsed = true;
			hasData = true;
		}

		if (this.noteBox.visible) {

			var noteData = {};

			if (this.noteBox.isTextboxEmpty())
				noteData.text = '';
			else
				noteData.text = this.noteBox.model.get('text');

			noteData.format = 'html';
			noteData.visible = this.noteBox.visible;
			noteData.delta = this.noteBox.model.get('delta');
			noteData.deltaAvailable = this.noteBox.model.get('deltaAvailable')

			userData[this.noteBoxLocalKey] = noteData;

			hasData = true;
		}

		if (hasData)
			return userData;
		else
			return null;
	},

	indentChildren: true,

	notesMenuClicked: function (noteType, visibility) {

		this.noteBox.setVisibilityAnimate(visibility);
		return true;
	},

	showDependenciesClicked:	function()  { this.model.trigger("ShowDependencies:clicked", this.model.get("name")); },
	collapseMenuClicked:		function()  { this.setCollapsedState(!this.model.get('collapsed'));	},
	noteOptions:				function()  { return [ { key: this.noteBoxKey, visible: this.noteBox.visible } ];			},
	hasNotes:					function()  { return this.model.get('name') !== null;															},
	isCollapsed:				function()  { return this.model.get('collapsed'); },
	_hoveringStart:				function(e) { this.toolbar.setVisibility(true);},
	_hoveringEnd:				function(e) { this.toolbar.setVisibility(false);},
	_mouseClicked: function (e) {	},

	eventEcho:					function (eventName, arg1, arg2, arg3, arg4, arg5, arg6, arg7) { this.trigger(eventName, arg1, arg2, arg3, arg4, arg5, arg6, arg7); },

	collapseOptions: function()
	{
		var collapsed = this.model.get('collapsed');
		return { menuText: (collapsed ? 'Expand' : 'Collapse'), collapsed: collapsed };
	},


	onCollapsedChange: function()
	{
		if (!this.settingUserData)
			this.$el.trigger("changed:userData", [this.userDataDetails, [{ key: 'collapsed', value: this.isCollapsed() }]]);
	},

	setCollapsedState: function (collapsed) {
		var self = this;
		if (collapsed)
		{
			window.slideAlpha(this.$el, 300, ['border-color', 'background-color'], [1, 0.5], 10, true, function () { self.$el.addClass('jasp-collapsed'); });
			this.$body.slideUp(300);
		}
		else
		{
			window.slideAlpha(self.$el, 600, ['border-color', 'background-color'], [0, 0], 20, true, function () {	self.$el.removeClass('jasp-collapsed'); });
			this.$body.slideDown(300);
		}

		this.model.set('collapsed', collapsed);
	},


	events: {
		'mouseenter': '_hoveringStart',
		'mouseleave': '_hoveringEnd',
		'click': '_mouseClicked',
	},


	constructChildren: function (constructor, data) {
		if (Array.isArray(data.meta)) { // the meta comes from a jaspResult analysis
			_.each(data.meta, function (meta) {
				/*if (meta.type == "collection" && data.collection[meta.name].title == "") { // remove collections without a title from view
					var dataWithSkip = jQuery.extend(true, {}, data);
					dataWithSkip.collection = dataWithSkip.collection[meta.name]["collection"];
					dataWithSkip.meta = meta.meta;
					this.constructChildren(constructor, dataWithSkip);
				} else {*/
					var itemResults = data.collection[meta.name];
					data.meta = meta;
					let itemView = constructor.call(this, itemResults, data, true);
					this.pushViews(itemView);
				//}
			}, this);
		} else {
			_.each(data.collection, function (itemResults) {
				var itemView = constructor.call(this, itemResults, data, true);
				this.pushViews(itemView);
			}, this);
		}
	},

	pushViews: function(itemView) {
		if (itemView !== null) {
			itemView.inCollection = true;

			if (itemView.resizer)
				this.listenTo(itemView.resizer, "ResizeableView:resizeStart", this.onResizingStart);

			this.listenTo(itemView, "all", this.eventEcho)
			this.views.push(itemView);
			this.localViews.push(itemView);
		}
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
                {
					//itemView.resizer.resizeStart(w, h, true);
                }
			}
		}
	},

	onViewResized: function (w, h) {
		for (var i = 0; i < this.localViews.length; i++) {
			var itemView = this.localViews[i];
			if (itemView.resizer) {
                if (!itemView.resizer.isMouseResizing()) {
					//itemView.resizer.resizeView(w, h);
                }
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
                {
					//itemView.resizer.resizeStop(w, h);
                }
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
			this.toolbar.title		= title;
			this.toolbar.titleTag	= titleFormat;

			if(title !== "")
			{
				this.toolbar.render();
				this.$el.append(this.toolbar.$el);
			}
		}

		if(this.model.get("collapsed") === undefined && this.model.get("initCollapsed") !== undefined)
			this.model.set("collapsed", this.model.get("initCollapsed"));

		var styleAttr = '';
		var collapsed = this.model.get("collapsed") ;
		styleAttr = collapsed ? ' style="display: none;"' : '';
		if (collapsed)
			this.$el.addClass('jasp-collapsed');
		this.$body = $('<div class="object-body"' + styleAttr + '></div>');


		for (var i = 0; i < this.views.length; i++) {
			var itemView = this.views[i];
			this.onItemRender(itemView);
			this.$body.append(itemView.$el);
		}

		this.$el.append(this.$body);
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
	},


	 getCitations: function() {
		 var cites = [];

		 for (var i = 0; i < this.views.length; i++)
		 {

			var optCitation = null;
			if (this.views[i].getCitations)
				optCitation = this.views[i].getCitations();

			 if (optCitation !== null)
				 cites = cites.concat(optCitation);
		 }

		 return cites;
	 }
});
