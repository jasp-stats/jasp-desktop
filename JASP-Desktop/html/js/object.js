JASPWidgets.object = Backbone.Model.extend({
	defaults: {
		title: '',
		status: 'waiting',
	}
});

JASPWidgets.objectConstructor = function (results, params, ignoreEvents) {
	var metaData = params.meta;
	var status = params.status;
	var name = metaData.name;
	var childOfCollection = params.childOfCollection;
	var indent = params.indent;
	var embeddedLevel = params.embeddedLevel;
	var namespace = params.namespace;
	var type = metaData;
	if (metaData.type)
		type = metaData.type;

	if (!_.has(results, "titleFormat"))
		results.titleFormat = 'h' + (embeddedLevel + 2);

	if (childOfCollection === false && !_.has(results, "name")) //this doesn't work for collection items as there will be no name in the meta
		results.name = name;

	if (!_.has(results, "status"))
		results.status = status;

	/*if (!_.has(results, "title"))
		window.displayWarningMessage("This analysis contains an object with no title.")*/

	var item;
	if (results.collection) {

		if (results.collection.length === 0)
			return null;

		_.each(results.collection, function (subItem) {
			var status = subItem.status;
			if (status === null || status === undefined)
				subItem.status = this.status;
		}, results);
	}

	var newNamespace = type;
	var includeNamespace = "";
	if (namespace !== undefined) {
		newNamespace =  namespace + '-' + type;
		includeNamespace = "jasp-" + newNamespace + " ";
	}

	var otherClasses = '';
	if (indent)
		otherClasses += ' jasp-indent';

	if (childOfCollection)
		otherClasses += ' jasp-collection-item jasp-collection-' + type;

	var itemModel = new JASPWidgets[type](results);
	var itemView = new JASPWidgets[type + "View"]({ className: "jasp-display-item " + includeNamespace + "jasp-" + type + " jasp-view" + otherClasses, model: itemModel });

	itemModel.on("CustomOptions:changed", function (options) {

		this.trigger("CustomOptions:changed", options)
	}, this.model);

    itemModel.on("SaveImage:clicked", function (options) {

        this.trigger("SaveImage:clicked", options)
    }, this.model);

    if (ignoreEvents === false) {
		this.listenTo(itemView, "toolbar:showMenu", function (obj, options) {

			this.trigger("toolbar:showMenu", obj, options);
		});
	}

	if (itemView.constructChildren) {
		var indentChildren = itemView.indentChildren === undefined ? false : itemView.indentChildren;
		itemView.constructChildren(JASPWidgets.objectConstructor, { meta: metaData.meta, status: results.status, namespace: newNamespace, childOfCollection: results.collection !== undefined, embeddedLevel: embeddedLevel + 1, indent: indentChildren });
		if (itemView.hasViews() === false) {
			itemView.close();
			return null;
		}
	}

	return itemView;
};

JASPWidgets.objectView = JASPWidgets.View.extend({
	
	initialize: function () {

		this.views = [];
		this.localViews = [];

		this.$el.addClass('jasp-collapsible');

		this.toolbar = new JASPWidgets.Toolbar({ className: "jasp-toolbar" })
		this.toolbar.setParent(this);

		this.listenTo(this.model, 'change:collapsed', this.onCollapsedChange);
	},

	events: {
		'mouseenter': '_hoveringStart',
		'mouseleave': '_hoveringEnd',
	},
	
	setNoteBox: function (key, localKey, noteBox) {
		this.noteBox = noteBox;

		if (this.indentChildren)
			noteBox.$el.addClass('jasp-indent');
		
		this.noteBoxKey = key;
		this.noteBoxLocalKey = localKey;
		if (this.notePositionBottom)
			this.views.push(noteBox);
		else
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

		if (this.noteBox && this.noteBox.visible) {

			var noteData = {};
			
			if (this.noteBox.isTextboxEmpty())
				noteData.text = '';
			else
				noteData.text = Mrkdwn.fromHtmlText(this.noteBox.model.get('text'));
			noteData.format = 'markdown';
			noteData.visible = this.noteBox.visible;

			userData[this.noteBoxLocalKey] = noteData;

			hasData = true;
		}

		if (hasData)
			return userData;
		else
			return null;
	},

	notesMenuClicked: function (noteType, visibility) {

		this.noteBox.setVisibilityAnimate(visibility);

		return true;
	},

	noteOptions: function () {
		if (this.noteBox) {
			var options = { key: this.noteBoxKey, menuText: 'Add Note', visible: this.noteBox.visible };

			return [options];
		}

		return null;
	},

	hasNotes: function () {
		return this.model.get('name') !== null;
	},

	_hoveringStart: function (e) {
		this.toolbar.setVisibility(true);
	},

	_hoveringEnd: function (e) {
		this.toolbar.setVisibility(false);
	},

	constructChildren: function (constructor, data) {
		var meta = data.meta;
		var status = this.model.get("status");
		for (var i = 0; i < meta.length; i++) {

			var modelData = this.model.get(meta[i].name);
			if (modelData) {
				var itemView = constructor.call(this, modelData, { meta: meta[i], status: status, namespace: data.namespace, childOfCollection: false, embeddedLevel: data.embeddedLevel, indent: data.indent }, false);
				if (itemView !== null) {
					this.localViews.push(itemView);
					this.views.push(itemView);
				}
			}
		}
	},

	hasViews: function() {
		return this.localViews.length > 0;
	},

	menuName: "Object",

	attachToolbar: function($toolbar) {
		this.$el.prepend(this.toolbar.$el);
	},

	render: function () {
		var $innerElement = this.$el;

		var title = this.model.get("title");
		var titleFormat = this.model.get("titleFormat")
		if (this.titleFormatOverride)
			titleFormat = this.titleFormatOverride;

		this.toolbar.titleTag = titleFormat;
		this.toolbar.title = title;
		this.toolbar.render();

		var collapsed = this.model.get("collapsed");

		var styleAttr = '';
		styleAttr = collapsed ? ' style="display: none;"' : '';

		if (collapsed)
			this.$el.addClass('jasp-collapsed');

		this.$body = $('<div class="object-body"' + styleAttr + '></div>');

		for (var i = 0; i < this.views.length; i++) {
			var itemView = this.views[i];
			itemView.render();
			this.$body.append(itemView.$el);
		}

		this.$el.append(this.$body);

		this.attachToolbar(this.toolbar.$el);

		if (this.onRender)
			this.onRender();

		return this;
	},

	onClose: function () {
		for (var i = 0; i < this.localViews.length; i++)
			this.localViews[i].close();

		this.localViews = [];
		this.views = [];

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

	collapseOptions: function () {
		var collapsed = this.model.get('collapsed');

		var text = collapsed ? 'Expand' : 'Collapse';

		return { menuText: text, collapsed: collapsed };
	},

	setCollapsedState: function(collapsed) {
		var self = this;
		if (collapsed) {		
			window.slideAlpha(this.$el, 300, ['border-color', 'background-color'], [1, 0.5], 10, true, function () {
				self.$el.addClass('jasp-collapsed');
			});
			this.$body.slideUp(300);
		}
		else {
			window.slideAlpha(self.$el, 600, ['border-color', 'background-color'], [0, 0], 20, true, function () {
				self.$el.removeClass('jasp-collapsed');
			});
			this.$body.slideDown(300);
		}
		this.model.set('collapsed', collapsed);
	},

	isCollapsed: function() {
		var collapsed = this.model.get('collapsed')
		if (collapsed)
			return true;

		return false;
	},

	collapseMenuClicked: function () {
		var collapsed = this.model.get('collapsed');
		this.setCollapsedState(!collapsed);
	},

	onCollapsedChange: function() {
		if (!this.settingUserData)
			this.$el.trigger("changed:userData", [this.userDataDetails, [{ key: 'collapsed', value: this.isCollapsed() }]]);
	},

	indentChildren: true,

	exportUseNBSPDefault: true,

	disableTitleExport: false,

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
