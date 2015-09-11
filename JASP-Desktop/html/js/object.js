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
		includeNamespace = " jasp-" + newNamespace + " ";
	}

	var indentClass = '';
	if (embeddedLevel > 1)
		indentClass = ' jasp-indent';

	var itemModel = new JASPWidgets[type](results);
	var itemView = new JASPWidgets[type + "View"]({ className: includeNamespace + "jasp-" + type + " jasp-view" + indentClass, model: itemModel });

	itemModel.on("CustomOptions:changed", function (options) {

		this.trigger("CustomOptions:changed", options)
	}, this.model);

	if (ignoreEvents === false) {
		this.listenTo(itemView, "toolbar:showMenu", function (obj, options) {

			this.trigger("toolbar:showMenu", obj, options);
		});
	}

	if (itemView.setObjectConstructor) {
		itemView.setObjectConstructor(JASPWidgets.objectConstructor, { meta: metaData.meta, status: results.status, namespace: newNamespace, childOfCollection: results.collection !== undefined, embeddedLevel: embeddedLevel + 1 });
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

		this.toolbar = new JASPWidgets.Toolbar({ className: "jasp-toolbar" })
		this.toolbar.setParent(this);
	},

	events: {
		'mouseenter': '_hoveringStart',
		'mouseleave': '_hoveringEnd',
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

	_hoveringStart: function (e) {
		this.toolbar.setVisibility(true);
	},

	_hoveringEnd: function (e) {
		this.toolbar.setVisibility(false);
	},

	setObjectConstructor: function (constructor, data) {
		var meta = data.meta;
		var status = this.model.get("status");
		for (var i = 0; i < meta.length; i++) {

			var modelData = this.model.get(meta[i].name);
			if (modelData) {
				var itemView = constructor.call(this, modelData, { meta: meta[i], status: status, namespace: data.namespace, childOfCollection: false, embeddedLevel: data.embeddedLevel }, false);
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

	render: function () {
		var $innerElement = this.$el;

		var title = this.model.get("title");
		var titleFormat = this.model.get("titleFormat")

		this.toolbar.titleTag = titleFormat;
		this.toolbar.title = title;
		this.toolbar.render();
		$innerElement.append(this.toolbar.$el);

		for (var i = 0; i < this.views.length; i++) {
			var itemView = this.views[i];
			itemView.render();
			this.$el.append(itemView.$el);
		}

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