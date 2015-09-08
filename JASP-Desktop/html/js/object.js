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
	var namespace = params.namespace;
	var type = metaData;
	if (metaData.type)
		type = metaData.type;

	if (!_.has(results, "status"))
		results.status = status;

	if (childOfCollection === false && !_.has(results, "name")) //this doesn't work for collection items as there will be 
		results.name = name;

	var item;
	if (results.collection) {
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

	var itemModel = new JASPWidgets[type](results);
	var itemView = new JASPWidgets[type + "View"]({ className: includeNamespace + "jasp-" + type + " jasp-view", model: itemModel });

	itemModel.on("CustomOptions:changed", function (options) {

		this.trigger("CustomOptions:changed", options)
	}, this.model);

	if (ignoreEvents === false) {
		this.listenTo(itemView, "toolbar:showMenu", function (obj, options) {

			this.trigger("toolbar:showMenu", obj, options);
		});
	}

	if (itemView.setObjectConstructor)
		itemView.setObjectConstructor(JASPWidgets.objectConstructor, { meta: metaData.meta, status: results.status, namespace: newNamespace, childOfCollection: results.collection !== undefined });

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
		var meta = data;
		var status = this.model.get("status");
		for (var i = 0; i < meta.length; i++) {

			var modelData = this.model.get(meta[i].name);
			if (modelData) {
				var itemView = constructor.call(this, modelData, constructor, { meta: meta[i].meta, status: status }, false);
				this.localViews.push(itemView);
			}
		}
	},

	menuName: "Object",

	render: function () {
		var $innerElement = this.$el;

		var title = this.model.get("title");

		this.toolbar.titleTag = "h3";
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
	}
});