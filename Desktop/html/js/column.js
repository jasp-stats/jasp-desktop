JASPWidgets.column = Backbone.Model.extend({

	defaults: {
	}
});

JASPWidgets.columnView = JASPWidgets.objectView.extend({

	menuName: "column",

	constructChildren: function (constructor, data) {
		return undefined;
	},

	disableTitleExport: true,
});
