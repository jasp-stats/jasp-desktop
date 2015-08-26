JASPWidgets.tables = Backbone.Collection.extend({

	model: JASPWidgets.table,
});

JASPWidgets.tablesView = JASPWidgets.CollectionView.extend({

	exportUseNBSP: true,

	createItemView: function (item) {
		return new JASPWidgets.tableView({ className: "jasp-tables-table jasp-table", model: item });
	},
});