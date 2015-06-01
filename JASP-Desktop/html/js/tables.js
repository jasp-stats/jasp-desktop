JASPWidgets.tables = Backbone.Collection.extend({

	model: JASPWidgets.table,
});

JASPWidgets.tablesView = JASPWidgets.CollectionView.extend({

	createItemView: function (item) {
		return new JASPWidgets.tableView({ className: "jasp-tables-table jasp-table", model: item });
	},
});

/*$.widget("jasp.tables", {

    options: {
        items : [ ],
        status : "waiting"
    },
    _create: function () {

        this.element.addClass("jasp-tables")

        this.tables = $(this.element)

        this.refresh()
    },
    _setOptions: function (options) {
        this._super(options)

        this.refresh()
    },
    refresh: function () {

        this.tables.empty()

        if (this.options.items && $.isArray(this.options.items) && this.options.items.length > 0)
        {
            for (var i = 0; i < this.options.items.length; i++)
            {
            	var options = this.options.items[i]
            	if ( ! options["status"])
            		options["status"] = this.options.status
            
                var table = $('<div class="jasp-tables-table"></div>')
                table.table(options)
                this.tables.append(table)
            }

        }
        else {

        }

    },
    _destroy: function () {
        this.element.removeClass("jasp-tables").text("")
    }
})*/
