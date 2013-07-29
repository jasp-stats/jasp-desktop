$.widget("jasp.tables", {

    options: {
        tables : [ ]
    },
    _create: function () {

        this.element.addClass("jasp-tables")

        this.tables = $(this.element)

        this.refresh()
    },
    /*_setOption: function (key, value) {
        if (key === "value") {
            value = this._constrain(value)
        }
        this._super(key, value)
    },*/
    _setOptions: function (options) {
        this._super(options)

        this.refresh()
    },
    refresh: function () {

        this.tables.empty()

        if (this.options.tables && $.isArray(this.options.tables) && this.options.tables.length > 0)
        {
            for (var i = 0; i < this.options.tables.length; i++)
            {
                var table = $('<div class="jasp-tables-table"></div>')
                table.table(this.options.tables[i])
                this.tables.append(table)
            }

        }
        else {

        }

    },
    _destroy: function () {
        this.element.removeClass("jasp-tables").text("")
    }
})
